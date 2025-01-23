.targetCohortLabels <- function(tcList) {
  tcl <- tibble::tibble(
    targetCohortId = purrr::map_dbl(tcList, ~.x$getId()),
    targetCohortName = purrr::map_chr(tcList, ~.x$getName())
  )
  return(tcl)
}

.opConverter <- function(op) {
  op <- switch(op,
               'at_least' = '>=',
               'at_most' = '<=',
               'exactly' = '=')
  return(op)
}

.findLineItemId <- function(lineItems, classType) {
  # get the class types
  lineClasses <- purrr::map_chr(lineItems, ~.x$lineItemClass)
  lineItemIds <- which(lineClasses == classType)
  return(lineItemIds)
}

# .getCsFromR6 <- function(lineItems) {
#
#   idsToPluck <- c(
#     .findLineItemId(lineItems = lineItems, classType = "ConceptSet"),
#     .findLineItemId(lineItems = lineItems, classType = "ConceptSetGroup")
#   )
#   filteredLineItems <- lineItems[idsToPluck]
#
#   csCapr <- purrr::map(
#     filteredLineItems,
#     ~.x$grabConceptSet()
#   ) |>
#     purrr::list_flatten()
#
#
#   # cs_id <- !duplicated(purrr::map_chr(csCapr, ~.x@id))
#   # cs_tbl2 <- csCapr[cs_id]
#
#   return(csCapr)
# }

.getCaprCs <- function(lineItems) {
  # get list ids for class types
  conceptSetLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSet")
  conceptSetGroupLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSetGroup")

  #make a subsetting vector of list ids
  idsToPluck <- c(conceptSetLineItems, conceptSetGroupLineItems)
  #filted line items to those with concepts
  filteredLineItems <- lineItems[idsToPluck]

  # get the capr concept sets for all the concepts
  caprCs <- purrr::map(
    filteredLineItems,
    ~.x$grabConceptSet()
  ) |>
    purrr::list_flatten()
  return(caprCs)
}

.caprToMetaTable <- function(caprCs) {
  # make a table identifying the codeset id for the query, unique to each cs
  tb <- tibble::tibble(
    id = purrr::map_chr(caprCs, ~.x@id),
    name = purrr::map_chr(caprCs, ~.x@Name)
  ) |>
    dplyr::mutate(
      csId = dplyr::dense_rank(id)
    ) |>
    tibble::rownames_to_column(var = "rowId")
  return(tb)
}


.setCsValueId <- function(lineItems) {
  # get list ids for class types
  conceptSetLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSet")
  conceptSetGroupLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSetGroup")

  # get the capr concept sets for all the concepts
  caprCs <- .getCaprCs(lineItems)

  # make a table identifying the codeset id for the query, unique to each cs
  tb <- .caprToMetaTable(caprCs)

  if (length(conceptSetGroupLineItems) > 0) {
    # get length of each conceptSetGroup
    csgLiLength <- lineItems[conceptSetGroupLineItems] |>
      purrr::map_int(~length(.x$grabConceptSet()))
    # make full list of ord ids
    fullListOfIds <- c(
      conceptSetLineItems,
      rep(conceptSetGroupLineItems, each = csgLiLength) # rep each group by num of concepts in group
    )
  } else {
    fullListOfIds <- c(
      conceptSetLineItems
    )
  }


# add the full list to tb to identify the ordinal of csId
  tb <- tb |>
    dplyr::mutate(
      ord = fullListOfIds
    )


  for (i in 1:nrow(tb)) {

    ordId <- tb$ord[i] # plock ord
    csId <- tb |> # get vector of csId corresponding to ord slot. if cs only 1 if csg more than 1
      dplyr::filter(
        ord == ordId
      ) |>
      dplyr::pull(csId)

    lineItems[[ordId]]$valueId <- csId
  }

  return(lineItems)

}



# function to get timeInterval and concept set / cohort combinations
.permuteTi <- function(lineItemObjects, timeIntervals) {

  # get number of items for each
  numObj <- length(lineItemObjects)
  numTis <- length(timeIntervals)

  # build out permutations
  objPerm <- rep(lineItemObjects, times = numTis)
  tiPerm <- rep(timeIntervals, each = numObj)

  permTiObj <- list(
    'objects' = objPerm,
    'timeIntervals' = tiPerm
  )
  return(permTiObj)

}



# function that translates the columns per domain
.domainTranslate <- function(domain) {
  # read domainTranslation file
  dt <- readr::read_csv(
    fs::path_package(package = "ClinicalCharacteristics", fs::path("csv", "domainTranslation.csv")),
    show_col_types = FALSE
  ) |>
    dplyr::filter(
      domain == !!domain # filter to domain of interest
    )

  return(dt)

}

.prepConceptSetOccurrenceQuerySql <- function(csTables, domain) {

  domainGroup <- csTables |>
    dplyr::filter(
      domainTable == !!domain
    )

  # get the value ids for the domain of interest and create a character string
  # to glue into sql
  codeset_ids <- domainGroup |>
    dplyr::arrange(valueId) |>
    dplyr::pull(valueId) |>
    unique() |>
    glue::glue_collapse(sep = ", ")

  time_labels <- domainGroup |>
    dplyr::arrange(timeLabel) |>
    dplyr::pull(timeLabel) |>
    unique() |>
    glue::glue_collapse(sep = "', '")

  domainTranslation <- .domainTranslate(domain)

  sql <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "conceptSetOccurrenceQuery.sql")
  ) |>
    readr::read_file() |>
    glue::glue()

  return(sql)

}

.truncDropTempTables <- function(tempTableName) {
  sql <- glue::glue("TRUNCATE TABLE {tempTableName}; DROP TABLE {tempTableName};")
  return(sql)
}

## Patient level Sql ---------------------

.buildDemoPatientLevelSql <- function(tsm, executionSettings, buildOptions){

  demoLines <- tsm |>
    dplyr::filter(
      grepl("Demographic", lineItemClass)
    )


  sqlDemographicsPath <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "demographics")
  )

  demoSql <- vector('list', length = nrow(demoLines))
  for (i in 1:nrow(demoLines)) {

    if (demoLines$personLineTransformation[i] == "binary") {
      valueId <- demoLines$valueId[i]
      valueDescription <- demoLines$valueDescription[i]
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoConcept.sql")) |>
        glue::glue() |>
        glue::glue_collapse("\n\n")
    }

    if (demoLines$personLineTransformation[i] == "age") {
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoAge.sql")) |>
        glue::glue_collapse("\n\n")
    }
  }

  demoSql2 <- do.call('c', demoSql) |>
    glue::glue_collapse("\n\n") |>
    SqlRender::render(
      patient_level_data = buildOptions$patientLevelDataTempTable,
      cdm_database_schema = executionSettings$cdmDatabaseSchema,
      target_table = buildOptions$targetCohortTempTable
    )

  return(demoSql2)
}



.buildOccurrencePatientLevelSql <- function(tsm, executionSettings, buildOptions) {

  statTypes <- tsm |>
    dplyr::select(personLineTransformation, lineItemClass) |>
    dplyr::distinct()

  # limit statTYpes to only concept set
  statType <- statTypes |>
    dplyr::filter(
      grepl("ConceptSet", lineItemClass)
    ) |>
    dplyr::pull(personLineTransformation) |>
    unique()

  sqlConceptSetPath <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "conceptSet")
  )

  # concept set anyCount
  if (any(statType == "anyCount")) { # this label will change
    anyCountSql <- fs::path(sqlConceptSetPath, "anyCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable
      )
  } else{
    anyCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "observedCount")) { # this label will change
    observedCountSql <- fs::path(sqlConceptSetPath, "observedCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    observedCountSql <- ""
  }

  # concept set timeTo
  if (any(statType == "timeTo")) {
    timeToSql <- fs::path(sqlConceptSetPath, "timeTo.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
        first = TRUE
      )
  } else{
    timeToFirstSql <- ""
  }

  sql <- c(anyCountSql, observedCountSql, timeToFirstSql) |>
    glue::glue_collapse(sep = "\n\n")

  return(sql)


}



.buildCohortPatientLevelSql <- function(tsm, executionSettings, buildOptions) {

  statTypes <- tsm |>
    dplyr::select(personLineTransformation, lineItemClass) |>
    dplyr::distinct()

  # limit statTYpes to only concept set
  statType <- statTypes |>
    dplyr::filter(
      grepl("Cohort", lineItemClass)
    ) |>
    dplyr::pull(personLineTransformation) |>
    unique()

  sqlConceptSetPath <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "cohort")
  )

  # concept set anyCount
  if (any(statType == "anyCount")) { # this label will change
    anyCountSql <- fs::path(sqlConceptSetPath, "anyCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable
      )
  } else{
    anyCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "observedCount")) { # this label will change
    observedCountSql <- fs::path(sqlConceptSetPath, "observedCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    observedCountSql <- ""
  }

  # concept set timeTo
  if (any(statType == "timeToFirst")) {
    timeToFirstSql <- fs::path(sqlConceptSetPath, "timeToFirst.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
        first = TRUE
      )
  } else{
    timeToFirstSql <- ""
  }

  sql <- c(anyCountSql, observedCountSql, timeToFirstSql) |>
    glue::glue_collapse(sep = "\n\n")

  return(sql)

}


## aggregation sql ------------------

.tempPsDatTable <- function(executionSettings, buildOptions) {

   # Create temp table joining patient date with ts meta
  patTsSql <- "
        DROP TABLE IF EXISTS @pat_ts_tab;
        CREATE TABLE @pat_ts_tab AS
        SELECT
          a.*, b.ordinal_id, b.section_label, b.line_item_label,
          b.value_description, b.statistic_type,
          b.aggregation_type, b.line_item_class
        FROM @patient_data a
        JOIN @ts_meta b
        ON a.value_id = b.value_id AND a.time_label = b.time_label;" |>
    SqlRender::render(
      patient_data = buildOptions$patientLevelDataTempTable,
      pat_ts_tab = buildOptions$patientLevelTableShellTempTable,
      ts_meta = buildOptions$tsMetaTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )
  return(patTsSql)
}

.initAggregationTables <- function(executionSettings, buildOptions) {

  initSummaryTableSql <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql/aggregate", "initSummaryTables.sql")
  ) |>
    readr::read_file() |>
    SqlRender::render(
      categorical_table = buildOptions$categoricalSummaryTempTable,
      continuous_table = buildOptions$continuousSummaryTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  return(initSummaryTableSql)

}

.getDenominator <- function(executionSettings, buildOptions) {

  denomSql <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql/aggregate/denom_any.sql")
  ) |>
    readr::read_file() |>
    SqlRender::render(
      target_cohort_table = buildOptions$targetCohortTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  return(denomSql)

}

.prepBreaksTable <- function(tsm, executionSettings) {

  breakLines <- tsm |>
    dplyr::filter(
      statisticType == "breaks"
    ) |>
    dplyr::pull(ordinalId)


  breakLineItems <- tableShell$getLineItems()[c(breakLines)]
  kk <- purrr::map(breakLineItems, ~.x$getStatistic()) |>
    purrr::map(~.x$getBreaksIfAny())

  sql <- purrr::map2(kk, breakLines,
                     ~.x$makeCaseWhenSql(ordinalId = .y)) |>
    glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

  breaksSql <- glue::glue(
    "DROP TABLE IF EXISTS #pat_ts_break;
    CREATE TABLE #pat_ts_break AS
    {sql};"
  )

  return(breaksSql)
}


.aggregateSql <- function(tsm, executionSettings, buildOptions) {

  aggregateSqlPaths <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql/aggregate")
  )

  # Step 1: Make a table with all the aggregation types
  aggSqlTb <- tibble::tibble(
    aggName = c("presence", "continuousDistribution", "breaks", "score"),
    aggType = c("categorical", "continuous", "categorical", "continuous"),
    aggSqlPath = fs::path(aggregateSqlPaths, aggName, ext = "sql")
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      aggSql = readr::read_file(aggSqlPath) # read in file
    )
  # Step 2: identify the correct temp tables
  aggSqlTb <- aggSqlTb |>
    dplyr::left_join(
      tibble::tibble(
        aggType = c("categorical", "continuous"),
        aggTable = c(buildOptions$categoricalSummaryTempTable, buildOptions$continuousSummaryTempTable)
      ),
      by = c("aggType")
    )

  # Step 3: Find the distinct stat types from the table shell
  statTypes <- tsm |>
    dplyr::select(statisticType) |>
    dplyr::distinct()

  # Step 4: Reduce the agg Sql to only the stat types

  aggSqlTb2 <- aggSqlTb |>
    dplyr::inner_join(statTypes, by = c("aggName" = "statisticType"))

  # Step 5:
  aggSqlBind <- vector('list', length = nrow(aggSqlTb2))
  for (i in 1:nrow(aggSqlTb2)) {

  # if categorical render with table
    if (aggSqlTb2$aggType[i] == "categorical") {

      if (aggSqlTb2$aggName[i] == "breaks") {
        breaksSql <- .prepBreaksTable(tsm, executionSettings)
        aggSql <- c(breaksSql, aggSqlTb2$aggSql[i]) |>
          glue::glue_collapse("\n\n")
      } else {
        aggSql <- aggSqlTb2$aggSql[i]
      }

      aggSqlBind[[i]] <- SqlRender::render(
        sql = aggSql,
        categorical_table = aggSqlTb2$aggTable[i],
        pat_ts_tab  = buildOptions$patientLevelTableShellTempTable
      ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )
    }
    # if continuous render with table
    if (aggSqlTb2$aggType[i] == "continuous") {
      aggSqlBind[[i]] <- SqlRender::render(
        sql = aggSqlTb2$aggSql[i],
        continuous_table = aggSqlTb2$aggTable[i],
        pat_ts_tab  = buildOptions$patientLevelTableShellTempTable
      ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )
    }

  }
  aggSqlBind <- do.call('c', aggSqlBind) |>
    glue::glue_collapse("\n\n")

  return(aggSqlBind)
}


# Get Results ------------------

.getCategoricalResults <- function(tsm, tc, executionSettings, buildOptions) {

  sql <- "SELECT * FROM @categorical_table;" |>
    SqlRender::render(
      categorical_table = buildOptions$categoricalSummaryTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  jj <- DatabaseConnector::querySql(
    connection = executionSettings$getConnection(),
    sql = sql,
    snakeCaseToCamelCase = TRUE
  )

  categoricalResults <- jj |>
    dplyr::inner_join(
      tsm |> dplyr::select(ordinalId, sectionLabel),
      by = c("ordinalId")
    ) |>
    dplyr::left_join(
      tc,
      by = c("targetCohortId")
    ) |>
    dplyr::select(
      targetCohortId, targetCohortName, ordinalId, sectionLabel, lineItemLabel, timeLabel, subjectCount, pct
    ) |>
    dplyr::arrange(
      targetCohortId, ordinalId, lineItemLabel
    ) |>
    dplyr::distinct()

  return(categoricalResults)

}


.getContinuousResults <- function(tsm, tc, executionSettings, buildOptions) {

  sql <- "SELECT * FROM @continuous_table;" |>
    SqlRender::render(
      continuous_table = buildOptions$continuousSummaryTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  jj <- DatabaseConnector::querySql(
    connection = executionSettings$getConnection(),
    sql = sql,
    snakeCaseToCamelCase = TRUE
  )

  continuousResults <- jj |>
    dplyr::inner_join(
      tsm |> dplyr::select(ordinalId, sectionLabel),
      by = c("ordinalId")
    ) |>
    dplyr::left_join(
      tc,
      by = c("targetCohortId")
    ) |>
    dplyr::select(
      targetCohortId, targetCohortName, ordinalId, sectionLabel, lineItemLabel, timeLabel, subjectCount:maximumValue
    ) |>
    dplyr::arrange(
      targetCohortId, ordinalId, lineItemLabel
    ) |>
    dplyr::distinct()

  return(continuousResults)

}
