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

.buildDemoPatientLevelSql <- function(tsm){

  demoLines <- tsm |>
    dplyr::filter(
      grepl("Demographic", lineItemClass)
    )


  statType <- demoLines |>
    dplyr::pull(personLineTransformation) |>
    unique()

  sqlDemographicsPath <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "demographics")
  )

  # concept demographic
  if (any(statType == "binary")) { # this label will change
    valueId <- demoLines$valueId
    valueDescription <- demoLines$valueDescription
    demoConceptSql <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoConcept.sql")) |>
      glue::glue() |>
      glue::glue_collapse("\n\n")
  } else{
    demoConceptSql <- ""
  }

  # concept age
  if (any(statType == "AgeDemographic")) { # this label will change
    demoAgeSql <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoAge.sql")) |>
      glue::glue() |>
      glue::glue_collapse("\n\n")
  } else{
    demoAgeSql <- ""
  }

  sql <- c(demoConceptSql, demoAgeSql) |>
    glue::glue_collapse(sep = "\n\n")

}


.buildOccurrencePatientLevelSql <- function(tsm) {

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
      readr::read_file()
  } else{
    anyCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "observedCount")) { # this label will change
    observedCountSql <- fs::path(sqlConceptSetPath, "observedCount.sql") |>
      readr::read_file()
  } else{
    observedCountSql <- ""
  }

  # concept set timeTo
  if (any(statType == "timeToFirst")) {
    timeToFirstSql <- fs::path(sqlConceptSetPath, "timeToFirst.sql") |>
      readr::read_file()
  } else{
    timeToFirstSql <- ""
  }

  sql <- c(anyCountSql, observedCountSql, timeToFirstSql) |>
    glue::glue_collapse(sep = "\n\n")

  return(sql)


}



.buildCohortPatientLevelSql <- function(tsm) {

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
      readr::read_file()
  } else{
    anyCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "observedCount")) { # this label will change
    observedCountSql <- fs::path(sqlConceptSetPath, "observedCount.sql") |>
      readr::read_file()
  } else{
    observedCountSql <- ""
  }

  # concept set timeTo
  if (any(statType == "timeToFirst")) {
    timeToFirstSql <- fs::path(sqlConceptSetPath, "timeToFirst.sql") |>
      readr::read_file()
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
        CREATE TABLE #pat_ts_tab AS
        SELECT
          a.*, b.ordinal_id, b.section_label, b.line_item_label,
          b.value_description, b.statistic_type,
          b.aggregation_type, b.line_item_class
        FROM @patient_data a
        JOIN @ts_meta b
        ON a.value_id = b.value_id AND a.time_label = b.time_label;" |>
    SqlRender::render(
      patient_data = buildOptions$patientLevelDataTempTable,
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
      aggSqlBind[[i]] <- SqlRender::render(
        sql = aggSqlTb2$aggSql[i],
        categorical_table = aggSqlTb2$aggTable[i]
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
        continuous_table = aggSqlTb2$aggTable[i]
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
      targetCohortId, ordinalId
    ) |>
    dplyr::distinct()

  return(categoricalResults)

}



# Archive ------------------------
#
#
# .isLineItemContinuous <- function(statType) {
#   if (statType %in% c("Age", "Year", "Count")) {
#     check <- TRUE
#   } else {
#     check <-FALSE
#   }
#   return(check)
# }
#
# .getLineItemClassType <- function(li, classType) {
#   lineClasses <- purrr::map_chr(li, ~class(.x)[1])
#   li <- li[which(lineClasses == classType)]
#   return(li)
# }
#
#
# # function to build Concept Set Meta table; route concept set build
# .conceptSetMeta <- function(csLineItems) {
#
#   ord <- tibble::tibble(
#     tsOrd = purrr::map_int(csLineItems, ~.x$ordinal)
#   )
#   # get concept set Ref
#   csMeta <- purrr::map_dfr(csLineItems, ~.x$getConceptSetRef()) |>
#     dplyr::mutate(
#       twLabel = glue::glue("{lb}d to {rb}d")
#     ) |>
#     dplyr::mutate(
#       tsCsId = dplyr::row_number(),
#       .before = 1
#     )
#   csMeta <- dplyr::bind_cols(ord, csMeta)
#   # get the distinct concept sets
#   distinct_cs <- csMeta |>
#     dplyr::select(
#       name, hash
#     ) |>
#     dplyr::distinct() |>
#     dplyr::mutate(
#       csId = dplyr::row_number(), .before = 1
#     )
#   # add back to csMeta
#   csMeta <- csMeta |>
#     dplyr::left_join(
#       distinct_cs, by = c("name", "hash")
#     )
#
#   # get distinct tw
#   distinct_tw <- csMeta |>
#     dplyr::select(
#       lb, rb, twLabel
#     ) |>
#     dplyr::distinct() |>
#     dplyr::mutate(
#       twId = dplyr::row_number(), .before = 1
#     )
#   # add back to csMeta
#   csMeta <- csMeta |>
#     dplyr::left_join(
#       distinct_tw, by = c("lb", "rb", "twLabel")
#     )
#   # get csId dm combos
#   csDmCombos <- csMeta |>
#     dplyr::select(csId, domain) |>
#     dplyr::distinct() |>
#     dplyr::group_by(domain) |>
#     dplyr::mutate(
#       csIdSet = paste(csId, collapse = ", ")
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::distinct()
#
#   # add back to csMeta
#   csMeta <- csMeta |>
#     dplyr::left_join(
#       csDmCombos, by = c("csId", "domain")
#     )
#
#   # get twId dm combos
#   twDmCombos <- csMeta |>
#     dplyr::select(twId, domain) |>
#     dplyr::distinct() |>
#     dplyr::group_by(domain) |>
#     dplyr::mutate(
#       twIdSet = paste(twId, collapse = ", ")
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::distinct()
#
#   # add back to csMeta
#   csMeta <- csMeta |>
#     dplyr::left_join(
#       twDmCombos, by = c("twId", "domain")
#     )
#
#   # Identify the temp tables
#   csTempTables <- csMeta |>
#     dplyr::select(csIdSet, twIdSet, domain) |>
#     dplyr::distinct() |>
#     dplyr::group_by(
#       domain
#     ) |>
#     dplyr::mutate(
#       'tempTableName' = glue::glue("#{domain}_{dplyr::row_number()}")
#     ) |>
#     dplyr::ungroup()
#
#   # add back to csMeta
#   csMeta <- csMeta |>
#     dplyr::left_join(
#       csTempTables , by = c("csIdSet", "twIdSet", "domain")
#     )
#
#   # Identify the stat type
#   statTb <- purrr::map_dfr(csLineItems, ~.x$getStatisticInfo())
#
#   # add back to csMeta
#   csMeta <- csMeta |>
#     dplyr::left_join(
#       statTb , by = c("tsOrd" = "ord")
#     )
#
#   # create the bitwise id
#   bitId <- csMeta |>
#     dplyr::select(tsCsId, tempTableName, sql) |>
#     dplyr::group_by(tempTableName, sql) |>
#     dplyr::summarize(
#       categoryId = sum(2^tsCsId),
#       .groups = "keep"
#     ) |>
#     dplyr::select(categoryId, tempTableName, sql)
#
#
#   # add back to csMeta
#   csMeta <- csMeta |>
#     dplyr::left_join(
#       bitId , by = c("tempTableName", "sql")
#     )
#
#   return(csMeta)
#
# }
#
#
# #
# # domain_translate <- function(domain) {
# #
# #   tt <- switch(domain,
# #                "condition_occurrence" = list(
# #                  'record_id' = "condition_occurrence_id",
# #                  'concept_id' ="condition_concept_id",
# #                  'concept_type_id' = "condition_type_concept_id",
# #                  'event_date' = "condition_start_date",
# #                  'source_concept_id' = "condition_source_concept_id"
# #                ),
# #                "drug_exposure" = list(
# #                  'record_id' = "drug_exposure_id",
# #                  'concept_id' = "drug_concept_id",
# #                  'concept_type_id' = "drug_type_concept_id",
# #                  'event_date' = "drug_exposure_start_date",
# #                  'source_concept_id' = "drug_source_concept_id"
# #                ),
# #                "procedure_occurrence" = list(
# #                  'record_id' = "procedure_occurrence_id",
# #                  'concept_id' = "procedure_concept_id",
# #                  'concept_type_id' = "procedure_type_concept_id",
# #                  'event_date' = "procedure_date",
# #                  'source_concept_id' = "procedure_source_concept_id"
# #                ),
# #                "observation" = list(
# #                  'record_id' = "observation_id",
# #                  'concept_id' = "observation_concept_id",
# #                  'concept_type_id' = "observation_type_concept_id",
# #                  'event_date' = "observation_date",
# #                  'source_concept_id' = "observation_source_concept_id"
# #                ),
# #                "device_exposure" = list(
# #                  'record_id' = "device_exposure_id",
# #                  'concept_id' = "device_concept_id",
# #                  'concept_type_id' = "device_type_concept_id",
# #                  'event_date' = "device_exposure_start_date",
# #                  'source_concept_id' = "device_source_concept_id"
# #                ),
# #                "measurement" = list(
# #                  'record_id' = "measurement_id",
# #                  'concept_id' = "measurement_concept_id",
# #                  'concept_type_id' = "measurement_type_concept_id",
# #                  'event_date' = "measurement_date",
# #                  'source_concept_id' = "measurement_source_concept_id"
# #                ),
# #                "visit_occurrence" = list(
# #                  'record_id' = "visit_occurrence_id",
# #                  'concept_id' = "visit_concept_id",
# #                  'concept_type_id' = "visit_type_concept_id",
# #                  'event_date' = "visit_start_date",
# #                  'source_concept_id' = "visit_source_concept_id"
# #                ),
# #                "provider" = list(
# #                  'concept_id' = "specialty_concept_id",
# #                  'merge_key' = "provider_id"
# #                ),
# #                "care_site" = list(
# #                  'concept_id' = "place_of_service_concept_id",
# #                  'merge_key' = "care_site_id"
# #                ),
# #                "gender" = list('concept_id' ="gender_concept_id"),
# #                "race" = list('concept_id' = "race_concept_id"),
# #                "ethnicity" = list('concept_id' = "ethnicity_concept_id")
# #   )
# #   return(tt)
# # }
#
#
#
# .prepCsExtract <- function(csIdSet, twIdSet, domain, tempTableName) {
#
#   # change names for glue
#   timeIds <- twIdSet
#   codesetIds <- csIdSet
#   csTempTableName <- tempTableName
#
#   # translate domains to correct column names
#   domain_trans <- domain_translate(domain)
#
#   # get conceptSet Sql
#   sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", "conceptSetQuery.sql")) |>
#     readr::read_file() |>
#     glue::glue()
#
#   return(sql)
# }
#
#
#
# .prepCsTransform <- function(categoryId, tempTableName, sql) {
#
#   # change names for glue
#   csTempTableName <- tempTableName
#   catId <- categoryId
#   # get conceptSet Sql
#   sql <- sql |>
#     glue::glue()
#
#   return(sql)
# }
#
# #TODO fix this to work with counts on a break
# .findConceptSetCategoryIds <- function(li) {
#
#   # of the categorical, find the unique cs group ids
#   csCatIds <- li |>
#     .getLineItemClassType(classType = "ConceptSetLineItem") |>
#     .conceptSetMeta() |>
#     dplyr::select(statType, categoryId) |>
#     dplyr::distinct() |>
#     dplyr::mutate(
#       distributionType = ifelse(statType == "Presence", "categorical", "continuous"),
#       tsClass = "ConceptSetLineItem"
#     ) |>
#     dplyr::select(categoryId, tsClass, distributionType)
#
#   return(csCatIds)
# }
#
# #TODO find regular categorical and those with breaks
# .findDemographicCategoryIds <- function(li) {
#   # of the categorical, find the unique demo ids
#   demoCatLi <- li|>
#     .getLineItemClassType(classType = "DemographicLineItem")
#
#
#   demoCatIds <- tibble::tibble(
#     'categoryId' = purrr::map_int(demoCatLi, ~.x$ordinal),
#     'tsClass' = "DemographicLineItem",
#     'distributionType' = purrr::map_chr(demoCatLi, ~.x$identifyStatType())
#   )
#
#
#   return(demoCatIds)
# }
