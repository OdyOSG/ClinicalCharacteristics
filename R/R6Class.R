# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    initialize = function(name,
                          targetCohorts,
                          lineItems) {
      .setString(private = private, key = "name", value = name)
      .setListofClasses(private = private, key = "targetCohorts", value = targetCohorts, classes = c("CohortInfo"))
      #.setClass(private = private, key = "executionSettings", value = executionSettings, class = "ExecutionSettings")
      .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
    },
    getName = function() {
      tsName <- private$name
      return(tsName)
    },

    getTableShellMeta = function() {
      tsLi <- self$getLineItems()
      tsMeta <- purrr::map_dfr(
        tsLi, ~.x$getLineItemMeta()
      )
      return(tsMeta)
    },

    getTargetCohorts = function() {
      tsTargetCohorts <- private$targetCohorts
      return(tsTargetCohorts)
    },
    getLineItems = function() {
      tsLineItems <- private$lineItems
      return(tsLineItems)
    },

    printJobDetails = function() {

      titleNm <- self$getName()

      tcs <- self$getTargetCohorts()
      cohortPrintInfo <- purrr::map_chr(tcs, ~.x$targetCohortDetails())

      # get line item info
      lineItems <- self$getLineItems()
      liPrintInfo <- purrr::map_chr(lineItems, ~.x$lineItemDetails())

      cli::cat_bullet(
        glue::glue_col("{yellow Job Details for Table Shell: {titleNm}}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      cli::cat_bullet(
        glue::glue("Target Cohort Details:"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      cli::cat_line(
        glue::glue("\t{cohortPrintInfo}")
      )


      cli::cat_bullet(
        glue::glue("Line Item tasks:"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      cli::cat_line(
        glue::glue("\t{liPrintInfo}")
      )

      invisible(liPrintInfo)

    },

    # function to instantiate tables for queries
    instantiateTables = function(executionSettings, buildOptions) {

      # ensure R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
      checkmate::assert_class(buildOptions, classes = "BuildOptions", null.ok = FALSE)

      # insert Ts Meta
      private$.insertTsMeta(
        executionSettings = executionSettings,
        buildOptions = buildOptions
      )

      # insert time windows
      private$.insertTimeWindows(
        executionSettings = executionSettings,
        buildOptions = buildOptions
      )

      invisible(executionSettings)

    },

    #key function to generate the table shell
    buildTableShellSql = function(executionSettings, buildOptions) {

      # ensure R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
      checkmate::assert_class(buildOptions, classes = "BuildOptions", null.ok = FALSE)

      cli::cat_bullet(
        glue::glue_col("{yellow Preparing Table Shell Sql}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      # collect all the sql
      fullSql <- c(

        # step 1: Make target Cohort table
        private$.makeTargetCohortTable(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # step 2: create targe cohort table
        private$.buildCodesetQueries(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 3: create concept set query
        private$.buildConceptSetOccurrenceQuery(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 4: transform to patient line data
        private$.transformToPatientLineData(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 5: Aggregate results
        private$.aggregateResults(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        )
      ) |>
        glue::glue_collapse(sep = "\n")


      return(fullSql)

    },

    outputResults = function(executionSettings, buildOptions) {

      tsm <- self$getTableShellMeta()

      tc <- self$getTargetCohorts() |>
        .targetCohortLabels()

      # categorical results
      cat_res <- .getCategoricalResults(tsm, tc, executionSettings, buildOptions) |>
        tibble::as_tibble()

      return(cat_res)
    }

    # function to aggregate categorical vars into table
    # aggregateTableShell = function(executionSettings, type, buildOptions) {
    #
    #   #identify which lineItems are continuous or categorical
    #   idList <- private$.identifyCategoryIds()
    #
    #   # get sql for categorical
    #   if (type == "categorical") {
    #
    #     categoricalIds <- idList |>
    #       dplyr::filter(distributionType == "categorical") |>
    #       dplyr::pull(categoryId) |>
    #       paste(collapse = ", ")
    #
    #
    #     sqlFile <- "aggregateCategorical.sql"
    #     # get sql from package
    #     sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
    #       readr::read_file() |>
    #       glue::glue() |>
    #       SqlRender::render(
    #         workDatabaseSchema = executionSettings$workDatabaseSchema,
    #         cohortTable = executionSettings$targetCohortTable,
    #         dataTable = buildOptions$resultsTempTable
    #       )
    #   }
    #
    #   # get sql for continuous
    #   if (type == "continuous") {
    #
    #     continuousIds <- idList |>
    #       dplyr::filter(distributionType == "continuous") |>
    #       dplyr::pull(categoryId) |>
    #       paste(collapse = ", ")
    #
    #     sqlFile <- "aggregateContinuous.sql"
    #     # get sql from package
    #     sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
    #       readr::read_file() |>
    #       glue::glue() |>
    #       SqlRender::render(
    #         dataTable = buildOptions$resultsTempTable
    #       )
    #   }
    #
    #   finalSql <- sql |>
    #     SqlRender::translate(
    #       targetDialect = executionSettings$getDbms(),
    #       tempEmulationSchema = executionSettings$tempEmulationSchema
    #     )
    #
    #   # get aggregateTable
    #   aggregateTable <- DatabaseConnector::querySql(
    #     connection = executionSettings$getConnection(),
    #     sql = finalSql
    #   ) |>
    #     tibble::as_tibble() |>
    #     dplyr::rename_with(tolower) |>
    #     dplyr::arrange(cohort_id, category_id, time_id, value_id)
    #
    #   # format results
    #   formattedTable <- private$.labelResults(
    #     results = aggregateTable,
    #     type = type
    #   )
    #
    #   return(formattedTable)
    #
    # }


  ),
  private = list(
    name = NULL,
    targetCohorts = NULL,
    lineItems = NULL,


    ### private methods ---------------
    # function to insert tsMeta
    .insertTsMeta = function(executionSettings, buildOptions) {
      # ensure that executionSettings R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      # get tsMeta
      tsMeta <- self$getTableShellMeta() |>
        dplyr::rename_with(snakecase::to_snake_case)

      cli::cat_bullet(
        glue::glue_col("Insert tsMeta table ---> {cyan {buildOptions$tsMetaTempTable}}"),
        bullet = "info",
        bullet_col = "blue"
      )

      # establish connection to database
      connection <- executionSettings$getConnection()

      if (is.null(connection)) {
        connection <- executionSettings$connect()
      }

      # insert the time windows into the database
      DatabaseConnector::insertTable(
        connection = connection,
        tableName = buildOptions$tsMetaTempTable,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        data = tsMeta,
        tempTable = TRUE
      )

      invisible(tsMeta)
    },

    .insertTimeWindows = function(executionSettings, buildOptions) {
      # ensure that executionSettings R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      # get concept set line items
      csLineItems <- self$getTableShellMeta() |>
        dplyr::filter(grepl("ConceptSet", lineItemClass))

      # make the time windows table
      time_tbl <- tibble::tibble(
        time_label = csLineItems$timeLabel
        ) |>
        dplyr::distinct() |>
        tidyr::separate_wider_delim(
          time_label,
          delim = " to ",
          names = c("time_a", "time_b"),
          cols_remove = FALSE
        ) |>
        dplyr::mutate(
          time_a = as.integer(gsub("d", "", time_a)),
          time_b = as.integer(gsub("d", "", time_b))
        ) |>
        dplyr::select(
          time_label, time_a, time_b
        )

      cli::cat_bullet(
        glue::glue_col("Insert timeWindows table ---> {cyan {buildOptions$timeWindowTempTable}}"),
        bullet = "info",
        bullet_col = "blue"
      )

      # establish connection to database
      connection <- executionSettings$getConnection()

      if (is.null(connection)) {
        connection <- executionSettings$connect()
      }

      # insert the time windows into the database
      DatabaseConnector::insertTable(
        connection = connection,
        tableName = buildOptions$timeWindowTempTable,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        data = time_tbl,
        tempTable = TRUE
      )

      invisible(time_tbl)
    },

    # function to get target cohort sql
    .makeTargetCohortTable = function(executionSettings, buildOptions) {

      sqlFile <- "targetCohort.sql"
      cohortIds <- purrr::map_int(
        private$targetCohorts,
        ~.x$getId()
      )

      # get sql from package
      sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
        readr::read_file() |>
        glue::glue()

      renderedSql <- sql |>
        SqlRender::render(
          target_table = buildOptions$targetCohortTempTable,
          work_database_schema = executionSettings$workDatabaseSchema,
          cohort_table = executionSettings$targetCohortTable
        ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      return(renderedSql)
    },



    # function to create sql for codset query
    .buildCodesetQueries = function(executionSettings, buildOptions) {

      #temporary change with class
      codesetTable <-  buildOptions$codesetTempTable

      # get concept set line items
      li <- self$getLineItems()

      # pluck the capr concept sets
      caprCs <- .getCaprCs(li)


      if (length(caprCs) >= 1) {
        #turn into query
        cs_query <- .bindCodesetQueries(caprCs, codesetTable = codesetTable) |>
          SqlRender::render(
            vocabulary_database_schema = executionSettings$cdmDatabaseSchema
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )
      } else{
        cs_query <- ""
      }

      return(cs_query)

    },

    # function to extract concept level information
    .buildConceptSetOccurrenceQuery = function(executionSettings, buildOptions) {

      # Step 1: Get the concept set meta
      csMeta <- self$getTableShellMeta() |>
        dplyr::filter(grepl("ConceptSet", lineItemClass))

      # only run if CSD in ts
      if (!is.null(csMeta)) {

        # Step 2: Prep the concept set extraction
        csTables <- csMeta |>
          dplyr::select(valueId, valueDescription, timeLabel, domainTable)

        # Step 3: get the domains to join
        domainTablesInUse <- unique(csTables$domainTable)
        # step 4: make the concept set occurrence sql
        conceptSetOccurrenceSqlGrp <- purrr::map(
          domainTablesInUse,
          ~.prepConceptSetOccurrenceQuerySql(
            csTables = csTables,
            domain = .x
          )
        ) |>
          glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

        conceptSetOccurrenceSql <- glue::glue(
          "CREATE TABLE @concept_set_occurrence_table AS
          {conceptSetOccurrenceSqlGrp}
          ;
          "
        )
        conceptSetOccurrenceSql <- conceptSetOccurrenceSql |>
          SqlRender::render(
            target_cohort_table = buildOptions$targetCohortTempTable,
            concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
            time_window_table = buildOptions$timeWindowTempTable,
            codeset_table = buildOptions$codesetTempTable,
            cdm_database_schema = executionSettings$cdmDatabaseSchema
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )

      } else {
        conceptSetOccurrenceSql <- ""
      }
      return(conceptSetOccurrenceSql)

    },

    .transformToPatientLineData = function(executionSettings, buildOptions) {

      # Step 1: make patient level data table
      ptDatTbSql <- fs::path_package(
        "ClinicalCharacteristics",
        fs::path("sql", "patientLevelData.sql")
      ) |>
        readr::read_file()

      # Step 2: run patient level queries
      tsm <- self$getTableShellMeta()
      # step 2a demographics

      demoPatientLevelSql <- .buildDemoPatientLevelSql(tsm)

      # step 2b concept set pat level
      csPatientLevelSql <- .buildOccurrencePatientLevelSql(tsm)

      # step 2c cohort pat level
      chPatientLevelSql <- .buildCohortPatientLevelSql(tsm)

      # full sql for sql
      ptFullSql <- c(ptDatTbSql, demoPatientLevelSql, csPatientLevelSql, chPatientLevelSql) |>
        glue::glue_collapse(sep = "\n\n") |>
        SqlRender::render(
          patient_level_data = buildOptions$patientLevelDataTempTable,
          concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
          target_table = buildOptions$targetCohortTempTable,
          cdm_database_schema = executionSettings$cdmDatabaseSchema
        ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      return(ptFullSql)
    },

    .aggregateResults = function(executionSettings, buildOptions) {

      tsm <- self$getTableShellMeta()

      # Create temp table joining patient date with ts meta
      patTsSql <- .tempPsDatTable(executionSettings, buildOptions)

      # make temp continuous + categorical table
      initSummaryTableSql <- .initAggregationTables(executionSettings, buildOptions)

      # get denominator
      denomSql <- .getDenominator(executionSettings, buildOptions)

      # make all the aggregate sql queries
      aggregateSqlQuery <- .aggregateSql(tsm, executionSettings, buildOptions)

      allSql <- c(patTsSql, initSummaryTableSql, denomSql, aggregateSqlQuery) |>
        glue::glue_collapse(sep = "\n\n")

      return(allSql)


    },



    # function to drop all cs Tables
    .dropTempTables = function(executionSettings, buildOptions) {

      # get temp table slot names
      tempTableSlots <- names(buildOptions)[grepl("TempTable",names(buildOptions))]

      # get table names
      tempTableNames <- purrr::map_chr(
        tempTableSlots,
        ~buildOptions[[.x]]
      )

      tempTables <- tibble::tibble(
        tempTableSlots = tempTableSlots,
        tempTableNames = tempTableNames
      ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          drop = grepl("\\#", tempTableNames) # check if temp
        )

      dropTempTableSql <- vector('list', length = nrow(tempTables))
      for (i in 1:nrow(tempTables)) {
        if (tempTables$drop[i]) {
          dropTempTableSql[[i]] <- .truncDropTempTables(
            tempTableName = tempTables$tempTableNames[i]
          )
        } else {
          dropTempTableSql[[i]] <- ""
        }
      }
      dropTempTableSql <- do.call("c", dropTempTableSql) |>
        glue::glue_collapse("\n") |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      return(dropTempTableSql)
    }
  )
)


# BuildOptions ----

#' @description
#' An R6 class to define build options for the tableShell
#'
#' @export
BuildOptions <- R6::R6Class(
  classname = "BuildOptions",
  public = list(
    initialize = function(keepResultsTable = NULL,
                          resultsTempTable = NULL,
                          codesetTempTable = NULL,
                          timeWindowTempTable = NULL,
                          targetCohortTempTable = NULL,
                          tsMetaTempTable = NULL,
                          conceptSetOccurrenceTempTable = NULL,
                          patientLevelDataTempTable = NULL,
                          categoricalSummaryTempTable = NULL,
                          continuousSummaryTempTable = NULL
                          ) {
      .setLogical(private = private, key = ".keepResultsTable", value = keepResultsTable)
      .setString(private = private, key = ".resultsTempTable", value = resultsTempTable)
      .setString(private = private, key = ".codesetTempTable", value = codesetTempTable)
      .setString(private = private, key = ".timeWindowTempTable", value = timeWindowTempTable)
      .setString(private = private, key = ".tsMetaTempTable", value = tsMetaTempTable)
      .setString(private = private, key = ".targetCohortTempTable", value = targetCohortTempTable)
      .setString(private = private, key = ".conceptSetOccurrenceTempTable", value = conceptSetOccurrenceTempTable)
      .setString(private = private, key = ".patientLevelDataTempTable", value = patientLevelDataTempTable)
      .setString(private = private, key = ".categoricalSummaryTempTable", value = categoricalSummaryTempTable)
      .setString(private = private, key = ".continuousSummaryTempTable", value = continuousSummaryTempTable)
    }
  ),
  private = list(
    .keepResultsTable = NULL,
    .resultsTempTable = NULL,
    .codesetTempTable = NULL,
    .timeWindowTempTable = NULL,
    .targetCohortTempTable = NULL,
    .tsMetaTempTable = NULL,
    .conceptSetOccurrenceTempTable = NULL,
    .patientLevelDataTempTable = NULL,
    .categoricalSummaryTempTable = NULL,
    .continuousSummaryTempTable = NULL
  ),

  active = list(

    keepResultsTable = function(value) {
      .setActiveLogical(private = private, key = ".keepResultsTable", value = value)
    },


    resultsTempTable = function(value) {
      .setActiveString(private = private, key = ".resultsTempTable", value = value)
    },


    codesetTempTable = function(value) {
      .setActiveString(private = private, key = ".codesetTempTable", value = value)
    },


    timeWindowTempTable = function(value) {
      .setActiveString(private = private, key = ".timeWindowTempTable", value = value)
    },

    targetCohortTempTable = function(value) {
      .setActiveString(private = private, key = ".targetCohortTempTable", value = value)
    },

    tsMetaTempTable = function(value) {
      .setActiveString(private = private, key = ".tsMetaTempTable", value = value)
    },

    conceptSetOccurrenceTempTable = function(value) {
      .setActiveString(private = private, key = ".conceptSetOccurrenceTempTable", value = value)
    },

    patientLevelDataTempTable = function(value) {
      .setActiveString(private = private, key = ".patientLevelDataTempTable", value = value)
    },

    categoricalSummaryTempTable = function(value) {
      .setActiveString(private = private, key = ".categoricalSummaryTempTable", value = value)
    },

    continuousSummaryTempTable = function(value) {
      .setActiveString(private = private, key = ".continuousSummaryTempTable", value = value)
    }


  )
)

# ExecutionSettings ----

#' @description
#' An R6 class to define an ExecutionSettings object
#'
#' @export
ExecutionSettings <- R6::R6Class(
  classname = "ExecutionSettings",
  public = list(
    initialize = function(connectionDetails = NULL,
                          connection = NULL,
                          cdmDatabaseSchema = NULL,
                          workDatabaseSchema = NULL,
                          tempEmulationSchema = NULL,
                          targetCohortTable = NULL,
                          cdmSourceName = NULL) {
      stopifnot(is.null(connectionDetails) || is.null(connection))
      .setClass(private = private, key = "connectionDetails", value = connectionDetails, class = "ConnectionDetails")
      .setClass(private = private, key = ".connection", value = connection,
                class = "DatabaseConnectorJdbcConnection", nullable = TRUE)
      .setString(private = private, key = ".cdmDatabaseSchema", value = cdmDatabaseSchema)
      .setString(private = private, key = ".workDatabaseSchema", value = workDatabaseSchema)
      .setString(private = private, key = ".tempEmulationSchema", value = tempEmulationSchema)
      .setString(private = private, key = ".targetCohortTable", value = targetCohortTable)
      .setString(private = private, key = ".cdmSourceName", value = cdmSourceName)
    },

    getDbms = function() {
      dbms <- private$connectionDetails$dbms
      return(dbms)
    },

    # connect to database
    connect = function() {

      # check if private$connection is NULL
      conObj <- private$.connection
      if (is.null(conObj)) {
        private$.connection <- DatabaseConnector::connect(private$connectionDetails)
      } else{
        cli::cat_bullet(
          "Connection object already open",
          bullet = "info",
          bullet_col = "blue"
        )
      }
    },

    # disconnect to database
    disconnect = function() {

      # check if private$connection is NULL
      conObj <- private$.connection
      if (class(conObj) == "DatabaseConnectorJdbcConnection") {
        # disconnect connection
        DatabaseConnector::disconnect(private$.connection)
        private$.connection <- NULL
      }

      cli::cat_bullet(
        "Connection object has been disconected",
        bullet = "info",
        bullet_col = "blue"
      )
      invisible(conObj)
    },

    #TODO make this more rigorous
    # add warning if no connection available
    getConnection = function() {
      conObj <- private$.connection
      return(conObj)
    }

  ),

  private = list(
    connectionDetails = NULL,
    .connection = NULL,
    .cdmDatabaseSchema = NULL,
    .workDatabaseSchema = NULL,
    .tempEmulationSchema = NULL,
    .targetCohortTable = NULL,
    .cdmSourceName = NULL
  ),

  active = list(

    cdmDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.cdmDatabaseSchema
        return(cds)
      }
      # replace the cdmDatabaseSchema
      .setString(private = private, key = ".cdmDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cdmDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    workDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.workDatabaseSchema
        return(cds)
      }
      # replace the workDatabaseSchema
      .setString(private = private, key = ".workDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('workDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },


    tempEmulationSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tes <- private$.tempEmulationSchema
        return(tes)
      }
      # replace the tempEmulationSchema
      .setString(private = private, key = ".tempEmulationSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('tempEmulationSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    targetCohortTable = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tct <- private$.targetCohortTable
        return(tct)
      }
      # replace the targetCohortTable
      .setString(private = private, key = ".targetCohortTable", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('targetCohortTable')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    cdmSourceName = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        csn <- private$.cdmSourceName
        return(csn)
      }
      # replace the cdmSourceName
      .setString(private = private, key = ".cdmSourceName", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cdmSourceName')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }

  )
)


# Cohort Info -----

#' @description
#' An R6 class to define a Cohort Info object
#' CohortInfo objects do not maintain any execution settings, just the id and name
#'
#' @export
CohortInfo <- R6::R6Class("CohortInfo",
  public = list(
    initialize = function(id, name) {
      .setNumber(private = private, key = "id", value = id)
      .setString(private = private, key = "name", value = name)
    },
    getId = function() {
      cId <- private$id
      return(cId)
    },
    getName = function(name) {
      cName <- private$name
      return(cName)
    },
    cohortDetails = function(){
      id <- self$getId()
      name <- self$getName()

      info <- glue::glue_col(
        "- CohortId: {green {id}}; CohortName: {green {name}}"
      )

      return(info)

    }
    # DEFUNCT: this is now one cohort per class
    # getSql = function() {
    #   sqlFile <- "targetCohort.sql"
    #   cohortId <- private$id
    #   # get sql from package
    #   sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
    #     readr::read_file() |>
    #     glue::glue()
    # }
  ),
  private = list(
    id = NULL,
    name = NULL
  )
)


# Statistic Class ---------------------

## Statistic Super-------------

#' @title
#' An R6 class to define a Statistic object
#'
#' @description
#' A Statistic is a type of metric to be used for characterization
#' Specific types of statistics are defined in derived classes
#'
#' @export
Statistic <- R6::R6Class(
  classname = "Statistic",
  public = list(
    initialize = function(statType, personLine, aggType) {
      .setString(private = private , key = "statisticType", value = statType)
      .setString(private = private , key = "personLineTransformation", value = personLine)
      .setString(private = private , key = "aggregationType", value = aggType)

    },
    getStatisticType = function() {
      statType <- private$statisticType
      return(statType)
    },
    getAggregationType = function() {
      aggType <- private$aggregationType
      return(aggType)
    },
    getPersonLineTransformation = function() {
      plt <- private$personLineTransformation
      return(plt)
    }
  ),
  private = list(
    statisticType = NA_character_,
    personLineTransformation = NA_character_,
    aggregationType = NA_character_
  )
)

## Demographic Stats----------------------


### Demographic Concept -----------------
DemographicConcept <- R6::R6Class(
  classname = "DemographicConcept",
  inherit = Statistic,
  public = list(
    initialize = function(demoCategory, demoLine, conceptColumn, conceptId) {
      super$initialize(
        personLine = "binary",
        statType = "presence",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = demoCategory)
      .setString(private = private, key = "demoLine", value = demoLine)
      .setString(private = private, key = "conceptColumn", value = conceptColumn)
      .setNumber(private = private, key = "conceptId", value = conceptId)
    },
    getConceptColumn = function() {
      rr <- private$conceptColumn
      return(rr)
    },
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}: {private$demoLine}")
      return(rr)
    },
    getConceptId = function() {
      rr <- private$conceptId
      return(rr)
    }
  ),
  private = list(
    demoCategory = NA_character_,
    demoLine = NA_character_,
    conceptColumn = NA_character_,
    conceptId = NA_integer_
  )
)

### Demographic Age ---------------------
DemographicAge <- R6::R6Class(
  classname = "DemographicAge",
  inherit = Statistic,
  public = list(
    initialize = function(statType, aggType, demoCategory, breaks = NULL) {
      super$initialize(
        personLine = "age",
        statType = statType,
        aggType = aggType)
      .setString(private = private, key = "demoCategory", value = "Age")
      .setClass(private = private, key = "breaks", value = breaks, class = "BreaksStrategy")
    },

    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    }
  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)





### Demographic Year -----------------
# DemographicYear <- R6::R6Class("DemographicYear",
#                            inherit = Statistic,
#                            public = list(
#                              initialize = function(breaks = NULL) {
#                                super$initialize(type = "Year")
#                                if (!is.null(breaks)) {
#                                  .setClass(private = private,
#                                            key = "breaks",
#                                            value = breaks,
#                                            class = "Breaks")
#                                }
#                                invisible(private)
#                              }
#                            ),
#                            private = list(
#                              breaks = NULL
#                            )
# )

## CS, CSG, Cohort Stats -----------------------------

### Presence -----------------------

Presence <- R6::R6Class(
  classname = "Presence",
  inherit = Statistic,
  public = list(
    initialize = function(personLine) {
      super$initialize(
        personLine = personLine,
        statType = "presence",
        aggType = "categorical"
      )
    }
  ),
  private = list()
)

### Breaks ------------------------
Breaks <- R6::R6Class(
  classname = "Breaks",
  inherit = Statistic,
  public = list(
    initialize = function(personLine, breaks) {
      super$initialize(
        personLine = personLine,
        statType = "breaks",
        aggType = "categorical"
      )
      .setClass(private = private, key = "breaks", value = breaks, class = "BreaksStrategy")
    }
  ),
  private = list(
    breaks = NULL
  )
)

### Distribution ------------------------
ContinuousDistribution <- R6::R6Class(
  classname = "ContinuousDistribution",
  inherit = Statistic,
  public = list(
    initialize = function(personLine) {
      super$initialize(
        personLine = personLine,
        statType = "continuousDistribution",
        aggType = "continuous"
      )
    }
  ),
  private = list(
  )
)

### Score ------------------------
Score <- R6::R6Class(
  classname = "Score",
  inherit = Statistic,
  public = list(
    initialize = function(personLine, score) {
      super$initialize(
        personLine = personLine,
        statType = "scoreTransformation",
        aggType = "continuous"
      )
      .setClass(private = private, key = "score", value = score, class = "ScoreWeight")
    }
  ),
  private = list(
    score = NULL
  )
)


## Count -----------------------

# Count <- R6::R6Class("Count",
#                      inherit = Statistic,
#                      public = list(
#                        initialize = function(breaks = NULL) {
#                          super$initialize(type = "Count")
#                          if (!is.null(breaks)) {
#                            .setClass(private = private, key = "breaks", value = breaks, class = "Breaks")
#                          }
#                          invisible(private)
#                        },
#                        getSql = function() {
#
#                          sqlFile <- "countStat.sql"
#                           # get sql from package
#                          sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
#                            readr::read_file() |>
#                            glue::glue()
#                          return(sql)
#                        }
#                      ),
#                      private = list(
#                        breaks = NULL
#                      )
# )


# LineItem Classes -----

## Line Item Super----------

#' @description
#' An R6 class to define a LineItem object
#' A LineItem is a single, explicitly defined characterization to appear in a Section
#' Derived classes exist off of LineItems
#'
#' @export
LineItem <- R6::R6Class(
classname = "LineItem",
public = list(
  initialize = function(
    sectionLabel,
    lineItemLabel = NA_character_,
    domainTable,
    lineItemClass,
    valueId = NA_integer_,
    valueDescription  = NA_integer_,
    statistic,
    timeInterval = NULL
  ) {
    .setString(private = private, key = ".sectionLabel", value = sectionLabel)
    .setString(private = private, key = ".lineItemLabel", value = lineItemLabel, naOk = TRUE)
    .setCharacter(private = private, key = ".domainTable", value = domainTable)
    .setString(private = private, key = ".lineItemClass", value = lineItemClass)
    .setNumber(private = private, key = ".valueId", value = valueId)
    .setString(private = private, key = ".valueDescription", value = valueDescription, naOk = TRUE)
    .setClass(private = private, key = "statistic", value = statistic, class = "Statistic")
    .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval", nullable = TRUE)
  },

  getLineItemMeta = function() {

    tw <- private$timeInterval
    if (is.null(tw)) {
      timeLabel <- "Static at Index"
    } else {
      timeLabel <- tw$getTimeLabel()
    }

    tb <- tibble::tibble(
      ordinalId = private$.ordinalId,
      sectionLabel = private$.sectionLabel,
      lineItemLabel = private$.lineItemLabel,
      valueId = private$.valueId,
      valueDescription = private$.valueDescription,
      timeLabel = timeLabel,
      personLineTransformation = private$statistic$getPersonLineTransformation(),
      statisticType = private$statistic$getStatisticType(),
      aggregationType = private$statistic$getAggregationType(),
      domainTable = private$.domainTable,
      lineItemClass = private$.lineItemClass
    )

    return(tb)
  }

),
private = list(
  .ordinalId = NA_integer_,
  .sectionLabel = NA_character_,
  .lineItemLabel = NA_character_,
  .valueId = NA_integer_,
  .valueDescription = NA_character_,
  timeInterval = NULL,
  statistic = NULL,
  .domainTable = NA_character_,
  .lineItemClass = NA_character_
),
active = list(
  ordinalId = function(ordinalId) {
    .setActiveNumber(private = private, key = ".ordinalId", value = ordinalId)
  },
  sectionLabel = function(sectionLabel) {
    .setActiveString(private = private, key = ".sectionLabel", value = sectionLabel)
  },
  lineItemLabel = function(lineItemLabel) {
    .setActiveString(private = private, key = ".lineItemLabel", value = lineItemLabel)
  },
  valueId = function(valueId) {
    .setActiveNumber(private = private, key = ".valueId", value = valueId)
  },
  valueDescription = function(valueDescription) {
    .setActiveString(private = private, key = ".valueDescription", value = valueDescription)
  },
  domainTable = function(domainTable) {
    .setActiveString(private = private, key = ".domainTable", value = domainTable)
  },
  lineItemClass = function(lineItemClass) {
    .setActiveString(private = private, key = ".lineItemClass", value = lineItemClass)
  }
)
)
#
# LineItem_old <- R6::R6Class("LineItem",
#   public = list(
#     initialize = function(name,
#                           #ordinal,
#                           definitionType,
#                           statistic) {
#       .setString(private = private, key = "name", value = name)
#       #.setNumber(private = private, key = "ordinal", value = ordinal)
#       # TODO change this to enforce definitionType from choice list
#       .setString(private = private, key = "definitionType", value = definitionType)
#       .setClass(private = private, key = "statistic", value = statistic, class = "Statistic")
#
#       invisible(self)
#     },
#     getName = function() {
#       name <- private$name
#       return(name)
#     },
#     getDefinitionType = function() {
#       liDefinitionType <- private$definitionType
#       return(liDefinitionType)
#     },
#     identifyStatType = function() {
#       statType <- private$statistic$getStatType() |>
#         .isLineItemContinuous()
#       if (statType) {
#         statType <- "continuous"
#       } else {
#         statType <- "categorical"
#       }
#       return(statType)
#     }
#
#   ),
#
#   private = list(
#     name = NULL,
#     .ordinal = NA_integer_,
#     definitionType = NULL,
#     statistic = NULL
#   ),
#
#   active = list(
#     ordinal = function(value) {
#       # return the value if nothing added
#       if(missing(value)) {
#         ord <- private$.ordinal
#         return(ord)
#       }
#       private$.ordinal <- as.integer(value)
#       invisible(private)
#     }
#   )
#
# )

## ConceptSetLineItem ----

#' @description
#' An R6 class to define a ConceptSetLineItem
#'
#' @export

ConceptSetLineItem <- R6::R6Class(
  classname = "ConceptSetLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(
      sectionLabel,
      domainTable,
      conceptSet,
      timeInterval,
      statistic,
      sourceConceptSet = NULL,
      typeConceptIds = c(),
      visitOccurrenceConceptIds = c()
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTable,
        lineItemClass = "ConceptSet",
        valueDescription = "codeset_id",
        statistic = statistic,
        lineItemLabel = conceptSet@Name,
        timeInterval = timeInterval
      )

      .setClass(private = private, key = "conceptSet", value = conceptSet, class = "ConceptSet")
      #.setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval", nullable = TRUE)
      # TODO change this to enforce domain from choice list
      .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "ConceptSet", nullable = TRUE)
      .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)
      .setNumber(private = private, key = "visitOccurrenceConceptIds", value = visitOccurrenceConceptIds, nullable = TRUE)

    },

        # helper to pull concept Capr class items
     grabConceptSet = function() {
        cs <- private$conceptSet
        return(cs)
     }
  ),
  private = list(
    conceptSet = NULL,
    sourceConceptSet = NULL,
    typeConceptIds = c(),
    visitOccurrenceConceptIds = c()
  ),
  active = list()
)
# ConceptSetLineItem_old <- R6::R6Class("ConceptSetLineItem",
#   inherit = LineItem,
#   public = list(
#
#     # initialize the class
#     initialize = function(name,
#                           #ordinal,
#                           statistic,
#                           domain,
#                           conceptSet,
#                           timeInterval,
#                           sourceConceptSet = NULL,
#                           typeConceptIds = c(),
#                           visitOccurrenceConceptIds = c()) {
#       super$initialize(name = name, definitionType = "ConceptSet",
#                        statistic = statistic)
#       .setString(private = private, key = "domain", value = domain)
#       .setClass(private = private, key = "conceptSet", value = conceptSet, class = "ConceptSet")
#       .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval")
#       # TODO change this to enforce domain from choice list
#       .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "ConceptSet", nullable = TRUE)
#       .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)
#       .setNumber(private = private, key = "visitOccurrenceConceptIds", value = visitOccurrenceConceptIds, nullable = TRUE)
#     },
#
#     # gather information for print
#     lineItemDetails = function() {
#
#       ord <- self$ordinal |>
#         tibble::as_tibble() |>
#         dplyr::rename(
#           ordinal = value
#           )
#
#       info <- self$getConceptSetRef() |>
#         dplyr::select(
#           -c(hash)
#         ) |>
#         dplyr::bind_cols(
#           ord
#         ) |>
#         glue::glue_data_col(
#           "{ordinal}) \\
#           ConceptSetName: {green {name}}; \\
#           ConceptSetDomain: {green {domain}}; \\
#           TimeWindow: {magenta {lb}d} to {magenta {rb}d}"
#         )
#
#       return(info)
#     },
#

#
#     # helper to retrieve the time windows in the clas
#     getTimeInterval = function() {
#       tw <- private$timeInterval$getTimeInterval()
#       return(tw)
#     },
#
#
#     # helper to get reference table of the concept sets in the class
#     getConceptSetRef = function() {
#
#       # # make key for cs use
#       csTbl <- tibble::tibble(
#         'name' = private$name,
#         'hash' = private$conceptSet@id,
#         'domain' = private$domain,
#         'lb' = private$timeInterval$getLb(),
#         'rb' = private$timeInterval$getRb()
#       )
#       return(csTbl)
#     },
#
#     getStatisticInfo = function() {
#       tb <- tibble::tibble(
#         'ord' = self$ordinal,
#         'statType' = private$statistic$getStatType(),
#         'sql' = private$statistic$getSql()
#       )
#       return(tb)
#     }
#   ),
#   private = list(
#     domain = NULL,
#     conceptSet = NULL,
#     timeInterval = NULL,
#     sourceConceptSet = NULL,
#     typeConceptIds = c(),
#     visitOccurrenceConceptIds = c()
#   )
# )

# DemographicLineItem -----

#' @description
#' An R6 class to handle the ...
#'
#' @export


DemographicLineItem <- R6::R6Class(
  classname = "DemographicLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(statistic = statistic) {
      super$initialize(
        sectionLabel = "Demographics",
        domainTable = "person",
        lineItemLabel = statistic$getDemoLabel(),
        lineItemClass = "Demographic",
        statistic = statistic,
        timeInterval = NULL
      )
    }),
  private = list()
)

# DemographicLineItem <- R6::R6Class("DemographicLineItem",
#   inherit = LineItem,
#   public = list(
#     initialize = function(name,
#                           #ordinal,
#                           statistic) {
#       super$initialize(name = name,
#                        #ordinal = ordinal,
#                        definitionType = "Demographic",
#                        statistic = statistic)
#     },
#
#     # gather information for print
#     lineItemDetails = function() {
#
#       ord <- self$ordinal
#
#       nm <- self$getName()
#
#       info <- glue::glue_col(
#           "{ord}) \\
#           DemographicName: {green {nm}}"
#       )
#
#       return(info)
#     },
#
#
#     # function to get sql
#     getSql = function() {
#
#       # get stat type
#       statType <- private$statistic$getStatType()
#
#       # prep sql if Age demographic
#       if (statType == "Age") {
#         sqlFile <- "demoAgeChar.sql"
#         ordinal <- self$ordinal
#         # get sql from package
#         sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
#           readr::read_file() |>
#           glue::glue()
#       }
#
#       # prep sql if Concept demographic
#       if (statType == "Concept") {
#         sqlFile <- "demoConceptChar.sql"
#         ordinal <- self$ordinal
#         conceptColumn <- private$statistic$getDemoColumn()
#         # get sql from package
#         sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
#           readr::read_file() |>
#           glue::glue()
#       }
#
#       # prep sql if Concept demographic
#       if (statType == "Year") {
#         sqlFile <- "demoYearChar.sql"
#         ordinal <- self$ordinal
#         # get sql from package
#         sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
#           readr::read_file() |>
#           glue::glue()
#       }
#
#       return(sql)
#     }
#   )
# )

## CohortLineItem ----

#' @description
#' An R6 class to define a CohortLineItem
#'
#' @export
CohortLineItem <- R6::R6Class(
  classname = "CohortLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(
    sectionLabel,
    domainTable,
    covariateCohort,
    timeInterval,
    statistic
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTable,
        lineItemClass = "Cohort",
        valueDescription = "cohort_definition_id",
        statistic = statistic,
        lineItemLabel = covariateCohort$getName(),
        timeInterval = timeInterval
      )
      # add cohortInfo class object
      .setClass(private = private, key = "covariateCohort", value = covariateCohort, class = "CohortInfo")

    }
  ),
  private = list(
    covariateCohort = NULL
  ),
  active = list()
)
# CohortLineItem <- R6::R6Class("CohortLineItem",
#   inherit = LineItem,
#   public = list(
#
#     # initialize the class
#     initialize = function(name,
#                           #ordinal,
#                           statistic,
#                           cohort,
#                           timeInterval) {
#       super$initialize(name = name,
#                        definitionType = "Cohort",
#                        statistic = statistic)
#       .setClass(private = private, key = "cohort", value = cohort, class = "CohortInfo")
#       .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval")
#     },
#
#     # gather information for print
#     lineItemDetails = function() {
#
#       ord <- self$ordinal |>
#         tibble::as_tibble() |>
#         dplyr::rename(
#           ordinal = value
#           )
#
#       info <- self$getCohortRef() |>
#         dplyr::select(
#           -c(hash)
#         ) |>
#         dplyr::bind_cols(
#           ord
#         ) |>
#         glue::glue_data_col(
#           "{ordinal}) \\
#           CohortName: {green {name}}; \\
#           TimeWindow: {magenta {lb}d} to {magenta {rb}d}"
#         )
#
#       return(info)
#     },
#
#     # helper to pull cohort items
#     grabCohort = function() {
#       c <- private$cohort
#       return(c)
#     },
#
#     # helper to retrieve the time windows in the class
#     getTimeInterval = function() {
#       tw <- private$timeInterval$getTimeInterval()
#       return(tw)
#     },
#
#     # helper to get reference table of the cohorts in the class
#     getCohortRef = function() {
#       cTbl <- tibble::tibble(
#         'name' = private$name,
#         'lb' = private$timeInterval$getLb(),
#         'rb' = private$timeInterval$getRb()
#       )
#       return(cTbl)
#     },
#
#     getStatisticInfo = function() {
#       tb <- tibble::tibble(
#         'ord' = self$ordinal,
#         'statType' = private$statistic$getStatType(),
#         'sql' = private$statistic$getSql()
#       )
#       return(tb)
#     }
#   ),
#   private = list(
#     cohort = NULL,
#     timeInterval = NULL
#   )
# )



ConceptSetGroupLineItem <- R6::R6Class(
  classname = "ConceptSetGroupLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(
    sectionLabel,
    groupLabel,
    conceptSets,
    domainTables,
    timeInterval,
    statistic
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTables,
        lineItemClass = "ConceptSetGroup",
        valueDescription = "codeset_id",
        statistic = statistic,
        lineItemLabel = groupLabel,
        timeInterval = timeInterval
      )
      csClasses <- rep("ConceptSet", length(conceptSets))
      .setListofClasses(private = private, key = "conceptSets", value = conceptSets, classes = csClasses)

    },

    grabConceptSet = function() {
      cs <- private$conceptSets
      return(cs)
    }
  ),
  private = list(
    conceptSets = NULL
  ),
  active = list()
)


# Helper Classes -----

## TimeInterval ------
TimeInterval <- R6::R6Class(
  "TimeInterval",
  public = list(
    initialize = function(lb, rb) {
      .setNumber(private = private, key = "lb", value = lb)
      .setNumber(private = private, key = "rb", value = rb)
      invisible(self)
    },
    getLb = function() {
      lb <- private$lb
      return(lb)
    },
    getRb = function() {
      rb <- private$rb
      return(rb)
    },
    getTimeLabel = function() {
      lbl <- glue::glue("{private$lb}d to {private$rb}d")
      return(lbl)
    },
    getTimeInterval = function() {
      tb <- tibble::tibble(
        lb = private$lb,
        rb = private$rb
      )
      return(tb)
    }
  ),
  private = list(
    'lb' = NA_integer_,
    'rb' = NA_integer_
  )
)
#
# TimeInterval_old <- R6::R6Class(
#   "TimeInterval",
#   public = list(
#     initialize = function(lb, rb) {
#       .setNumber(private = private, key = "lb", value = lb)
#       .setNumber(private = private, key = "rb", value = rb)
#       invisible(self)
#     },
#     getLb = function() {
#       lb <- private$lb
#       return(lb)
#     },
#     getRb = function() {
#       rb <- private$rb
#       return(rb)
#     },
#     getTimeInterval = function() {
#       tb <- tibble::tibble(
#         lb = private$lb,
#         rb = private$rb
#       )
#       return(tb)
#     }
#   ),
#   private = list(
#     'lb' = NA_integer_,
#     'rb' = NA_integer_
#   )
# )


# Legacy -------------


## Section ------

## @description
###' An R6 class to define a Section object
## A Section object is a logical encapsulation of LineItems
## Sections have names and ordinals for the final output
##
## @export
## Section <- R6::R6Class("Section",
#   public = list(
#     initialize = function(name,
#                           ordinal,
#                           lineItems) {
#       .setString(private = private, key = "name", value = name)
#       .setNumber(private = private, key = "ordinal", value = ordinal)
#       .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
#     },
#     getName = function() {
#       name <- private$name
#       return(name)
#     },
#     getOrdinal = function() {
#       sectionOrdinal <- private$ordinal
#       return(sectionOrdinal)
#     },
#     getLineItems = function() {
#       sectionLineItems <- private$lineItems
#       return(sectionLineItems)
#     }
#   ),
#   private = list(
#     name = NULL,
#     ordinal = NA,
#     lineItems = NULL
#   )
# )



## TimeWindow ------
#
# TimeWindow <- R6::R6Class(
#   "TimeWindow",
#   public = list(
#     initialize = function(windows) {
#       .setListofClasses(
#         private = private,
#         key = "windows",
#         value = windows,
#         classes = c("TimeInterval")
#       )
#       invisible(self)
#     },
#     length = function() {
#       ll <- length(private$windows)
#       return(ll)
#     },
#     getTimeIntervals = function() {
#
#       tis <- purrr::map_dfr(
#         private$windows,
#         ~tibble::tibble(
#           'lb' = .x$getLeftBound(),
#           'rb' = .x$getRightBound()
#         )
#       )
#       return(tis)
#     }
#   ),
#   private = list(
#     windows = NULL
#   )
# )

# # GenderDefinition -----
#
# #' @description
# #' An R6 class to define a GenderDefinition object.
# #'
# #' @export
# GenderDefinition <- R6::R6Class("GenderDefinition",
#  inherit = LineItem,
#  public = list(
#    initialize = function(name,
#                          genderConceptIds) {
#      super$setDomainIds(domainIds = "Gender")
#      super$setName(name = name)
#      super$setConceptIds(conceptIds = genderConceptIds)
#
#      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#      super$setTemplateSql(templateSql = templateSql)
#    }
#  )
# )




# # IndexYearDefinition -----

# #' @description
# #' An R6 class to define a IndexYearDefinition object.
# #'
# #' @export
# IndexYearDefinition <- R6::R6Class("IndexYearDefinition",
#  inherit = LineItem,
#  public = list(
#    initialize = function(name = indexYear,
#                          indexYear) {
#      super$setName(name = name)
#      super$setDomainIds(domainIds = "IndexYear")
#      super$setMinThreshold(minThreshold = indexYear)
#      super$setMaxThreshold(maxThreshold = indexYear) # is it fine to just stuff this in min and max thresholds?
#
#      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#      super$setTemplateSql(templateSql = templateSql)
#    }

# # ConceptSetGroupDefinition ----

# #' @description
# #' An R6 class to define a ConceptSetGroupDefinition
# #' A line item that uses a Concept Set Group
# #'
# #' @export
# ConceptSetGroupDefinition <- R6::R6Class("ConceptSetGroupDefinition",
#  inherit = LineItem,
#  public = list(
#    initialize = function(ConceptSetLineItems) {
#      .setListofClasses(private = private, key = "ConceptSetLineItems",
#                        value = ConceptSetLineItems,
#                        classes = c("ConceptSetLineItem"))
#    },
#    getConceptSetLineItems = function() {
#      ConceptSetLineItems <- private$ConceptSetLineItems
#      return(ConceptSetLineItems)
#     }
#   ),
#   private = list(
#     ConceptSetLineItems = c()
#   )
# ))




# # CohortDefinition ----

# #' @description
# #' An R6 class to define a CohortDefinition
# #'
# #' @export
# CohortDefinition <- R6::R6Class("CohortDefinition",
#   inherit = LineItem,
#   public = list(
#     initialize = function(name,
#                           cohortDefinitionId, cohortDatabaseSchema, cohortTable) {
#       super$setName(name = name)
#       super$setDomainIds(domainIds = "Cohort")
#       super$setAssetId(assetId = cohortDefinitionId)
#
#      # Could simply use an execution settings object, but the scope here is much less, as we
#      # need the same connection and database
#      .setString(private = private, key = "cohortDatabaseSchema", value = cohortDatabaseSchema)
#      .setString(private = private, key = "cohortTable", value = cohortTable)
#
#       templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#       super$setTemplateSql(templateSql = templateSql)
#     }
#   ),
#     private = list(
#     cohortDatabaseSchema = NULL,
#     cohortTable = NULL)
# )

# # RaceDefinition ------

# #' @description
# #' An R6 class to define a RaceDefinition object.
# #'
# #' @export
# RaceDefinition <- R6::R6Class("RaceDefinition", list(
#   inherit = LineItem,
#   public = list(
#     initialize = function(name,
#                           raceConceptIds) {
#       super$setDomainIds(domainIds = "Race")
#       super$setName(name = name)
#       super$setConceptIds(conceptIds = raceConceptIds)

#       templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#       super$setTemplateSql(templateSql = templateSql)
#     },
#     getRaceConceptIds = function() {
#      raceConceptIds <- private$raceConceptIds
#      return(raceConceptIds)
#     }
#   )
# ))

# ######## MORE TO-DO #####


# # ValueDefinition ----

# #' @description
# #' An R6 class to handle the ...
# #'
# #' @export
# ValueDefinition <- R6::R6Class("ValueDefinition", list(
#   inherit = LineItem,

#   domainIds = c(),
#   thresholdMin = NA,
#   thresholdMax = NA,
#   unitConceptIds = c(),
#   unitConversions = c()
# ))

# # UnitConversion ----

# #' @description
# #' An R6 class to handle the ...
# #'
# #' @export
# UnitConversion <- R6::R6Class("UnitConversion", list(
#   originalUnitConceptId = NA,
#   targetUnitConceptId = NA,
#   multiplierToOriginal = NA
# ))

# # BreaksStrategy ----

# #' @description
# #' An R6 class to handle the ...
# #'
# #' @export
# BreaksStrategy <- R6::R6Class("BreaksStrategy", list(
#   name = NULL,
#   breaks = NULL,
#   initialize = function(name,
#                         breaks) {
#     self$name <- name
#     self$breaks <- breaks
#   }
# ))
