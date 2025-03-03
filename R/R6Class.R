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

      tcs <- self$getTargetCohorts()
      cohortPrintInfo <- purrr::map_chr(tcs, ~.x$cohortDetails()) |>
        glue::glue_collapse("\n")

      # get line item info
      tsm <- self$getTableShellMeta()
      tsmInfo <- tsm |>
        dplyr::select(ordinalId, sectionLabel, lineItemLabel, timeLabel, statisticType) |>
        dplyr::distinct() |>
        glue::glue_data_col(
          "\t{ordinalId}) {green {sectionLabel}}: {yellow {lineItemLabel}} ({magenta {timeLabel}}) || Stat Type {blue {statisticType}}"
        )|>
        glue::glue_collapse("\n")


      cli::cat_line(
        glue::glue("Target Cohort Details:\n{cohortPrintInfo}")
      )

      cli::cat_line(
        glue::glue("Line Item tasks:\n{tsmInfo}")
      )

      invisible(tsmInfo)

    },

    # function to instantiate tables for queries
    instantiateTables = function(executionSettings, buildOptions) {

      # ensure R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
      checkmate::assert_class(buildOptions, classes = "BuildOptions", null.ok = FALSE)



      # create concept set occurrence table
      createOccurrenceTableSql <- fs::path_package(
        package = "ClinicalCharacteristics",
        fs::path("sql", "createOccurrenceTable.sql")
      ) |>
        readr::read_file() |>
        SqlRender::render(
          concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable
        ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      cli::cat_bullet(
        glue::glue_col("Create conceptSetOccurrence table ---> {cyan {buildOptions$conceptSetOccurrenceTempTable}}"),
        bullet = "info",
        bullet_col = "blue"
      )

      DatabaseConnector::executeSql(
        connection = executionSettings$getConnection(),
        sql = createOccurrenceTableSql
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
      self$printJobDetails()

      # collect all the sql
      fullSql <- c(

        # Step 1: Insert ts meta
        .insertTableSql(
          executionSettings = executionSettings,
          tableName = buildOptions$tsMetaTempTable,
          data = self$getTableShellMeta()
        ),

        # Step 1: Insert time windows
        private$.insertTimeWindows(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # step 3: Make target Cohort table
        private$.makeTargetCohortTable(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # step 4: create targe cohort table
        private$.buildCodesetQueries(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 5a: create concept set query
        private$.buildConceptSetOccurrenceQuery(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 5b: create source concept set query
        private$.buildSourceConceptOccurrenceQuery(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 5c: create cohort occ query
        private$.buildCohortOccurrenceQuery(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 6: transform to patient line data
        private$.transformToPatientLineData(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 7: Aggregate results
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

      # categorical results
      cts_res <- .getContinuousResults(tsm, tc, executionSettings, buildOptions) |>
        tibble::as_tibble()


      res <- list(
        'categorical' = cat_res,
        'continuous' = cts_res
      )

      return(res)
    },

    # function to drop all cs Tables
    dropTempTables = function(executionSettings, buildOptions) {

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

      DatabaseConnector::executeSql(
        connection = executionSettings$getConnection(),
        sql = dropTempTableSql
      )

      return(dropTempTableSql)
    }

  ),
  private = list(
    name = NULL,
    targetCohorts = NULL,
    lineItems = NULL,


    ### private methods ---------------
    # function to insert tsMeta deprecated
    # .insertTsMeta = function(executionSettings, buildOptions) {
    #   # ensure that executionSettings R6 object used
    #   checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
    #
    #   # get tsMeta
    #   tsMeta <- self$getTableShellMeta() |>
    #     dplyr::rename_with(snakecase::to_snake_case)
    #
    #
    #
    #
    #   # old version
    #   # cli::cat_bullet(
    #   #   glue::glue_col("Insert tsMeta table ---> {cyan {buildOptions$tsMetaTempTable}}"),
    #   #   bullet = "info",
    #   #   bullet_col = "blue"
    #   # )
    #   #
    #   # # establish connection to database
    #   # connection <- executionSettings$getConnection()
    #   #
    #   # if (is.null(connection)) {
    #   #   connection <- executionSettings$connect()
    #   # }
    #   #
    #   # # insert the time windows into the database
    #   # DatabaseConnector::insertTable(
    #   #   connection = connection,
    #   #   tableName = buildOptions$tsMetaTempTable,
    #   #   tempEmulationSchema = executionSettings$tempEmulationSchema,
    #   #   data = tsMeta,
    #   #   tempTable = TRUE
    #   # )
    #   #
    #   # invisible(tsMeta)
    # },

    .insertTimeWindows = function(executionSettings, buildOptions) {
      # ensure that executionSettings R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      # get concept set line items
      twLineItems <- self$getTableShellMeta() |>
        dplyr::filter(!grepl("Static at Index", timeLabel))

      if (nrow(twLineItems) > 0) {
        # make the time windows table
        time_tbl <- tibble::tibble(
          time_label = twLineItems$timeLabel
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
      } else{

        time_tbl <- tibble::tibble(
          time_label = NA_character_,
          time_a = NA_integer_,
          time_b = NA_integer_
        )

      }

      # Insert time windows sql
      time_sql <- .insertTableSql(
        executionSettings = executionSettings,
        tableName = buildOptions$timeWindowTempTable,
        data = time_tbl
      )

      return(time_sql)

      # old version
      # cli::cat_bullet(
      #   glue::glue_col("Insert timeWindows table ---> {cyan {buildOptions$timeWindowTempTable}}"),
      #   bullet = "info",
      #   bullet_col = "blue"
      # )
      #
      # # establish connection to database
      # connection <- executionSettings$getConnection()
      #
      # if (is.null(connection)) {
      #   connection <- executionSettings$connect()
      # }
      #
      # # insert the time windows into the database
      # DatabaseConnector::insertTable(
      #   connection = connection,
      #   tableName = buildOptions$timeWindowTempTable,
      #   tempEmulationSchema = executionSettings$tempEmulationSchema,
      #   data = time_tbl,
      #   tempTable = TRUE
      # )
      #
      # invisible(time_tbl)
    },

    # function to get target cohort sql
    .makeTargetCohortTable = function(executionSettings, buildOptions) {

      sqlFile <- "targetCohort.sql"
      cohortIds <- purrr::map_int(
        private$targetCohorts,
        ~.x$getId()
      ) |>
        glue::glue_collapse(", ")

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
        dplyr::filter(lineItemClass %in% c("ConceptSet", "ConceptSetGroup"))

      # only run if CSD in ts
      if (nrow(csMeta) > 0) {

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
            domain = .x,
            type = "standard"
          )
        ) |>
          glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

        conceptSetOccurrenceSql <- glue::glue(
          "INSERT INTO @concept_set_occurrence_table
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

    .buildSourceConceptOccurrenceQuery = function(executionSettings, buildOptions) {

      # Step 1: Get the concept set meta
      scsMeta <- self$getTableShellMeta() |>
        dplyr::filter(lineItemClass == "SourceConceptSet")

      # only run if CSD in ts
      if (nrow(scsMeta) > 0) {

        li <- self$getLineItems()
        sourceCodeSetQuery <- .sourceConceptQuery(
          lineItems = li,
          scsMeta = scsMeta,
          executionSettings = executionSettings,
          buildOptions = buildOptions
        )

        # Step 2: Prep the concept set extraction
        scsTables <- scsMeta |>
          dplyr::select(valueId, valueDescription, timeLabel, domainTable)

        # Step 3: get the domains to join
        domainTablesInUse <- unique(scsTables$domainTable)
        # step 4: make the source concept set occurrence sql
        sourceSonceptSetOccurrenceSqlGrp <- purrr::map(
          domainTablesInUse,
          ~.prepConceptSetOccurrenceQuerySql(
            csTables = scsTables,
            domain = .x,
            type = "source"
          )
        ) |>
          glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

        sourceConceptSetOccurrenceSql <- glue::glue(
          "{sourceCodeSetQuery}
          INSERT INTO @concept_set_occurrence_table
          {sourceSonceptSetOccurrenceSqlGrp}
          ;
          ")
        sourceConceptSetOccurrenceSql <- sourceConceptSetOccurrenceSql |>
          SqlRender::render(
            target_cohort_table = buildOptions$targetCohortTempTable,
            concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
            time_window_table = buildOptions$timeWindowTempTable,
            source_codeset_table = buildOptions$sourceCodesetTempTable,
            cdm_database_schema = executionSettings$cdmDatabaseSchema
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )

      } else {
        sourceConceptSetOccurrenceSql <- ""
      }

      return(sourceConceptSetOccurrenceSql)

    },

    .buildCohortOccurrenceQuery = function(executionSettings, buildOptions) {
      # Step 1: Get the cohort meta
      chMeta <- self$getTableShellMeta() |>
        dplyr::filter(grepl("Cohort", lineItemClass))

      # only run if Cohorts are in ts
      if (nrow(chMeta) > 0) {

        # Step 2: Prep the cohort extraction
        cohort_ids <- chMeta$valueId |> unique() |> glue::glue_collapse(", ")
        time_labels <- chMeta$timeLabel |> unique() |> glue::glue_collapse("', '")
        domain <- chMeta$domainTable |> unique()

        # Step 3: make the cohort occurrence sql

        cohortOccurrenceSql <- fs::path_package(
          package = "ClinicalCharacteristics",
          fs::path("sql/cohortOccurrenceQuery.sql")
        ) |>
          readr::read_file() |>
          glue::glue()

        # render sql
        cohortOccurrenceSql <- cohortOccurrenceSql |>
          SqlRender::render(
            target_cohort_table = buildOptions$targetCohortTempTable,
            cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
            time_window_table = buildOptions$timeWindowTempTable,
            work_database_schema = executionSettings$workDatabaseSchema
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )

      } else {
        cohortOccurrenceSql <- ""
      }

      return(cohortOccurrenceSql)

    },

    .transformToPatientLineData = function(executionSettings, buildOptions) {

      # Step 1: make patient level data table
      ptDatTbSql <- fs::path_package(
        "ClinicalCharacteristics",
        fs::path("sql", "patientLevelData.sql")
      ) |>
        readr::read_file() |>
        SqlRender::render(
          patient_level_data = buildOptions$patientLevelDataTempTable
        )

      # Step 2: run patient level queries
      tsm <- self$getTableShellMeta()
      # step 2a demographics

      demoPatientLevelSql <- .buildDemoPatientLevelSql(tsm, executionSettings, buildOptions)

      # step 2b concept set pat level
      csPatientLevelSql <- .buildOccurrencePatientLevelSql(tsm, executionSettings, buildOptions)

      # step 2c cohort pat level
      chPatientLevelSql <- .buildCohortPatientLevelSql(tsm, executionSettings, buildOptions)

      # full sql for sql
      ptFullSql <- c(ptDatTbSql, demoPatientLevelSql, csPatientLevelSql, chPatientLevelSql) |>
        glue::glue_collapse(sep = "\n\n") |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      return(ptFullSql)
    },

    .aggregateResults = function(executionSettings, buildOptions) {

      # tsm <- self$getTableShellMeta()
      ts <- self

      # Create temp table joining patient date with ts meta
      patTsSql <- .tempPsDatTable(executionSettings, buildOptions)

      # make temp continuous + categorical table
      initSummaryTableSql <- .initAggregationTables(executionSettings, buildOptions)

      # make all the aggregate sql queries
      aggregateSqlQuery <- .aggregateSql(ts, executionSettings, buildOptions)

      allSql <- c(patTsSql, initSummaryTableSql, aggregateSqlQuery) |>
        glue::glue_collapse(sep = "\n\n")

      return(allSql)
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
    initialize = function(codesetTempTable = NULL,
                          sourceCodesetTempTable = NULL,
                          timeWindowTempTable = NULL,
                          targetCohortTempTable = NULL,
                          tsMetaTempTable = NULL,
                          conceptSetOccurrenceTempTable = NULL,
                          cohortOccurrenceTempTable = NULL,
                          patientLevelDataTempTable = NULL,
                          patientLevelTableShellTempTable = NULL,
                          categoricalSummaryTempTable = NULL,
                          continuousSummaryTempTable = NULL
                          ) {
      .setString(private = private, key = ".codesetTempTable", value = codesetTempTable)
      .setString(private = private, key = ".sourceCodesetTempTable", value = sourceCodesetTempTable)
      .setString(private = private, key = ".timeWindowTempTable", value = timeWindowTempTable)
      .setString(private = private, key = ".tsMetaTempTable", value = tsMetaTempTable)
      .setString(private = private, key = ".targetCohortTempTable", value = targetCohortTempTable)
      .setString(private = private, key = ".conceptSetOccurrenceTempTable", value = conceptSetOccurrenceTempTable)
      .setString(private = private, key = ".cohortOccurrenceTempTable", value = cohortOccurrenceTempTable)
      .setString(private = private, key = ".patientLevelDataTempTable", value = patientLevelDataTempTable)
      .setString(private = private, key = ".patientLevelTableShellTempTable", value = patientLevelTableShellTempTable)
      .setString(private = private, key = ".categoricalSummaryTempTable", value = categoricalSummaryTempTable)
      .setString(private = private, key = ".continuousSummaryTempTable", value = continuousSummaryTempTable)
    }
  ),
  private = list(
    .codesetTempTable = NULL,
    .sourceCodesetTempTable = NULL,
    .timeWindowTempTable = NULL,
    .targetCohortTempTable = NULL,
    .tsMetaTempTable = NULL,
    .conceptSetOccurrenceTempTable = NULL,
    .cohortOccurrenceTempTable = NULL,
    .patientLevelDataTempTable = NULL,
    .patientLevelTableShellTempTable = NULL,
    .categoricalSummaryTempTable = NULL,
    .continuousSummaryTempTable = NULL
  ),

  active = list(


    codesetTempTable = function(value) {
      .setActiveString(private = private, key = ".codesetTempTable", value = value)
    },

    sourceCodesetTempTable = function(value) {
      .setActiveString(private = private, key = ".sourceCodesetTempTable", value = value)
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

    cohortOccurrenceTempTable = function(value) {
      .setActiveString(private = private, key = ".cohortOccurrenceTempTable", value = value)
    },

    patientLevelDataTempTable = function(value) {
      .setActiveString(private = private, key = ".patientLevelDataTempTable", value = value)
    },

    patientLevelTableShellTempTable = function(value) {
      .setActiveString(private = private, key = ".patientLevelTableShellTempTable", value = value)
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
        "\t- Cohort Id: {green {id}}; Cohort Name: {green {name}}"
      )

      return(info)

    }
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
    },
    getBreaksIfAny = function() {
      if (self$getStatisticType() == "breaks") {
        br <- private$breaks
      } else {
        br <- NULL
      }
      return(br)
    },
    getWeightsIfAny = function() {
      if (self$getStatisticType() == "scoreTransformation") {
        ww <- self$weight
      } else {
        ww <- NULL
      }
      return(ww)
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
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = TRUE)
    },

    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },


    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)

### Demographic Index Year ---------------------
DemographicIndexYear <- R6::R6Class(
  classname = "DemographicIndexYear",
  inherit = Statistic,
  public = list(
    initialize = function(breaks) {
      super$initialize(
        personLine = "year",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Year")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },

    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },


    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)


### Demographic Cohort Follow up ---------------------
DemographicCohortTime <- R6::R6Class(
  classname = "DemographicCohortTime",
  inherit = Statistic,
  public = list(
    initialize = function() {
      super$initialize(
        personLine = "cohort_follow_up",
        statType = "continuousDistribution",
        aggType = "continuous")
      .setString(private = private, key = "demoCategory", value = "Cohort Follow up")
    },

    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    }

  ),
  private = list(
    demoCategory = NA_character_
  )
)


### Demographic Location ---------------------
DemographicLocation <- R6::R6Class(
  classname = "DemographicLocation",
  inherit = Statistic,
  public = list(
    initialize = function(breaks) {
      super$initialize(
        personLine = "location",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Location")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },

    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },


    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)


### Demographic Payer Type ---------------------
DemographicPayerType <- R6::R6Class(
  classname = "DemographicPayerType",
  inherit = Statistic,
  public = list(
    initialize = function(breaks) {
      super$initialize(
        personLine = "payer_type",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Payer Type")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },

    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },


    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)

### Demographic Race ---------------------
DemographicRace <- R6::R6Class(
  classname = "DemographicRace",
  inherit = Statistic,
  public = list(
    initialize = function(breaks) {
      super$initialize(
        personLine = "race",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Race")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },

    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },


    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)


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
    initialize = function(personLine, weight) {
      super$initialize(
        personLine = personLine,
        statType = "scoreTransformation",
        aggType = "continuous"
      )
      .setNumber(private = private, key = ".weight", value = weight)
    }
  ),
  private = list(
    .weight = NULL
  ),
  active =list(
    weight = function(weight) {
      .setActiveNumber(private = private, key = ".weight", value = weight)
    }
  )
)



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
        lineItemClass = private$.lineItemClass,
        lineItemLabel = private$.lineItemLabel,
        valueId = private$.valueId,
        valueDescription = private$.valueDescription,
        timeLabel = timeLabel,
        personLineTransformation = private$statistic$getPersonLineTransformation(),
        statisticType = private$statistic$getStatisticType(),
        aggregationType = private$statistic$getAggregationType(),
        domainTable = private$.domainTable
      )

      return(tb)
    },

    getStatistic = function() {
      st <- private$statistic
      return(st)
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

SourceConcepSet <- R6::R6Class(
  classname = "SourceConceptSet",
  public = list(

    initialize = function(
      sourceConceptId,
      sourceConceptName,
      sourceConceptSet
    ) {
      .setString(private = private, key = ".sourceConceptId", value = sourceConceptId)
      .setString(private = private, key = ".sourceConceptName", value = sourceConceptName)
      .setDataFrame(private = private, key = "sourceConceptSet",
                    colN = 4,
                    value = sourceConceptSet)
    },

    getSourceConceptTable = function() {
      tt <- private$sourceConceptSet
      return(tt)
    }

  ),
  private = list(
    .sourceConceptId = NA_character_,
    .sourceConceptName = NA_character_,
    sourceConceptSet = NULL
  ),
  active = list(
    sourceConceptName = function(sourceConceptName) {
      .setActiveString(private = private, key = ".sourceConceptName", value = sourceConceptName)
    },
    sourceConceptId = function(sourceConceptId) {
      .setActiveString(private = private, key = ".sourceConceptId", value = sourceConceptId)
    }
  )
)

## SourceConceptSetLineItem ----

#' @description
#' An R6 class to define a SourceConceptSetLineItem
#'
#' @export
SourceConceptSetLineItem <- R6::R6Class(
  classname = "SourceConceptSetLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(
    sectionLabel,
    domainTable,
    sourceConceptSet,
    timeInterval,
    statistic,
    typeConceptIds = c()
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTable,
        lineItemClass = "SourceConceptSet",
        valueDescription = "source_codeset_id",
        statistic = statistic,
        lineItemLabel = sourceConceptSet$sourceConceptName,
        timeInterval = timeInterval
      )

      .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "SourceConceptSet")
      .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)

    },

    grabSourceConceptSet = function() {
      scs <- private$sourceConceptSet
      return(scs)
    }
  ),
  private = list(
    sourceConceptSet = NULL,
    typeConceptIds = c()
  ),
  active = list()
)



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

## Concept Set Group -----------------
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

## Breaks Strategy -----------------
BreaksStrategy <- R6::R6Class(
  classname = "BreaksStrategy",
  public = list(

    initialize = function(name, labels, breaks, type) {

      .setString(private = private, key = ".name", value = name)
      .setCharacter(private = private, key = ".labels", value = labels)
      .setListofClasses(private = private, key = ".breaks", classes = character(0), value = breaks)
      .setString(private = private, key = ".type", value =  type)

    },

    makeCaseWhenSql = function(ordinalId) {

      # if the breaks are values do it this way
      if (self$type == "value") {
        sql_when <- tibble::tibble(
          lhs = self$breaks |> as.numeric(),
          rhs = dplyr::lead(self$breaks |> as.numeric()) - 0.01,
          label = self$labels
        ) |>
          dplyr::mutate(
            #ord = dplyr::row_number(),
            expr_left = glue::glue("{lhs} <= a.value"),
            expr_right = dplyr::if_else(!is.na(rhs), glue::glue("a.value <= {rhs}"), ""),
            expr_both = glue::glue("WHEN ({expr_left} AND {expr_right}) THEN '{label}'"),
            expr_both = dplyr::if_else(is.na(rhs), gsub(" AND ", "", expr_both), expr_both)
          ) |>
          dplyr::pull(expr_both) |>
          glue::glue_collapse(sep = "\n")
      }

      # if breaks are concepts do it this way
      if (self$type == "concept") {

        sql_when <- tibble::tibble(
          lhs = purrr::map(self$breaks, ~glue::glue_collapse(.x, sep = ", ")),
          label = self$labels
        ) |>
          dplyr::mutate(
            expr_both = glue::glue("WHEN a.value IN ({lhs}) THEN '{label}'")
          ) |>
          dplyr::pull(expr_both) |>
          glue::glue_collapse(sep = "\n")

      }

      # make final case when sql
      case_when_sql <- c(
        "SELECT *,",
        "\nCASE ",
        glue::glue_collapse(sql_when, sep = "\n\t"),
        "\nELSE 'Other' END AS break_id",
        "\nFROM @pat_ts_tab a",
        "\nWHERE ordinal_id = {ordinalId}"
      ) |>
        glue::glue_collapse() |>
        glue::glue()

      return(case_when_sql)
    }

  ),
  private = list(
    .name = NA_character_,
    .type = NA_character_,
    .labels = NA_character_,
    .breaks = NULL
  ),

  active = list(
    name = function(name) {
      .setActiveString(private = private, key = ".name", value = name)
    },
    type = function(type) {
      .setActiveString(private = private, key = ".type", value = type)
    },
    labels = function(labels) {
      .setActiveCharacter(private = private, key = ".labels", value = labels)
    },
    breaks = function(breaks) {
      .setActiveList(private = private, key = ".breaks", value = breaks, classes = character(0))
    }
  )
)

