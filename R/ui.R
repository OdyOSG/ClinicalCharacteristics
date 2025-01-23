

#' @title
#' Function to generate results for the table shell object
#'
#' @param tableShell The TableShell object to used for generation
#' @param executionSettings The ExecutionSettings object used to generate table shell
#' @param buildOptions The BuildOptions object used to generate table shell
#'
#' @return A list containing a tibble for categorical and continuous results
#'
#' @export
generateTableShell <- function(tableShell, executionSettings, buildOptions = NULL) {

  # add default build options if non provided
  if (is.null(buildOptions)){
    buildOptions <- defaultTableShellBuildOptions()
  }

  # Step 1: instantiate Tables
  tableShell$instantiateTables(
    executionSettings = executionSettings,
    buildOptions = buildOptions
  )

  # Step 2: Build Sql
  tsSql <- tableShell$buildTableShellSql(
    executionSettings = executionSettings,
    buildOptions = buildOptions
  )

  # Step 3: Execute them on dbms
  cli::cat_bullet(
    glue::glue_col("{yellow Executing Table Shell Sql}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  ## Execute on db
  DatabaseConnector::executeSql(
    connection = executionSettings$getConnection(),
    sql = tsSql
  )

  # Step 4: Extract Summaries
  cli::cat_bullet(
    glue::glue_col("{yellow Summarizing Table Shell}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  res <- tableShell$outputResults(executionSettings, buildOptions)


  # Step 5: Drop Temp Tables
  cli::cat_bullet(
    glue::glue_col("{yellow Drop Temp Tables from Process}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  tableShell$dropTempTables(executionSettings, buildOptions)

  return(res)
}


#' @title
#' Function that previews sql script used to generate results for table shell
#'
#' @param tableShell The TableShell object to used for generation
#' @param executionSettings The ExecutionSettings object used to generate table shell
#' @param buildOptions The BuildOptions object used to generate table shell
#'
#' @return A monaco widget in the viewer tab of RStudio with the sql script
#'
#' @export
reviewTableShellSql <- function(tableShell,
                                executionSettings,
                                buildOptions = NULL,
                                saveName = NULL,
                                savePath = here::here()){

  if (is.null(buildOptions)){
    buildOptions <- defaultTableShellBuildOptions()
  }

  if (is.null(saveName)) {
    saveName <- glue::glue("{tableShell$getName()}_table_shell") |>
      tolower() |>
      snakecase::to_snake_case() |>
      fs::path(ext = "sql")
  }

  # make save path
  savePath <- fs::path(savePath, saveName)

  # make sql file for table shell run
  tsSql <- tableShell$buildTableShellSql(
    executionSettings = executionSettings,
    buildOptions = buildOptions
  )

  cli::cat_bullet(
    glue::glue_col("Saving Table Shell SQL to {cyan {savePath}}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  readr::write_file(
    x = tsSql,
    file = savePath
  )

  invisible(tsSql)
}

# Archive -------------------------
# generateTableShell2 <- function(tableShell, executionSettings, buildOptions = NULL) {
#
#   if (is.null(buildOptions)){
#     buildOptions <- defaultTableShellBuildOptions()
#   }
#
#   # Step 0: user print specifying job info
#   tableShell$printJobDetails()
#
#   # Step 1: insert time windows
#   tableShell$insertTimeWindows(executionSettings, buildOptions)
#
#   # Step 2: make sql file for table shell run
#   sql <- tableShell$buildTableShellSql(executionSettings, buildOptions)
#
#   # Step 3: Execute them on dbms
#   cli::cat_bullet(
#     glue::glue_col("{yellow Executing shell sql}"),
#     bullet = "pointer",
#     bullet_col = "yellow"
#   )
#   DatabaseConnector::executeSql(connection = executionSettings$getConnection(), sql = sql)
#
#   # Step 4: Transform results (continuous => categorical; categorical => continuous)
#   # tableShell$categorizeItems(executionSettings)
#   # tableShell$scoreItems(executionSettings)
#
#   # Step 5: Aggregate Results Table
#   # step 5a: aggregate categorical results first
#   cli::cat_bullet(
#     glue::glue_col("{yellow Aggregating and formatting results}"),
#     bullet = "pointer",
#     bullet_col = "yellow"
#   )
#   categoricalResults <- tableShell$aggregateTableShell(executionSettings, buildOptions, type = "categorical")
#   #categoricalResultsFormatted <- tableShell$.labelResults(categoricalResultsRaw, type = "categorical")
#
#   #step 5b: aggregate continuous results second
#   continuousResults <- tableShell$aggregateTableShell(executionSettings, buildOptions, type = "continuous")
#   #continuousResultsFormatted <- tableShell$.labelResults(continuousResultsRaw, type = "continuous")
#
#   # keep dat Temp table for future use
#   if (buildOptions$keepResultsTable) {
#     # TODO create ctas to save temp table
#   } else{
#     #close on exit
#     executionSettings$disconnect()
#   }
#
#   tableShellResults <- list(
#     'categorical' = categoricalResults,
#     'continuous' = continuousResults
#   )
#
#   return(tableShellResults)
# }




# function that shows sql used to make table shell
# reviewTableShellSql <- function(tableShell, executionSettings, buildOptions = NULL){
#
#   if (is.null(buildOptions)){
#     buildOptions <- defaultTableShellBuildOptions()
#   }
#
#   # make sql file for table shell run
#   sql <- tableShell$buildTableShellSql(executionSettings, buildOptions)
#
#   cli::cat_bullet(
#     glue::glue("Opening Table Shell Query Sql in Monaco widget"),
#     bullet = "pointer",
#     bullet_col = "yellow"
#   )
#
#   mnc <- monaco::monaco(
#     contents = sql,
#     language = "sql",
#     theme = "vs"
#   )
#
#   return(mnc)
# }


# function that creates text summary of how table shells were created
# buildTableShellReport <- function(tableShell) {
#
# }
#
# # function to save table shell results
# saveTableShell <- function(result, outputPath) {
#
# }

# creates reactable output of the tables shells
# previewTableShell <- function(results, type) {
#
#   if (type == "categorical") {
#     cat_dat <- results$categorical
#
#     res_tb <- reactable::reactable(
#       data = cat_dat,
#       columns = list(
#         'cohortId' = reactable::colDef(name = "Cohort Id"),
#         'cohortName' = reactable::colDef(name = "Cohort Name"),
#         'categoryId' = reactable::colDef(name = "Category Id"),
#         'categoryLabel' = reactable::colDef(name = "Category Name"),
#         'timeId' = reactable::colDef(name = "Time Id"),
#         'twLabel' = reactable::colDef(name = "Time Name"),
#         'valueId' = reactable::colDef(name = "Value Id"),
#         'name' = reactable::colDef(name = "Value Name"),
#         'n' = reactable::colDef(
#           name = "n", format = reactable::colFormat(separators = TRUE)
#         ),
#         'pct' = reactable::colDef(
#           name = "pct", format = reactable::colFormat(percent = TRUE, digits = 2)
#         )
#       ),
#       highlight = TRUE,
#       bordered = TRUE,
#       outlined = TRUE,
#       resizable = TRUE,
#       filterable = TRUE,
#       searchable = TRUE
#     )
#   }
#
#   if (type == "continuous") {
#
#     cts_dat <- results$continuous
#
#     res_tb <- reactable::reactable(
#       data = cts_dat,
#       columns = list(
#         'cohortId' = reactable::colDef(name = "Cohort Id"),
#         'cohortName' = reactable::colDef(name = "Cohort Name"),
#         'categoryId' = reactable::colDef(name = "Category Id"),
#         'categoryLabel' = reactable::colDef(name = "Category Name"),
#         'timeId' = reactable::colDef(name = "Time Id"),
#         'twLabel' = reactable::colDef(name = "Time Name"),
#         'valueId' = reactable::colDef(name = "Value Id"),
#         'n' = reactable::colDef(
#           name = "n", format = reactable::colFormat(separators = TRUE)
#         ),
#         'mean' = reactable::colDef(
#           name = "mean", format = reactable::colFormat(separators = TRUE, digits = 2)
#         ),
#         'sd' = reactable::colDef(
#           name = "sd", format = reactable::colFormat(separators = TRUE, digits = 2)
#         ),
#         'min' = reactable::colDef(
#           name = "min", format = reactable::colFormat(separators = TRUE)
#         ),
#         'p25' = reactable::colDef(
#           name = "25th", format = reactable::colFormat(separators = TRUE, digits = 0)
#         ),
#         'median' = reactable::colDef(
#           name = "median", format = reactable::colFormat(separators = TRUE, digits = 0)
#         ),
#         'p75' = reactable::colDef(
#           name = "75th", format = reactable::colFormat(separators = TRUE, digits = 0)
#         ),
#         'max' = reactable::colDef(
#           name = "max", format = reactable::colFormat(separators = TRUE)
#         )
#       ),
#       highlight = TRUE,
#       bordered = TRUE,
#       outlined = TRUE,
#       resizable = TRUE,
#       filterable = TRUE,
#       searchable = TRUE
#     )
#   }
#
#   return(res_tb)
# }
