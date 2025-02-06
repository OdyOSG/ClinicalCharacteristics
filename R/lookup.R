#' Function to create a source concept set
#'
#' @param sourceConceptTable a dataframe with source concepts from the OMOP vocabulary
#' @param name the name of source concept set
#'
#' @return a SourceConceptSet R6 class specifing the source concepts in use
#' @export
sourceConceptSet <- function(sourceConceptTable, name) {
  scs <- SourceConcepSet$new(
    sourceConceptName = glue::glue("{name} (Source Concept)"),
    sourceConceptId = digest::digest(sourceConceptTable, algo = "md5"),
    sourceConceptSet = sourceConceptTable
  )
  return(scs)
}

#' Function to look up source concepts in the OMOP Vocabulary
#'
#' @param codes a character string of codes to search
#' @param vocabulary the vocabulary to use in search of codes
#' @param executionSettings The ExecutionSettings object used to connect to the dbms
#'
#' @return a tibble of four columns: conceptId, conceptName, conceptCode, vocabularyId
#'
#' @export
lookupSourceConcepts <- function(codes, vocabulary, executionSettings) {

  # collapse codes to a string
  codeList <- codes |>
    glue::glue_collapse(sep = "', '")


  sql <- "/* Lookup source concept */
  SELECT concept_id, concept_name, concept_code, vocabulary_id
  FROM @vocabulary_database_schema.concept
  WHERE concept_code IN ('@code_list')
    AND vocabulary_id = '@vocabulary';
  " |>
    SqlRender::render(
      vocabulary_database_schema = executionSettings$cdmDatabaseSchema,
      code_list = codeList,
      vocabulary = vocabulary
    )

  cli::cat_bullet(
    glue::glue_col("Look-up source codes in {yellow {vocabulary}}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  # establish connection to database
  connection <- executionSettings$getConnection()

  if (is.null(connection)) {
    connection <- executionSettings$connect()
  }


  ## Execute on db
  tb <- DatabaseConnector::querySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE
  ) |>
    tibble::as_tibble()

  executionSettings$disconnect()

  return(tb)

}


