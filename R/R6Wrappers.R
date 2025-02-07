#' @title
#' Create an empty TableShell object and set its title
#'
#' @param title The title of the TableShell
#' @param targetCohorts A list of TargetCohort objects
#' @param lineItems A list of lineItem objects
#'
#' @return A TableShell object
#'
#' @export
createTableShell <- function(title,
                             targetCohorts,
                             lineItems) {
    tableShell <- TableShell$new(name = title,
                                 targetCohorts = targetCohorts,
                                 lineItems = lineItems)
    return(tableShell)
}

#' @title
#' Parse cohort info from a data frame
#'
#' @param df The data frame containing the information for the cohorts (id and name)
#'
#' @return A list of CohortInfo objects
#'
#' @export
parseCohortInfoFromDf <- function(df) {
  cohortInfo <- purrr::pmap(df, function(id, name) {
    createCohortInfo(id, name)
  })
  return(cohortInfo)
}


#' @title
#' Create a CohortInfo object for a cohort and set its attributes
#'
#' @param id The ID of the cohort
#' @param name The name of the cohort
#'
#' @return A CohortInfo object
#'
#' @export
createCohortInfo <- function(id, name) {
  cohortInfo <- CohortInfo$new(id, name)
  return(cohortInfo)
}

#' @title
#' Create an ExecutionSettings object and set its attributes
#'
#' @param connectionDetails A DatabaseConnector connectionDetails object (optional if connection is specified)
#' @param connection A DatabaseConnector connection object (optional if connectionDetails is specified)
#' @param cdmDatabaseSchema The schema of the OMOP CDM database
#' @param workDatabaseSchema The schema to which results will be written
#' @param tempEmulationSchema Some database platforms like Oracle and Snowflake do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
#' @param targetCohortTable The name of the table where the target cohort(s) are stored
#' @param cdmSourceName A human-readable name for the OMOP CDM source
#'
#' @return An ExecutionSettings object
#' @export
createExecutionSettings <- function(connectionDetails,
                                    connection = NULL,
                                    cdmDatabaseSchema,
                                    workDatabaseSchema,
                                    tempEmulationSchema,
                                    targetCohortTable,
                                    cdmSourceName) {
  executionSettings <- ExecutionSettings$new(connectionDetails = connectionDetails,
                                             connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             workDatabaseSchema = workDatabaseSchema,
                                             tempEmulationSchema = tempEmulationSchema,
                                             targetCohortTable = targetCohortTable,
                                             cdmSourceName = cdmSourceName)
  return(executionSettings)
}

#' @title
#' Default build options to generate table shell
#'
#' @param connectionDetails A DatabaseConnector connectionDetails object (optional if connection is specified)
#' @param codesetTempTable the name of the codeset table used in execution. Defaults as a temp table #codeset
#' @param timeWindowTempTable the name of the time Window table used in execution. Defaults as a temp table #time_windows
#' @param targetCohortTempTable the name of the target cohort table used in execution. Defaults as a temp table #target_cohorts
#' @param tsMetaTempTable the name of the table shell meta table used in execution. Defaults as a temp table #ts_meta
#' @param conceptSetOccurrenceTempTable the name of the concept set occurrence table used in execution. Defaults as a temp table #concept_set_occ
#' @param cohortOccurrenceTempTable the name of the cohort occurrence  table used in execution. Defaults as a temp table #cohort_occ
#' @param patientLevelDataTempTable the name of the patient level data table used in execution. Note this does not contain info of the table shell. Defaults as a temp table #patient_data
#' @param patientLevelTableShellTempTable the name of the patient level data table with additional meta info used in execution. Defaults as a temp table #pat_ts_tab
#' @param categoricalSummaryTempTable the name of the categorical summary table used in execution. Defaults as a temp table #categorical_table
#' @param continuousSummaryTempTable the name of the continuous summary table used in execution. Defaults as a temp table #continuous_table
#'
#' @return A BuildOptions object
#' @export
defaultTableShellBuildOptions <- function(codesetTempTable = "#codeset",
                                          sourceCodesetTempTable = "#source_codeset",
                                          timeWindowTempTable = "#time_windows",
                                          targetCohortTempTable = "#target_cohorts",
                                          tsMetaTempTable = "#ts_meta",
                                          conceptSetOccurrenceTempTable = "#concept_set_occ",
                                          cohortOccurrenceTempTable = "#cohort_occ",
                                          patientLevelDataTempTable = "#patient_data",
                                          patientLevelTableShellTempTable = "#pat_ts_tab",
                                          categoricalSummaryTempTable = "#categorical_table",
                                          continuousSummaryTempTable = "#continuous_table"
                                          ) {

  buildOpts <- BuildOptions$new(
    codesetTempTable = codesetTempTable,
    sourceCodesetTempTable = sourceCodesetTempTable,
    timeWindowTempTable = timeWindowTempTable,
    tsMetaTempTable = tsMetaTempTable,
    targetCohortTempTable = targetCohortTempTable,
    conceptSetOccurrenceTempTable = conceptSetOccurrenceTempTable,
    cohortOccurrenceTempTable = cohortOccurrenceTempTable,
    patientLevelDataTempTable = patientLevelDataTempTable,
    patientLevelTableShellTempTable = patientLevelTableShellTempTable,
    categoricalSummaryTempTable = categoricalSummaryTempTable,
    continuousSummaryTempTable = continuousSummaryTempTable
  )
  return(buildOpts)

}


#' @title
#' Create a single time interval
#' @param lb the left bound of the time interval
#' @param rb the right bound of the time interval
#'
#' @return A time interval object
#'
#' @export
timeInterval <- function(lb, rb) {
  ti <- TimeInterval$new(lb = lb, rb = rb)
  return(ti)
}


# createPresence <- function(operator = "at_least", occurrences = 1) {
#   pres <- Presence$new(operator = operator, occurrences = occurrences)
#   return(pres)
# }

#' @title
#' Create a presence stat where any occurrence is valid
#'
#' @return A presence stat object
#'
#' @export
anyPresenceStat <- function() {
  pres <- Presence$new(personLine = "anyCount")
  return(pres)
}

#' @title
#' Create a presence stat where only occurrence during the observation period are valid
#'
#' @return A presence stat object
#'
#' @export
observedPresenceStat <- function() {
  pres <- Presence$new(personLine = "observedCount")
  return(pres)
}



#' @title
#' Create a count stat where any occurrence is valid.
#'
#'
#' @return A stat object continuousDistribution
#'
#' @export
anyCountCtsStat <- function() {
    stat <- ContinuousDistribution$new(personLine = "anyCount")
  return(stat)
}

#' @title
#' Create a count stat with breaks where any occurrence is valid.
#'
#' @param breaks a breaksStrategy object dictating how to classify counts into categories.
#' If null then this defaults to a continuous distribution
#'
#' @return A stat object breaks
#'
#' @export
anyCountBreaksStat <- function(breaks) {
  stat <- Breaks$new(personLine = "anyCount", breaks = breaks)
  return(stat)
}

#' @title
#' Create a count stat where only occurrence during the observation period are valid
#'
#'
#' @return A stat object continuousDistribution
#'
#' @export
observedCountCtsStat <- function() {
  stat <- ContinuousDistribution$new(personLine = "observedCount")
  return(stat)
}

#' @title
#' Create a count stat with breaks where only occurrence during the observation period are valid
#'
#' @param breaks a breaksStrategy object dictating how to classify counts into categories.
#' If null then this defaults to a continuous distribution
#'
#' @return A stat object breaks
#'
#' @export
observedCountBreaksStat <- function(breaks) {
  stat <- Breaks$new(personLine = "observedCount", breaks = breaks)
  return(stat)
}


#' @title
#' Create a time to stat where any occurrence is valid
#'
#'
#' @return A stat object continuousDistribution
#'
#' @export
anyTimeToFirstCtsStat <- function() {
  stat <- ContinuousDistribution$new(personLine = "anyTimeToFirst")
  return(stat)
}

#' @title
#' Create a time to stat with breaks where any occurrence is valid
#'
#' @param breaks a breaksStrategy object dictating how to classify counts into categories.
#' If null then this defaults to a continuous distribution
#'
#' @return A stat object breaks
#'
#' @export
anyTimeToFirstBreaksStat <- function(breaks) {
  stat <- Breaks$new(personLine = "anyTimeToFirst", breaks = breaks)
  return(stat)
}

#' @title
#' Create a continuous time to stat where only occurrence during the observation period are valid
#'
#'
#' @return A stat object continuousDistribution
#'
#' @export
observedTimeToFirstCtsStat <- function() {
  stat <- ContinuousDistribution$new(personLine = "observedTimeToFirst")
  return(stat)
}

#' @title
#' Create a time to stat with breaks where only occurrence during the observation period are valid
#'
#' @param breaks a breaksStrategy object dictating how to classify counts into categories.
#' If null then this defaults to a continuous distribution
#'
#' @return A stat object breaks
#'
#' @export
observedTimeToFirstBreaksStat <- function(breaks) {
  stat <- Breaks$new(personLine = "observedTimeToFirst", breaks = breaks)
  return(stat)
}

anyScore <- function(weight) {
  stat <- Score$new(
    personLine = "anyCount",
    weight = weight
  )
  return(stat)
}


#' @title
#' Create a concept set line item and set its attributes
#'
#' @param sectionLabel (OPTIONAL) The name of the line item (if not provided, the name will be set to the Capr concept set name)
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param domain The domain of the concept set (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param conceptSet The Capr concept set object
#' @param timeInterval The Time Interval object used for the line item
#' @param sourceConceptSet (OPTIONAL) A Capr concept set of source concept IDs to use to limit the concept set
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A ConceptSetLineItem object
#'
#' @export
createConceptSetLineItem <- function(sectionLabel = NA_character_,
                                     domain,
                                     conceptSet,
                                     timeInterval,
                                     statistic,
                                     sourceConceptSet = NULL,
                                     typeConceptIds = c(),
                                     visitOccurrenceConceptIds = c()) {

  if(is.na(sectionLabel)) {
    sectionLabel <- conceptSet@Name
  }

  csDefinition <- ConceptSetLineItem$new(sectionLabel = sectionLabel,
                                         domainTable = domain,
                                         conceptSet = conceptSet,
                                         timeInterval = timeInterval,
                                         statistic = statistic,
                                         sourceConceptSet = sourceConceptSet,
                                         typeConceptIds = typeConceptIds,
                                         visitOccurrenceConceptIds = visitOccurrenceConceptIds)
  return(csDefinition)
}



#' @title
#' Create a batch of concept set line items from a list of Capr concept sets.
#'
#' @description
#' The name of each line item will be set to the name of its Capr concept set. All line items will use the same statistic, domain, type concepts, and visit concepts. It is not possible to specify source concept IDs.
#' @param sectionLabel The name of the concept set batch
#' @param statistic The Statistic object to be used to evaluate the line items
#' @param domain The domain of the concept sets (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param conceptSets A list of concept set Capr objects
#' @param timeIntervals A list of TimeInterval class objects
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A list of ConceptSetLineItem objects
#'
#' @export
createConceptSetLineItemBatch <- function(
    sectionLabel,
    domain,
    conceptSets,
    timeIntervals,
    statistic,
    typeConceptIds = c(),
    visitOccurrenceConceptIds = c()) {

  checkmate::assert_list(x = conceptSets, types = c("ConceptSet"), null.ok = FALSE, min.len = 1)
  checkmate::assert_list(x = timeIntervals, types = c("TimeInterval"), null.ok = FALSE, min.len = 1)

  # build permutations of concepts and timeIntervals
  permDf <- .permuteTi(conceptSets, timeIntervals)

  # deal with different statistic input
  # typical only add a single statistic class
  # but if there is a list of statistics for a score handle it
  if (class(statistic) == "list") {
    if (length(statistic) != length(permDf[[1]])) {
      stop("If the statistic is a list, it must be the same length as all combinations of conceptSets and timeIntervals")
    }
    statPerm <- statistic
  } else {
    if (class(statistic)[[2]] == "Statistic") {
      statPerm <- rep(list(statistic), each = length(permDf[[1]]))
    } else{
      stop("statistic slot is not of Statistic Class")
    }
  }


  # create batch of concept set line items
  csLiBatch <- purrr::pmap(
    list(
      permDf$objects,
      permDf$timeIntervals,
      statPerm
    ),
    ~createConceptSetLineItem(
      sectionLabel = sectionLabel,
      statistic = ..3,
      domain = domain,
      conceptSet = ..1,
      timeInterval = ..2,
      sourceConceptSet = NULL,
      typeConceptIds = typeConceptIds,
      visitOccurrenceConceptIds = visitOccurrenceConceptIds
    )
  ) |>
    unname()

  return(csLiBatch)
}


#' @title
#' Create a source concept set line item and set its attributes
#'
#' @param sectionLabel (OPTIONAL) The name of the line item (if not provided, the name will be set to the Capr concept set name)
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param domain The domain of the concept set (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param sourceConceptSet A SourceConcept R6 object created using the `sourceConceptSet` function
#' @param timeInterval The Time Interval object used for the line item
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#'
#' @return A SourceConceptSetLineItem object
#'
#' @export
createSourceConceptSetLineItem <- function(sectionLabel = NA_character_,
                                           domain,
                                           sourceConceptSet,
                                           timeInterval,
                                           statistic,
                                           typeConceptIds = c()) {

  if(is.na(sectionLabel)) {
    sectionLabel <- sourceConceptSet$sourceConceptName
  }

  scsDefinition <- SourceConceptSetLineItem$new(
    sectionLabel = sectionLabel,
    domainTable = domain,
    sourceConceptSet = sourceConceptSet,
    timeInterval = timeInterval,
    statistic = statistic,
    typeConceptIds = typeConceptIds
  )
  return(scsDefinition)

}


#' @title
#' Create a batch of source concept set line items from a list of SourceConceptSet classes.
#'
#' @param sectionLabel (OPTIONAL) The name of the line item (if not provided, the name will be set to the Capr concept set name)
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param domain The domain of the concept set (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param sourceConceptSet A list of SourceConcept R6 object created using the `sourceConceptSet` function
#' @param timeIntervals A list of TimeInterval class objects
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#'
#' @return A list of SourceConceptSetLineItem objects
#'
#' @export
createSourceConceptSetLineItemBatch <- function(sectionLabel,
                                           domain,
                                           sourceConceptSets,
                                           timeIntervals,
                                           statistic,
                                           typeConceptIds = c()) {


  checkmate::assert_list(x = sourceConceptSets, types = c("SourceConceptSet"), null.ok = FALSE, min.len = 1)
  checkmate::assert_list(x = timeIntervals, types = c("TimeInterval"), null.ok = FALSE, min.len = 1)

  # build permutations of concepts and timeIntervals
  permDf <- .permuteTi(sourceConceptSets, timeIntervals)

  # create batch of concept set line items
  scsLiBatch <- purrr::map2(
    permDf$objects,
    permDf$timeIntervals,
    ~createSourceConceptSetLineItem(
      sectionLabel = sectionLabel,
      statistic = statistic,
      domain = domain,
      sourceConceptSet = .x,
      timeInterval = .y,
      typeConceptIds = typeConceptIds
    )
  ) |>
    unname()

  return(scsLiBatch)
}




#' @title
#' Create a demographic line item and set its attributes
#'
#' @param statistic The Statistic object to be used to evaluate the line item
#' @return A DemographicLineItem object
#'
#' @export
createDemographicLineItem <- function(statistic) {
  dcli <- DemographicLineItem$new(
    statistic = statistic
  )
  statLabel <- class(statistic)[[1]]

  if (statLabel == "DemographicAge") {
    dcli$valueId <- -999
    dcli$valueDescription <- "year_of_birth"
  }

  if (statLabel == "DemographicConcept") {
    dcli$valueId <- statistic$getConceptId()
    dcli$valueDescription <- statistic$getConceptColumn()
  }

  return(dcli)
}

#' @title
#' Create a age statistic with breaks
#'
#' @param breaks a breaksStrategy object dictating how to classify counts into categories
#' @return A DemographicAge Statistic class object with breaks
#'
#' @export
ageCharBreaks <- function(breaks) {
  ageChar <- DemographicAge$new(
    statType = "breaks",
    aggType = "categorical",
    breaks = breaks
  )
  return(ageChar)
}

#' @title
#' Create a age statistic as continuous
#'
#' @return A DemographicAge Statistic class object as continuous
#'
#' @export
ageCharCts <- function() {
  ageChar <- DemographicAge$new(
    statType = "continuousDistribution",
    aggType = "continuous",
    breaks = NULL
  )
  return(ageChar)
}

#' @title
#' Create a male concept stat
#'
#' @return A DemographicConcept Statistic class object indicating a male concept
#'
#' @export
maleGender <- function() {
  maleConcept <- DemographicConcept$new(
    demoCategory = "Gender",
    demoLine = "Male",
    conceptColumn = "gender_concept_id",
    conceptId = 8507L
  )
  return(maleConcept)
}

#' @title
#' Create a female concept stat
#'
#' @return A DemographicConcept Statistic class object indicating a female concept
#'
#' @export
femaleGender <- function() {
  femaleConcept <- DemographicConcept$new(
    demoCategory = "Gender",
    demoLine = "Female",
    conceptColumn = "gender_concept_id",
    conceptId = 8532L
  )
  return(femaleConcept)
}

#' @title
#' Create a breaks Strategy object for categorizing
#'
#' @param name the name of the breaks
#' @param breaks a vector with cut points to user
#' @param labels a character vector indicating how to label the cut-point. Can stay NULL where a default label is given
#'
#' @return A BreaksStreategy object
#'
#' @export
newBreaks <- function(name, breaks, labels = NULL) {
  if (is.null(labels)) {
    a <- dplyr::lead(breaks)
    lab <- glue::glue("[{breaks}-{a})")[-length(breaks)]
    labels <- c(lab, paste0(dplyr::last(breaks), "+"))
  }

  br <- BreaksStrategy$new(
    name = name,
    breaks = breaks,
    labels = labels
  )

  return(br)
}

#' @title
#' Create a cohort line item and set its attributes
#'
#' @param sectionLabel (OPTIONAL) The name of the line item (if not provided, the name will be set to the cohort name from the CohortInfo object)
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param cohort A CohortInfo object
#' @param timeInterval The TimeInterval object used for the line item
#'
#' @return A CohortLineItem object
#'
#' @export
createCohortLineItem <- function(sectionLabel = NA_character_,
                                 covariateCohort,
                                 cohortTable,
                                 timeInterval,
                                 statistic) {

  if(is.na(sectionLabel)) {
    sectionLabel <- covariateCohort$getName()
  }

  chDefinition <- CohortLineItem$new(sectionLabel = sectionLabel,
                                     domainTable = cohortTable,
                                     covariateCohort = covariateCohort,
                                     timeInterval = timeInterval,
                                     statistic = statistic)
  chDefinition$valueId <- covariateCohort$getId()
  return(chDefinition)

}

#' @title
#' Create a batch of cohort line items from a list of CohortInfo objects.
#'
#' @description
#' The name of each line item will be set to the name of its cohort from the CohortInfo object.
#' @param sectionLabel The name of the cohort batch
#' @param statistic The Statistic object to be used to evaluate the line items
#' @param cohorts A list of CohortInfo objects
#' @param timeIntervals A list of TimeInterval class objects
#'
#' @return A list of CohortLineItem objects
#'
#' @export
createCohortLineItemBatch <- function(
    sectionLabel,
    covariateCohorts,
    cohortTable,
    statistic,
    timeIntervals) {

  checkmate::assert_list(x = covariateCohorts, types = c("CohortInfo"), null.ok = FALSE, min.len = 1)
  checkmate::assert_list(x = timeIntervals, types = c("TimeInterval"), null.ok = FALSE, min.len = 1)

  # build permutations of concepts and timeIntervals
  permDf <- .permuteTi(covariateCohorts, timeIntervals)

  # create batch of concept set line items
  # create batch of concept set line items
  chLiBatch <- purrr::map2(
    permDf$objects,
    permDf$timeIntervals,
    ~createCohortLineItem(
      sectionLabel = sectionLabel,
      covariateCohort = .x,
      cohortTable,
      timeInterval= .y,
      statistic = statistic
    )
  ) |>
    unname()


  return(chLiBatch)
}


createConceptSetGroupLineItem <- function(sectionLabel = NA_character_,
                                          groupLabel,
                                          conceptSets,
                                          domainTables,
                                          timeInterval,
                                          statistic) {

  if(is.na(sectionLabel)) {
    sectionLabel <- groupLabel
  }

  csgDefinition <- ConceptSetGroupLineItem$new(
    sectionLabel = sectionLabel,
    groupLabel = groupLabel,
    domainTables = domainTables,
    conceptSets = conceptSets,
    timeInterval = timeInterval,
    statistic = statistic
  )
  return(csgDefinition)

}

#' @title
#' Combine all lineItems to enter into the tableShell slot
#'
#' @param ... A list of lineItems created from various calls
#' @return a flattened list of lineItems
#' @export
lineItems <- function(...) {
  listOfLineItems <- list(...) |>
    purrr::list_flatten()
  # ensure that all elements are lineItems
  checkmate::assert_list(x = listOfLineItems, types = "LineItem", null.ok = FALSE, min.len = 1)

  # add in ordinals
  ii <- seq_along(listOfLineItems)
  for(i in ii) {
    listOfLineItems[[i]]$ordinalId <- i
  }

  # add value ids for the concept sets
  listOfLineItems <- .setCsValueId(listOfLineItems)

  # add value ids for the source concept sets
  listOfLineItems <-  .setSourceValueId(listOfLineItems)

  return(listOfLineItems)
}

