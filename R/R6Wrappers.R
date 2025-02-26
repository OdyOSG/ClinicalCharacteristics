# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of ClinicalCharacteristics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Create an empty TableShell object and set its title
#'
#' @template Title
#' @template TargetCohorts
#' @template LineItems
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


#' Parse cohort info from a data frame
#'
#' @param df                                The data frame containing the information for the cohorts (id and name)
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


#' Create a CohortInfo object for a cohort and set its attributes
#'
#' @param id                                The ID of the cohort
#' @param name                              The name of the cohort
#'
#' @return A CohortInfo object
#'
#' @export
createCohortInfo <- function(id, name) {
  cohortInfo <- CohortInfo$new(id, name)
  return(cohortInfo)
}


#' Create an ExecutionSettings object and set its attributes
#'
#' @template ConnectionDetails
#' @template CdmDatabaseSchema
#' @template WorkDatabaseSchema
#' @template TempEmulationSchema
#' @template TargetCohortTable
#' @template CdmSourceName
#'
#' @return An ExecutionSettings object
#'
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


#' Default build options to generate table shell
#'
#' @template ConnectionDetails
#' @template DefaultTableShellBuildOptions
#'
#' @return A BuildOptions object
#'
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


#' Create a single time interval
#'
#' @param lb                                the left bound of the time interval
#' @param rb                                the right bound of the time interval
#'
#' @return A time interval object
#'
#' @export
timeInterval <- function(lb, rb) {
  ti <- TimeInterval$new(lb = lb, rb = rb)
  return(ti)
}


#' Create a presence stat where any occurrence is valid
#'
#' @return A presence stat object
#'
#' @export
anyPresenceStat <- function() {
  pres <- Presence$new(personLine = "anyCount")
  return(pres)
}


#' Create a presence stat where only occurrence during the observation period are valid
#'
#' @return A presence stat object
#'
#' @export
observedPresenceStat <- function() {
  pres <- Presence$new(personLine = "observedCount")
  return(pres)
}



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


#' Create a count stat with breaks where any occurrence is valid.
#'
#' @template Breaks
#'
#' @return A stat object breaks
#'
#' @export
anyCountBreaksStat <- function(breaks) {
  stat <- Breaks$new(personLine = "anyCount", breaks = breaks)
  return(stat)
}


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


#' Create a count stat with breaks where only occurrence during the observation period are valid
#'
#' @template Breaks
#'
#' @return A stat object breaks
#'
#' @export
observedCountBreaksStat <- function(breaks) {
  stat <- Breaks$new(personLine = "observedCount", breaks = breaks)
  return(stat)
}



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


#' Create a time to stat with breaks where only occurrence during the observation period are valid
#'
#' @template Breaks
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



#' Create a concept set line item and set its attributes
#'
#' @template SectionLabel
#' @template Statistic
#' @template Domain
#' @template ConceptSet
#' @template TimeInterval
#' @template SourceConceptSet
#' @template TypeConceptIds
#' @template VisitOccurrenceConceptIds
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




#' Create a batch of concept set line items from a list of Capr concept sets.
#'
#' @description
#' The name of each line item will be set to the name of its Capr concept set. All line items will use the same statistic, domain, type concepts, and visit concepts. It is not possible to specify source concept IDs.
#'
#' @template SectionLabel
#' @template Statistic
#' @template ConceptSets
#' @template TimeIntervals
#' @template TypeConceptIds
#' @template VisitOccurrenceConceptIds
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
  if (class(statistic)[[1]] == "list") {
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



#' Create a source concept set line item and set its attributes
#'
#' @template SectionLabel
#' @template Statistic
#' @template Domain
#' @template SourceConceptSet
#' @template TimeInterval
#' @template TypeConceptIds
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



#' Create a batch of source concept set line items from a list of SourceConceptSet classes.
#'
#' @template SectionLabel
#' @template Statistic
#' @template Domain
#' @template SourceConceptSets
#' @template TimeIntervals
#' @template TypeConceptIds
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





#' Create a demographic line item and set its attributes
#'
#' @template Statistic
#'
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

  if (statLabel == "DemographicIndexYear") {
    dcli$valueId <- -999
    dcli$valueDescription <- "cohort_start_date"
  }

  if (statLabel == "DemographicCohortTime") {
    dcli$valueId <- -999
    dcli$valueDescription <- "cohort_follow_up"
  }

  if (statLabel == "DemographicPayerType") {
    dcli$valueId <- -999
    dcli$valueDescription <- "payer_concept_id"
  }

  if (statLabel == "DemographicLocation") {
    dcli$valueId <- -999
    dcli$valueDescription <- "location_id"
  }


  return(dcli)
}



#' Create an index year char
#'
#' @template Breaks
#'
#' @return A DemographicIndexYear Statistic class object
#'
#' @export
indexYear <- function(breaks = NULL) {

  if (is.null(breaks)) {
    breaks <- defaultYearGrp()
  }

  indexYearChar <- DemographicIndexYear$new(breaks)
  return(indexYearChar)
}



#' Create a cohort follow up time char
#'
#' @return A DemographicCohortTime Statistic class object
#'
#' @export
cohortFollowupTime <- function() {

  cohortFollowupChar <- DemographicCohortTime$new()
  return(cohortFollowupChar)
}



#' Create a location char
#'
#' @template Breaks
#'
#' @return A DemographicLocation Statistic class object
#'
#' @export
personLocation <- function(breaks) {

  personLocationChar <- DemographicLocation$new(breaks)
  return(personLocationChar)
}



#' Create an payer type char
#'
#' @template Breaks
#'
#' @return A DemographicIndexYear Statistic class object
#'
#' @export
payerType <- function(breaks = NULL) {

  if (is.null(breaks)) {
    breaks <- soptPayers()
  }

  payerTypeChar <- DemographicPayerType$new(breaks)
  return(payerTypeChar)
}



#' Create a age statistic with breaks
#'
#' @template Breaks
#'
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


#' Create a breaks Strategy object for categorizing
#'
#' @param name the name of the breaks
#' @template Breaks
#' @param labels a character vector indicating how to label the cut-point. Can stay NULL where a default label is given
#'
#' @return A BreaksStreategy object
#'
#' @export
newValueBreaks <- function(name, breaks, labels = NULL) {
  if (is.null(labels)) {
    a <- dplyr::lead(breaks)
    lab <- glue::glue("[{breaks}-{a})")[-length(breaks)]
    labels <- c(lab, paste0(dplyr::last(breaks), "+"))
  }

  br <- BreaksStrategy$new(
    name = name,
    breaks = breaks,
    labels = labels,
    type = "value"
  )

  return(br)
}

newConceptBreaks <- function(name, breaks, labels) {

  br <- BreaksStrategy$new(
    name = name,
    breaks = breaks,
    labels = labels,
    type = "concept"
  )

  return(br)
}



#' Create a cohort line item and set its attributes
#'
#' @template SectionLabel
#' @template Statistic
#' @param cohort                A CohortInfo object
#' @template TimeInterval
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


#' Create a batch of cohort line items from a list of CohortInfo objects.
#'
#' @description
#' The name of each line item will be set to the name of its cohort from the CohortInfo object.
#'
#' @template SectionLabel
#' @param covariateCohorts                  A list of CohortInfo objects
#' @template CohortTable
#' @template Statistic
#' @template TimeIntervals
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


#' Create a concept set group item and set its attributes
#'
#' @template SectionLabel
#' @param groupLabel the label of the group
#' @template ConceptSets
#' @param domainTables a vector of domains corresponding to the concept set
#' @template TimeInterval
#' @template Statistic
#'
#' @return A CohortLineItem object
#'
#' @export
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

  lineItemClassType <- purrr::map_chr(listOfLineItems, ~.x$lineItemClass)

  if (any(lineItemClassType == "ConceptSet")) {
    # add value ids for the concept sets
    listOfLineItems <- .setCsValueId(listOfLineItems)
  }

  if (any(lineItemClassType == "SourceConceptSet")) {
  # add value ids for the source concept sets
  listOfLineItems <-  .setSourceValueId(listOfLineItems)
  }

  return(listOfLineItems)
}

