#' @title
#' Create a breaks Strategy object for year
#'
#' @param startYear the year to start the year group sequence. By default this is the year 2000
#'
#' @return A BreaksStrategy object with defaults assumptions for 5 year age groups
#'
#' @export
defaultYearGrp <- function(startYear = NULL) {

  if(is.null(startYear)) {
    startYear <- 2000
  }

  thisYear <- lubridate::year(lubridate::today())
  x <- seq(startYear, thisYear, by = 1)
  #a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}")

  br <- newValueBreaks(
    name = glue::glue("Default Years ({startYear}-{thisYear})"),
    breaks = x |> as.list()
  )

  br$labels <- lab

  return(br)
}

.soptPayers <- function() {
  soptPayersTypes <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("csv", "soptPayerTypes.csv")
  ) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::arrange(conceptId)

  br <- newConceptBreaks(
    name = glue::glue("SOPT Payer Types"),
    breaks = as.list(soptPayersTypes$conceptId),
    labels = soptPayersTypes$conceptName
  )

  return(br)

}


.raceBreaks <- function() {

  br <- newConceptBreaks(
    name = glue::glue("Race Categories"),
    breaks = list(
      c(8657, 38003572, 38003573), # American Indian or alaska native
      c(8515, 38003574, 38003575, 38003576, 38003577, 38003578,
        38003579, 38003580, 38003581, 38003582, 38003583,
        38003584, 38003585, 38003586, 38003587, 38003588,
        38003589, 38003590, 38003591, 38003592, 38003593,
        38003594, 38003595, 38003596, 38003597), # asian
      c(8516, 38003598, 38003599, 38003600, 38003601,
        38003602, 38003603, 38003604, 38003605, 38003606,
        38003607, 38003608, 38003609), # black
      c(8557, 38003610, 38003611, 38003612, 38003613), # Native Hawaiian or pacific islander
      c(8527, 38003614, 38003615, 38003616), #white
      c(8522, 8552, 0) # unknown
    ),
    labels = c("American Indian or Alaska native", "Asian",
               "Black or African American", "Native Hawaiian or Other Pacific Islander",
               "White", "Unknown or Other")
  )
  return(br)

}

#' @title
#' Create a breaks Strategy object for age into 5 year groups
#'
#' @return A BreaksStrategy object with defaults assumptions for 5 year age groups
#'
#' @export
age5yrGrp <- function() {

  x <- seq(0,130, by = 5)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]

  br <- newValueBreaks(
    name = "5-Year Age Groups",
    breaks = x |> as.list()
  )

  br$labels <- c(lab, paste0(dplyr::last(x), "+"))

  return(br)
}

#' @title
#' Create a breaks Strategy object for age into 10 year groups
#'
#' @return A BreaksStrategy object with defaults assumptions for 10 year age groups
#'
#' @export
age10yrGrp <- function() {

  x <- seq(0,130, by = 10)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]

  br <- newValueBreaks(
    name = "10-Year Age Groups",
    breaks = x |> as.list()
  )

  br$labels <- c(lab, paste0(dplyr::last(x), "+"))

  return(br)
}

#' @title
#' Convenience function to add male and female line items for demographic characterization
#'
#' @return a list of two line items for male and female gender
#'
#' @export
addDefaultGenderLineItems <- function() {
  li <- list(
    createDemographicLineItem(maleGender()),
    createDemographicLineItem(femaleGender())
  )
  return(li)
}

# deprecated
addDefaultRaceLineItems <- function() {

  white <- DemographicConcept$new(
    demoCategory = "Race",
    demoLine = "White",
    conceptColumn = "race_concept_id",
    conceptId = 8527L
  )

  black <- DemographicConcept$new(
    demoCategory = "Race",
    demoLine = "Black",
    conceptColumn = "race_concept_id",
    conceptId = 8516L
  )

  asian <- DemographicConcept$new(
    demoCategory = "Race",
    demoLine = "Asian",
    conceptColumn = "race_concept_id",
    conceptId = 8515L
  )

  undefined <- DemographicConcept$new(
    demoCategory = "Race",
    demoLine = "Not Reported",
    conceptColumn = "race_concept_id",
    conceptId = 0L
  )


  li <- list(
    createDemographicLineItem(statistic = white),
    createDemographicLineItem(statistic = black),
    createDemographicLineItem(statistic = asian),
    createDemographicLineItem(statistic = undefined)
  )
  return(li)
}


#' @title
#' Convenience function to add default ethnicity line items
#'
#' @return a list of line items for default ethnicity categories (hispanic, not hispanic, not reported)
#'
#' @export
addDefaultEthnicityLineItems <- function() {

  hispanic <- DemographicConcept$new(
    demoCategory = "Ethnicity",
    demoLine = "Hispanic",
    conceptColumn = "ethnicity_concept_id",
    conceptId = 38003563L
  )

  notHispanic <- DemographicConcept$new(
    demoCategory = "Ethnicity",
    demoLine = "Not Hispanic",
    conceptColumn = "ethnicity_concept_id",
    conceptId = 38003564L
  )

  undefined <- DemographicConcept$new(
    demoCategory = "Ethnicity",
    demoLine = "Not Reported",
    conceptColumn = "ethnicity_concept_id",
    conceptId = 0L
  )


  li <- list(
    createDemographicLineItem(statistic = hispanic),
    createDemographicLineItem(statistic = notHispanic),
    createDemographicLineItem(statistic = undefined)
  )
  return(li)
}

#' @title
#' Convenience function to add quan charlson comorbidity score
#'
#' @description
#' The Quan Charlson Comorbidity score is a measure for predicting 10 year survival. It is a modification to the
#' Charlson Score by Quan et al (doi: 10.1097/01.mlr.0000182534.19832.83). The method presented in this packages follows
#' the SNOMED adaption of Quan Charlson tested on OMOP CDM by Fortin et al (doi: 10.1186/s12911-022-02006-1). This function
#' will add the elements needed for each comorbidity line item and the appropriate weights needed to convert the categorization
#' of comorbidities into a score.
#'
#' @param timeWindow the interval to assess the comorbidity score, by default baseline it -365 to -1 days
#'
#' @return a list of line items for running quan charlson comorbidity score. This will determine the proportion of persons with
#' each comorbidity and the overall score per patient in the cohort
#'
#' @export
quanCharlsonComorbidityScore <- function(timeWindow = NULL) {

  if (is.null(timeWindow)) {
    timeWindow <- list(timeInterval(lb = -365, rb = -1))
  }

  # get quan charlson concept files
  quanCharlsonConceptFiles <- fs::path_package(
    package = "ClinicalCharacteristics",
    "conceptSets/QuanCharlson"
  ) |>
    fs::dir_ls()
  quanCharlsonConceptNames <- tools::file_path_sans_ext(basename(quanCharlsonConceptFiles)) |>
    snakecase::to_title_case()

  quanCharlsonConcepts <- purrr::map2(
    quanCharlsonConceptFiles, # json files
    quanCharlsonConceptNames, # comorbidity names,
    ~Capr::readConceptSet(path = .x, name = .y)
  ) |>
    purrr::set_names(quanCharlsonConceptNames)

  weights <- list(
    anyScore(weight = 4), # aids/hiv
    anyScore(weight = 2), # any malignancy
    anyScore(weight = 0), # cerebrovascular disease
    anyScore(weight = 1), # chronic pulmonary disease
    anyScore(weight = 2), # congestive heart failure
    anyScore(weight = 2), # dementia
    anyScore(weight = 0), # mild/moderate diabetes
    anyScore(weight = 1), # diabetes with chronic complications
    anyScore(weight = 2), # hemiplegia or paralegia
    anyScore(weight = 6), # metastatic tumor
    anyScore(weight = 2), # mild liver disease
    anyScore(weight = 4), # moderate to severe liver disease
    anyScore(weight = 0), # myocardial infarction
    anyScore(weight = 0), # peptic ulcer disease
    anyScore(weight = 0), # peripheral vascular disease
    anyScore(weight = 1), # renal disease
    anyScore(weight = 1) # rheumotological disease
  )


  batchLines <- createConceptSetLineItemBatch(
    sectionLabel = "Quan Charlson",
    domain = "condition_occurrence",
    conceptSets = quanCharlsonConcepts,
    timeIntervals = timeWindow,
    statistic = weights
  )

  return(batchLines)

}
