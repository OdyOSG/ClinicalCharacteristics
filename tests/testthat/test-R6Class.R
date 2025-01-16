library(testthat)
testthat::local_edition(3)

test_that("TableShell object is initialized correctly", {
  targetCohort <- CohortInfo$new(id = 1, name = "Cohort 1")
  stat <- Statistic$new(statType = "test", personLine = "test", aggType = "test")
  lineItem <- LineItem$new(
    sectionLabel = "sectionLabel",
    lineItemClass = "lineItemClass",
    statistic = stat,
    domainTable = "test"
  )
  tableShell <- TableShell$new(name = "Table 1",
                               targetCohorts = list(targetCohort),
                               lineItems = list(lineItem))

  expect_true(inherits(tableShell, "TableShell"))
  expect_equal(tableShell$getName(), "Table 1")
  expect_equal(length(tableShell$getTargetCohorts()), 1)
})

test_that("CohortInfo object creates", {
  ci <- CohortInfo$new(1, "Test Name")
  expect_true(inherits(ci, "CohortInfo"), info = "ci should be a CohortInfo object")
  expect_equal(ci$getId(), 1)
  expect_equal(ci$getName(), "Test Name")
})

test_that("ExecutionSettings object creates", {
  es <- ExecutionSettings$new(connectionDetails = connectionDetailsEunomia,
                              connection = NULL,
                              cdmDatabaseSchema = "fake_cdm_database_schema",
                              workDatabaseSchema = "fake_work_database_schema",
                              tempEmulationSchema = "fake_temp_emulation_schema",
                              targetCohortTable = "fake_target_cohort_table",
                              cdmSourceName = "fake_cdm_source_name")
  expect_true(inherits(es, "ExecutionSettings"), info = "es should be an ExecutionSettings object")
})

test_that("LineItem class initializes correctly", {
  sectionLabel <- "Section A"
  lineItemLabel <- "Item 1"
  domainTable <- "Table 1"
  lineItemClass <- "Class A"
  valueId <- 1
  valueDescription <- "Description 1"
  statistic <- Statistic$new(statType = "test", personLine = "test", aggType = "test")
  timeInterval <- TimeInterval$new(0,365)

  lineItem <- LineItem$new(
    sectionLabel = sectionLabel,
    lineItemLabel = lineItemLabel,
    domainTable = domainTable,
    lineItemClass = lineItemClass,
    valueId = valueId,
    valueDescription = valueDescription,
    statistic = statistic,
    timeInterval = timeInterval
  )

  liMeta <- lineItem$getLineItemMeta()

  expect_true(inherits(lineItem, "LineItem"))
  expect_true(inherits(liMeta, "tbl_df"))
  expect_equal(ncol(liMeta), 11)
})

test_that("ConceptSetLineItem object initializes correctly", {
  conceptSet <- ConceptSetLineItem$new(sectionLabel = "CS",
                                       statistic = Statistic$new(statType = "test", personLine = "test", aggType = "test"),
                                       domainTable = "domain",
                                       conceptSet = Capr::cs(1335471, name = "test"),
                                       timeInterval = TimeInterval$new(0,365),
                                       sourceConceptSet = Capr::cs(1335471, name = "test_source"),
                                       typeConceptIds = c(67890),
                                       visitOccurrenceConceptIds = c(12345, 67890))
  expect_true(inherits(conceptSet, "ConceptSetLineItem"))
  expect_true(inherits(conceptSet, "LineItem"))
})

test_that("CohortLineItem initializes correctly", {
  cohortInfo <- CohortInfo$new(id = 1, name = "Test Cohort")
  timeInterval <- TimeInterval$new(0, 365)
  statistic <- Statistic$new(statType = "test", personLine = "test", aggType = "test")
  cohortLi <- CohortLineItem$new(sectionLabel = "C",
                                 statistic = statistic,
                                 covariateCohort = cohortInfo,
                                 timeInterval = timeInterval,
                                 domainTable = "cohort")

  expect_true(inherits(cohortLi, "CohortLineItem"))
  expect_true(inherits(cohortLi, "LineItem"))
})

test_that("Presence object initializes correctly", {
  presence <- Presence$new(personLine = "anyCount")
  expect_true(inherits(presence, "Presence"))
  expect_true(inherits(presence, "Statistic"))
})

test_that("DemographicConcept object initializes correctly", {
  maleConcept <- DemographicConcept$new(
    demoCategory = "Gender",
    demoLine = "Male",
    conceptColumn = "gender_concept_id",
    conceptId = 8507L
  )
  expect_true(inherits(maleConcept, "DemographicConcept"))
  expect_true(inherits(maleConcept, "Statistic"))
  expect_equal(maleConcept$getConceptId(), 8507L)
})

test_that("ContinuousDistribution object initializes correctly", {
  cd <- ContinuousDistribution$new(personLine = "anyCount")
  expect_true(inherits(cd, "ContinuousDistribution"))
  expect_true(inherits(cd, "Statistic"))
  expect_equal(cd$getPersonLineTransformation(), "anyCount")
})
