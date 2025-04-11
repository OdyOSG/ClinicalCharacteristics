library(testthat)

test_that("createTableShell returns a TableShell object with the correct name", {
  targetCohort <- CohortInfo$new(id = 1, name = "Cohort 1")
  stat <- Statistic$new(statType = "test", personLine = "test", aggType = "test")
  lineItem <- LineItem$new(
    sectionLabel = "sectionLabel",
    domain = "Hello",
    lineItemClass = "lineItemClass",
    statistic = stat
  )
  tableShell <- createTableShell(title = "Table 1",
                                 targetCohorts = list(targetCohort),
                                 lineItems = list(lineItem))
  expect_true(inherits(tableShell, "TableShell"))
  expect_equal(tableShell$getTitle(), "Table 1")
})

test_that("parseCohortInfoFromDf parses cohort info from a data frame", {
  df <- data.frame(id = c(1, 2), name = c("Cohort 1", "Cohort 2"))
  cohorts <- parseCohortInfoFromDf(df)
  expect_true(all(sapply(cohorts, inherits, "CohortInfo")))
})

test_that("createCohortInfo returns a CohortInfo object with the correct attributes", {
  id <- 1
  name <- "Test Cohort"
  cohortInfo <- createCohortInfo(id, name)
  expect_true(inherits(cohortInfo, "CohortInfo"))
  expect_equal(cohortInfo$getId(), id)
  expect_equal(cohortInfo$getName(), name)
})

test_that("createExecutionSettings returns an ExecutionSettings object", {
  executionSettings <- createExecutionSettings(connectionDetails = connectionDetailsEunomia,
                                              connection = NULL,
                                              cdmDatabaseSchema = "cdm_schema",
                                              workDatabaseSchema = "work_schema",
                                              tempEmulationSchema = "fake_temp_emulation_schema",
                                              cohortTable = "fake_cohort_table",
                                              cdmSourceName = "fake_cdm_source_name")
  expect_true(inherits(executionSettings, "ExecutionSettings"))
})

test_that("createConceptSetLineItem creates a ConceptSetLineItem object", {
  csLi <- createConceptSetLineItem(sectionLabel = "Concept Set 1",
                                   statistic = Presence$new(personLine = "anyCount"),
                                   domain = "drug_exposure",
                                   conceptSet = Capr::cs(1335471, name = "test"),
                                   timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(csLi, "ConceptSetLineItem"))
  expect_true(inherits(csLi, "LineItem"))
  expect_equal(csLi$lineItemClass, "ConceptSet")
})

test_that("createConceptSetLineItem creates a ConceptSetLineItem object - no name specified", {
  csLi <- createConceptSetLineItem(statistic = Presence$new(personLine = "anyCount"),
                                   domain = "Drug",
                                   conceptSet = Capr::cs(1335471, name = "test"),
                                   timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(csLi, "ConceptSetLineItem"))
  expect_true(inherits(csLi, "LineItem"))
  expect_equal(csLi$lineItemClass, "ConceptSet")
  expect_equal(csLi$lineItemLabel, "test")
})

test_that("createCohortLineItem creates a CohortLineItem object", {
  cohortInfo <- CohortInfo$new(id = 1, name = "Test Cohort")
  cLi <- createCohortLineItem(sectionLabel = "Cohort 1",
                              statistic = Presence$new(personLine = "anyCount"),
                              covariateCohort = cohortInfo,
                              cohortTable = "test_cohort_table",
                              timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(cLi, "CohortLineItem"))
  expect_true(inherits(cLi, "LineItem"))
  expect_equal(cLi$lineItemClass, "Cohort")
})

test_that("createCohortLineItem creates a CohortLineItem object - no name specified", {
  cohortInfo <- CohortInfo$new(id = 1, name = "Test Cohort")
  cLi <- createCohortLineItem(statistic = Presence$new(personLine = "anyCount"),
                              covariateCohort = cohortInfo,
                              cohortTable = "test_cohort_table",
                              timeInterval = TimeInterval$new(0,365))
  expect_true(inherits(cLi, "CohortLineItem"))
  expect_true(inherits(cLi, "LineItem"))
  expect_equal(cLi$lineItemClass, "Cohort")
  expect_equal(cLi$lineItemLabel, "Test Cohort")
})

test_that("createConceptSetLineItemBatch creates a list of ConceptSetLineItem objects", {
  conceptSets <- list(Capr::cs(1335471, name = "A"), Capr::cs(1340128, name = "B"), Capr::cs(1341927, name = "C"))
  csList <- createConceptSetLineItemBatch(sectionLabel = "Drug Exposures",
                                          statistic = Presence$new(personLine = "anyCount"),
                                          domain = "drug_exposure",
                                          conceptSets = conceptSets,
                                          timeIntervals = list(TimeInterval$new(0,365)))
  expect_equal(length(csList), 3)
  expect_true(inherits(csList[[1]], "ConceptSetLineItem"))
  expect_true(inherits(csList[[1]], "LineItem"))
  expect_equal(csList[[1]]$lineItemLabel, "A")
})

test_that("createCohorttLineItemBatch creates a list of CohortLineItem objects", {
  cohorts <- list(CohortInfo$new(id = 1, name = "A"), CohortInfo$new(id = 2, name = "B"))
  cohortList <- createCohortLineItemBatch(sectionLabel = "Cohort Batch",
                                          statistic = Presence$new(personLine = "anyCount"),
                                          covariateCohorts = cohorts,
                                          cohortTable = "test_cohort_table",
                                          timeIntervals = list(TimeInterval$new(0,365)))
  expect_equal(length(cohortList), 2)
  expect_true(inherits(cohortList[[1]], "CohortLineItem"))
  expect_true(inherits(cohortList[[1]], "LineItem"))
  expect_equal(cohortList[[1]]$lineItemLabel, "A")
})

# Breaks Strategy --------------------

test_that("newBreaks makes a BreaksStrategy Object",{
  newBreaksStrategy <- newValueBreaks(name = "test", breaks = c(0,2,4))
  expect_equal(newBreaksStrategy$name, "test")
  expect_equal(class(newBreaksStrategy)[[1]], "BreaksStrategy")
  expect_equal(newBreaksStrategy$labels[1], "[0-2)")

  newBreaksStrategy$labels <- c("0-1", "2-3", "4+")
  expect_equal(newBreaksStrategy$labels[1], "0-1")

})


# Stat Builders -------------------------

## Demographics -------------------------

test_that("maleGender makes a DemographicConcept Class", {
  maleGenderChar <- maleGender()
  expect_equal(maleGenderChar$getConceptId(), 8507)
  expect_equal(maleGenderChar$getDemoLabel(), "Gender: Male")
  expect_equal(maleGenderChar$getConceptColumn(), "gender_concept_id")
})


test_that("femaleGender makes a DemographicConcept Class", {
  femaleGenderChar <- femaleGender()
  expect_equal(femaleGenderChar$getConceptId(), 8532)
  expect_equal(femaleGenderChar$getDemoLabel(), "Gender: Female")
  expect_equal(femaleGenderChar$getConceptColumn(), "gender_concept_id")
})



test_that("ageChar makes a DemographicAge Class", {

  ageChar1 <- ageCharCts()
  expect_equal(ageChar1$getStatisticType(), "continuousDistribution")
  expect_equal(ageChar1$getDemoLabel(), "Age")
  expect_equal(ageChar1$getAggregationType(), "continuous")

   ageChar2 <- ageCharBreaks(
    breaks = newBreaks(name = "Child to Adult", breaks = c(0, 18, 65))
  )

  expect_equal(ageChar2$getStatisticType(), "breaks")
  expect_equal(ageChar2$getDemoLabel(), "Age")
  expect_equal(ageChar2$getAggregationType(), "categorical")
  expect_true(inherits(ageChar2$getBreaksIfAny(), "BreaksStrategy"))

})

## Other stats ----------------------

test_that("anyPresenceStat makes a Presence Class", {

  pres <- anyPresenceStat()
  expect_equal(pres$getStatisticType(), "presence")
  expect_equal(pres$getPersonLineTransformation(), "anyCount")
  expect_equal(pres$getAggregationType(), "categorical")
  expect_true(inherits(pres, "Presence"))
})


test_that("observedPresenceStat makes a Presence Class", {

  obsPres <- observedPresenceStat()
  expect_equal(obsPres$getStatisticType(), "presence")
  expect_equal(obsPres$getPersonLineTransformation(), "observedCount")
  expect_equal(obsPres$getAggregationType(), "categorical")
  expect_true(inherits(obsPres, "Presence"))
})


test_that("anyCountCtsStat makes a ContinuousDistribution Class", {

  tst1 <- anyCountCtsStat()
  expect_equal(tst1$getStatisticType(), "continuousDistribution")
  expect_equal(tst1$getPersonLineTransformation(), "anyCount")
  expect_equal(tst1$getAggregationType(), "continuous")
  expect_true(inherits(tst1, "ContinuousDistribution"))

})


test_that("observedCountCtsStat makes a ContinuousDistribution Class", {

  tst1 <- observedCountCtsStat()
  expect_equal(tst1$getStatisticType(), "continuousDistribution")
  expect_equal(tst1$getPersonLineTransformation(), "observedCount")
  expect_equal(tst1$getAggregationType(), "continuous")
  expect_true(inherits(tst1, "ContinuousDistribution"))

})

test_that("anyCountBreaksStat with breaks makes a breaks Class", {

  br <- newBreaks(name = "test", breaks = c(1,2,3))
  tst1 <- anyCountBreaksStat(breaks = br)
  expect_equal(tst1$getStatisticType(), "breaks")
  expect_equal(tst1$getPersonLineTransformation(), "anyCount")
  expect_equal(tst1$getAggregationType(), "categorical")
  expect_true(inherits(tst1, "Breaks"))

})


test_that("observedCountBreaksStat with breaks makes a breaks Class", {

  br <- newBreaks(name = "test", breaks = c(1,2,3))
  tst1 <- observedCountBreaksStat(breaks = br)
  expect_equal(tst1$getStatisticType(), "breaks")
  expect_equal(tst1$getPersonLineTransformation(), "observedCount")
  expect_equal(tst1$getAggregationType(), "categorical")
  expect_true(inherits(tst1, "Breaks"))

})


test_that("anyTimeToFirstBreaksStat with breaks makes a breaks Class", {

  br <- newBreaks(name = "test", breaks = c(1,2,3))
  tst1 <- anyTimeToFirstBreaksStat(breaks = br)
  expect_equal(tst1$getStatisticType(), "breaks")
  expect_equal(tst1$getPersonLineTransformation(), "anyTimeToFirst")
  expect_equal(tst1$getAggregationType(), "categorical")
  expect_true(inherits(tst1, "Breaks"))

})


test_that("anyTimeToFirstCtsStat makes a ContinuousDistribution Class", {

  tst1 <- anyTimeToFirstCtsStat()
  expect_equal(tst1$getStatisticType(), "continuousDistribution")
  expect_equal(tst1$getPersonLineTransformation(), "anyTimeToFirst")
  expect_equal(tst1$getAggregationType(), "continuous")
  expect_true(inherits(tst1, "ContinuousDistribution"))

})

# Line Item Builder ---------------------

test_that("create a set of lineItems", {

  cs1 <- list(
    't2d' = Capr::cs(Capr::descendants(201826), name = "t2d"),
    'ckd' = Capr::cs(Capr::descendants(46271022), name = "ckd")
  )

  cs2 <- list(
    'sglt2' = Capr::cs(Capr::descendants(1123627), name = "sglt2"),
    'glp1' = Capr::cs(Capr::descendants(1123618), name = "glp1")
  )

  tw1 <- list(
    timeInterval(lb = -365, rb = -1)
  )
  tw2 <- list(
    timeInterval(lb = 0, rb = 90),
    timeInterval(lb = 0, rb = 365)
  )

  lineItems = lineItems(
    createDemographicLineItem(maleGender()),
    createConceptSetLineItemBatch(
      sectionLabel = "Baseline Conditions",
      domain = "condition_occurrence",
      statistic = anyPresenceStat(),
      conceptSets = cs1,
      timeIntervals = tw1
    ),
    createConceptSetLineItemBatch(
      sectionLabel = "Post-Index Drugs",
      domain = "drug_exposure",
      statistic = anyPresenceStat(),
      conceptSets = cs2,
      timeIntervals = tw2
    )
  )

  # check demo meta
  demoMeta <- lineItems[[1]]$getLineItemMeta()
  expect_equal(demoMeta$valueId, 8507L)

  # check t2d meta
  t2dMeta <- lineItems[[2]]$getLineItemMeta()
  expect_equal(t2dMeta$valueId, 1)
  expect_equal(t2dMeta$lineItemLabel, "t2d")

  # check drug meta
  glp1Meta1 <- lineItems[[5]]$getLineItemMeta()
  glp1Meta2 <- lineItems[[7]]$getLineItemMeta()
  expect_equal(glp1Meta1$valueId, glp1Meta2$valueId)
  expect_equal(glp1Meta1$ordinalId, 5L)
  expect_equal(glp1Meta2$ordinalId, 7L)
  expect_equal(glp1Meta1$timeLabel, "0d to 90d")
  expect_equal(glp1Meta2$timeLabel, "0d to 365d")

})
