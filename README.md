# ClinicalCharacteristics

<!-- badges: start -->
<!-- badges: end -->

The goal of `ClinicalCharacteristics` is to characterize a patient population using OMOP data. This approach uses
a table shell approach where characteristic extraction are limited to what is specified in the build object.

## Installation

To install `ClinicalCharacteristics`, follow these steps:

1) clone the repository 
2) Open the `ClinicalCharacteristics.RProj` file in the repository
3) Navigate to the build tab in RStudio and select Install
4) Exit out of the `ClinicalCharacteristics.RProj` session
5) **Recommended** create a new `RProj` to test the package.
6) Use CohortGenerator to build a cohort def in scratch

## Example

To run this example you need to install `Capr`

``` r
library(ClinicalCharacteristics)
library(Capr)

# add execution settings CHANGE ME
executionSettings <- createExecutionSettings(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "cdm_schema",
  workDatabaseSchema = "work_schema",
  tempEmulationSchema = "scratch_schema",
  targetCohortTable = "cohort",
  cdmSourceName = "my_cdm"
)


# add concept sets

cs1 <- list(
  't2d' = cs(descendants(201826), name = "t2d"),
  'ckd' = cs(descendants(46271022), name = "ckd")
)

cs2 <- list(
  'sglt2' = cs(descendants(1123627), name = "sglt2"),
  'glp1' = cs(descendants(1123618), name = "glp1")
)

# add cohorts

ch <- list(
  createCohortInfo(id = 3, name = "Procedure Cohort 1"),
  createCohortInfo(id = 4, name = "Procedure Cohort 2")
)


# time windows
tw1 <- list(
  timeInterval(lb = -365, rb = -1)
)
tw2 <- list(
  timeInterval(lb = 0, rb = 90),
  timeInterval(lb = 0, rb = 365)
)


tw3 <- list(
  timeInterval(lb = -365, rb = -1),
  timeInterval(lb = 0, rb = 90),
  timeInterval(lb = 0, rb = 365)
)


# make table shell

tableShell <- createTableShell(
  title = "Test",
  targetCohorts = list(
    createCohortInfo(id = 1, name = "Target Cohort 1"),
    createCohortInfo(id = 2, name = "Target Cohort 2")
  ),
  lineItems = lineItems(
    createDemographicLineItem(maleGender()),
    createDemographicLineItem(ageChar(breaks = age5yrGrp())),
    createDemographicLineItem(ageChar()),
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
    ),
    createCohortLineItemBatch(
      sectionLabel = "Post-Index Procedures",
      covariateCohorts = ch,
      cohortTable = "cohort",
      timeIntervals = tw3,
      statistic = anyPresenceStat()
    )
  )
)

# run
res <- generateTableShell(tableShell, executionSettings)

```

