library(testthat)

test_that("defaultYearGrp returns a BreaksStrategy object",{

  tst <- defaultYearGrp()
  expect_equal(tst$name,"Default Years (2000-2025)")
  expect_equal(class(tst)[[1]], "BreaksStrategy")
  expect_equal(tst$labels[1], "2000")
})

test_that("defaultYearGrp return correct years if startYear != NULL", {
  tst <- defaultYearGrp(startYear = 2010)
  expect_equal(tst$name, "Default Years (2010-2025)")
  expect_equal(tst$labels[1], "2010")
})

test_that("age5yrGrp returns a convenient breaks object", {

  tst <- age5yrGrp()
  expect_equal(tst$name, "5-Year Age Groups")
  expect_equal(class(tst)[[1]], "BreaksStrategy")
  expect_equal(tst$labels[1], "0-4")

})

test_that("age 5yrGrp breaks are correctly set", {
  tst <- age5yrGrp()
  expected_breaks <- as.list(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60,
                               65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115,
                               120, 125, 130))

  expect_equal(tst$breaks, expected_breaks)
})



test_that("age10yrGrp returns a convenient breaks object", {

  tst <- age10yrGrp()
  expect_equal(tst$name, "10-Year Age Groups")
  expect_equal(class(tst)[[1]], "BreaksStrategy")
  expect_equal(tst$labels[1], "0-9")

})


test_that("age 10yrGrp breaks are correctly set", {
  tst <- age10yrGrp()
  expected_breaks <- as.list(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130))
  expect_equal(tst$breaks, expected_breaks)
})


test_that("addDefaultRaceLineItems returns concept demo line items", {

  tst <- addDefaultRaceLineItems()
  expect_equal(class(tst), "list")
  expect_equal(tst[[1]]$lineItemClass, "Demographic")
  expect_equal(class(tst[[1]])[[1]], "DemographicLineItem")
  expect_equal(tst[[1]]$lineItemLabel, "Race: White")

})

test_that("addDefaultGenderLineItems returns concept demo line items", {

  tst <- addDefaultGenderLineItems()
  expect_equal(class(tst), "list")
  expect_equal(tst[[1]]$lineItemClass, "Demographic")
  expect_equal(class(tst[[1]])[[1]], "DemographicLineItem")
  expect_equal(tst[[1]]$lineItemLabel, "Gender: Male")

})

test_that("addDefaultEthnicityLineItems returns concept demo line items", {

  tst <- addDefaultEthnicityLineItems()
  expect_equal(class(tst), "list")
  expect_equal(tst[[1]]$lineItemClass, "Demographic")
  expect_equal(class(tst[[1]])[[1]], "DemographicLineItem")
  expect_equal(tst[[1]]$lineItemLabel, "Ethnicity: Hispanic")

})


test_that("quanCharlsonComorbidityScore returns concept list of concept set line items with a score stat", {

  tst <- quanCharlsonComorbidityScore()
  expect_equal(class(tst), "list")
  expect_equal(length(tst), 17) # there are 17 charlson comorbid categories
  expect_equal(tst[[1]]$lineItemClass, "ConceptSet")
  expect_equal(class(tst[[1]])[[1]], "ConceptSetLineItem")
  expect_equal(tst[[2]]$lineItemLabel, "Any Malignancy")
  expect_equal(class(tst[[1]]$getStatistic())[[1]], "Score")
  expect_equal(tst[[2]]$getStatistic()$weight, 2)

})
