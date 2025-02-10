library(testthat)

test_that("age5yrGrp returns a convenient breaks object", {

  tst <- age5yrGrp()
  expect_equal(tst$name, "5-Year Age Groups")
  expect_equal(class(tst)[[1]], "BreaksStrategy")
  expect_equal(tst$labels[1], "0-4")

})


test_that("age10yrGrp returns a convenient breaks object", {

  tst <- age10yrGrp()
  expect_equal(tst$name, "10-Year Age Groups")
  expect_equal(class(tst)[[1]], "BreaksStrategy")
  expect_equal(tst$labels[1], "0-9")

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
  expect_equal(tst[[2]]$getStatistic()$weight, 4)

})
