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
