test_that("creating combinations works", {
  expectedSet <- list(numeric(0), 1, 2, 3, c(1, 2), c(1, 3), c(2, 3), c(1, 2, 3))
  
  expect_setequal(.getAllCombination(c(1, 2, 3)), expectedSet)
  expect_setequal(.getAllCombination(1), list(integer(0), 1))
})

test_that("getting model formula", {
  expect_s3_class(.getModelFormula("nameBin", "diameter"), class = "formula")
  expect_equal(deparse(.getModelFormula("nameBin", "diameter")), "nameBin ~ diameter")
  expect_equal(deparse(.getModelFormula("nameBin", numeric(0))), "nameBin ~ 1")
  expect_equal(deparse(.getModelFormula("nameBin", c("diameter", "red"))), "nameBin ~ diameter + red")
})

test_that("too much variables!", {
  possiblePred <- colnames(creditData)
  expect_error(countInfCrit(creditData, "status", possiblePred))
})

test_that("variable not in dataset", {
  possiblePred <- colnames(citrus)[2:6]
  
  expect_error(countInfCrit(citrus, "nameBin", "cat"))
  expect_error(countInfCrit(citrus, "cat", possiblePred))
})
