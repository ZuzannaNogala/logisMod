expectList <- list("Tested_predictors" = c("diameter", "red"), 
                   "Deviance_statistic" = 5614.595,
                   df = 2,
                   "Critical_value" = qchisq(0.95, 2),
                   "p_value" = 0)


test_that("getting deviance statistic test's parameters works", {
  models <- createModels(citrus, nameBin ~ diameter + green + blue + red, nameBin ~ blue + green)
  
  expect_equal(deviance_test(models), expectList)
})

test_that("equal amount of predictors", {
  models <- createModels(citrus, nameBin ~ diameter + green, nameBin ~ blue + green)
  expect_error(deviance_test(models))
})

test_that("wrong null hipothesis", {
  models <- createModels(citrus, nameBin ~ weight, nameBin ~ diameter + green)
  expect_error(deviance_test(models))
})

test_that("wrong null hipothesis", {
  models <- createModels(citrus, nameBin ~ diameter + green + blue + red, nameBin ~ blue + green, nameBin ~ diameter + green + blue)
  expect_error(deviance_test(models))
})
