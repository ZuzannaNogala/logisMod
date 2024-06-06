test_that("what if we have more than two variables?", {
  model <- createModels(citrus, nameBin ~ red + diameter)[[1]]
  
  expect_error(resPlot(model))
  expect_error(resPlot(citrus, "nameBin", c("red", "diameter")))
})

test_that("variable with type other than numeric", {
  model <- createModels(creditData, status ~ occupation_type)[[1]]
  
  expect_error(resPlot(model))
  expect_error(resPlot(creditData, "status", "occupation_type"))
})
