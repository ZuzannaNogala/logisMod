test_that("what if we have more than two variables?", {
  model <- buildModel(nameBin ~ red + diameter, citrus)
  
  expect_error(resPlot(model))
})
