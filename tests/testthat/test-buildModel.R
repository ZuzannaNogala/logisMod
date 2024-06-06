test_that("check class of returned object", {
  modelFormula <- nameBin ~ red
  
  suppressWarnings(
    expect_s3_class(createModels(citrus, modelFormula)[[1]], class = "glm"),
    expect_s3_class(createModels(citrus, modelFormula)[[1]], class = "LogisMod")
  )
})

