modelFormula <- nameBin ~ red

test_that("check class of returned object", {
  suppressWarnings(
    expect_s3_class(buildModel(modelFormula, citrus), class = "glm"),
    expect_s3_class(buildModel(modelFormula, citrus), class = "LogisMod")
  )
})

