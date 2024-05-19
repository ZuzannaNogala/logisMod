possiblePred <- colnames(citrus)[2:6]

test_that("check class of returned object", {
  suppressWarnings(
    expect_s3_class(buildModel(citrus, "nameBin", possiblePred, smmr = FALSE), class = "glm")
  )
})

test_that("do we get a 2 elements list??", {
  suppressWarnings(
    expect_length(buildModel(citrus, "nameBin", possiblePred), 2)
    )
})
