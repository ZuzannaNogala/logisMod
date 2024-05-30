model <- logisMod(nameBin ~  diameter + red + green, citrus)

test_that("too long sequance", {
  expect_error(drawROC(seq(0, 2, by = 0.01), model, "nameBin"))
})


test_that("is drawing working?", {
  expect_no_error(drawROC(seq(0, 1, by = 0.05), model, "nameBin"))
})