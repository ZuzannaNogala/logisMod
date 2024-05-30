test_that("No errors to visualization of boxplot?", {
  model <- logisMod(nameBin ~ green + red + weight, citrus)
  
  expect_no_error(visual_boxplot(model, "nameBin", "weight"))
})
