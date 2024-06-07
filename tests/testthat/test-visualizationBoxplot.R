test_that("No errors to visualization of boxplot?", {
  expect_no_error(visual_boxplot(citrus, "nameBin", "weight"))
})
