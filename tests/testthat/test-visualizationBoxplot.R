test_that("No errors to visualization of boxplot?", {
  expect_no_error(visual_boxplot(citrus, "nameBin", "weight"))
})

test_that("wrong spelling variables' names", {
  expect_error(visual_boxplot(citrus, "nameBin", "kapibara"),
               regexp = "check name")
})