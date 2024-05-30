test_that("Does computing response prediction works?", {
  model <- logisMod(nameBin ~ green + diameter, citrus)
  expect_preds <- predict.glm(model, citrus, type = "response")
  
  expect_equal(resp_prediction_for_model(model), expect_preds)
})