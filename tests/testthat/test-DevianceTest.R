expectList <- list("Tested_predictors" = c("diameter", "red"), 
                   "Deviance_statistic" = 5614.595,
                   df = 2,
                   "Critical_value" = qchisq(0.95, 2),
                   "p_value" = 0)


test_that("getting deviance statistic test's parameters works", {
  mdl_full <- glm(nameBin ~ diameter + green + blue  + red, 
                  data = citrus, family = binomial("logit"))
  mdl_unfull <- glm(nameBin ~ blue + green, 
                    data = citrus, family = binomial("logit"))
  
  expect_equal(deviance_test(model_H0 = mdl_unfull, model_H1 = mdl_full), expectList)
})

