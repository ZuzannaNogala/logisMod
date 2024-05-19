test_that("error check", {
  expect_error(.splitSet(citrus, 14000), regexp =  "smaller")
})

test_that("warning check", {
  expect_warning(.splitSet(citrus, 140), regexp =  "not divided")
})

dataToSplit <- data.frame(a = c(1, 2, 3, 4),
                          b = c(1, 4, 9, 16))

set.seed(8)
indexes <- 1:4
shuffledRows <- sample(indexes, 4)

result <- list()
result[[1]] <- shuffledRows[c(1, 2)]
result[[2]] <- shuffledRows[c(3, 4)]

test_that("is data well partitioned??", {
  expect_length(.splitSet(dataToSplit, 2), 2)
  
  set.seed(8)
  expect_equal(.splitSet(dataToSplit, 2), result)
})
