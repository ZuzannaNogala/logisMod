n <- 88
planMatrix <- matrix(rnorm(3*n), nrow = n, ncol = 3)
beta <- c(1, 2, 1)

Y <- planMatrix %*% beta + rnorm(n, sd = 0.01)
firstCase(Y, planMatrix, beta)
