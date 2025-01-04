n <- 100
set.seed(1)
x <- rnorm(n, 0, 1)
e <- rnorm(n, 0, 1)
y1 <- 1 + 2 * x + e
u1 <- runif(n, 0, 1)
u2 <- runif(n, 0, 1)
df1 <- data.frame(y1, x, u2, u2)
matdata <- matrix(NA, n, 1)
for (i in 1:n) {
  if (df1[i, 2] < median(df1[, 2]) && df1[i, 3] < 0.5) {
    matdata[i, 1] <- NA
  } else if (df1[i, 2] >= median(df1[, 2]) && df1[i, 4] < 0.25) {
    matdata[i, 1] <- NA
  } else {
    matdata[i, 1] <- df1[i, 1]
  }
}
y2 <- matdata[, 1]
df1 <- data.frame(y2, x)
