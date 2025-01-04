# miceによるt検定
library(mice)
library(norm2)
M <- 5
mu <- 300
emResult <- emNorm(df1, iter.max = 10000)
max1 <- emResult$iter * 2
imp <- mice(data = df1, m = M, seed = 1, meth = "norm", maxit = max1)
n <- nrow(df1)
Q <- rep(NA, M)
U <- rep(NA, M)
for (i in 1:M) {
  Q[i] <- mean(complete(imp, i)$income)
  U[i] <- var(complete(imp, i)$income) / n
}
poolQ <- pool.scalar(Q, U, rule = "rubin1987")
qbar <- poolQ$qbar
TotalVar <- poolQ$t
lambda <- (1 + 1 / poolQ$m) * poolQ$b / TotalVar

v1 <- (M - 1) / lambda^2
v2 <- ((n - 1 + 1) / (n - 1 + 3)) * (n - 1) * (1 - lambda)
v <- (v1 * v2) / (v1 + v2)
t <- (qbar - mu) / sqrt(TotalVar)
p <- pt(t, v, lower.tail = FALSE) * 2
t
v
p
qbar
sqrt(TotalVar * n)
