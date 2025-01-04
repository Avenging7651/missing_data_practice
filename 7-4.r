# Ameliaのみでt test
library(Amelia)
library(mice)
M <- 5
set.seed(1)
a_out <- amelia(df1, m = M)
n <- nrow(df1)
mu <- 300
Q <- rep(NA, M)
U <- rep(NA, M)
for (i in 1:M) {
  Q[i] <- mean(a_out$imputations[i][[1]]$income)
  U[i] <- var(a_out$imputations[i][[1]]$income) / n
}
# pool.scalar()のmethodはruleに変更されているので修正
poolQ <- pool.scalar(Q, U, rule = "rubin1987")
qbar <- poolQ$qbar
TotalVar <- poolQ$t
#  pool.scalar()のlambdaの戻り値は以下の変更で消滅したので修正
# https://github.com/amices/mice/commit/038298233b615f74c8047e820bc4f89dc43cf4b6?diff=split&w=0#diff-5d7957960e83c870baca[…]bd605762d6282bf3c4ff639ac0
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
