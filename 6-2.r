library(mice)
library(norm2)
library(lattice)
M <- 5
seed2 <- 1
em_result <- emNorm(df1, iter.max = 10000)
max2 <- em_result$iter * 2
imp <- mice(data = df1, m = M, seed = seed2, meth = "norm", maxit = max2)
xyplot(imp, y2 ~ x, pch = c(1, 16))
densityplot(imp)
