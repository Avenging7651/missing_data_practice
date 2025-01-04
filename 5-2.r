library(mice)
library(norm2)
M <- 3
seed2 <- 1
em_result <- emNorm(df1, iter.max = 10000)
max1 <- em_result$iter * 2
imp <- mice(data = df1, seed = seed2, meth = "norm", maxit = max1)
