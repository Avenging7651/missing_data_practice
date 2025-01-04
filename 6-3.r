library(norm2)
library(lattice)
library(miceadds)
m <- 5
seed2 <- 1
em_result <- emNorm(df1, iter.max = 10000)
max1 <- em_result$iter * 2
imp.list <- as.list(NULL)
for (j in 1:m) {
  mcmc_result <- mcmcNorm(em_result, iter = max1)
  imp.list[[j]] <- impNorm(mcmc_result)
}
a.mids <- datlist2mids(imp.list) # nolint: object_name_linter.
xyplot(a.mids, y2 ~ x, pch = c(1, 16))
densityplot(a.mids)
