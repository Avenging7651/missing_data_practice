library(norm2)
M <- 3
set.seed(1)
em_result <- emNorm(df1, iter.max = 10000)
max1 <- em_result$iter * 2
imp_list <- as.list(NULL)
for (i in 1:M) {
  imp_list[[i]] <- em_result |>
    mcmcNorm(iter = max1) |>
    impNorm()
}
