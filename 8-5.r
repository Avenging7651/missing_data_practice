# BP検定による誤差項の不均一分散の診断

library(lmtest)
library(Amelia)
library(tidyverse)

df1 <- read_csv("8-2data.csv")
M <- 5
set.seed(1)
# 対数に変換
df1 <- df1 |>
  select(-country) |>
  mutate(gdp = log(gdp)) |>
  mutate(centralbank = log(centralbank))

a.out <- df1 |>
  as.data.frame() |> # data.frameでないとameliaが使えないので変換をする
  amelia(m = M)


BP <- matrix(NA, M, 1)
for (i in 1:M) {
  BP[i, 1] <- a.out$imputations[i][[1]] |>
    lm(df1$gdp ~ ., data = _) |>
    bptest() |>
    _$p.value
}
summary(BP)
