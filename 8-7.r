# VIFによる多重共線性の診断
library(car)
library(Amelia)
library(tidyverse)
df1 <- read_csv("8-2data.csv")
set.seed(1)
M <- 5
# 対数に変換
df1 <- df1 |>
  select(-country) |>
  mutate(gdp = log(gdp)) |>
  mutate(centralbank = log(centralbank))

a.out <- df1 |>
  as.data.frame() |> # data.frameでないとameliaが使えないので変換をする
  amelia(m = M)

n.out <- 10

outlier <- matrix(NA, M, n.out)
for (i in 1:M) {
  outlier[i, ] <- a.out$imputations[i][[1]] |>
    lm(df1$gdp ~ ., data = _) |>
    outlierTest(n.max = n.out) |>
    _$rstudent |>
    names()
}

outlier
