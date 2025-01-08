# アメリアによｐる
library(Amelia)
library(miceadds)
library(mice)
library(tidyverse)
library(patchwork)
df1 <- read_csv("8-2data.csv")
# 対数に変換
df1 <- df1 |>
  select(-country) |>
  mutate(gdp = log(gdp)) |>
  mutate(centralbank = log(centralbank))

M <- 100
set.seed(1)

modelA <- df1 |>
  as.data.frame() |> # data.frameでないとameliaが使えないので変換をする
  amelia(m = M) |>
  _$imputations |>
  datlist2mids() |>
  with(lm(gdp ~ freedom + centralbank + gini)) # lm.midsがUse with(imp, lm(yourmodel))警告するのでwithを使う

summary(pool(modelA))
pool.r.squared(modelA)
