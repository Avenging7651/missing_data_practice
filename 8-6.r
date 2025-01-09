# VIFによる多重共線性の診断
library(car)
library(Amelia)

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

p <- ncol(df1)

VIF <- matrix(NA, M, p - 1)
for (i in 1:M) {
  VIF[i, ] <- a.out$imputations[i][[1]] |>
    lm(df1$gdp ~ ., data = _) |>
    vif()
}
summary(VIF)
