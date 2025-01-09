# JB検定による誤差項の正規性の診断
# normtestはcranのチェックを通らなくなったらしくarchiveされている
# 代わりにDescToolsを用いる
# 参考 https://minato.sip21c.org/swtips/R.html
library(DescTools)
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



JB <- matrix(NA, M, 1)
for (i in 1:M) {
  JB[i, 1] <- a.out$imputations[i][[1]] |>
    lm(df1$gdp ~ ., data = _) |>
    resid() |>
    JarqueBeraTest(robust = FALSE, method = "mc", N = 1000) |>
    _$p.value
}
summary(JB)
