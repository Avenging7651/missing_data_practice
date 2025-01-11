library(tidyverse)
library(norm2)
library(mice)
library(miceadds)
library(DescTools)
library(lmtest)
library(car)
df1 <- read_csv("8-2data.csv")
seed2 <- 1
set.seed(seed2)
M <- 5
# 対数に変換
df1 <- df1 |>
  select(-country) |>
  mutate(gdp = log(gdp)) |>
  mutate(centralbank = log(centralbank))

emResult <- df1 |>
  emNorm(iter.max = 10000)
max2 <- emResult$iter * 2
imp <- mice(data = df1, m = M, seed = seed2, meth = "norm", maxit = max2)
modelM <- with(imp, lm(gdp ~ freedom + centralbank + gini))
summary(pool(modelM))
pool.r.squared(modelM)
p <- ncol(df1)
JB <- matrix(NA, M, 1)
BP <- matrix(NA, M, 1)
VIF <- matrix(NA, M, p - 1)
outlier <- matrix(NA, M, 10)
for (i in 1:M) {
  miceimp <- complete(imp, i)
  model <- lm(gdp ~ freedom + centralbank + gini, data = miceimp)
  residuals <- resid(model)

  JB[i, 1] <- residuals |>
    JarqueBeraTest(robust = FALSE, method = "mc", N = 1000) |>
    _$p.value

  BP[i, 1] < bptest(model)$p.value

  VIF[i, ] <- vif(model)

  outlier[i, ] <- outlierTest(model, n.max = n.out) |>
    _$rstudent |>
    names()
}

summary(JB)
summary(BP)
summary(VIF)
outlier
