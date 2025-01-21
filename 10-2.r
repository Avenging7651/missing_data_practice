library(tidyverse)
library(mice)
library(norm2)
library(miceadds)
library(brglm2)
M <- 5
seed2 <- 1
df <- read_csv("10-1data.csv") |>
  select(-country) |>
  mutate(centralbank = log(centralbank))

emResult <- df |>
  as.data.frame() |>
  emNorm(iter.max = 10000)
max2 <- emResult$iter * 2
imp <- mice(
  data = df, m = M, seed = seed2,
  meth = c("", "norm", "norm", "norm"), maxit = max2
)
model2M <- with(
  imp,
  glm(gdp ~ freedom + centralbank + gini, family = binomial)
)
summary(pool(model2M))


## 小標本や分離問題(complete separation)を含むデータに対してはbias-reduced法がいいらしい
model2M_brglm <- with(
  imp,
  glm(gdp ~ freedom + centralbank + gini, family = binomial, method = "brglmFit")
)
summary(pool(model2M_brglm))
