library(mice)
library(norm2)
library(miceadds)
library(tidyverse)

df1 <- read_csv("9-3data.csv") |>
  select(-country) |>
  mutate(gdp = log(gdp)) |>
  mutate(centralbank = log(centralbank))

M <- 5
seed2 <- 1
set.seed(1)

emResult <- df1 |>
  emNorm(iter.max = 10000)
max2 <- emResult$iter * 2
imp <- mice(
  data = df1,
  m = M,
  seed = seed2,
  meth = c("", "logreg", "norm", "norm"),
  maxit = max2
)

modelM <- with(imp, lm(gdp ~ freedom + centralbank + gini))
summary(pool(modelM))
pool.r.squared(modelM)