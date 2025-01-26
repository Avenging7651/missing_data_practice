library(tidyverse)
library(hot.deck)
library(miceadds)
library(lattice)
# library(conflicted)
# conflicts_prefer(dplyr::select) #MASSのselectと干渉するので
# 下準備 ----
df <- read_csv("10-1data.csv") |>
  dplyr::select(-country) |>
  mutate(centralbank = log(centralbank)) |>
  as.data.frame()

# ここからコード ----
M <- 5
seed2 <- 1

a.mids2 <- df |>
  hot.deck(
    m = M,
    method = "best.cell",
    sdCutoff = 1,
    weightedAffinity = FALSE,
    impContinuous = "HD"
  ) |>
  hd2amelia() |>
  _$imputations |>
  datlist2mids()
modelMH2 <- a.mids2 |>
  with(glm(gdp ~ freedom + centralbank + gini, family = binomial))
summary(pool(modelMH2))
densityplot(a.mids2)
