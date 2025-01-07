library(tidyverse)
library(patchwork)
df1 <- read_csv("8-2data.csv")
summary(df1)

# まずはデータのヒストグラムを確認して分布の状況を確かめることで用いる手法を検討する
# baseRのヒストグラムの幅を自動的に決める関数
brx <- function(x) pretty(range(na.omit(x)), n = nclass.Sturges(na.omit(x)), min.n = 1)

gdp <- ggplot(df1) +
  geom_histogram(aes(x = df1$gdp), breaks = brx(df1$gdp)) +
  labs(title = "Histogram of gdp")
freedom <- ggplot(df1) +
  geom_histogram(
    aes(x = df1$freedom),
    breaks = brx(df1$freedom)
  ) +
  labs(title = "Histogram of freedom")
centralbank <- ggplot(df1) +
  geom_histogram(aes(x = df1$centralbank), breaks = brx(df1$centralbank)) +
  labs(title = "Histogram of centralbank")
gini <- ggplot(df1) +
  geom_histogram(aes(x = df1$gini), breaks = brx(df1$gini)) +
  labs(title = "Histogram of gini")
(gdp + freedom) / (centralbank + gini)

# gdpとcentralbankはよくある経済データの右に歪んだ分布なので自然対数に変換する
# freedomは(0,100)なので対数オッズ変換を行う
df1 <- df1 |>
  mutate(gdp = log(gdp)) |>
  mutate(freedom = log(freedom / (100 - freedom))) |>
  mutate(centralbank = log(centralbank))

gdp <- ggplot(df1) +
  geom_histogram(aes(x = df1$gdp), breaks = brx(df1$gdp)) +
  labs(title = "Histogram of gdp")
freedom <- ggplot(df1) +
  geom_histogram(aes(x = df1$freedom), breaks = brx(df1$freedom)) +
  labs(title = "Histogram of freedom")
centralbank <- ggplot(df1) +
  geom_histogram(aes(x = df1$centralbank), breaks = brx(df1$centralbank)) +
  labs(title = "Histogram of centralbank")
gini <- ggplot(df1) +
  geom_histogram(aes(x = df1$gini), breaks = brx(df1$gini)) +
  labs(title = "Histogram of gini")
(gdp + freedom) / (centralbank + gini)
