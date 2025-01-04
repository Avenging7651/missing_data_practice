library(Amelia)
library(MKmisc)
M <- 5
set.seed(1)
a_out <- amelia(df1, m = M)
mi.t.test(a_out$imputations, x = "income", mu = 300)
