library(Amelia)
library(lattice)
library(miceadds)
M <- 5
set.seed(1)
a_out <- amelia(df1, m = M)
overimpute(a_out, var = 1)
disperse(a_out, dims = , m = 100)
ord <- order(df1$x)
df2 <- df1[ord, ]
missmap(df2)
a.mids <- datlist2mids(a_out$imputations)
densityplot(a.mids)
