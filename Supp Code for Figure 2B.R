install.packages("easypackages")
library("easypackages")
libraries("readr", "data.table", "dplyr", "gtsummary","openxlsx", "readxl", "MASS", "ggplot2")


ggplot(as.data.frame(mfa), aes(x=avg_Rcount, y=Pred/avg_Rcount)) + 
  geom_point() + 
  geom_hline(yintercept = 1)


ggplot(as.data.frame(mfa), aes(x=Pred, y=Pred/avg_Rcount)) + 
  geom_point() + 
  geom_hline(yintercept = 1)


ggplot(as.data.frame(mfa), aes(x = 1, y=log2(Pred))) +
  geom_violin()

hist(as.data.frame(mfa))
