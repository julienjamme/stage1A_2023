
library("dplyr")

corr_tab <- data.frame(
  cjB = c(11:14,21:23,31:32,41,51,61)
)
corr_tab$cjA <- round(corr_tab$cjB/10)

corr_tab2 <- data.frame(
  trB = c(11:12,21,31,41,51)
)
corr_tab2$trA <- round(corr_tab2$trB/10)

corr_tab3 <- tibble(
  niv0 = "ALL",
  niv1 = c(rep("A",2),rep("B",3), "C"),
  niv2 = c("A1","A2","B1",rep("B2",2), "C"),
  niv3 = c("A1","A2","B1", "B21", "B22", "C")
)

corr_tab4 <- tibble(
  niv1 = c(rep("A",2),rep("B",5), "C"),
  niv2 = c("A1","A2","B1",rep("B2",4), "C"),
  niv3 = c("A1","A2","B1", rep("B21",2),rep("B22",2), "C"),
  niv4 = c("A1","A2","B1", "B211","B212" ,"B221","B222", "C")
)