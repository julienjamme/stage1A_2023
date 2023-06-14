
# Cas 1 -------------------------------------------------------------------

tab <- expand.grid(
  cj = paste0("cj", c(1:6,"T")),
  tr = paste0("tr", c(1:5,"T")),
  stringsAsFactors = FALSE
)

tab <- as.data.frame(tab)
str(tab)

tab$cj_tr <- paste(tab$cj, tab$tr, sep = "_")

split(tab, tab$tr)

# cj1_tr1 => cj1_trT et cjT_tr1



# Cas 2 -------------------------------------------------------------------
corr_tab <- data.frame(
  cjB = c(11:14,21:23,31:32,41,51,61)
)
corr_tab$cjA <- round(corr_tab$cjB/10)

tab2 <- expand.grid(
  cj = paste0("cj", c(unique(corr_tab$cjA),corr_tab$cjB,"T")),
  tr = paste0("tr", c(1:5,"T")),
  stringsAsFactors = FALSE
)

tab2 <- as.data.frame(tab2)
str(tab2)

tab2$cj_tr <- paste(tab2$cj, tab2$tr, sep = "_")
split(tab2, tab2$tr)


# Cas table triée selon leur table de correspondance -------------------------------------------------------------

library(dplyr)


# Exemple de table de correspondance pour les variables "A" et "B"
corr_tab_A <- tibble(
  niv1 = c(rep("A1",5),rep("A2",2),"A3"),
  niv2 = c(rep("A11",3),"A12","A2","A21","A3"),
  niv3 = c("A1","A11","A111","A112","A12","A2","A21","A3")
)

corr_tab_B <- tibble(
  niv1 = c(rep("B1",5),rep("B2",2),"B3"),
  niv2 = c("B1",rep("B11",3),"B12","B2","B21","B3"),
  niv3 = c("B1","B11","B111","B112","B12","B2","B21","B3")
)

# Création du croisement cartésien entre A et B, sans oublier le total
tab_c <- expand.grid(
  A = c(c("A1","A11","A111","A112","A12","A2","A21","A3"),"AT"),
  B = c(c("B1","B11","B111","B112","B12","B2","B21","B3"),"BT"),
  stringsAsFactors = FALSE
)
tab_c <- as.data.frame(tab_c)

# tab_c <- tab_c[order(tab_c$A,decreasing = TRUE),]
# sensible à l'ordre !

# Création du croisement cartésien table de corrélation A et B avec ajout du total
tab_c_corr1 <- expand.grid(
  corr_A = c(corr_tab_A$niv1,"AT"),
  corr_B = c(corr_tab_B$niv1,"BT"),
  stringsAsFactors = FALSE
)
tab_c_corr1 <- as.data.frame(tab_c_corr1)

# str(tab_c)

tab_c$A_B <- paste(tab_c$A, tab_c$B, sep = "_")

split(tab_c, tab_c_corr1$corr_A)


# case table non triée selon table de correspondance ------------------------------------------------------------------



