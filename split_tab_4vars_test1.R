
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


# case table non triée selon leur table de correspondance ------------------------------------------------------------------


# Exemple de table de correspondance pour les variables "A" et "B"
# Enlève les niveaux non nécessaires par rapport au cas précédent
corr_tab_A <- tibble(
  niv1 = c(rep("A1",3),rep("A2",2),"A3"),
  niv2 = c(rep("A11",2),"A12","A21","A22","A3"),
  niv3 = c("A111","A112","A12","A21","A22","A3")
)

corr_tab_B <- tibble(
  niv1 = c(rep("B1",3),rep("B2",2),"B3"),
  niv2 = c(rep("B11",2),"B12","B21","B22","B3"),
  niv3 = c("B111","B112","B12","B21","B22","B3")
)


# Création du croisement cartésien entre A et B, sans oublier le total
tab_c <- expand.grid(
  A = c(c("A111","A112","A12","A21","A22","A3"),"AT"),
  B = c(c("B111","B112","B12","B21","B22","B3"),"BT"),
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


tab_c_shuffle = tab_c[sample(1:nrow(tab_c)), ]

tmp <- tab_c_shuffle[order(tab_c_shuffle$B, tab_c_shuffle$A),]

split(tmp, tab_c_corr1$corr_A)
# -> ça marche


################################################
################################################
# Deuxième teste avec des hierarchies différentes
################################################
################################################


# Exemple de table de correspondance pour les variables "A" et "B"
# Enlève les niveaux non nécessaires par rapport au cas précédent
corr_tab_A <- tibble(
  niv1 = c(rep("A1",3),rep("A2",2),"A3"),
  niv2 = c(rep("A11",2),"A12","A21","A22","A3"),
  niv3 = c("A111","A112","A12","A21","A22","A3")
)

corr_tab_B <- tibble(
  niv1 = c(rep("B1",2),rep("B2",2),"B3",rep("B4",2)),
  niv2 = c("B11","B12","B21","B22","B3","B41","B42"),
)


# Création du croisement cartésien entre A et B, sans oublier le total
# Ajout de tous les sous totaux :)

sous_totA <- setdiff(union(corr_tab_A$niv1,corr_tab_A$niv2), 
                     corr_tab_A$niv3)

sous_totB <- setdiff(corr_tab_B$niv1, 
                     corr_tab_B$niv2)


tab_c <- expand.grid(
  A = c(corr_tab_A$niv3,sous_totA,"AT"),
  B = c(corr_tab_B$niv2,sous_totB,"BT"),
  stringsAsFactors = FALSE
)

tab_c <- as.data.frame(tab_c)

# Création du croisement cartésien table de corrélation A et B avec ajout du total
tab_c_corr1 <- expand.grid(
  corr_A = c(corr_tab_A$niv1,sous_totA,"AT"),
  corr_B = c(corr_tab_B$niv1,sous_totB,"BT"),
  stringsAsFactors = FALSE
)
tab_c_corr1 <- as.data.frame(tab_c_corr1)

tab_c_corr2 <- expand.grid(
  corr_A = c(corr_tab_A$niv1,sous_totA,"AT"),
  corr_B = c(corr_tab_B$niv1,sous_totB,"BT"),
  stringsAsFactors = FALSE
)
tab_c_corr2 <- as.data.frame(tab_c_corr2)

# str(tab_c)

tab_c$A_B <- paste(tab_c$A, tab_c$B, sep = "_")

split_table<- split(tab_c, tab_c_corr1$corr_A)


# rbind ???

# to do :
# il faudrait ne pas spiter par rapport à tous les sous totaux
# mais uniquement les sous totaux grossier (niveau 1 + total)
# -> trouver comment ne pas trop split
# ou bien -> trouver comment merge les sous totaux



# Autre approche : utilisation de colonne supplémentaire représentant
# les niveaux de correspondances

library(dplyr)

# table de correspondance de la table A
corr_tab_A <- tibble(
  niv1 = c(rep("A1",3),rep("A2",2),"A3"),
  niv2 = c(rep("A11",2),"A12","A21","A22","A3"),
  niv3 = c("A111","A112","A12","A21","A22","A3")
)

# table de correspondance de la table Ab
# afin d'avoir un autre exemple empirique
corr_tab_Ab <- tibble(
  niv1 = c(rep("A1",5),rep("A2",2),"A3"),
  niv2 = c(rep("A11",4),"A12","A21","A22","A3"),
  niv3 = c(rep("A111",3),"A112","A12","A21","A22","A3"),
  niv4 = c(rep("A1111",2),"A1112","A112","A12","A21","A22","A3"),
  niv5 = c("A11111","A11112","A1112","A112","A12","A21","A22","A3")
)


# Création de la table A à partir de la table de correpondance
tabA <- Reduce(union, corr_tab_A)

# Création de colonne correspondant aux niveaux de correspondances
# comme dans une table de correspondance
# sans redondance des niveaux -> utilisation de ""

#fait à la main (laborieux)
# ne marche que si tabA n'est pas un dataframe de base

tabA <- as.data.frame(tabA) %>%
  mutate(level1 = case_when(
    tabA %in% corr_tab_A$niv1 ~ tabA,
    tabA %in% corr_tab_A$niv2 ~ corr_tab_A$niv1[match(tabA, corr_tab_A$niv2)],
    tabA %in% corr_tab_A$niv3 ~ corr_tab_A$niv1[match(tabA, corr_tab_A$niv3)]
  )) %>%
  mutate(level2 = case_when(
    tabA %in% corr_tab_A$niv1 ~ "",
    tabA %in% corr_tab_A$niv2 ~ tabA,
    tabA %in% corr_tab_A$niv3 ~ corr_tab_A$niv2[match(tabA, corr_tab_A$niv3)]
  )) %>%
  mutate(level3 = case_when(
    tabA %in% corr_tab_A$niv1 ~ "",
    tabA %in% corr_tab_A$niv2 ~ "",
    tabA %in% corr_tab_A$niv3 ~ tabA
  ))

tabA


# to do : trouver comment le faire dynamiquement

# it fails for now :(
tabAdf <- as.data.frame(Reduce(union, corr_tab_A))
# Create a new column "level1" in tabA
tabAdf <- tabAdf %>%
  mutate(level1 = case_when(
    tabAdf %in% corr_tab_A$niv1 ~ tabAdf,
    tabAdf %in% corr_tab_A$niv2 ~ corr_tab_A$niv1[match(tabAdf, corr_tab_A$niv2)],
    tabAdf %in% corr_tab_A$niv3 ~ corr_tab_A$niv1[match(tabAdf, corr_tab_A$niv3)]
  ))

tabAdf


# filtration pour split ensuite en sous tableaux hierarchique
# a l'air de bien marcher, à vérifier pour généraliser la formule pour n niveau
# hierarchique

# groupes de niveau 0
tabA %>% 
  filter(level2 == "") %>% 
  select(tabA)

# groupes de niveau 1
tabA1b <- tabA %>%
  filter(level3 == "") %>%
  split(.$level1)

tabA1b

# groupes de niveau 2
tabA2b <- tabA %>% 
  filter(level3 != "" | (level3 == "" & level2 != "")) %>% 
  split(.$level2)

tabA2b



