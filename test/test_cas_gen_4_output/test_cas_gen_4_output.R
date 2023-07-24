library(dplyr)

source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8") # à enlever un jour
source("R/nb_tab.R",encoding = "UTF-8")

# Test 1 data avec aucune var hier -----------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #81 rows = 3^4

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test1",
  v1 = "SEX",
  v2 = "AGE"
)

#Les variables ACt et GEO ont été fusionnées
# On vérifie le nombre de tableau (ici les variables non hier ont 1 seul noeud)
length(res$tabs) == 2 * nb_noeuds(hrcfiles = NULL, v="ACT") * nb_noeuds(hrcfiles = NULL, v="GEO")

# Test sur les valeurs
# Reappariement avec les données de départ pour vérifier 
# que les nouvelles tables fournissent les bonnes valeurs
res$tabs$tab_SEX %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>%
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>%
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu: TRUE pour aucune incohérence

res$tabs$tab_AGE %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu: TRUE pour aucune incohérence

# => test validé (valeurs sont cohérentes)

# Test de la composition des tables 
res$tabs$tab_SEX %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==` (0) # Attendu = 0 pour aucune valeur manquante aprs fusion
# => toutes les cellules dans tab_SEX sont dans data

res$tabs$tab_AGE %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==` (0) #Attendu = 0 => toutes les valeurs de TAB_AGE sont dans data

res$tabs$tab_AGE %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  unique() %>% 
  nrow() %>% 
  `==`(nrow(data)) # Attendu TRUE => en empilant les deux tables
# et en ne gardant que les lignes uniques on obtient une table 
# qui contient exactement le nb de lignes du tableau de départ

res$tabs$tab_AGE %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  unique() %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) # Attendu TRUE pour aucune incohérences dans les valeurs

res$tabs$tab_AGE %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  unique() %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>%
  is.na() %>% 
  sum() %>% 
  `==`(0)# Attendu 0 pour aucune présence de valeurs manquantes

# Tests de composition: ok pour les cellules

# Les deux tables sont-elles bien les tables attendues ?

res_attendu_1 <- data %>% 
  mutate(SEX_AGE = paste0(SEX, "_", AGE)) %>% 
  filter(!(AGE == "Total" & SEX != "Total")) %>% #on supprime les sous-totaux par sexe
  select(-SEX,-AGE) %>% 
  rename(VALUE_bis = VALUE)

res_attendu_1 %>% 
  full_join(res$tabs$tab_AGE, by = c("SEX_AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)# Attendu = 0 pour aucune valeur manquante
#cad aucun pb d'appariement

res_attendu_1 %>% 
  full_join(res$tabs$tab_AGE, by = c("SEX_AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu 0

identical(
  res_attendu_1 %>% rename(VALUE = VALUE_bis) %>% arrange(SEX_AGE, ACT, GEO),
  res$tabs$tab_AGE %>% arrange(SEX_AGE, ACT, GEO)
)
# les deux tables sont identiques => test validé sur cette table


res_attendu_2 <- data %>% 
  mutate(SEX_AGE = paste0(SEX, "_", AGE)) %>% 
  filter(!(SEX == "Total" & AGE != "Total")) %>% #on supprime les sous-totaux par sexe
  select(-SEX,-AGE)

identical(
  res_attendu_2 %>% arrange(SEX_AGE, ACT, GEO),
  res$tabs$tab_SEX %>% arrange(SEX_AGE, ACT, GEO)
)
# les deux tables sont identiques
# les tests de composition des tables sont validés!

# La correspondance entre hrc et tableaux

res$tabs$tab_SEX %>% 
  filter(SEX_AGE != "Total_Total") %>% 
  pull(SEX_AGE) %>% 
  unique() %>% 
  sort() %>% 
  `==`(
    read.table(res$hrcs$tab_SEX) %>% 
      mutate(V1 = gsub("@","",V1)) %>% 
      pull(V1) %>% 
      sort()
  ) %>% 
  all()

res$tabs$tab_AGE %>% 
  filter(SEX_AGE != "Total_Total") %>% 
  pull(SEX_AGE) %>% 
  unique() %>% 
  sort() %>% 
  `==`(
    read.table(res$hrcs$tab_AGE) %>% 
      mutate(V1 = gsub("@","",V1)) %>% 
      pull(V1) %>% 
      sort()
  ) %>% 
  all()


# Test 2 data avec une seule var hier -----------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #189 rows = 7*3^3

hrc_act <- "test/test_cas_gen_4_output/test2/hrc_ACT.hrc"

sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test2" 
)



# On vérifie le nombre de tableau (ici les variables non hier ont 1 seul noeud)
length(res$tabs) == 2 * nb_noeuds(hrcfiles = NULL, v="SEX") * nb_noeuds(hrcfiles = NULL, v="GEO")

# Maintenant ACT est hier donc on ne le choisi pas en priorité
# GEO,SEX etAGe ont sont non hier et ont le même nombre de modalité
# donc on prend les deux premiers
all(sort(res$vars) == sort(unlist(list("SEX","GEO"))))

# Test sur les valeurs
# Reappariement avec les données de départ pour vérifier 
# que les nouvelles tables fournissent les bonnes valeurs
res$tabs$tab_GEO %>% 
  tidyr::separate(GEO_SEX, into = c("GEO","SEX"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>%
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>%
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu: TRUE pour aucune incohérence

res$tabs$tab_SEX %>% 
  tidyr::separate(GEO_SEX, into = c("GEO","SEX"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu: TRUE pour aucune incohérence

# => test validé (valeurs sont cohérentes)

# Test de la composition des tables 
res$tabs$tab_GEO %>% 
  tidyr::separate(GEO_SEX, into = c("GEO","SEX"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==` (0) # Attendu = 0 pour aucune valeur manquante aprs fusion
# => toutes les cellules dans tab_SEX sont dans data

res$tabs$tab_SEX %>% 
  tidyr::separate(GEO_SEX, into = c("GEO","SEX"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==` (0) #Attendu = 0 => toutes les valeurs de TAB_AGE sont dans data

res$tabs$tab_GEO %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(GEO_SEX, into = c("GEO","SEX"), sep = "_") %>% 
  unique() %>% 
  nrow() %>% 
  `==`(nrow(data)) # Attendu TRUE => en empilant les deux tables
# et en ne gardant que les lignes uniques on obtient une table 
# qui contient exactement le nb de lignes du tableau de départ

res$tabs$tab_GEO %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(GEO_SEX, into = c("GEO","SEX"), sep = "_") %>% 
  unique() %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) # Attendu TRUE pour aucune incohérences dans les valeurs

res$tabs$tab_GEO %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(GEO_SEX, into = c("GEO","SEX"), sep = "_") %>% 
  unique() %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>%
  is.na() %>% 
  sum() %>% 
  `==`(0)# Attendu 0 pour aucune présence de valeurs manquantes

# Tests de composition: ok pour les cellules

# Les deux tables sont-elles bien les tables attendues ?

res_attendu_1 <- data %>% 
  mutate(GEO_SEX = paste0(GEO, "_", SEX)) %>% 
  filter(!(GEO == "Total" & SEX != "Total")) %>% #on supprime les sous-totaux par sexe
  select(-GEO,-SEX) %>% 
  rename(VALUE_bis = VALUE)

res_attendu_1 %>% 
  full_join(res$tabs$tab_GEO, by = c("GEO_SEX","ACT","AGE")) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)# Attendu = 0 pour aucune valeur manquante
#cad aucun pb d'appariement

res_attendu_1 %>% 
  full_join(res$tabs$tab_GEO, by = c("GEO_SEX","ACT","AGE")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu 0

identical(
  res_attendu_1 %>% rename(VALUE = VALUE_bis) %>% arrange(GEO_SEX, ACT, AGE),
  res$tabs$tab_GEO %>% arrange(GEO_SEX, ACT, AGE)
)
# les deux tables sont identiques => test validé sur cette table


res_attendu_2 <- data %>% 
  mutate(GEO_SEX = paste0(GEO, "_", SEX)) %>% 
  filter(!(SEX == "Total" & GEO != "Total")) %>% #on supprime les sous-totaux par sexe
  select(-SEX,-GEO)

identical(
  res_attendu_2 %>% arrange(GEO_SEX, ACT, AGE),
  res$tabs$tab_SEX %>% arrange(GEO_SEX, ACT, AGE)
)
# les deux tables sont identiques
# les tests de composition des tables sont validés!

# La correspondance entre hrc et tableaux

res$tabs$tab_GEO %>% 
  filter(GEO_SEX != "Total_Total") %>% 
  pull(GEO_SEX) %>% 
  unique() %>% 
  sort() %>% 
  `==`(
    read.table(res$hrcs$tab_GEO) %>% 
      mutate(V1 = gsub("@","",V1)) %>% 
      pull(V1) %>% 
      sort()
  ) %>% 
  all()

res$tabs$tab_SEX %>% 
  filter(GEO_SEX != "Total_Total") %>% 
  pull(GEO_SEX) %>% 
  unique() %>% 
  sort() %>% 
  `==`(
    read.table(res$hrcs$tab_SEX) %>% 
      mutate(V1 = gsub("@","",V1)) %>% 
      pull(V1) %>% 
      sort()
  ) %>% 
  all()

# Test 3 data avec deux var hier -----------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) #441 rows = 7*7*3^2

hrc_act <- "test/test_cas_gen_4_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output//test3" 
)

# On vérifie le nombre de tableau (ici les variables non hier ont 1 seul noeud)
length(res$tabs) == 2 * nb_noeuds(hrcfiles = NULL, v="SEX") * nb_noeuds(hrcfiles = NULL, v="AGE")

length(res$tabs) == calculer_nb_tab("SEX","AGE",hrcfiles=NULL)

# Maintenant ACT est hier donc on ne le choisi pas en priorité
# GEO,SEX etAGe ont sont non hier et ont le même nombre de modalité
# donc on prend les deux premiers
all(sort(res$vars) == sort(unlist(list("SEX","AGE"))))

# le résultat:
names(res)
all(names(res$tabs) == names(res$hrcs))
all(names(res$tabs) == names(res$alt_tot))

# Test sur les valeurs
# Reappariement avec les données de départ pour vérifier 
# que les nouvelles tables fournissent les bonnes valeurs
res$tabs$tab_SEX %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>%
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>%
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu: TRUE pour aucune incohérence

res$tabs$tab_AGE %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) #Attendu: TRUE pour aucune incohérence

# => test validé (valeurs sont cohérentes)

# Test de la composition des tables 
res$tabs$tab_SEX %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0) # Attendu = 0 pour aucune valeur manquante aprs fusion
# => toutes les cellules dans tab_SEX sont dans data

res$tabs$tab_AGE %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0) #Attendu = 0 => toutes les valeurs de TAB_AGE sont dans data

res$tabs$tab_AGE %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  unique() %>% 
  nrow() %>% 
  `==`(nrow(data)) # Attendu TRUE => en empilant les deux tables
# et en ne gardant que les lignes uniques on obtient une table 
# qui contient exactement le nb de lignes du tableau de départ

res$tabs$tab_AGE %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  unique() %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0) # Attendu TRUE pour aucune incohérences dans les valeurs

res$tabs$tab_AGE %>% 
  bind_rows(res$tabs$tab_SEX) %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  unique() %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>%
  is.na() %>% 
  sum() %>% 
  `==`(0) # Attendu 0 pour aucune présence de valeurs manquantes

# Tests de composition: ok pour les cellules

# Les deux tables sont-elles bien les tables attendues ?

res_attendu_1 <- data %>% 
  mutate(SEX_AGE = paste0(SEX, "_", AGE)) %>% 
  filter(!(AGE == "Total" & SEX != "Total")) %>% #on supprime les sous-totaux par sexe
  select(-SEX,-AGE) %>% 
  rename(VALUE_bis = VALUE)

res_attendu_1 %>% 
  full_join(res$tabs$tab_AGE, by = c("SEX_AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)# Attendu = 0 pour aucune valeur manquante
#cad aucun pb d'appariement

res_attendu_1 %>% 
  full_join(res$tabs$tab_AGE, by = c("SEX_AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0)  #Attendu 0

identical(
  res_attendu_1 %>% rename(VALUE = VALUE_bis) %>% arrange(SEX_AGE, ACT, GEO),
  res$tabs$tab_AGE %>% arrange(SEX_AGE, ACT, GEO)
)
# les deux tables sont identiques => test validé sur cette table


res_attendu_2 <- data %>% 
  mutate(SEX_AGE = paste0(SEX, "_", AGE)) %>% 
  filter(!(SEX == "Total" & AGE != "Total")) %>% #on supprime les sous-totaux par sexe
  select(-SEX,-AGE)

identical(
  res_attendu_2 %>% arrange(SEX_AGE, ACT, GEO),
  res$tabs$tab_SEX %>% arrange(SEX_AGE, ACT, GEO)
)
# les deux tables sont identiques
# les tests de composition des tables sont validés!


# La correspondance entre hrc et tableaux

res$tabs$tab_SEX %>% 
  filter(SEX_AGE != "Total_Total") %>% 
  pull(SEX_AGE) %>% 
  unique() %>% 
  sort() %>% 
  `==`(
    read.table(res$hrcs$tab_SEX) %>% 
      mutate(V1 = gsub("@","",V1)) %>% 
      pull(V1) %>% 
      sort()
  ) %>% 
  all()

res$tabs$tab_AGE %>% 
  filter(SEX_AGE != "Total_Total") %>% 
  pull(SEX_AGE) %>% 
  unique() %>% 
  sort() %>% 
  `==`(
    read.table(res$hrcs$tab_AGE) %>% 
      mutate(V1 = gsub("@","",V1)) %>% 
      pull(V1) %>% 
      sort()
  ) %>% 
  all()


# Test 4: trois variables hiérarchiques -----------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) #1029 rows = 7*7*7*3

hrc_act <- "test/test_cas_gen_4_output/test4/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test4/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_age <- "test/test_cas_gen_4_output/test4/hrc_AGE.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("AGE1","AGE2")) %>% 
  sdcHierarchies::hier_add(root = "AGE1", nodes = c("AGE11","AGE12")) %>% 
  sdcHierarchies::hier_add(root = "AGE2", nodes = c("AGE21","AGE22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_age, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test4" 
)

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age)

# Tous les fichiers hiérarchiques ont 3 noeuds
nb_noeuds(hrcfiles = hrcfiles, v="ACT")
nb_noeuds(hrcfiles = hrcfiles, v="GEO")
nb_noeuds(hrcfiles = hrcfiles, v="AGE")

# On prend donc la seule var non hier + la première var hier
all(sort(res$vars) == sort(unlist(list("SEX","ACT"))))

# Vérification du nombre de tableaux
length(res$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v="SEX") * nb_noeuds(hrcfiles = hrcfiles, v="ACT")

length(res$tabs) == calculer_nb_tab("SEX","ACT",hrcfiles=hrcfiles)

names(res)
res$vars
names(res$tabs) # pb1: les noms des tables sont à revoir pour être plus explicite
all(names(res$hrcs) == names(res$tabs))
all(names(res$alt_tot) == names(res$tabs))

mods_sex <- data %>% pull(SEX) %>% unique()
mods_act <- data %>% pull(ACT) %>% unique()

# test d'incohérences 1: ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(SEX_ACT, into = c("SEX","ACT"), sep = "_") %>% 
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
      summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
      pull(nb_incoherences) %>% 
      `==`(0)
  }
) %>% 
  unlist() %>% # converti en vector logique pour all()
  all()

# test d'incohérences 2: ok
do.call("rbind",res$tabs) %>% 
  tidyr::separate(SEX_ACT, into = c("SEX","ACT"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0)

# test composition 1 : ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(SEX_ACT, into = c("SEX","ACT"), sep = "_") %>% 
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
      is.na() %>% 
      sum() %>% 
      `==`(0)
  }
) %>% 
  unlist() %>% 
  all()

# test composition 2 : ok
do.call("rbind",res$tabs) %>% 
  tidyr::separate(SEX_ACT, into = c("SEX","ACT"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)


# les cellules communes des tableaux


# test hrc
purrr::map(
  names(res$tabs),
  function(df){
    p <- res$tabs[[df]] %>% 
      filter(SEX_ACT != res$alt_tot[[df]]) %>% 
      pull(SEX_ACT) %>% 
      unique() %>% 
      sort() %>% 
      `==`(
        read.table(res$hrcs[[df]]) %>% 
          mutate(V1 = gsub("@","",V1)) %>% 
          pull(V1) %>% 
          sort()
      ) %>% 
      all()
  }
) %>% 
  unlist() %>% 
  all()

# Test 5: quatre variables hiérarchiques -------------------------------


data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
  SEX = c("Total", "F", "M","F1","F2","M1","M2"),
  AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) #2401 rows = 7***4

hrc_act <- "test/test_cas_gen_4_output/test5/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test5/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_age <- "test/test_cas_gen_4_output/test5/hrc_AGE.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("AGE1","AGE2")) %>% 
  sdcHierarchies::hier_add(root = "AGE1", nodes = c("AGE11","AGE12")) %>% 
  sdcHierarchies::hier_add(root = "AGE2", nodes = c("AGE21","AGE22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_age, row.names = F, col.names = F, quote = F)

hrc_sex <- "test/test_cas_gen_4_output/test5/hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("F","M")) %>% 
  sdcHierarchies::hier_add(root = "F", nodes = c("F1","F2")) %>% 
  sdcHierarchies::hier_add(root = "M", nodes = c("M1","M2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age, SEX = hrc_sex)

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = hrcfiles,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test5" 
)


# Tous les fichiers hiérarchiques ont 3 noeuds
nb_noeuds(hrcfiles = hrcfiles, v="ACT")
nb_noeuds(hrcfiles = hrcfiles, v="GEO")
nb_noeuds(hrcfiles = hrcfiles, v="AGE")
nb_noeuds(hrcfiles = hrcfiles, v="SEX")

# On prend donc les deux premières var
all(sort(res$vars) == sort(unlist(list("ACT","GEO"))))

# Vérification du nombre de tableaux
length(res$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v="GEO") * nb_noeuds(hrcfiles = hrcfiles, v="ACT")
length(res$tabs) == calculer_nb_tab("GEO","ACT",hrcfiles=hrcfiles)

names(res)
res$vars
names(res$tabs) # pb1: les noms des tables sont à revoir pour être plus explicite
all(names(res$hrcs) == names(res$tabs))
all(names(res$alt_tot) == names(res$tabs))

mods_sex <- data %>% pull(SEX) %>% unique()
mods_act <- data %>% pull(ACT) %>% unique()

# test d'incohérences 1: ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>% 
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
      summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
      pull(nb_incoherences) %>% 
      `==`(0)
  }
) %>% 
  unlist() %>% # converti en vector logique pour all()
  all()

# test d'incohérences 2: ok
do.call("rbind",res$tabs) %>% 
  tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0)

# test composition 1 : ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>% 
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
      is.na() %>% 
      sum() %>% 
      `==`(0)
  }
) %>% 
  unlist() %>% 
  all()

# test composition 2 : ok
do.call("rbind",res$tabs) %>% 
  tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)


# test hrc
purrr::map(
  names(res$tabs),
  function(df){
    p <- res$tabs[[df]] %>% 
      filter(ACT_GEO != res$alt_tot[[df]]) %>% 
      pull(ACT_GEO) %>% 
      unique() %>% 
      sort() %>% 
      `==`(
        read.table(res$hrcs[[df]]) %>% 
          mutate(V1 = gsub("@","",V1)) %>% 
          pull(V1) %>% 
          sort()
      ) %>% 
      all()
  }
) %>% 
  unlist() %>% 
  all()

# Test 6: Fusion d'une variable hiérarchique de profondeur 4 avec var non hier------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2","B11","B12","B111","B112"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #297 rows = 11*3^3

hrc_act <- "test/test_cas_gen_4_output/test6/hrc_ACT.hrc"

sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_add(root = "B1", nodes = c("B11","B12")) %>%
  sdcHierarchies::hier_add(root = "B11", nodes = c("B111","B112")) %>%
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

hrcfiles = c(ACT = hrc_act)

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = hrcfiles,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test6",
  v1 = "ACT"
)

# On a forcé v1 = ACT
# v2 est donc la première var non hier (puisque même modalité), ie GEO
all(sort(res$vars) == sort(unlist(list("ACT","GEO"))))

# On vérifie le nombre de tableau (ici les variables non hier ont 1 seul noeud)
length(res$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v="ACT") * nb_noeuds(hrcfiles = hrcfiles, v="GEO")

# test hrc
purrr::map(
  names(res$tabs),
  function(df){
    p <- res$tabs[[df]] %>% 
      filter(GEO_ACT != res$alt_tot[[df]]) %>% 
      pull(GEO_ACT) %>% 
      unique() %>% 
      sort() %>% 
      `==`(
        read.table(res$hrcs[[df]]) %>% 
          mutate(V1 = gsub("@","",V1)) %>% 
          pull(V1) %>% 
          sort()
      ) %>% 
      all()
  }
) %>% 
  unlist() %>% 
  all()

# Test 7: Fusion de deux var hier - profondeur 3 avec profondeur 4------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2","B11","B12","B111","B112"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22", "AGE221", "AGE222"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #297 rows = 11*3^3

hrc_act <- "test/test_cas_gen_4_output/test7/hrc_ACT.hrc"

sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_add(root = "B1", nodes = c("B11","B12")) %>%
  sdcHierarchies::hier_add(root = "B11", nodes = c("B111","B112")) %>%
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_age <- "test/test_cas_gen_4_output/test7/hrc_AGE.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("AGE1","AGE2")) %>% 
  sdcHierarchies::hier_add(root = "AGE1", nodes = c("AGE11","AGE12")) %>% 
  sdcHierarchies::hier_add(root = "AGE2", nodes = c("AGE21","AGE22")) %>% 
  sdcHierarchies::hier_add(root = "AGE22", nodes = c("AGE221","AGE222")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_age, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

hrcfiles = c(ACT = hrc_act, AGE = hrc_age)

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = hrcfiles,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test7",
  v1 = "ACT",
  v2 = "AGE"
)

# On a forcé v1 = ACT
# v2 est donc la première var non hier (puisque même modalité), ie GEO
all(sort(res$vars) == sort(unlist(list("ACT","AGE"))))

# On vérifie le nombre de tableau (ici les variables non hier ont 1 seul noeud)
length(res$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v="ACT") * nb_noeuds(hrcfiles = hrcfiles, v="AGE")

# test hrc
purrr::map(
  names(res$tabs),
  function(df){
    p <- res$tabs[[df]] %>% 
      filter(ACT_AGE != res$alt_tot[[df]]) %>% 
      pull(ACT_AGE) %>% 
      unique() %>% 
      sort() %>% 
      `==`(
        read.table(res$hrcs[[df]]) %>% 
          mutate(V1 = gsub("@","",V1)) %>% 
          pull(V1) %>% 
          sort()
      ) %>% 
      all()
  }
) %>% 
  unlist() %>% 
  all()
