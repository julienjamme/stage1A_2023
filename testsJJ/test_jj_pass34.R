library(dplyr)

source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")


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
  hrc_dir = "hrc_alt" 
)
# Le code renvoie une erreur car il n'est pas adapté au cas où hrcfiles = NULL
# => faire en sorte de prendre en compte le cas où aucune variable n'est hier

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

hrc_act <- "testsJJ/test2/hrc_ACT.hrc"

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
  hrc_dir = "hrc_alt" 
)
# Le code génère une erreur subscript out of bounds
# car le cas avec 1 seule var hierarchique est mal codé dans la fonction
# En effet, la fonction get_2_smallest est appelée sur les hrcfiles alors qu'il
# y a qu'une var hrc
# => test invalidé


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

hrc_act <- "testsJJ/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "testsJJ/test3/hrc_GEO.hrc"
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
  hrc_dir = "testsJJ/test3" 
)
# Le code ne génère pas d'erreur
# le résultat:
names(res)
all(names(res$tabs) == names(res$hrcs))
all(names(res$tabs) == names(res$alt_tot))

names(res$hrcs$tab_SEX) == "SEX_AGE" 
names(res$hrcs$tab_AGE) == "SEX_AGE" 
# pb1: la hrc n'a pas de nom
# attendu: c(SEX_AGE = fichier hrc)
# donc l'attendu est :
# res$hrcs = list(tab_SEX = c(SEX_AGE = "hrc_tab_SEX.hrc"), tab_AGE = c(SEX_AGE = "hrc_tab_AGE.hrc"))

names(res$alt_tot$tab_SEX) == "SEX_AGE"
names(res$alt_tot$tab_AGE) == "SEX_AGE"
# pb2: les alt_tot n'a pas de nom
# attendu ici: c(SEX_AGE = "Total_Total")
# donc l'attendu est  
# res$alt_tot = list(tab_SEX = c(SEX_AGE = "Total_Total"), tab_AGE = c(SEX_AGE = "Total_Total"))

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
  sum() # Attendu = 0 pour aucune valeur manquante aprs fusion
# => toutes les cellules dans tab_SEX sont dans data

res$tabs$tab_AGE %>% 
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  left_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  is.na() %>% 
  sum() #Attendu = 0 => toutes les valeurs de TAB_AGE sont dans data

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
  sum() # Attendu 0 pour aucune présence de valeurs manquantes

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
  sum() # Attendu = 0 pour aucune valeur manquante
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

hrc_act <- "testsJJ/test4/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "testsJJ/test4/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_age <- "testsJJ/test4/hrc_AGE.hrc"
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
  hrc_dir = "testsJJ/test4" 
)
# pas d'erreur
names(res)
res$vars
names(res$tabs) # pb1: les noms des tables ne sont pas pertinents
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
)

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
      sum()
  }
)

# test composition 2 : ok
do.call("rbind",res$tabs) %>% 
  tidyr::separate(SEX_ACT, into = c("SEX","ACT"), sep = "_") %>% 
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  is.na() %>% 
  sum()


# les cellules communes des tableaux




