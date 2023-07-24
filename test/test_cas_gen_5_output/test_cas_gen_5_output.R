library(dplyr)

source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")
source("R/nb_tab.R",encoding = "UTF-8")


# test 1 : table avec 5 var non hier : 2 couples de variables créés --------

data <- expand.grid(
  ACT = c("Total", "A", "B"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #243 rows = 3^5

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB")

# Résultat de la fonction
res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test1" 
)

names(res)
res$vars # Nous avons bien ACT,GEO puis SEX, AGE qui ont été fusionnées

#Les variables ACt et GEO ont été fusionnées
# On vérifie le nombre de tableau (ici les variables non hier ont 1 seul noeud)
length(res$tabs) == 4 * nb_noeuds(hrcfiles = NULL, v="ACT") * 
                        nb_noeuds(hrcfiles = NULL, v="GEO") * 
                        nb_noeuds(hrcfiles = NULL, v="SEX") * 
                        nb_noeuds(hrcfiles = NULL, v="AGE")

length(res$tabs) == calculer_nb_tab("ACT","GEO","SEX","AGE",hrcfiles=NULL)
    
# test d'incohérences 1: ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>%
      tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
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
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0)

# test composition 1 : ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>%
      tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
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
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)

# test 2 : table avec 5 var non hier : 3 variables fusinnées en une --------

data <- expand.grid(
  ACT = c("Total", "A", "B"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #243 rows = 3^5

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB")

# Résultat de la fonction
res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test2",
  v1 = "ACT",
  v2 = "GEO",
  v4 = "ACT_GEO"
)

names(res)
res$vars # Nous avons bien ACT,GEO puis SEX, AGE qui ont été fusionnées

#Les variables ACt et GEO ont été fusionnées
# On vérifie le nombre de tableaux
# Il n'y a pas de formule simple dans le cas 3 variables en une
length(res$tabs) == 2 * nb_noeuds(hrcfiles = NULL, v="ACT") * 
  nb_noeuds(hrcfiles = NULL, v="GEO") * 
  nb_noeuds(hrcfiles = NULL, v="SEX") * 
  (1/2 * 2 * nb_noeuds(hrcfiles = res$hrcs5_4[[1]], hrc_name = FALSE) +
   1/2 * 2 * nb_noeuds(hrcfiles = res$hrcs5_4[[7]], hrc_name = FALSE)   )
# Explication de la formule :

# Lors du passage 5-> 4, on créé 
# 2 * nb_noeuds(hrcfiles = NULL, v="ACT") * nb_noeuds(hrcfiles = NULL, v="GEO")
# tableaux

# Lors du passage 4->3, on fusionne SEx et ACT_GEO, 
# néanmoins ACT_GEO n'a pas la même hierarchie dans chacune des tableaux à 4 dimensions
# comme ici il n'y a que 2 table de dimension 4, la première a une hierarchie 

# pour ACT_GEO de res$hrcs5_4[[1]] (les 6 premiers sont identiques),
# la seconde res$hrcs5_4[[7]] (les 6 derniers sont identiques)
# chacun de ses sous cas crée deux tableaux mais chaque cas ne se produit qu'une fois sur deux
# donc on mutiplie par 2 x 1/2 chacun

# puis on mutliplie par nb_noeuds(hrcfiles = NULL, v="SEX")
# la généralisation est plus compliqué s'il y a plus que 2 tables de dimensions 4
# mais reste possible :)

# Rq :
# nb_noeuds(hrcfiles = res$hrcs5_4[[1]], hrc_name = FALSE) = nombre de modalité de ACt
# nb_noeuds(hrcfiles = res$hrcs5_4[[7]], hrc_name = FALSE) = nombre de moalité de GEO


# Avec la fonction dédiée
n_mod_v1 = length(unique(data[["ACT"]]))
n_mod_v2 = length(unique(data[["GEO"]]))

length(res$tabs) == calculer_nb_tab(v1="ACT",
                                    v2="GEO",
                                    v3="SEX",
                                    hrcfiles = NULL,
                                    data=data)


# test d'incohérences 1: ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(SEX_ACT_GEO, into = c("SEX","ACT","GEO"), sep = "_") %>%
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
      summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
      pull(nb_incoherences) %>% 
      `==`(0)
  }
) %>% 
  unlist() %>% # converti en vector logique pour all()
  all()

# test d'incohérences 2: ok
do.call("rbind",res$tabs) %>% 
  tidyr::separate(SEX_ACT_GEO, into = c("SEX","ACT","GEO"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0)

# test composition 1 : ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(SEX_ACT_GEO, into = c("SEX","ACT","GEO"), sep = "_") %>%
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
      is.na() %>% 
      sum() %>% 
      `==`(0)
  }
) %>% 
  unlist() %>% 
  all()

# test composition 2 : ok
do.call("rbind",res$tabs) %>% 
  tidyr::separate(SEX_ACT_GEO, into = c("SEX","ACT","GEO"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)

# test 3 : fusion de deux couples composé de 2 var hier chacun --------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
  SEX = c("Total", "F", "M","F1","F2","M1","M2"),
  AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) # 7203 rows = 7**4*3

hrc_act <- "test/test_cas_gen_5_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_5_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_age <- "test/test_cas_gen_5_output/test3/hrc_AGE.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("AGE1","AGE2")) %>% 
  sdcHierarchies::hier_add(root = "AGE1", nodes = c("AGE11","AGE12")) %>% 
  sdcHierarchies::hier_add(root = "AGE2", nodes = c("AGE21","AGE22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_age, row.names = F, col.names = F, quote = F)

hrc_sex <- "test/test_cas_gen_5_output/test3/hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("F","M")) %>% 
  sdcHierarchies::hier_add(root = "F", nodes = c("F1","F2")) %>% 
  sdcHierarchies::hier_add(root = "M", nodes = c("M1","M2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB")

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age, SEX = hrc_sex)

# Résultat de la fonction
res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = hrcfiles,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test3",
  v1 = "ACT",
  v2 = "GEO",
  v3 = "SEX",
  v4 = "AGE"
)

#Les variables ACt et GEO ont été fusionnées
# On vérifie le nombre de tableau 
# (ici nous n'avons pas 3 var fusionnées donc c'est la formule simple)
length(res$tabs) == 4 * nb_noeuds(hrcfiles = hrcfiles, v="ACT") * 
                        nb_noeuds(hrcfiles = hrcfiles, v="GEO") * 
                        nb_noeuds(hrcfiles = hrcfiles, v="SEX") * 
                        nb_noeuds(hrcfiles = hrcfiles, v="AGE")

length(res$tabs) == calculer_nb_tab("ACT","GEO","SEX","AGE",hrcfiles=hrcfiles)

# test d'incohérences 1: ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>%
      tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
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
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  pull(nb_incoherences) %>% 
  `==`(0)

# test composition 1 : ok
purrr::map(
  res$tabs,
  function(df){
    df %>% 
      tidyr::separate(ACT_GEO, into = c("ACT","GEO"), sep = "_") %>%
      tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
      rename(VALUE_bis = VALUE) %>% 
      left_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
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
  tidyr::separate(SEX_AGE, into = c("SEX","AGE"), sep = "_") %>%
  rename(VALUE_bis = VALUE) %>% 
  full_join(data, by = c("SEX","AGE","ACT","GEO","ECO")) %>% 
  summarise(nb_incoherences = sum(VALUE != VALUE_bis)) %>% 
  is.na() %>% 
  sum() %>% 
  `==`(0)
