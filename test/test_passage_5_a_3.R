# Vider l'environnement global
rm(list = ls())

library(dplyr)
source(file = "R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("Ensemble","riche","pauvre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc" )

tot_code<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="Total", ECO = "Ensemble")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

# obtention de V1 et v2
var_cat <- names(totcode)
var_sans_hier <- intersect(
  setdiff(names(dfs), names(hrcfiles)),
  var_cat
)
dfs_var_sans_hier <- subset(dfs,select = var_sans_hier)
# res_var<-get_2_smallest(hrcfiles,totcode)
# v1 <- names(res_var)[[1]]
# v2 <- names(res_var)[[2]]

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE
v1 = NULL
v2 = NULL
v3 = NULL
v4 = NULL
sep = "_"

res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)
str(res)

# test séparateur
res_plusplus_ <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name, sep = "++")

# Test pour fusionner trois variables ensemble :) (mauvaise idée, trop de noeuds !)
res_SEX_AGE_ECO <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE,hrc_dir = dir_name, v3 = "SEX",v4 = "AGE_ECO")

# test des erreurs
res_ACT_ACT <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE,
                                   hrc_dir = dir_name, v3 = "ACT",v4 = "ACT")
# Erreur. Vous essayez de fusionner une variable avec elle-même
res_MAVAR3 <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE,
                                  hrc_dir = dir_name, v3 = "MAVAR")
# v3 n'est pas une variable catégorielle
res_MAVAR4 <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE,
                                  hrc_dir = dir_name, v4 = "MAVAR")
# v4 n'est pas une variable catégorielle

# bon format
str(res)

# nombre tableau cohérent
# Passage 5 -> 4
# AGE, ECO non hrc => 2 tableaux crées lors de leur fusion
# Passage 4 -> 3
# GEO, ACT var hrc moins de noeuds : 3, 2 => 3 * 2 combinaisons de hrc simple crée
# que l'on décline en 2 fois à chaque fois 
# (à cause de la fusion des 2 vars pour tenir compte de la hierarchie créée)
# 2 * 3 * 2 * 2 = 24, le compte est bon
length(res$tabs)
 
###############

# Test pour savoir si on peut fusionner 3 variables ensembles :)
data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("Ensemble","riche","pauvre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc3.hrc", GEO = "hrc/hrc3.hrc", SEX = "hrc/hrc3.hrc" )

tot_code<-c(SEX="Total",AGE="Ensemble", GEO="Total", ACT="Total", ECO = "Ensemble")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

# obtention de V1 et v2
var_cat <- names(totcode)
var_sans_hier <- intersect(
  setdiff(names(dfs), names(hrcfiles)),
  var_cat
)
dfs_var_sans_hier <- subset(dfs,select = var_sans_hier)
# res_var<-get_2_smallest(hrcfiles,totcode)
# v1 <- names(res_var)[[1]]
# v2 <- names(res_var)[[2]]

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE

res5_4 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)

# On vérifie que la variable fusionnée a moins de noeuds en moyenne que les autres variables
test_nb_tabs_3hrc(res5_4$hrcs, names(res5_4$hrcs)[1], res5_4$alt_tot)
test_nb_tabs_3hrc(res5_4$hrcs, names(res5_4$hrcs)[2], res5_4$alt_tot)

test_nb_tabs_3hrc(hrcfiles, names(hrcfiles)[1], totcode)
test_nb_tabs_3hrc(hrcfiles, names(hrcfiles)[2], totcode)
test_nb_tabs_3hrc(hrcfiles, names(hrcfiles)[3], totcode)

# On obtient bien 
# Passage 4 à 3: chr [1:2] "AGE_ECO" "ACT"
res5_3 <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)


#########
data <- expand.grid(
  ACT = c("Ensemble","Est","Ouest","Est1","Ouest1","Est2","Ouest2"),
  SEX = c("Ensemble","Nord","Sud","Nord1","Sud1","Nord2"),
  GEO = c("Ensemble","Oui","Non","Oui1","Non1"),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("Ensemble","riche","pauvre","autre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrcfiles <- NULL

totcode <- c(SEX="Ensemble",AGE="Ensemble", GEO="Ensemble", ACT="Ensemble", ECO = "Ensemble")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

# obtention de V1 et v2
var_cat <- names(totcode)
var_sans_hier <- intersect(
  setdiff(names(dfs), names(hrcfiles)),
  var_cat
)
dfs_var_sans_hier <- subset(dfs,select = var_sans_hier)
# res_var<-get_2_smallest(hrcfiles,totcode)
# v1 <- names(res_var)[[1]]
# v2 <- names(res_var)[[2]]

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE
v1 <- NULL
v2 <- NULL
v3 <- NULL
v4 <- NULL

res5_3 <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)

res5_4 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)

nb_noeuds <- lapply(names(res5_4$hrcs),
                    function(x) test_nb_tabs_3hrc(res5_4$hrcs, x, res5_4$alt_tot))

rep(res5_4$hrcs[[1]],nb_noeuds[1])

unlist(lapply(1:length(res5_4$hrcs), function(i) rep(res5_4$hrcs[[i]], nb_noeuds[i])))



