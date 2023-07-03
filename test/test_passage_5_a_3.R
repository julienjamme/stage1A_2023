# Vider l'environnement global
rm(list = ls())

library(dplyr)
source(file = "R/passage_5_3.R",encoding = "UTF-8")

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
res_var<-get_2_smallest(hrcfiles,totcode)
v1 <- names(res_var)[[1]]
v2 <- names(res_var)[[2]]

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE

res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)

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


##################################





data2 <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays","Paris","province"),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("Ensemble","riche","pauvre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()
dir_name <- "output"
hrcfiles
res2 <- passer_de_5_a_3_var(data2,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, dir_name)
