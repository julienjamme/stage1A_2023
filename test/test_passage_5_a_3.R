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
res_var<-get_2_smallest(dfs_var_sans_hier)
v1 <- names(res_var)[[1]]
v2 <- names(res_var)[[2]]

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE

res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)

