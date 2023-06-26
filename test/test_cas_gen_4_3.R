# Vider l'environnement global
rm(list = ls())



library(dplyr)
source("finaux/cas_gen_4_3.R",encoding = "UTF-8")

##############################################################
##############################################################
####################### Cas 2 var hrc ########################
##############################################################
##############################################################

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[7:12]),
  AGE = c("Total",LETTERS[15:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")

tot_code<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

##################################
########## Vérification ##########
##################################
res <- passer_de_4_a_3_var(data,"mon_data_frame",tot_code, hrc_files,sep_dir = TRUE, hrc_dir = "hrc_alt" )
res2 <- passage_4_3_cas_2_non_hr(data,"mon_data_frame","SEX","AGE", tot_code, dir_name = "hrc_alt")

identical(res,res2)
# [1] TRUE

##############################################################
##############################################################
####################### Cas 1 var hrc ########################
##############################################################
##############################################################

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc" )

tot_code<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

var_cat <- names(totcode)

var_sans_hier <- intersect(
  setdiff(names(dfs), names(hrcfiles)),
  var_cat
)

v1 <- var_sans_hier[1]
v2<-plus_petit_hrc(hrc_files)



dir_name <- dirname(hrcfiles[1])

##################################
########## Vérification ##########
##################################
res <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles)
res2 <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)

identical(res,res2)

############################################################


source("finaux/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", 
              SEX = "hrc/hrc3.hrc", AGE = "hrc/exemple_1.hrc")

tot_code<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

v1 <- "ACT"
v2 <- "GEO"
totcode <- tot_code
hrcfiles <- hrc_files

# pour supprimer les .hrc facilement
dir_name <- "hrc_alt"

res <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE)
res2 <- passage_4_3_cas_0_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)

identical(res,res2)
