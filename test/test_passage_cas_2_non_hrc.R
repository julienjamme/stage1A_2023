# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("finaux/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("test/test_tableau.R",encoding="UTF-8")
data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:26]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))

hrc_files = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")

tot_code<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

var_sans_hier <- names(tot_code)[1:2]
v1 <- var_sans_hier[1]
v2 <- var_sans_hier[2]

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "output"


res <- passage_4_3_cas_2_non_hr(data,nom_dfs,v1,v2, tot_code,dir_name)
unique(res$tabs$nom_data_frame_SEX)
#On s'attend à une réponse du type 
#list(tabs=list(nom_data_frame_v1,nom_data_frame_v2),
#     hrcs=list(nom_data_frame_v1="dirname/hrc_nom_data_frame_v1.hrc ,
#               nom_data_frame_v2="dirname/hrc_nom_data_frame_v2.hrc )
#     var(c(v1,v2))

library(tidyverse)

#On veut montrer paint 
res$tabs$
#On créer un tableau contenant toutes les données de data et la variable V1_V2
tab<- data %>% 
  select(1:2)
tab$v3 <- paste(data[[v1]], data[[v2]], sep = "_")

# On filtre 
data_voulu1<-tab %>% 
  filter(data[[v2]] == var2_total | (data[[v1]] !=var1_total & data[[v2]] != var2_total))
data_voulu2<-tab %>% 
  filter(data[[v1]] == var1_total | (data[[v1]] !=var1_total & data[[v2]] != var2_total))
#On range les deux tableaux pour pouvoir voir avec arrange si ce sont les mêmes

data_voulu1<-data_voulu1 %>% arrange(across(where(is.character)))
data_voulu2<-data_voulu2 %>% arrange(across(where(is.character)))

res$tabs$nom_data_frame_SEX<-res$tabs$nom_data_frame_SEX %>% arrange(across(where(is.character)))
res$tabs$nom_data_frame_AGE<-res$tabs$nom_data_frame_AGE %>% arrange(across(where(is.character)))
#Les mêmes modalités

identical(unique(data_voulu1$v3),
          unique(res$tabs$nom_data_frame_SEX$SEX_AGE))

identical(unique(data_voulu2$v3),
          unique(res$tabs$nom_data_frame_AGE$SEX_AGE))
#On a le on tableau 

identical(res$tabs$nom_data_frame_SEX$SEX_AGE,data_voulu1$v3)
identical(res$tabs$nom_data_frame_AGE$SEX_AGE,data_voulu2$v3)

test_var<- identical(var_fuse,c(v1,v2)) #var est bon 
res$tabs[["nom_data_frame_SEX"]][["SEX_AGE"]]
#########################################################

t<-test_tableau(ca_pizzas_4vars,v1,v2,res2,totcode1)
t2<-test_tableau(data,v1,v2,res,totcode )

#########################################################
#########################################################
#########################################################
###############################################
##############################################
#DEUXIEME DATA ###############################
load("data/ca_pizzas_4vars.RData")
hrc_activity <- rtauargus::write_hrc2(
  corr_act, 
  "hrc/activity_2_niv.hrc", 
  adjust_unique_roots = TRUE
)
hrc_nuts <- rtauargus::write_hrc2(
  corr_nuts,
  "hrc/nuts23.hrc", 
  adjust_unique_roots = TRUE
)
hrcfiles1<-c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts)
totcode1<-c(ACTIVITY="TOTAL",NUTS23="Total",treff="Total",cj="Total")
nom_dfs1<-"pizza"
res2<-passage_4_3_cas_2_non_hr(ca_pizzas_4vars,nom_dfs1,v1 = "treff",v2="cj",totcode1,dir_name)



##############################################################################
# Vérification fichiers hrc
##############################################################################
# Vérification premier fichier hrc

list_tab <- res$tabs
str(list_tab)
list_hrc <- res$hrcs
str(list_hrc)
var_fuse <- res$vars

# Test sur les chemins des fichiers hrc 
chemin <- getwd()
n_mod_v1 <- length(unique(data[[v1]]))
n_mod_hors_tot_v1 <- n_mod_v1 - 1
n_mod_v2 <- length(unique(data[[v2]]))
n_mod_hors_tot_v2 <- n_mod_v2 - 1
res2$tabs$
hrc <- list_hrc[[1]]

total <- "Total_Total"

list_test <- list()
read.table(hrc)
res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
sdcHierarchies::hier_convert(as = "sdc")


res_split <- lapply(res_sdc$dims,names)
read.table(list_hrc$nom_data_frame_SEX)

res_dt <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
  sdcHierarchies::hier_convert(as = "dt")

# Un seul grand total
list_test$"test_1" <-nrow(res_dt %>% filter(level == "@")) == 1

# Bon nombre de sous totaux
list_test$"test_2" <- nrow(res_dt %>% filter(level == "@@")) == n_mod_hors_tot_v1

# Bon nombre de élémentaire (non sous totaux)
list_test$"test_3" <- nrow(res_dt %>% filter(level == "@@@")) == n_mod_hors_tot_v1 * n_mod_hors_tot_v2

# Vérification du nombre de branche attaché à chaque noeux
# ie nombre de sous totaux pour chaque total
is_OK = TRUE
for (i in seq_along(res_split)){
  sous_tot <- res_split[[i]][[1]]
  
  # profondeur qui par construction (var non hier) est entre 1 et 3
  # dans l'ensemble des modalités, 
  # donc pour le "sous total" est entre 1 et 3-1 = 2
  deep = str_count(res_dt[name == sous_tot] %>% select(level),"@")
  #print(deep)
  if (deep == 1 & length(res_split[[i]]) != n_mod_v1){
    is_OK == FALSE
  } else if(deep == 2 & length(res_split[[i]]) != n_mod_v2){
    is_OK = FALSE
  }
}
list_test$"test_4" <- is_OK == TRUE

list_test_1 <- list_test
all(list_test_1)

###################################################################

# Vérification second fichier hrc
hrc <- list_hrc[[2]]

total <- "Total_Total"

list_test <- list()

res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
  sdcHierarchies::hier_convert(as = "sdc")


res_split <- lapply(res_sdc$dims,names)


res_dt <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
  sdcHierarchies::hier_convert(as = "dt")

# Un seul grand total
list_test$"test_1" <- nrow(res_dt %>% filter(level == "@")) == 1

# Bon nombre de sous totaux
list_test$"test_2" <- nrow(res_dt %>% filter(level == "@@")) == n_mod_hors_tot_v2

# Bon nombre de élémentaire (non sous totaux)
list_test$"test_3" <- nrow(res_dt %>% filter(level == "@@@")) == n_mod_hors_tot_v1 * n_mod_hors_tot_v2

# Vérification du nombre de branche attaché à chaque noeux
# ie nombre de sous totaux pour chaque total
is_OK = TRUE
for (i in seq_along(res_split)){
  sous_tot <- res_split[[i]][[1]]
  
  # profondeur qui par construction (var non hier) est entre 1 et 3
  # dans l'ensemble des modalités, 
  # donc le "sous total" est entre 1 et 3-1 = 2
  deep = str_count(res_dt[name == sous_tot] %>% select(level),"@")
  
  if (deep == 1 & length(res_split[[i]]) != n_mod_v2){
    is_OK == FALSE
  } else if(deep == 2 & length(res_split[[i]]) != n_mod_v1){
    is_OK = FALSE
  }
}
list_test$"test_4" <- is_OK == TRUE

list_test_2 <- list_test
all(list_test_2)




##########################################################
##########################################################
# TEST TABLEAU DE LA BONNE FORME
##########################################################
##########################################################




