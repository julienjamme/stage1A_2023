# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("finaux/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")

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
str(res)
#On s'attend à une réponse du type 
#list(tabs=list(nom_data_frame_v1,nom_data_frame_v2),
#     hrcs=list(nom_data_frame_v1="dirname/hrc_nom_data_frame_v1.hrc ,
#               nom_data_frame_v2="dirname/hrc_nom_data_frame_v2.hrc )
#     var(c(v1,v2))


list_tab <- res$tabs
str(list_tab)
list_hrc <- res$hrcs
str(list_hrc)
var_fuse <- res$vars
test_var<- identical(var_fuse,c(v1,v2)) #var est bon 
 
n_mod_v1 <- length(unique(data[[v1]]))
n_mod_hors_tot_v1 <- n_mod_v1 - 1
n_mod_v2 <- length(unique(data[[v2]]))
n_mod_hors_tot_v2 <- n_mod_v2 - 1


##############################################################################
# Vérification fichiers hrc
##############################################################################
# Vérification premier fichier hrc
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
table_V1<-as.data.frame(res_dt$name[res_dt$name!=total])
identical()

var1_total <- totcode[v1]
var2_total <- totcode[v2]

#les différentes modalités des 2 variables
mods1 <- unique(data[[v1]])
mods2 <- unique(data[[v2]])

var1_mods_hors_tot <- mods1[mods1 != var1_total]
var2_mods_hors_tot <- mods2[mods2 != var2_total]

# nombre de modalité pour chaque var
var1_mods_n <- length(var1_mods_hors_tot)
var2_mods_n <- length(var2_mods_hors_tot)

#  que l'on veut 
tab1_niv1 <- expand.grid(
  v1 = sort(rep(var1_mods_hors_tot, var2_mods_n)),
  v2 = var2_total,
  stringsAsFactors = FALSE
) %>% as.data.frame()

tab1_niv1$v3 <- paste(tab1_niv1$v1, tab1_niv1$v2, sep = "_")

tab1_niv2 <- expand.grid(
  v1 = var1_mods_hors_tot,
  v2 = var2_mods_hors_tot,
  stringsAsFactors = FALSE
) %>% as.data.frame()

tab1_niv2 <- tab1_niv2[order(tab1_niv2$v1, tab1_niv2$v2), ]
tab1_niv2$v3 <- paste(tab1_niv2$v1, tab1_niv2$v2, sep = "_")

#La table de correspondance
tab1_corresp <- data.frame(
  Niv1 = tab1_niv1$v3,
  Niv2 = tab1_niv2$v3,
  stringsAsFactors = FALSE
)

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
##########################################################


library(tidyverse)

#On veut montrer paint 

#On créer un tableau contenant toutes les données de data et la variable V1_V2
tab<- data %>% 
  select(1:2)
tab$v3 <- paste(data[[v1]], data[[v2]], sep = "_")

# On filtre 
data_voulu<-tab %>% 
  filter(data[[v2]] == var2_total | (data[[v1]] !=var1_total & data[[v2]] != var2_total))

#On range les deux tableaux pour pouvoir voir avec arrange si ce sont les mêmes

data_voulu<-data_voulu %>% arrange(across(where(is.character)))
res$tabs$nom_data_frame_SEX<-res$tabs$nom_data_frame_SEX %>% arrange(across(where(is.character)))

#Les mêmes modalités

identical(unique(data_voulu$v3),
unique(res$tabs$nom_data_frame_SEX$SEX_AGE))

#On a le on tableau 

identical(res$tabs$nom_data_frame_SEX$SEX_AGE,data_voulu$v3)




#########################################################
data_split <- unique(rbind(res$tabs$nom_data_frame_SEX$SEX_AGE,res$tabs$nom_data_frame_AGE))

# Recréation des variables SEX et AGE.
# Ceci ne marche uniquement parce qu'il n'y a pas de "_" 
# dans les modalités de SEX et AGE
data_fuse <- data_split %>%
  separate(SEX_AGE, into = c("SEX", "AGE"), sep = "_")


data_sort <- data[order(data$ACT, data$GEO, data$SEX, data$AGE),]
data_sort <- data_sort %>% select(order(colnames(data_sort)))

data_fuse_sort <- data_fuse[order(data_fuse$ACT, data_fuse$GEO, data_fuse$SEX, data_fuse$AGE),]
data_fuse_sort <- data_fuse_sort %>% select(order(colnames(data_fuse_sort)))

identical(data_sort,data_fuse_sort)

# On vérifie que les colonnes sont bien identiques
# identical(data_sort, data_fuse_sort) ne fonctionne pas... A demander pourquoi
identical(data_sort$ACT, data_fuse_sort$ACT)
identical(data_sort$AGE, data_fuse_sort$AGE)
identical(data_sort$GEO, data_fuse_sort$GEO)
identical(data_sort$SEX, data_fuse_sort$SEX)
identical(data_sort$VALUE, data_fuse_sort$VALUE)