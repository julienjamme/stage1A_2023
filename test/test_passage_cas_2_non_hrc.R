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
list_tab <- res$tabs
str(list_tab)
list_hrc <- res$hrcs
str(list_hrc)
var_fuse <- res$vars

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

res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
sdcHierarchies::hier_convert(as = "sdc")


res_split <- lapply(res_sdc$dims,names)


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