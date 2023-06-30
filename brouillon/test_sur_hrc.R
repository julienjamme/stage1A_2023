##############################################################################
# Vérification fichiers hrc
##############################################################################
# Vérification premier fichier hrc

library(tidyverse)

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


