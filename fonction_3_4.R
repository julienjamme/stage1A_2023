



passer_3_4(list_df,list_var,dir_name){
  
  
  
  
  
  
  
  
  
}


###
###
# Découpage des différentes possibilités
###
###


# Trouver toutes les positions de _ dans la chaîne
#library(stringr)

#chaine <- "ma_variable_nom_tres_complexe"
#sous_chaine <- "_"

# Trouver toutes les positions de _ dans la chaîne
#positions <- str_locate_all(chaine, sous_chaine)[[1]][, "start"]

# Générer toutes les possibilités de division
#possibilites <- lapply(positions, function(pos) {
 # partie1 <- str_sub(chaine, end = pos - 1)
 # partie2 <- str_sub(chaine, start = pos + str_length(sous_chaine))
 # list(partie1, partie2)
#})

#possibilites <- Filter(function(x) !is.null(x), possibilites)

#######################################


load("data/ca_pizzas_4vars.RData")
source("finaux/cas_gen_4_3.R")
str(ca_pizzas_4vars)
str(corr_act)
str(corr_nuts)
library("dplyr")
source("finaux/cas_gen_4_3.R")
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
hrcfiles<-c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts)
totcode<-c(ACTIVITY="TOTAL",NUTS23="Total",treff="Total",cj="Total")
nom_dfs<-"pizza"
res<-passer_de_4_a_3_var(ca_pizzas_4vars,nom_dfs,totcode,hrcfiles,sep_dir = TRUE)
library(stringr)

mod1<-unique(ca_pizzas_4vars$treff)
mod2<-unique(ca_pizzas_4vars$cj)
sous_chaine <- "_"
var<-list()
for (chaine in (res$tabs$pizza_treff$treff_cj)){
  # Trouver toutes les positions de _ dans la chaîne
  positions <- str_locate_all(chaine, sous_chaine)[[1]][, "start"]
  var_poss<-lapply(positions, function(pos) {
   partie1 <- str_sub(chaine, end = pos - 1)
    partie2 <- str_sub(chaine, start = pos + str_length(sous_chaine))
    list(partie1, partie2)
  })

 
  var<-append(var,lapply(var_poss, function(x) if (x[[1]] %in% mod1 & x[[2]] %in% mod2){
    return(c(x[[1]],x[[2]]))
  }))
}
res$tabs$pizza_treff$treff<-unlist(lapply(var,function(x) return (x[[1]])),recursive=FALSE)
res$tabs$pizza_treff$cj<-unlist(lapply(var,function(x) return (x[[2]])),recursive=FALSE)


# elem = un élément de possibilité
# fonction qui regarde si elem[1] est une possibilité de la var 1
# et elem[2] une possibilité de var 2

# Requirement : la fonction au dessus ne renvoie TRU que pour un seul élément
# ie : les variables n'ont pas des noms bizarres


# to do : dans le code_split, faire un intersect avec les modalités présentes