

# Vider l'environnement global
# Vider l'environnement global
rm(list = ls())

library(stringr)
source("R/function_passer_3_4.R")

load("data/ca_pizzas_4vars.RData")
source("R/cas_gen_4_3.R")

library("dplyr")

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



##

traiter_un_cluster <- function(cluster,nom){
  res <- tab_multi_manager(
    list_tables = cluster$param_argus$liste_tab,
    list_explanatory_vars = cluster$param_argus$liste_expl_var,
    #hrc = c(ceff = hrc_ceff),
    alt_hrc = cluster$param_argus$liste_alt_hrc,
    alt_totcode = cluster$param_argus$liste_alt_tot,
    #totcode = "30",
    value = "value",
    freq = "nb_obs",
    dir_name = file.path(rep_argus_files,nom),
    secret_var = "is_secret_prim"
  )
}





