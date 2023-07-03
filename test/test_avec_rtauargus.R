

# Vider l'environnement global
# Ne pas faire de controle A + Enter 

####################
#####DONNEES########
####################

rm(list = ls())

library(stringr)
source("R/function_passer_3_4.R")
source("R/format.R")
source("tauargus_4_3.R")
load("data/ca_pizzas_4vars.RData")
source("R/cas_gen_4_3.R")
library("purrr")
library("dplyr")
library("rtauargus")
loc_tauargus <- "C:/Users/ZOW2JK/Downloads/oui/TauArgus4.2.3/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

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

#On a le format utilisé pour rtauargus

list_tabs<-tabs_5_4_to_3(ca_pizzas_4vars,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,"output")

list_tab<-list_tabs$tabs
list_vars<-list(
  T1=c("ACTIVITY","NUTS23","treff_cj"),
  T2=names(list_tab$T2)[sapply(list_tab$T2, is.character)]
)

####################
######TEST##########
####################

# On pose le secret primaire

liste_tabs_exemple <- purrr::map(
  list_tab,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
)})

# On pose le secret secondaire 

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list(T1=c("ACTIVITY","NUTS23","treff_cj"),
                               T2=c("ACTIVITY","NUTS23","treff_cj")) ,
  dir_name = "test_avec_rtauargus",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff_cj="Total_Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  alt_hrc = list_tabs$hrcs,
  alt_totcode = list_tabs$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  
)

str(exemple_masq)

tau<-tauargus_4_3(res,liste_tabs_exemple,totcode,"nb_obs","pizzas_tot")
identical(tau,exemple_masq)

# On essaye de comparer avec le tableau à 4 variables catégorielles

tabs_exemple <- (
  ca_pizzas_4vars %>% 

      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot),
        pizzas_tot= abs(pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      ))

# Ne pas tourner car long 
# Exemple de tableau à 4 variables 

exemple_masq2 <- tab_rtauargus(
  tabs_exemple,
  files_name = "ca_pizzas_4vars" ,
  explanatory_vars = c("ACTIVITY","NUTS23","treff","cj"),
  dir_name = "test_avec_rtauargus",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff="Total",cj="Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  verbose = FALSE
)



