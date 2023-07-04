

# Vider l'environnement global
# Ne pas faire de controle A + Enter 

####################
#####DONNEES########
####################



source("R/function_passer_3_4.R")
source("R/format.R")
#source("brouillon/tauargus_4_3.R")
load("data/ca_pizzas_4vars.RData")
source("R/cas_gen_4_3.R")
library("purrr")
library("dplyr")
library("rtauargus")
loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
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

list_res<-tabs_5_4_to_3(ca_pizzas_4vars,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,"output")

list_tab<-list_res$tabs
list_vars<-list(
  T1=c("ACTIVITY","NUTS23","treff_cj"),
  T2=names(list_tab$T2)[sapply(list_tab$T2, is.character)]
)

####################
######TEST##########  LISTE DE TABLEAUX A 3 DIMENSION
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
  dir_name = "test_avec_rtauargus/hierarchie_2/3/mod",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff_cj="Total_Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  alt_hrc = list_res$hrcs,
  alt_totcode = list_res$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  
)

str(exemple_masq)

value<-"pizzas_tot"
freq<-"nb_obs"

#tau<-tau_argus_4_3(list_res,liste_tabs_exemple,totcode,freq,value,hrcfiles)
#identical(tau,exemple_masq)

res_4_3<-list(
  tabs=exemple_masq,
  hrcs=res$hrcs,
  vars=res$vars
)

p<-passer_3_41(res_4_3,ca_pizzas_4vars)
# A travailler
ca_pizzas_4vars$nb_obs <- ceiling(ca_pizzas_4vars$nb_obs)
p_sans_argus <- p[!sapply(p, is.logical)]

setdiff(p_sans_argus,ca_pizzas_4vars)


p_compt <- p %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      is_secret_4 ~ "D",
      TRUE ~ "V"
    )
  )

#nombre enlevé 
p_compt %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )

# A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A              2147  21701410.     19.8         3.13
# 2 B               639  29207577.      5.89        4.22
# 3 D              3921 142310503.     36.1        20.6 
# 4 V              4147 499212329.     38.2        72.1 
#  

###################################################################
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
  dir_name = "test_avec_rtauargus/hierarchie_2/4/mod",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff="Total",cj="Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  verbose = FALSE
)

T4_masq <- exemple_masq2 %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      TRUE ~ Status,
    )
  )

T4_masq %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )

# Modular 3h...
# # A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A              2147  21720282.     19.8         3.14
# 2 B               639  29244675.      5.89        4.22
# 3 D              4264 171641619.     39.3        24.8 
# 4 V              3804 469881214.     35.0        67.9 




## Hypercube


exemple_masq3 <- tab_rtauargus(
  tabs_exemple,
  files_name = "ca_pizzas_4vars" ,
  explanatory_vars = c("ACTIVITY","NUTS23","treff","cj"),
  dir_name = "test_avec_rtauargus/hierarchie_2/4/hypercube",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff="Total",cj="Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  verbose = FALSE,
  suppress = "GH(1,100)"
  
)


# On a 4222 lignes supprimés

#######################################
############DONNEES2###################
#######################################


data <- read.csv("data/table_test.csv")


hrc_files = c(ACTIVITY = "hrc/corresp_activity.hrc", NUMBER_EMPL = "hrc/hrc_nb_empl.hrc",GEO="hrc/corresp_geo.hrc")


totcode<-c(ACTIVITY="Total",NUMBER_EMPL="Total", GEO="Total",PERS="Total")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

hrcfiles <- hrc_files
passer_de_4_a_3_var(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir = FALSE, hrc_dir = "hrc_alt")
list_res2<-tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir,hrc_dir)

list_tab2<-list_res2$tabs


