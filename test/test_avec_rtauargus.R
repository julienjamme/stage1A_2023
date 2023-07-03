

# Vider l'environnement global
# Ne pas faire de controle A + Enter 

####################
#####DONNEES########
####################


library(stringr)
source("R/function_passer_3_4.R")
source("R/format.R")
source("R/tauargus_4_3.R")
load("data/ca_pizzas_4vars.RData")
source("R/cas_gen_4_3.R")
library("purrr")
library("dplyr")
library("rtauargus")
source("R/function_passer_3_4.R")
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

list_res<-tabs_5_4_to_3(ca_pizzas_4vars,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,"output")

list_tab<-list_res$tabs
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
  alt_hrc = list_res$hrcs,
  alt_totcode = list_res$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  
)

str(exemple_masq)

tot_code<-c(ACTIVITY="Total",NUTS23="Total",treff_cj="Total_Total")
value<-"pizzas_tot"
freq<-"nb_obs"

tau<-tau_argus_4_3(list_res,liste_tabs_exemple,tot_code,freq,value,hrcfiles)
identical(tau,exemple_masq)

res_4_3<-list(
  tabs=exemple_masq,
  hrcs=res$hrcs,
  vars=res$vars
)

p<-passer_3_41(res_4_3,ca_pizzas_4vars)

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
# 3 D              3890 141724101.     35.8        20.5 
# 4 V              4178 499798732.     38.5        72.2

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

comptage_ex<-exemple_masq2%>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      TRUE ~ Status,
    )
  )

comptage_ex %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) 
# On a 4222 lignes supprimés

#######################################
############DONNEES2###################
#######################################


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
v2<-plus_petit_hrc(hrc_files,tot_code)



dir_name <- dirname(hrcfiles[1])



list_res2<-tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir=FALSE,hrc_dir="hrc_alt")


