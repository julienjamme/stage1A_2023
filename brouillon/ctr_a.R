
load("data/ca_test_3_hrc.RData")

source("R/function_passer_3_4.R")
source("R/format.R")
#source("brouillon/tauargus_4_3.R")
load("data/ca_pizzas_4vars.RData")
source("R/cas_gen_4_3.R")
library("purrr")
library("dplyr")
library("rtauargus")
source("test/test_nbs_tabs.R")
loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)
source("R/passage_4_3_cas_1_non_hrc.R")
source("R/passage_4_3_cas_2_non_hrc.R")
source("R/choisir_sep.R")
source("R/cas_gen_4_3.R")
source("R/passage_4_3_cas_0_non_hrc.R")


hrc_nuts <- rtauargus::write_hrc2(
  corr_nuts,
  "hrc/nuts23.hrc", 
  adjust_unique_roots = TRUE
)

totcode<-c(cj="Total",NUTS="Total", PAYS="Total",treff="Total")



niv1_mod <- c("FR", "GB", "NL", "BE", "EC", "LU", "IT", "CH",
              "MX", "DE", "IE", "QA", "ES", "AU", "US", "CA",
              "PT", "BS", "MA", "NO", "LB", "MU", "AT", "KW",
              "SE", "JP", "DK", "TR", "FI", "RU", "SG", "CN",
              "AE", "GH", "HK", "IN", "TN", "TW", "KR", "GG",
              "RO", "SI", "RS", "CY", "IL", "JE", "PL", "PA",
              "MT", "TH", "EE", "CO", "SA", "SK", "GR", "BH",
              "CW", "SM", "LT", "HU", "VG", "ZA", "CZ")


corr_pays<-data.frame(
  pays= c(rep("FR",100),niv1_mod),
  dep= c(c("75", "92", "40", "51", "64", "78", "57", "973", "94", "86", "74", "67", "88",
           "77", "34", "83", "24", "13", "47", "44","22", "11", "36", "23", "39", "81",
           "63", "21", "32", "46", "04", "84", "38", "68", "30", "62", "14", "60", "19", "48",
           "06", "971", "35", "12", "16", "42", "05", "2A", "09", "56", "972", "89", "28", 
           "15", "41", "01", "55", "37", "80", "69","73", "58", "02", "29", "65", "66", "87", 
           "85", "95", "27", "974", "07", "52", "25", "03", "71", "93", "2B", "26", "72",
           "70", "17", "59", "54", "49", "18", "79", "50", "33", "08", "43", "61", "31", 
           "90", "76", "82", "10", "45", "53", "91", "976",niv1_mod[niv1_mod != "FR"])
         
  )
)


hrc_pays <- rtauargus::write_hrc2(
  corr_pays,
  "hrc/pays_test.hrc", 
  adjust_unique_roots = TRUE
)

dfs<-test_3hrc
nom_dfs <- "table_test_3_hrc"
sep_dir<-FALSE

hrc__files<-c(NUTS=hrc_nuts,PAYS=hrc_pays)

list_res2<-tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrc__files ,sep_dir)


source("R/fonction_general_choisit_sep_et_reduit_dim.R")
library(stringr)
source("test/test_nbs_tabs.R")

gen<-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode,hrc__files,sep_dir = TRUE,
                       hrc_dir = "output",select_hier = TRUE)

list_res2<-gen
list_tab2<-list_res2$tabs



####################
######TEST##########  LISTE DE TABLEAUX A 3 DIMENSION
####################

# On pose le secret primaire

liste_tabs_exemple <- purrr::map(
  list_tab2,
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

# On pose le secret avec un grand tableau avec une hiérarchie et
# et on observe que la taille des tableuax est un problème 

# exemple_masq <- tab_multi_manager(
#   list_tables = liste_tabs_exemple,
#   list_explanatory_vars = list_vars ,
#   dir_name = "test_avec_rtauargus/hierarchie_2/1/mod",
#   totcode = c(ACTIVITY="Total",ident="Total",cj_pays="Total_Total"),
#   hrc = c(ACTIVITY=hrc_activity),
#   alt_hrc = list_res2$hrcs,
#   alt_totcode = list_res2$alt_tot,
#   value = "pizzas_tot",
#   maxscore = "pizzas_max",
#   freq = "nb_obs",
#   secret_var = "is_secret_prim",
#   
# )

list_tab2<-list_res2$tabs

freq<-"nb_obs"
value<-"pizzas_tot"

totcode<-c(cj="Total",`NUTS+++PAYS`="Total_Total", treff="Total")


#On récupère les varibales des différentes tables

var_cross<-paste(list_res2$vars[1],list_res2$vars[2],sep="_")
d<- intersect(names(list_res2$tabs$table_test_3_hrc1), names(totcode))

n<-length(list_tab2)
list_vars<-replicate(n,d,simplify=FALSE)
names(list_vars)<- c(paste0(nom_dfs,1:n,sep=""))


masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_vars ,
  dir_name = "test_avec_rtauargus/hierarchie_3/mod",
  totcode = totcode,
  hrc = hrc__files[!(names(hrc__files) %in% list_res2$vars)],
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim")