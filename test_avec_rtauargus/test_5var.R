library(dplyr)
source("R/function_passer_3_4.R")
source("R/function_passer_3_4.R")
source("R/format.R")
source("R/passage_4_3_cas_0_non_hrc.R")
source("R/passage_4_3_cas_1_non_hrc.R")
source("R/passage_4_3_cas_2_non_hrc.R")
source("R/passage_5_3.R")
    #source("brouillon/tauargus_4_3.R")
library(tictoc)
load("data/test_5_var.RData")
source("R/cas_gen_4_3.R")
library("purrr")
library("dplyr")
source("R/fonction_general_choisit_sep_et_reduit_dim.R")
source("R/choisir_sep.R")

library("rtauargus")

loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

totcode<-c(treff="Total",cj="Total",A10="Total",type_distrib="Total",nuts1="Total")

test_5_hrc<-gen_tabs_5_4_to_3(test_5_var,"var",hrcfiles=NULL,totcode,sep_dir = TRUE,hrc_dir = "output", liste_sep=c("\\_"))



list_tab2<-test_5_hrc$tabs
value<-"pizzas_tot"
max<-"pizzas_max"
freq<-"nb_obs"
liste_tabs_exemple2 <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max  > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs) ,
        pizzas_tot = abs(ceiling(pizzas_tot))
      )})


totcode<-c(A10="Total",treff_cj="Total_Total", type_distrib_nuts1="Total_Total")


#On récupère les varibales des différentes tables

var_cross<-paste(v[1],test_5_hrc$vars[2],sep="_")
d<- c("treff_cj","A10","type_distrib_nuts1")

n<-length(list_tab2)


list_vars<-replicate(n,d,simplify=FALSE)
names(list_vars)<- c(paste0("var",1:n,sep=""))

#On regarde le secret
# A lancer ce soir pour des raisons de temps
tic()

masq2 <- tab_multi_manager(
  list_tables = liste_tabs_exemple2,
  list_explanatory_vars = list_vars ,
  dir_name = "test_avec_rtauargus/test-table5_var",
  totcode = totcode,
  alt_hrc = test_5_hrc$hrcs,
  alt_totcode = test_5_hrc$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim")


t<-toc()
toc()
#3661
tic()
tab_secret<-test_5_var %>% 
  mutate(
    is_secret_freq = nb_obs > 0 & nb_obs < 3,
    is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
  ) %>% 
  mutate(
    is_secret_prim = is_secret_freq | is_secret_dom,
    nb_obs = ceiling(nb_obs) ,
    pizzas_tot = abs(ceiling(pizzas_tot))
  )

tic()
# On ne peut pas faire passer une fonction à 5 dimensions dans rtauargus
masq3<- tab_rtauargus(
  tab_secret,
  files_name = "T5",
  dir_name = "test_avec_rtauargus/test_table5_var/",
  explanatory_vars = c("treff","cj","A10","type_distrib","nuts1"),
  totcode = c(treff="Total",cj="Total",A10="Total",type_distrib ="Total",nuts1="Total"),
  secret_var = "is_secret_prim",
  value = "pizzas_tot",
  freq = "nb_obs",
  verbose =FALSE,
  suppress="GH(1,100)"
  )
# Error in batch file
# Error in file(file, "rt") : cannot open the connection
# In addition: Warning message:
#   In file(file, "rt") :
#   cannot open file 'test_avec_rtauargus/test_table5_var//T5.csv': No such file or directory

t<-toc()


###TEST POUR VOIR LIMITE DE TAILLE SI 0 HIERARCHIE

