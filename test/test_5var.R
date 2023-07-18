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
source("test/test_nbs_tabs.R")
library("rtauargus")
source("R/passer_de_3_a_4_ou_5.R")

loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

totcode<-c(treff="Total",cj="Total",A10="Total",type_distrib="Total",nuts1="Total")

test_5_hrc<-gen_tabs_5_4_to_3(test_5_var,"var",hrcfiles=NULL,totcode,sep_dir = TRUE,hrc_dir = "output",vec_sep =c("\\_"))



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

var_cross<-paste(test_5_hrc$vars[1],test_5_hrc$vars[2],sep="_")
d<- c("treff_cj","A10","type_distrib_nuts1")

n<-length(list_tab2)


list_vars<-replicate(n,d,simplify=FALSE)
names(list_vars)<- c(paste0("var",1:n,sep=""))

#On regarde le secret

tic()

masq2 <- tab_multi_manager(
  list_tables = liste_tabs_exemple2,
  list_explanatory_vars = test_5_hrc$vars ,
  dir_name = "test_avec_rtauargus/test-table5_var/hypercurbe",
  totcode = test_5_hrc$totcode,
  alt_hrc = test_5_hrc$hrcs,
  alt_totcode = test_5_hrc$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim",
  suppress="GH(1,100)")

t<-toc()
# 738 s

res3<-test_5_hrc
res3$tabs<-masq2
res3_5_r_base <- passer_a_4_ou_5_r_base(res3)
# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_5_r_base))


p_compt <- data_fusion %>% 
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
# statut_final n_cell  val_cell pc_n_cell pc_val_cell
# <chr>         <int>     <dbl>     <dbl>       <dbl>
#   1 A              1027  12925118     18.3         2.10
# 2 B               311  19840893      5.54        3.22
# 3 D              3664 472986771     65.3        76.8 
# 4 V               610 109768267     10.9        17.8 
# Je lance avec modular et hypercube  stat de secret different dossier markdown

#3661

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

#######################################

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
source("test/test_nbs_tabs.R")
library("rtauargus")
load("data/ca_pizzas_4vars.RData")
loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)
load("data/test_5_var_1hrc.RData")

hrc_nuts <- rtauargus::write_hrc2(
  corr_nuts,
  "hrc/nuts23.hrc", 
  adjust_unique_roots = TRUE
)


totcode<-c(treff="Total",cj="Total",A10="Total",type_distrib="Total",NUTS="Total")

test_5_1_hrc<-gen_tabs_5_4_to_3(test_5_var_1hrc,"var",hrcfiles=hrc_nuts,totcode,sep_dir = TRUE,hrc_dir = "output",vec_sep =c("\\_"))


list_tab2<-test_5_1_hrc$tabs
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
lapply(liste_tabs_exemple2, function(tab) list(sum(tab$is_secret_prim),sum(tab$is_secret_prim) * 100 / nrow(tab)))

tic()

masq2 <- tab_multi_manager(
  list_tables = liste_tabs_exemple2,
  list_explanatory_vars = test_5_1_hrc$vars ,
  dir_name = "test_avec_rtauargus/test-table5_var/hypercurbe",
  totcode = test_5_1_hrc$totcode,
  alt_hrc = test_5_1_hrc$hrcs,
  alt_totcode = test_5_1_hrc$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim")

t<-toc()


# > masq2 <- tab_multi_manager(
#   +   list_tables = liste_tabs_exemple2,
#   +   list_explanatory_vars = test_5_1_hrc$vars ,
#   +   dir_name = "test_avec_rtauargus/test-table5_var/hypercurbe",
#   +   totcode = test_5_1_hrc$totcode,
#   +   alt_hrc = test_5_1_hrc$hrcs,
#   +   alt_totcode = test_5_1_hrc$alt_tot,
#   +   value = value,
#   +   freq = freq,
#   +   secret_var = "is_secret_prim")
# --- Current table to treat:  var1 ---
#   --- Current table to treat:  var2 ---
#   --- Current table to treat:  var3 ---
#   --- Current table to treat:  var4 ---
#   --- Current table to treat:  var1 ---
#   --- Current table to treat:  var2 ---
#   --- Current table to treat:  var3 ---
#   --- Current table to treat:  var4 ---
#   --- Current table to treat:  var1 ---
#   --- Current table to treat:  var2 ---
#   --- Current table to treat:  var3 ---
#   > 
#   > t<-toc()
# 10571.37 sec elapsed
# edit : it was modular



res3<-test_5_1_hrc
res3$tabs<-masq2
res3_5_r_base <- passer_a_4_ou_5_r_base(res3)
# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_5_r_base))


p_compt <- data_fusion %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      is_secret_11 ~ "D",
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

# # A tibble: 4 × 5
# statut_final n_cell  val_cell pc_n_cell pc_val_cell
# <chr>         <int>     <dbl>     <dbl>       <dbl>
#   1 A              3526  34620584     20.1         3.75
# 2 B              1201  41303216      6.84        4.47
# 3 D              8354 294485122     47.6        31.9 
# 4 V              4468 552913453     25.5        59.9
