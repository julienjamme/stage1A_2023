
####################
######TEST##########
####################
library(dplyr)
source("R/function_passer_3_4.R")
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
# On pose le secret primaire

data <- read.csv("data/table_test.csv")


hrc_files = c(ACTIVITY = "hrc/corresp_activity.hrc", NUMBER_EMPL = "hrc/hrc_nb_empl.hrc",GEO="hrc/corresp_geo.hrc")


totcode<-c(ACTIVITY="Total",NUMBER_EMPL="Total", GEO="Total",PERS="Total")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir<-TRUE
hrcfiles <- hrc_files
passer_de_4_a_3_var(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir = FALSE, hrc_dir = "hrc_alt")
list_res2<-tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir,hrc_dir= "hrc_alt")

list_tab2<-list_res2$tabs

liste_tabs_exemple2 <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (value != 0) & (max > 0.85*value)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

freq<-"nb_obs"
value<-"value"

totcode<-c(ACTIVITY="Total",PERS_NUMBER_EMPL="Total_Total", GEO="Total")


#On récupère les varibales des différentes tables

var_cross<-paste(list_res2$vars[1],list_res2$vars[2],sep="_")
d<- intersect(names(list_res2$tabs$nom_data_frame1), names(totcode))

n<-length(list_tab2)
list_vars<-replicate(n,d,simplify=FALSE)
names(list_vars)<- c(paste0(nom_dfs,1:n,sep=""))


#On regarde le secret

#FOnctionne
masq2 <- tab_multi_manager(
  list_tables = liste_tabs_exemple2,
  list_explanatory_vars = list_vars  ,
  dir_name = "test_avec_rtauargus/hierarchie_3/3/mod",
  totcode = totcode,
  hrc = hrcfiles[!(names(hrcfiles) %in% list_res2$vars)],
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim",
  suppress="GH(1,100)")


res_4_3<-list(
  tabs=masq,
  hrcs=list_res2$hrcs,
  vars=list_res2$vars
)

p<-passer_3_41(res_4_3,data)
# A travailler
data$nb_obs <- ceiling(data$nb_obs)
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


res_list<-list(
  tabs=masq,
  hrcs=liste_res2$hrcs,
  vars=liste_res2$vars
)


hrc_files = c(ACTIVITY = "hrc/corresp_activity.hrc", NUMBER_EMPL = "hrc/hrc_nb_empl.hrc",GEO="hrc/corresp_geo.hrc")


totcode<-c(ACTIVITY="Total",NUMBER_EMPL="Total", GEO="Total",PERS="Total")

# pour execution ligne à ligne
nom_dfs<-"tau_argus_natest"
v1<-"PERS"
v2<-"GEO"
dir_name<-"output"
res<-passage_4_3_cas_1_non_hr(dfs,nom_dfs,v1,v2,totcode ,hrcfiles,dir_name,sep="_")

list_res2<-format(res,nom_dfs)

list_tab2<-list_res2$tabs

liste_tabs_exemple2 <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (value != 0) & (max > 0.85*value)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

freq<-"nb_obs"
value<-"value"

totcode<-c(NUMBER_EMPL="Total",PERS_GEO="Total_Total", ACTIVITY="Total")


#On récupère les varibales des différentes tables

var_cross<-paste(list_res2$vars[1],list_res2$vars[2],sep="_")
d<- intersect(names(list_res2$tabs$tau_argus_natest1), names(totcode))

n<-length(list_tab2)
list_vars<-replicate(n,d,simplify=FALSE)
names(list_vars)<- c(paste0(nom_dfs,1:n,sep=""))


masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple2,
  list_explanatory_vars = list_vars  ,
  dir_name = "test_avec_rtauargus/hierarchie_3/3/mod",
  totcode = totcode,
  hrc = hrcfiles[!(names(hrcfiles) %in% list_res2$vars)],
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim")
