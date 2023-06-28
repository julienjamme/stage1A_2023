

# Vider l'environnement global
# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("finaux/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("finaux/cas_gen_4_3.R",encoding = "UTF-8")
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

v1 <- "AGE"
v2 <- "SEX"
totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "output"

GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1))


res <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)
res2 <- passer_de_4_a_3_var(dfs, nom_dfs,totcode,hrcfiles)
res[[1]][2]
length(res[[2]])




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
unique(data)
source("finaux/cas_gen_4_3.R",encoding = "UTF-8")
source("finaux/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
data<-read.csv("fichiers_tests/table_test.csv")
data<- data %>% 
  filter(NUMBER_EMPL !="E1T9")
str(data)
data2
hrc_files=c(ACTIVITY="fichiers_tests/corresp_activity.hrc",GEO="fichiers_tests/corresp_geo.hrc")
read.table(hrc_files[3])
tot_code<-c(ACTIVITY="Total",GEO="Total", NUMBER_EMPL="Total",PERS="Total")
nom_dfs<-"table_test"
dfs <-data
hrcfiles<-hrc_files
totcode<-tot_code
dir_name = "output"

v1<-"PERS"
v2<-"NUMBER_EMPL"
list_data<-passer_de_4_a_3_var(data,nom_dfs,tot_code ,hrc_files)
list_data<-passage_4_3_cas_1_non_hr(data, nom_dfs,v1,v2,tot_code,hrc_files,dir_name)
