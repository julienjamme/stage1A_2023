# Vider l'environnement global
rm(list = ls())

library(dplyr)
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R")

#####################
######PREMIER EXEMPLE
#####################

# Données ###########

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
v2 <- "ACT"
totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- dirname(hrcfiles[1])

GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1))

#######################################
############TEST#######################
#######################################

res <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)
#On a bien la bonne forme 
str(res)

#On a le bon nombre de tableau
identical(as.integer(test_nb_tabs_3hrc(hrc_files,v2,totcode)),length(res$tabs))

#test plus_petit_hrc
identical("ACT",plus_petit_hrc(hrcfiles,totcode))

identical("SEX",plus_petit_hrc(hrcfiles,totcode))

#Les fichiers hrcs sont stockés dans le bon endroit et nommé
dirname(res$hrcs$nom_data_frame_Total_AGE)

#Les tables ont bien les bonnes modalités et les noms liant tableaux et hrcs sont les bons

c_AGE<-c("Ensemble_Total","adulte_Total","enfant_Total","adulte_A","adulte_B","adulte_C","enfant_A","enfant_B","enfant_C")
identical(sort(unique(res$tabs$nom_data_frame_Total_AGE$AGE_ACT)),sort(c_AGE))
read.table(res$hrcs$nom_data_frame_Total_AGE)


c_ACT<- c("Ensemble_Total","Ensemble_A"  ,   "Ensemble_B" ,   
           "Ensemble_C" , "adulte_A"  , "adulte_B",      
           "adulte_C" , "enfant_A","enfant_B","enfant_C" )
identical(sort(unique(res$tabs$nom_data_frame_Total_ACT$AGE_ACT)),sort(c_ACT))
read.table(res$hrcs$nom_data_frame_Total_ACT)


cb_ACT<-c("Ensemble_B" ,"adulte_B" ,  "adulte_B1" , "adulte_B2",  "enfant_B","enfant_B1" , "enfant_B2" )
cb_AGE<-c( "Ensemble_B" ,"Ensemble_B1" ,"Ensemble_B2", "adulte_B1",
           "adulte_B2" ,  "enfant_B1"  , "enfant_B2" )
identical(sort(unique(res$tabs$nom_data_frame_B_AGE$AGE_ACT)),sort(cb_ACT))
identical(sort(unique(res$tabs$nom_data_frame_B_ACT$AGE_ACT)),sort(cb_AGE))



#############################
#########Données 2###########
#############################

data2 <- expand.grid(
  KEBAB = c("Kebab",read.table("hrc/kebab.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total","F","M"),
  GEO = c("Pays",read.table("hrc/nuts23.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Total",read.table("hrc/age.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

hrc_files = c(AGE = "hrc/age.hrc", GEO = "hrc/nuts23.hrc", KEBAB = "hrc/kebab.hrc" )




# pour execution ligne à ligne
dfs <- data2
nom_dfs <- "kebab"

v1 <- "SEX"
v2 <- "AGE" # On essaye avec une variable aux hasard

totcode<-c(KEBAB="Kebab",SEX="Total",GEO="Pays",AGE="Total")

identical("AGE",plus_petit_hrc(hrc_files,totcode))
identical("KEBAB",plus_petit_hrc(hrc_files,totcode))


hrcfiles <- hrc_files

dir_name <- "output"

#ACT possède 2 niveau

#test data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==

res2 <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)
#On a bien la bonne forme 
str(res2)
#On a le bon nombre de tableau
identical(as.integer(test_nb_tabs_3hrc(hrcfiles,v2,totcode)),length(res2$tabs))
#Les fichiers hrcs sont stockés dans le bon endroit et nommé
dirname(res2$hrcs$kebab_Total_SEX)

#Les tables ont bien les bonnes modalités et les noms liant tableaux et hrcs sont les bons

c_SEX<- c("F_18_25", "F_26_50", "F_Total","F_50_75", "M_18_25","M_50_75", "M_26_50", "M_Total", "Total_Total")
identical(sort(unique(res2$tabs$kebab_Total_SEX$SEX_AGE)),sort(c_SEX))
read.table(res2$hrcs$kebab_Total_SEX)


