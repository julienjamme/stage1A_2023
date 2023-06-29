# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("test/test_tableau.R",encoding="UTF-8")

###############################
##########DONNEES##############
###############################

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:26]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))

hrc_files = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")

tot_code<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

var_sans_hier <- names(tot_code)[1:2]
v1 <- var_sans_hier[1]
v2 <- var_sans_hier[2]

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "output"

###################
######TEST#########
###################

res <- passage_4_3_cas_2_non_hr(data,nom_dfs,v1,v2, tot_code,dir_name)

#On s'attend à une réponse du type 
#list(tabs=list(nom_data_frame_v1,nom_data_frame_v2),
#     hrcs=list(nom_data_frame_v1="dirname/hrc_nom_data_frame_v1.hrc ,
#               nom_data_frame_v2="dirname/hrc_nom_data_frame_v2.hrc )
#     var(c(v1,v2))

str(res)

#On a le bon nombre de tableau
length(res$tabs)

#Les fichiers hrcs sont stockés dans le bon endroit et nommé
dirname(res$hrcs$nom_data_frame_SEX)

#Les tables ont bien les bonnes modalités et les noms liant tableaux et hrcs sont les bons
vecteur <- c("Total_Total", "A_Total", "B_Total", "C_Total", "D_Total", "E_Total",
             "A_U", "B_U", "C_U", "D_U", "E_U",
             "A_V", "B_V", "C_V", "D_V", "E_V",
             "A_W", "B_W", "C_W", "D_W", "E_W",
             "A_X", "B_X", "C_X", "D_X", "E_X",
             "A_Y", "B_Y", "C_Y", "D_Y", "E_Y",
             "A_Z", "B_Z", "C_Z", "D_Z", "E_Z")

identical(sort(unique(res$tabs$nom_data_frame_SEX$SEX_AGE)),sort(vecteur))
read.table(res$hrcs$nom_data_frame_SEX)

#########################################################
t2<-test_tableau(data,v1,v2,res,totcode)



#####################################
#DEUXIEME DATA ######################
#####################################

load("data/ca_pizzas_4vars.RData")
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
hrcfiles1<-c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts)
totcode1<-c(ACTIVITY="TOTAL",NUTS23="Total",treff="Total",cj="Total")
nom_dfs1<-"pizza"

####################
#####TEST###########
####################

res2<-passage_4_3_cas_2_non_hr(ca_pizzas_4vars,nom_dfs1,v1 = "treff",v2="cj",totcode1,dir_name)

str(res2)

#On a le bon nombre de tableau
length(res2$tabs)

#Les fichiers hrcs sont stockés dans le bon endroit et nommé
dirname(res2$hrcs$pizza_treff)

vecteur2 <- c("Total_Total", "tr1_Total", "tr2_Total", "tr3_Total", "tr1_PA", 
              "tr1_LL", "tr2_LL", "tr1_SP", "tr2_PA", "tr2_SP", "tr3_LL", "tr3_SP")


identical(sort(unique(res2$tabs$pizza_treff$treff_cj)),sort(vecteur2))
read.table(res2$hrcs$pizza_treff)

##########################################################
##########################################################
# TEST TABLEAU DE LA BONNE FORME
##########################################################
##########################################################

t<-test_tableau(ca_pizzas_4vars,v1,v2,res2,totcode1)


