

##############
# Donnees####
##############
library(stringr)
source("R/function_passer_3_4.R")

load("data/ca_pizzas_4vars.RData")
source("R/cas_gen_4_3.R")

library("dplyr")

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

#######################################
############TEST#######################
#######################################

l<-list()
source("R/function_passer_3_4.R")

l<-passer_3_41(res,ca_pizzas_4vars)
#On a le bon format
str(l)

#On a bien toutes les collonnes 

identical(sort(l$treff),sort(ca_pizzas_4vars$treff))

identical(sort(l$cj),sort(ca_pizzas_4vars$cj))


#####################
######DONNEES2#######
#####################

source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")


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
v2 <- "ACT"#donné grâce à plus petit hrc

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- dirname(hrcfiles[1])

GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1))

res2 <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)

########################
########TEST###########
########################

l2<-list()

l2<-passer_3_41(res2,dfs)
#On a le bon format
str(l2)

#On a bien toutes les collonnes

identical(sort(l2$AGE),sort(data$AGE))

identical(sort(l2$ACT),sort(data$ACT))
