# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")

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
identical("ACT",plus_petit_hrc(hrc_files))
totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- dirname(hrcfiles[1])

GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1))
#ACT possède 2 niveau
#Donc il y aura 4 tableaux (2 (cas sans hiérarchie)+ 1(n_niveau1) )

res <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)
str(res$hrcs)
dirname(res$hrcs$nom_data_frame_Total_AGE)

#le fichier est bien ranger dans les hrc 

c_AGE<-c("Ensemble_Total","adulte_Total","enfant_Total","adulte_A","adulte_B","adulte_C","enfant_A","enfant_B","enfant_C")
identical(sort(unique(res$tabs$nom_data_frame_Total_AGE$AGE_ACT)),sort(c_AGE))


c_ACT<- c("Ensemble_Total","Ensemble_A"  ,   "Ensemble_B" ,   
           "Ensemble_C" , "adulte_A"  , "adulte_B",      
           "adulte_C" , "enfant_A","enfant_B","enfant_C" )
identical(sort(unique(res$tabs$nom_data_frame_Total_ACT$AGE_ACT)),sort(c_ACT))

cb_ACT<-c("Ensemble_B" ,"adulte_B" ,  "adulte_B1" , "adulte_B2",  "enfant_B","enfant_B1" , "enfant_B2" )
  
cb_AGE<-c( "Ensemble_B" ,"Ensemble_B1" ,"Ensemble_B2", "adulte_B1"  ,
           "adulte_B2" ,  "enfant_B1"  , "enfant_B2" )
identical(sort(unique(res$tabs$nom_data_frame_B_AGE$AGE_ACT)),sort(cb_ACT))
identical(sort(unique(res$tabs$nom_data_frame_B_ACT$AGE_ACT)),sort(cb_AGE))


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
v2 <- "AGE"#donné grâce à plus petit hrc
identical("AGE",plus_petit_hrc(hrc_files))
totcode<-c(KEBAB="Kebab",SEX="Total",GEO="Pays",AGE="Total")

hrcfiles <- hrc_files

dir_name <- "output"

#ACT possède 2 niveau


res2 <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)
length(res2$tabs)

c_SEX<- c("F_18_25", "F_26_50", "F_Total", "M_18_25", "M_26_50", "M_Total", "Total_Total")
identical(sort(unique(res2$tabs$kebab_Total_SEX$SEX_AGE)),sort(c_SEX))


c_AGE1<-  c("Total_Total", "Total_18_25", "F_18_25", "M_18_25", "Total_26_50", "F_26_50", "M_26_50")
  identical(sort(unique(res2$tabs$kebab_Total_AGE$SEX_AGE)),sort(c_AGE1))

c_SEX1<-c("Total_18_25", "F_18_25", "M_18_25", "F_18", "M_18", "F_19", "M_19")

c_AGE2<-c("F_18", "F_19", "M_18", "M_19", "Total_18", "Total_18_25", "Total_19")

identical(sort(unique(res2$tabs$kebab_18_25_SEX$SEX_AGE)),sort(c_SEX1))
identical(sort(unique(res2$tabs$kebab_18_25_AGE$SEX_AGE)),sort(c_AGE2))

dirname(res2$hrcs$kebab_Total_SEX)
# bein rangé dans les hrc/
str(res$hrcs)
str(res2$hrcs)
#On a le bon format

read.table("hrc/hrc_kebab_Total_SEX.hrc")
#On a le bon fichier hrc

#AGE possède 2 niveau
#Donc il y aura 4 tableaux 


data3 <- expand.grid(
  KEBAB = c("Kebab",read.table("hrc/kebab.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total","F","M"),
  GEO = c("Pays",read.table("hrc/nuts23.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Total",read.table("hrc/age2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

hrc_files = c(AGE = "hrc/age2.hrc", GEO = "hrc/nuts23.hrc", KEBAB = "hrc/kebab.hrc" )


res3 <- passage_4_3_cas_1_non_hr(data3, nom_dfs,v1,v2,totcode,hrc_files,dir_name)

str(res3$hrcs)

#il y a le bon nombre de tableau