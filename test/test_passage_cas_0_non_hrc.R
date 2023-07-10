# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R", encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R", encoding = "UTF-8")
source("R/cas_gen_4_3.R", encoding = "UTF-8")

# donnée 1 : première table ----------------------------------------------------------------

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrcfiles = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", 
             SEX = "hrc/hrc3.hrc", AGE = "hrc/exemple_1.hrc")


totcode<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

v1 <- "ACT"
v2 <- "GEO"

# pour supprimer les .hrc facilement
dir_name <- "output"


# test 1 ------------------------------------------------------------------

res <- passage_4_3_cas_0_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)

#on a le bon format
str(res$alt_tot)

#On a le bon nombre de tableau
length(res$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v=v1) * nb_noeuds(hrcfiles = hrcfiles, v=v2)

#Les fichiers hrcs sont stockés dans le bon endroit et nommé
dirname(res$hrcs$nom_data_frame_Total_Pays_ACT) == dir_name

#Les tables ont bien les bonnes modalités et les noms liant tableaux et hrcs sont les bons

c_ACT<- c("Total_Pays", "A_Pays", "B_Pays", "C_Pays", "A_Reg", "B_Reg", "C_Reg", "A_Reg_", "B_Reg_", "C_Reg_")


identical(sort(unique(res$tabs$nom_data_frame_Total_Pays_ACT$ACT_GEO)),sort(c_ACT))
read.table(res$hrcs$nom_data_frame_Total_Pays_ACT)


# donnée 2 : seconde table ----------------------------------------------------------------

data2 <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/age.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

v1<-"ACT"
v2<-"AGE"
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/exemple_1.hrc", 
              SEX = "hrc/hrc3.hrc", AGE = "hrc/age.hrc")

tot_code<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")
nom_dfs <- "nom_data_frame"
dir_name <- "output"


res2 <- passage_4_3_cas_0_non_hr(data2, nom_dfs,v1,v2,tot_code,hrc_files,dir_name)

#on a le bon format
str(res2)

#On a le bon nombre de tableau
length(res2$tabs) == 2 * nb_noeuds(hrcfiles = hrc_files, v=v1) * nb_noeuds(hrcfiles = hrc_files, v=v2)

#Les tables ont bien les bonnes modalités et les noms liant tableaux et hrcs sont les bons

c_AGE<- c( "A_18_25","Total_18_25" , "B_18_25", "C_18_25", "A_18", "B_18", "C_18", "A_19", "B_19", "C_19")

identical(sort(unique(res2$tabs$nom_data_frame_Total_18_25_ACT$ACT_AGE)),sort(c_AGE))
read.table(res2$hrcs$nom_data_frame_Total_18_25_ACT)


# donnée 3 ------------------------------------------------------------------

data2 <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/age.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

v1<-"ACT"
v2<-"AGE"
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/exemple_1.hrc", 
              SEX = "hrc/hrc3.hrc", AGE = "hrc/age.hrc")

tot_code<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")
nom_dfs <- "nom_data_frame"
dir_name <- "output"

# test 3 : test séparateur ------------------------------------------------------------------

res3 <- passage_4_3_cas_0_non_hr(data2, nom_dfs,v1,v2,tot_code,hrc_files,dir_name, sep = "+++")


# Les 5 premières lignes ont bien le séparateur
all(unlist(lapply(1:5, function(i) str_detect(data2[i,][[1]], "\\+++")))) == FALSE

all(unlist(lapply(1:5, function(i) str_detect(res3$tabs$nom_data_frame_Total_LETTRE_ACT$`ACT+++AGE`[i], "\\+++"))))

# il y a bien une colone avec le séparateur
any(str_detect(names(data2), "\\+++")) == FALSE
any(str_detect(names(res3$tabs$nom_data_frame_Total_LETTRE_ACT), "\\+++"))
