# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("finaux/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("test/test_tableau.R", encoding = "UTF-8")
data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", 
              SEX = "hrc/hrc3.hrc", AGE = "hrc/exemple_1.hrc")

tot_code<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

v1 <- "ACT"
v2 <- "GEO"
totcode <- tot_code
hrcfiles <- hrc_files

# pour supprimer les .hrc facilement
dir_name <- "output"

res <- passage_4_3_cas_0_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)
str(res)
#on a le bon format
length(res$tabs)
str(res2$vars)
#hrc2 =4 + hrc3=8 =12

data2 <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/age.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

v2<-"AGE"
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/exemple_1.hrc", 
              SEX = "hrc/hrc3.hrc", AGE = "hrc/age.hrc")
res2 <- passage_4_3_cas_0_non_hr(data2, nom_dfs,v1,v2,totcode,hrc_files,dir_name)

str(res2$vars)

length(res2$tabs)
# hrc2 =4 et age =12 vu avant 