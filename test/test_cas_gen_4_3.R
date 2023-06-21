# Vider l'environnement global
rm(list = ls())



library(dplyr)
source("finaux/cas_gen_4_3.R",encoding = "UTF-8")



data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[7:12]),
  AGE = c("Total",LETTERS[15:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")

tot_code<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

# Test pour 2 variables non hier
res <- passer_de_4_a_3_var(data,"mon_data_frame",tot_code, hrc_files)

res2 <- passage_4_3_cas_2_non_hr(data,"mon_data_frame","SEX","AGE", tot_code,"hrc")
# > identical(res,res2)
# [1] TRUE
