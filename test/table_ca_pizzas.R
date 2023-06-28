load("data/ca_pizzas_4vars.RData")
source("finaux/cas_gen_4_3.R")
str(ca_pizzas_4vars)
str(corr_act)
str(corr_nuts)
library("dplyr")
source("finaux/cas_gen_4_3.R")
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

str(res)
###
library(tidyverse)

data <- ca_pizzas_4vars

data_split <- unique(rbind(res$tabs$pizza_cj,res$tabs$pizza_treff))
data_fuse <- data_split %>%
  separate(paste(res$vars[1],res$vars[2],sep="_"), into = res$vars, sep = "_")
colonnes<-lapply(data,colnames)

data_sort<-lapply(data, data[order(data$colonnes)])
data_sort <- lapply(list(data), function(df) df[do.call(order, df), ])

unlist

data_sort <- data[order(data$ACT, data$GEO, data$SEX, data$AGE),]
data_sort <- data_sort %>% select(order(colnames(data_sort)))

data_fuse_sort <- data_fuse[order(data_fuse$ACT, data_fuse$GEO, data_fuse$SEX, data_fuse$AGE),]
data_fuse_sort <- data_fuse_sort %>% select(order(colnames(data_fuse_sort)))

identical(data_sort,data_fuse_sort)

# On vérifie que les colonnes sont bien identiques
# identical(data_sort, data_fuse_sort) ne fonctionne pas... A demander pourquoi
identical(data_sort$ACT, data_fuse_sort$ACT)
identical(data_sort$AGE, data_fuse_sort$AGE)
identical(data_sort$GEO, data_fuse_sort$GEO)
identical(data_sort$SEX, data_fuse_sort$SEX)
identical(data_sort$VALUE, data_fuse_sort$VALUE)