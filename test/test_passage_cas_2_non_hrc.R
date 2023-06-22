# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("finaux/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")

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

var_sans_hier <- names(tot_code)[1:2]
v1 <- var_sans_hier[1]
v2 <- var_sans_hier[2]

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- dirname(hrcfiles[1])


res <- list(passage_4_3_cas_2_non_hr(data,nom_dfs,v1,v2, tot_code,dir_name))


#########################################
#### Utilisation future de l'output #####
#########################################

# un élement de la liste retourné
# il contient entre autre 2 tableaux
elem <- res[[1]]

# pour le premier tableau contenu :
new_hrc_files_1 <- hrcfiles

new_var <- paste(elem[[3]][1], elem[[3]][2], sep="_")

hrc_created_1 <- elem[[2]][1]
new_hrc_files_1[new_var] <- hrc_created

tab_1 <- elem[[1]][1]

# puis travailler sur tab_1 avec l'ensemble des hierarchies new_hrc_files_1
