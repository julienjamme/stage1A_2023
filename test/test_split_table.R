source("R/reduce_dims.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/length_tabs.R",encoding = "UTF-8")
source("R/chercher_combinaison_variable_a_fusionner.R",encoding = "UTF-8")
source("R/split_table.R",encoding = "UTF-8")
source("R/passer_de_3_a_4_ou_5.R")
library(dplyr)
library(stringr)

# TEST 4 DIPMENSIONS 

# DONNEES

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrcfiles = c(GEO = "hrc/hrc2.hrc")
totcode<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir = TRUE
hrc_dir = "test/test_fonction_general/test2"

res1 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                           sep_dir = sep_dir,
                           hrc_dir = hrc_dir,
                           nb_tab = 'min')

max_1<-max(sapply(res1$tabs, nrow))

# split_tab 

res_f <- split_tab(res = res1,
                   var_fus = paste(res1$fus_vars[1],
                                    res1$fus_vars[2],
                                    sep = res1$sep),
                   LIMIT = 1500)

max_2<-max(sapply(res_f$tabs, nrow))
# On a bien de plus petites tables
max_2<max_1

# On observe si on split bien les tables
data_fus<-passer_a_4_ou_5(res_f$tabs,res_f)
dim(setdiff(data_fus,data))[1] == 0 #TRUE il ny aucune lignes différentes

# On observe si les variables sont les bonnes 

names(res_f$vars)==names(res_f$tabs)
lapply(1:length(res_f$tabs), function(i) {
  sort(res_f$vars[[i]]) == sort(names(res1$totcode)) &
    names(res_f$tabs[[i]]) == names(res1$tabs[[1]])
})

# On observe si les alt_hrs sont bien NULL
is.null(res_f$hrcs)

# On vérifie les alt_tots

names(res_f$alt_tot)==names(res_f$tabs)
read.table(res1$hrcs$nom_data_frame1$`SEX_+_AGE`)
read.table(res1$hrcs$nom_data_frame2$`SEX_+_AGE`)
alt_tot_prevu1<- c("Total_+_Total","A_+_Total","B_+_Total","C_+_Total","D_+_Total","E_+_Total")
alt_tot_prevu2<- c("Total_+_Total","Total_+_U","Total_+_V","Total_+_W","Total_+_X","Total_+_Y")
unname(unlist(res_f$alt_tot))==c(alt_tot_prevu1,alt_tot_prevu2)

# TEST 5 DIMENSIONS ET DEUX VARIABLES FUISONNES
#DONNEES 

data <- expand.grid(
  ACT = c("KEBAB",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("PIB","riche","pauvre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))

hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc" )

tot_code<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="KEBAB", ECO = "PIB")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "OPPO"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "test/test_fonction_general/test1"
hrc_dir <- dir_name
sep_dir <- TRUE

res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles, 
                            sep_dir= TRUE,
                            hrc_dir = "test/test_fonction_general/test1",vec_sep =  c("\\___"),
                            nb_tab = 'min')
length(res5_3$tabs)

res_test$fus_vars
res_test <- split_tab(res = res5_3,
                   var_fus = "AGE___ECO",
                   LIMIT = 1)
res_test2 <-split_tab(res = res_test ,
                      var_fus = "ACT___GEO",
                      LIMIT = 1)

# On observe si on split bien les tables
data_fus<-passer_a_4_ou_5(res_test$tabs,res_test)
dim(setdiff(data_fus,data))[1] == 0 #TRUE il ny aucune lignes différentes

# On observe si les variables sont les bonnes 

names(res_test$vars)==names(res_test$tabs)
lapply(1:length(res_f$tabs), function(i) {
  sort(res_test$vars[[i]]) == sort(names(res_test$totcode)) &
    names(res_test$tabs[[i]]) == names(res_test$tabs[[1]])
})

# On observe si les alt_hrs créer sont les bons
# On a les bons noms 
lapply(1:length(res_test$hrcs),function(i) names(res_test$hrcs[[i]][1])=="ACT___GEO")
res_test$hrcs
# On vérifie les alt_tots

names(res_f$alt_tot)==names(res_f$tabs)
read.table(res1$hrcs$nom_data_frame1$`SEX_+_AGE`)
read.table(res1$hrcs$nom_data_frame2$`SEX_+_AGE`)
alt_tot_prevu1<- c("Total_+_Total","A_+_Total","B_+_Total","C_+_Total","D_+_Total","E_+_Total")
alt_tot_prevu2<- c("Total_+_Total","Total_+_U","Total_+_V","Total_+_W","Total_+_X","Total_+_Y")
unname(unlist(res_f$alt_tot))==c(alt_tot_prevu1,alt_tot_prevu2)


# TEST 5 DIMENSIONS ET UNE VARIABLES FUISONNES
# DONNEES
data <- expand.grid(
  ACT = c("KEBAB",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("PIB","riche","pauvre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc" )

tot_code<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="KEBAB", ECO = "PIB")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "test/test_fonction_general/test3"
hrc_dir <- dir_name
sep_dir <- TRUE

liste_sep = c("\\+", "\\!", "\\?","\\:",
              "\\;","\\~","\\&","\\#")

v1 = "AGE"
v2 = "ECO"
v3 = NULL
v4 = "AGE+++ECO"

res5_3_ <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                            vars_a_fusionner = c("AGE","ECO","ACT"),
                            sep_dir = sep_dir,
                            hrc_dir = hrc_dir)

res_test <- split_tab(res = res5_3,
                      var_fus = "ACT_+_AGE_+_ECO",
                      LIMIT = 1000)

