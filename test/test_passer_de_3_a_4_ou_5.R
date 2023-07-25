rm(list = ls())

library(dplyr)

source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/passer_de_3_a_4_ou_5.R",encoding = "UTF-8")
source("R/reduce_dims.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")

# Test dimension 4 --------------------------------------------------------

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
#hrcfiles = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")
hrcfiles = c(GEO = "hrc/hrc2.hrc")
totcode<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir = TRUE
hrc_dir = "hrc_alt"

res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,vec_sep = c("\\_"))
res4_3$fus_vars

# res3_4 <- passer_a_4_ou_5(res4_3)
data_fusion <- passer_a_4_ou_5(masq=res4_3$tabs,res=res4_3)


# 0 ligne différente
dim(setdiff(data_fusion,data))[1] == 0

str(data_fusion)
# Test dimension 5 - 2 couples créés --------------------------------------

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

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE

res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,vec_sep = c("\\___"))

# res3_5 <- passer_a_4_ou_5(res5_3)


res3_5<- passer_a_4_ou_5(masq=res5_3$tabs,res=res5_3)

# on ajoute les lignes de tous les tableaux

# 0 ligne différente
dim(setdiff(res3_5,data))[1] == 0


# Test dimension 5 - 3 variables fusionnées v1_v2_v3 --------------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #243 rows = 3^5

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB")

# Résultat de la fonction
# On force la fusion de 3 variables

res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test2",
  v1 = "ACT",
  v2 = "GEO",
  v4 = "ACT_GEO"
)
res$vars

res_vars_fus<-format(res=res,nom_dfs = "tab",sep="_",totcode,hrcfiles=NULL)
str(res_vars_fus)
# res3_5_1 <- passer_a_4_ou_5(res5_3_1)
res3_5_1<- passer_a_4_ou_5(masq=res_vars_fus$tabs,res_vars_fus)

# on ajoute les lignes de tous les tableaux
# 0 ligne différente
dim(setdiff(res3_5_1,data))[1] == 0

# Test dimension 5 - 3 variables fusionnées v3_v1_v2 --------------------------------------

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

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE

liste_sep = c("\\+", "\\!", "\\?","\\:",
              "\\;","\\~","\\&","\\#")

v1 = "AGE"
v2 = "ECO"
v3 = NULL
v4 = "AGE_+_ECO"

res5_3_2 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles
                      ,vec_sep="_+_")

# res3_5_2 <- passer_a_4_ou_5(res5_3_2)
res3_5_2 <- passer_a_4_ou_5(res5_3_2$tabs,res=res5_3_2)

# on ajoute les lignes de tous les tableaux


# 0 ligne différente
dim(setdiff(res3_5_2,data))[1] == 0


# Test autre séparateur ---------------------------------------------------

data <- expand.grid(
  `ACT+` = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
#hrcfiles = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")
hrcfiles = c(GEO = "hrc/hrc2.hrc")
totcode<-c(SEX="Total",AGE="Total", GEO="Total", `ACT+`="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir = TRUE
hrc_dir = "hrc_alt"

res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,liste_sep = c("\\+","\\!"))

res4_3$sep == "!!!"

# res3_4 <- passer_a_4_ou_5(res4_3)
res3_4_r_base <- passer_a_4_ou_5_r_base(res4_3)

# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_4_r_base))

# 0 ligne différente
dim(setdiff(data_fusion,data))[1] == 0

# Test sep concaténé ---------------------------------------------------

data <- expand.grid(
  `ACT!+` = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
#hrcfiles = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")
hrcfiles = c(GEO = "hrc/hrc2.hrc")
totcode<-c(SEX="Total",AGE="Total", GEO="Total", `ACT!+`="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir = TRUE
hrc_dir = "hrc_alt"

res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,liste_sep = c("\\+","\\!"))

res4_3$sep == "+!"

# res3_4 <- passer_a_4_ou_5(res4_3)
res3_4_r_base <- passer_a_4_ou_5_r_base(res4_3)

# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_4_r_base))

# 0 ligne différente
dim(setdiff(data_fusion,data))[1] == 0
