rm(list = ls())

library(dplyr)

source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/passer_de_3_a_4_ou_5.R",encoding = "UTF-8")
source("R/fonction_general_choisit_sep_et_reduit_dim.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")

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

res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles)

# res3_4 <- passer_a_4_ou_5(res4_3)
res3_4_r_base <- passer_a_4_ou_5_r_base(res4_3)

# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_4_r_base))

# 0 ligne différente
dim(setdiff(data_fusion,data))[1] == 0


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

liste_sep = c("\\+", "\\!", "\\?","\\:",
              "\\;","\\~","\\&","\\#")

res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles)

# res3_5 <- passer_a_4_ou_5(res5_3)
res3_5_r_base <- passer_a_4_ou_5_r_base(res5_3)

# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_5_r_base))

# 0 ligne différente
dim(setdiff(data_fusion,data))[1] == 0


# Test dimension 5 - 3 variables fusionnées v1_v2_v3 --------------------------------------

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
v4 = NULL
v3 = "AGE+++ECO"

res5_3_1 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                            v1=v1,v2=v2,v3=v3,v4=v4)

# res3_5_1 <- passer_a_4_ou_5(res5_3_1)
res3_5_1_r_base <- passer_a_4_ou_5_r_base(res5_3_1)

# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_5_1_r_base))

# 0 ligne différente
dim(setdiff(data_fusion,data))[1] == 0

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
v4 = "AGE+++ECO"

res5_3_2 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                            v1=v1,v2=v2,v3=v3,v4=v4)

# res3_5_2 <- passer_a_4_ou_5(res5_3_2)
res3_5_2_r_base <- passer_a_4_ou_5_r_base(res5_3_2)

# on ajoute les lignes de tous les tableaux
data_fusion <- unique(do.call("rbind",res3_5_2_r_base))

# 0 ligne différente
dim(setdiff(data_fusion,data))[1] == 0


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
