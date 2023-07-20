rm(list = ls())

library(dplyr)
library(data.table)
library(stringr)
source(file = "R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/fonction_general_choisit_sep_et_reduit_dim.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")

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
nom_dfs <- "OPPO"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "test/test_fonction_general/test1"
hrc_dir <- dir_name
sep_dir <- TRUE

liste_sep = c("\\+", "\\!", "\\?","\\:",
              "\\;","\\~","\\&","\\#")

res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles, 
                            sep_dir= TRUE,
                            hrc_dir = "test/test_fonction_general/test1")

# on a le bon format de sortie
names(res5_3) 

# Les noms sont cohérents entre eux
all(names(res5_3$tabs) == names(res5_3$hrcs))
all(names(res5_3$tabs) == names(res5_3$alt_tot))

# Les noms sont bien de la forme non_dfsi
purrr::map2(names(res5_3$tabs), 1:length(res5_3$tabs),
            ~ .x == paste(nom_dfs, .y, sep="")) %>% 
  unlist() %>% 
  all()

# Le séparateur est bien +++


var_fusionnes <- c(paste(res5_3$vars[[1]][1],res5_3$vars[[1]][2],sep="+++"),
                   paste(res5_3$vars[[2]][1],res5_3$vars[[2]][2],sep="+++"))


# les totaux sont bien de la forme c(v1_v2 = tot_v1_v2, ...)
purrr::map(res5_3$alt_tot,
           function(x) return(
             sapply(names(x), function(i) return(i %in% var_fusionnes))
             )
           ) %>% 
  unlist() %>% 
  all()

# il y a bien 2 alt_tot : v1_v2 et v3_v4
purrr::map(res5_3$alt_tot,
           function(x) return(length(x) == 2)
) %>% 
  unlist() %>% 
  all()

# les hrc sont bien de la forme c(v1_v2 = hrc_v1_v2, ...)
purrr::map(res5_3$hrcs,
           function(x) return(
             sapply(names(x), function(i) return(i %in% var_fusionnes))
           )
) %>% 
  unlist() %>% 
  all()

# il y a bien 2 hrc : v1_v2 et v3_v4
purrr::map(res5_3$hrcs,
           function(x) return(length(x) == 2)
) %>% 
  unlist() %>% 
  all()


# Il n'y a pas de + dans les colonnes initiale 

!any(sapply(names(data), function(i) return(grepl("\\+", i))))

# Il y a bien des colonnes avec un + après passage
all(sapply(res5_3$tabs,
           function(tab) return(any(sapply(names(tab), 
                                function(i) return(grepl("\\+", i)
                                                   ))))))


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
hrc_dir = "test/test_fonction_general/test2"

res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                           sep_dir = sep_dir,
                           hrc_dir = hrc_dir)
str(res4_3)


# on a le bon format de sortie
names(res4_3) 

# Les noms sont cohérents entre eux
all(names(res4_3$tabs) == names(res4_3$hrcs))
all(names(res4_3$tabs) == names(res4_3$alt_tot))

# Les noms sont bien de la forme non_dfsi
purrr::map2(names(res4_3$tabs), 1:length(res4_3$tabs),
            ~ .x == paste(nom_dfs, .y, sep="")) %>% 
  unlist() %>% 
  all()

# Le séparateur est bien +++


var_fusionnes <- c(paste(res4_3$vars[1],res4_3$vars[2],sep="+++"))


# les totaux sont bien de la forme c(v1_v2 = tot_v1_v2, ...)
purrr::map(res4_3$alt_tot,
           function(x) return(
             sapply(names(x), function(i) return(i %in% var_fusionnes))
           )
) %>% 
  unlist() %>% 
  all()

# les hrc sont bien de la forme c(v1_v2 = hrc_v1_v2, ...)
purrr::map(res4_3$hrcs,
           function(x) return(
             sapply(names(x), function(i) return(i %in% var_fusionnes))
           )
) %>% 
  unlist() %>% 
  all()


# Il n'y a pas de + dans les colonnes initiale 

!any(sapply(names(data), function(i) return(grepl("\\+", i))))

# Il y a bien des colonnes avec un + après passage
all(sapply(res4_3$tabs,
           function(tab) return(any(sapply(names(tab), 
                                           function(i) return(grepl("\\+", i)
                                           ))))))

# Test dimension 5 - 3 variables fusionnées en une --------------------------------------


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

res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                            v1=v1,v2=v2,v3=v3,v4=v4,
                            sep_dir = sep_dir,
                            hrc_dir = hrc_dir)

# on a le bon format de sortie
names(res5_3) 

# Les noms sont cohérents entre eux
all(names(res5_3$tabs) == names(res5_3$hrcs))
all(names(res5_3$tabs) == names(res5_3$alt_tot))

# Les noms sont bien de la forme non_dfsi
purrr::map2(names(res5_3$tabs), 1:length(res5_3$tabs),
            ~ .x == paste(nom_dfs, .y, sep="")) %>% 
  unlist() %>% 
  all()

# Les infos relatifs aux variables fusionnés à chaque étape sont présentes
length(res5_3$vars) == 2

var_fusionnes <- c(paste(res5_3$vars[[2]][1],res5_3$vars[[2]][2],sep="+++"))


# les totaux sont bien de la forme c(v1_v2 = tot_v1_v2, ...)
purrr::map(res5_3$alt_tot,
           function(x) return(
             sapply(names(x), function(i) return(i %in% var_fusionnes))
           )
) %>% 
  unlist() %>% 
  all()

# il y a bien qu'un seul total (on se fiche de v1_v2 puisque nous avons créé v1_v2_v3)
purrr::map(res5_3$alt_tot,
           function(x) return(length(x) == 1)
           ) %>% 
  unlist() %>% 
  all()

# les hrc sont bien de la forme c(v1_v2 = hrc_v1_v2, ...)
purrr::map(res5_3$hrcs,
           function(x) return(
             sapply(names(x), function(i) return(i %in% var_fusionnes))
           )
) %>% 
  unlist() %>% 
  all()

# il y a bien qu'un seul hrc (on se fiche de v1_v2 puisque nous avons créé v1_v2_v3)
purrr::map(res5_3$hrcs,
           function(x) return(length(x) == 1)
) %>% 
  unlist() %>% 
  all()


# Le séparateur est bien +++ (visuellement)

# Il n'y a pas de + dans les colonnes initiales 

!any(sapply(names(data), function(i) return(grepl("\\+", i))))

# Il y a bien des colonnes avec un + après passage
all(sapply(res5_3$tabs,
           function(tab) return(any(sapply(names(tab), 
                                           function(i) return(grepl("\\+", i)
                                           ))))))
