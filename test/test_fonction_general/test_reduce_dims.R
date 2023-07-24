rm(list = ls())

library(dplyr)
library(data.table)
library(stringr)
source(file = "R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/reduce_dims.R",encoding = "UTF-8")
<<<<<<< HEAD:test/test_fonction_general/test_fonction_general_choisit_sep_et_reduit_dim.R
source("R/cas_gen_4_3.R",encoding = "UTF-8")
=======
source("R/passage_4_3.R",encoding = "UTF-8")
>>>>>>> 1dc07ad6575fac8ed8af7f837e0a033230478eaf:test/test_fonction_general/test_reduce_dims.R
source("R/choisir_sep.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/length_tabs.R",encoding = "UTF-8")
source("R/nb_tab.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")
source("R/chercher_combinaison_variable_a_fusionner.R",encoding = "UTF-8")
library(tictoc)
library("rtauargus")
loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)
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


res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles, 
                            sep_dir= TRUE,
                            hrc_dir = "test/test_fonction_general/test1",vec_sep =  c("\\_+"),
                            nb_tab = 'min')
length(res5_3$tabs)

max(sapply(res5_3$tabs, nrow))


# Passage de 5 à 3: 9.97 sec elapsed
# > length(res5_3$tabs)
# [1] 24
# > 
# > max(sapply(res5_3$tabs, nrow))
# [1] 1050

res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles, 
                            sep_dir= TRUE,
                            hrc_dir = "test/test_fonction_general/test1",vec_sep =  c("\\_+"),
                            nb_tab = 'max')

length(res5_3$tabs)

max(sapply(res5_3$tabs, nrow))


# Passage de 5 à 3: 150.2 sec elapsed
# > 
# > length(res5_3$tabs)
# [1] 360
# > 
# > max(sapply(res5_3$tabs, nrow))
# [1] 297


res5_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles, 
                            sep_dir= TRUE,
                            hrc_dir = "test/test_fonction_general/test1",vec_sep =  c("\\_+"),
                            nb_tab = 'smart',
                            LIMIT = 600)
length(res5_3$tabs)

max(sapply(res5_3$tabs, nrow))

# Choix des variables: 12.08 sec elapsed
# Passage de 5 à 3: 26.51 sec elapsed
# > smart_tab <- length(res5_3$tabs)
# > 
# > max(sapply(res5_3$tabs, nrow))
# [1] 450
# > smart_tab
# [1] 72

# On perd certe 12s pour choisir les variables, mais
# passer de 5-> 3 prend maintenant 26 sec au lieu de 150
# ainsi nous gagnons 150-26-12 = 136sec = 2.3min :

# + tout le temps futur gagné en traitant moins de tableaux dans tauargus !



choisir_sep(data, liste_sep=c("\\_+"))

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

sep <- res5_3$sep
var_fusionnes <- c(paste(res5_3$fus_vars[[1]][1],res5_3$fus_vars[[1]][2],sep=sep),
                   paste(res5_3$fus_vars[[2]][1],res5_3$fus_vars[[2]][2],sep=sep))


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
                           hrc_dir = hrc_dir,
                           nb_tab = 'min')
length(res4_3$tabs)

max(sapply(res4_3$tabs, nrow))

res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                           sep_dir = sep_dir,
                           hrc_dir = hrc_dir,
                           nb_tab = 'max')
length(res4_3$tabs)

max(sapply(res4_3$tabs, nrow))

# Passage de 4 à 3: 1.32 sec elapsed
# > length(res4_3$tabs)
# [1] 4
# > 
# > max(sapply(res4_3$tabs, nrow))
# [1] 1332


res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                           sep_dir = sep_dir,
                           hrc_dir = hrc_dir,
                           nb_tab = 'smart',
                           LIMIT = 1400)
length(res4_3$tabs)

max(sapply(res4_3$tabs, nrow))

# Choix des variables: 1 sec elapsed
# [1] "Choix des variables: 1 sec elapsed"
# Passage de 4 à 3: 1.41 sec elapsed
# [1] "Passage de 4 à 3: 1.41 sec elapsed"
# > length(res4_3$tabs)
# [1] 4
# > 
# > max(sapply(res4_3$tabs, nrow))
# [1] 1260
# La nouvelle implémentation peut permettre de trouver des tableaux moins grands
# à nombre de tableaux généré égaux

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

sep <- res4_3$sep
var_fusionnes <- c(paste(res4_3$fus_vars[1],res4_3$fus_vars[2],sep=sep))


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
                            vars_a_fusionner = c("AGE","ECO","ACT"),
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
length(res5_3$fus_vars) == 2

sep <- res5_3$sep
var_fusionnes <- c(paste(res5_3$fus_var[[1]],res5_3$fus_var[[2]],sep=sep))


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


# separateur --------------------------------------------------------------


#TEST DES SEPARATEURS avec tauargus
# le _ ou "___"

load("data/ca_test_0_hrc.RData")
hrcfiles<-NULL
totcode<-c(treff="Total",cj="Total", A10="Total",type_distrib="Total")
dfs <- res_all_dtp
nom_dfs <- "table_test_0_hrc"

list_res2<-
  gen_tabs_5_4_to_3( dfs,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,hrc_dir = "output",vec_sep = c("\\___") )

list_tab2<-list_res2$tabs

liste_tabs_exemple <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85 * pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/test_separateur/underscore",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
)


list_res2<-
  gen_tabs_5_4_to_3( dfs,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,hrc_dir = "output",vec_sep = c("\\_") )

list_tab2<-list_res2$tabs

liste_tabs_exemple <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/test_separateur/mod",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
 
)

# label ne fonctionnant pas -----------------------------------------------


# le "+++" 
list_res2<-
  gen_tabs_5_4_to_3( dfs,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,hrc_dir = "output",vec_sep = c("\\+\\+\\+") )

list_tab2<-list_res2$tabs

liste_tabs_exemple <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/test_separateur/plus",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim"
)
# Error in `[.data.frame`(res_import, , explanatory_vars, drop = FALSE) : 
#   colonnes non définies sélectionnées
# Table1 passe mais pas les autres je pense les regex
# le "???"

list_res2<-
  gen_tabs_5_4_to_3( dfs,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,hrc_dir = "output",vec_sep = c("\\???") )

list_tab2<-list_res2$tabs

liste_tabs_exemple <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})


exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/test_separateur/interrogation",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim"
)
# --- Current table to treat:  table_test_0_hrc1 ---
#   Error in `[.data.frame`(res_import, , explanatory_vars, drop = FALSE) : 
#   colonnes non définies sélectionnées

list_res2<-
  gen_tabs_5_4_to_3( dfs,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,hrc_dir = "output",vec_sep = c("zzz") )


list_tab2<-list_res2$tabs

liste_tabs_exemple <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/test_separateur/zzz",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim"
)
#Fonctionne sans expression régulière

# Label fonctionnant ----------------------------------------------------


list_res2<-
  gen_tabs_5_4_to_3( dfs,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,hrc_dir = "output",vec_sep = c("\\_+_") )

list_tab2<-list_res2$tabs

liste_tabs_exemple <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/test_separateur/underscore_plus_underscore",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim"
)

str(exemple_masq)


res_4_3<-list(
  tabs=exemple_masq,
  hrcs=list_res2$hrcs,
  sep=list_res2$sep,
  fus_vars=list_res2$fus_vars
)

p<-passer_a_4_ou_5_r_base(res_4_3)
data_fusion <- unique(do.call("rbind",p))

data_compt <- data_fusion %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      is_secret_4 ~ "D",
      TRUE ~ "V"
    )
  )

#nombre enlevé 

data_compt %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )


# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A                79   2172240.     11.5        0.706
# 2 B                35   5497851.      5.08       1.79 
# 3 D               250  38848471.     36.3       12.6  
# 4 V               325 261228913.     47.2       84.9  

# ON va tester avec un autre

list_res2<-
  gen_tabs_5_4_to_3( dfs,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,hrc_dir = "output",vec_sep = c("\\_+?") )


list_tab2<-list_res2$tabs

liste_tabs_exemple <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/test_separateur/underscore_plus_interrogation",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim"
)
