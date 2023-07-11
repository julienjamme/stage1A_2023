

# Vider l'environnement global
# Ne pas faire de controle A + Enter 

####################
#####DONNEES########
####################



source("R/function_passer_3_4.R")
source("R/format.R")
#source("brouillon/tauargus_4_3.R")
load("data/ca_pizzas_4vars.RData")
source("R/cas_gen_4_3.R")
library("purrr")
library("dplyr")
library("rtauargus")
source("test/test_nbs_tabs.R")
loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

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

#On a le format utilisé pour rtauargus

list_res<-tabs_5_4_to_3(ca_pizzas_4vars,nom_dfs,totcode,hrcfiles,sep_dir = TRUE,"output")

list_tab<-list_res$tabs
list_vars<-list(
  T1=c("ACTIVITY","NUTS23","treff_cj"),
  T2=names(list_tab$T2)[sapply(list_tab$T2, is.character)]
)

####################
######TEST##########  LISTE DE TABLEAUX A 3 DIMENSION
####################

# On pose le secret primaire

liste_tabs_exemple <- purrr::map(
  list_tab,
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

# On pose le secret secondaire 

exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list(pizza1=c("ACTIVITY","NUTS23","treff_cj"),
                               pizza2=c("ACTIVITY","NUTS23","treff_cj")) ,
  dir_name = "test_avec_rtauargus/hierarchie_2/3/mod",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff_cj="Total_Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  alt_hrc = list_res$hrcs,
  alt_totcode = list_res$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  
)

str(exemple_masq)

value<-"pizzas_tot"
freq<-"nb_obs"

#tau<-tau_argus_4_3(list_res,liste_tabs_exemple,totcode,freq,value,hrcfiles)
#identical(tau,exemple_masq)

res_4_3<-list(
  tabs=exemple_masq,
  hrcs=res$hrcs,
  vars=res$vars
)

p<-passer_3_41(res_4_3,ca_pizzas_4vars)
# A travailler
ca_pizzas_4vars$nb_obs <- ceiling(ca_pizzas_4vars$nb_obs)
p_sans_argus <- p[!sapply(p, is.logical)]

setdiff(p_sans_argus,ca_pizzas_4vars)


p_compt <- p %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      is_secret_4 ~ "D",
      TRUE ~ "V"
    )
  )

#nombre enlevé 
p_compt %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )

# A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A              2147  21701410.     19.8         3.13
# 2 B               639  29207577.      5.89        4.22
# 3 D              3921 142310503.     36.1        20.6 
# 4 V              4147 499212329.     38.2        72.1 
#  

###################################################################
# On essaye de comparer avec le tableau à 4 variables catégorielles

tabs_exemple <- (
  ca_pizzas_4vars %>% 
    
    mutate(
      is_secret_freq = nb_obs > 0 & nb_obs < 3,
      is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot),
      pizzas_tot= abs(pizzas_tot)
    ) %>% 
    mutate(
      is_secret_prim = is_secret_freq | is_secret_dom,
      nb_obs = ceiling(nb_obs)
    ))

# Ne pas tourner car long 
# Exemple de tableau à 4 variables 

exemple_masq2 <- tab_rtauargus(
  tabs_exemple,
  files_name = "ca_pizzas_4vars" ,
  explanatory_vars = c("ACTIVITY","NUTS23","treff","cj"),
  dir_name = "test_avec_rtauargus/hierarchie_2/4/mod",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff="Total",cj="Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  verbose = FALSE
)

T4_masq <- exemple_masq2 %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      TRUE ~ Status,
    )
  )

T4_masq %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )

# Modular 3h...
# # A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A              2147  21720282.     19.8         3.14
# 2 B               639  29244675.      5.89        4.22
# 3 D              4264 171641619.     39.3        24.8 
# 4 V              3804 469881214.     35.0        67.9 




## Hypercube


exemple_masq3 <- tab_rtauargus(
  tabs_exemple,
  files_name = "ca_pizzas_4vars" ,
  explanatory_vars = c("ACTIVITY","NUTS23","treff","cj"),
  dir_name = "test_avec_rtauargus/hierarchie_2/4/hypercube",
  totcode = c(ACTIVITY="Total",NUTS23="Total",treff="Total",cj="Total"),
  hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  verbose = FALSE,
  suppress = "GH(1,100)"
  
)


# On a 4222 lignes supprimés

#######################################
############DONNEES2###################
#######################################


load("data/ca_test_1_hrc.RData")
hrc_activity <- rtauargus::write_hrc2(
  corr_act, 
  "hrc/activity_2_niv.hrc", 
  adjust_unique_roots = TRUE
)

hrc_files = c(ACTIVITY = hrc_activity)


totcode<-c(ACTIVITY="Total",pays="Total", cj="Total",ident="Total")

# pour execution ligne à ligne
dfs <- res_all_dtph2
nom_dfs <- "table_test_1_hrc"

hrcfiles <- hrc_files
res<-passer_de_4_a_3_var(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir = FALSE, hrc_dir = "hrc_alt")
list_res2<-tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir,hrc_dir)

list_tab2<-list_res2$tabs


list_vars<-list(
  table_test_1_hrc1=c("ACTIVITY","ident","cj_pays"),
  table_test_1_hrc2=c("ACTIVITY","ident","cj_pays")
)

####################
######TEST##########  LISTE DE TABLEAUX A 3 DIMENSION
####################

# On pose le secret primaire

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

# On pose le secret avec un grand tableau avec une hiérarchie et
# et on observe que la taille des tableuax est un problème 

# exemple_masq <- tab_multi_manager(
#   list_tables = liste_tabs_exemple,
#   list_explanatory_vars = list_vars ,
#   dir_name = "test_avec_rtauargus/hierarchie_2/1/mod",
#   totcode = c(ACTIVITY="Total",ident="Total",cj_pays="Total_Total"),
#   hrc = c(ACTIVITY=hrc_activity),
#   alt_hrc = list_res2$hrcs,
#   alt_totcode = list_res2$alt_tot,
#   value = "pizzas_tot",
#   maxscore = "pizzas_max",
#   freq = "nb_obs",
#   secret_var = "is_secret_prim",
#   
# )

load("data/ca_test_0_hrc.RData")
hrcfiles<-NULL
totcode<-c(treff="Total",cj="Total", A10="Total",type_distrib="Total")
dfs <- res_all_dtp
nom_dfs <- "table_test_0_hrc"

list_res2<-tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles ,sep_dir,hrc_dir)



list_tab2<-list_res2$tabs


list_vars<-list(
  table_test_0_hrc1=c("type_distrib","A10","treff_cj"),
  table_test_0_hrc2=c("type_distrib","A10","treff_cj")
)

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
  list_explanatory_vars = list_vars ,
  dir_name = "test_avec_rtauargus/hierarchie_2/0/mod",
  totcode = totcode<-c(treff_cj="Total_Total", A10="Total",type_distrib="Total"),
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
)

res_4_3<-list(
  tabs=exemple_masq,
  hrcs=list_res2$hrcs,
  vars=list_res2$vars
)

p<-passer_3_41(res_4_3,res_all_dtp)
# A travailler

p_compt <- p %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      is_secret_5 ~ "D",
      TRUE ~ "V"
    )
  )

#nombre enlevé 

p_compt %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )
# 
# # A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A                79   2172240.     11.5        0.706
# 2 B                35   5497851.      5.08       1.79 
# 3 D               250  38848471.     36.3       12.6  
# 4 V               325 261228913.     47.2       84.9

tabs_exemple4 <- (
  res_all_dtp %>% 
    mutate(
      is_secret_freq = nb_obs > 0 & nb_obs < 3,
      is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot),
      pizzas_tot= abs(pizzas_tot)
    ) %>% 
    mutate(
      is_secret_prim = is_secret_freq | is_secret_dom,
      nb_obs = ceiling(nb_obs)
    ))

exemple_masq3 <- tab_rtauargus(
  tabs_exemple4,
  files_name = "table_test_0_hrc" ,
  explanatory_vars = c("treff","A10","type_distrib","cj"),
  dir_name = "test_avec_rtauargus/hierarchie_2/4/hypercube",
  totcode = c(A10="Total",type_distrib="Total",treff="Total",cj="Total"),
  value = "pizzas_tot",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  verbose = FALSE
)

T4_masq <- exemple_masq3 %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      TRUE ~ Status,
    )
  )

T4_masq %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )
# 
# A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A                79   2172240.     11.5        0.706
# 2 B                35   5499388.      5.08       1.79 
# 3 D               302  36512640.     43.8       11.9  
# 4 V               273 263564744.     39.6       85.6  
# 
identical(sort(p_compt$type_distrib),sort(res_all_dtp$type_distrib))
res_sort<-arrange(res_all_dtp,"treff","cj","type_distrub","A10")
T4_sort<-arrange(T4_masq,"treff","cj","type_distrub","A10")
p_sort<-arrange(p_compt,"treff","cj","type_distrub","A10")

diff_indices <- which(T4_sort$statut_final != p_sort$statut_final)
diff_rows <- T4_masq[diff_indices, ]
diff_rows[1]
p_compt[1,]
T4_masq[1, ]





niv1_mod <- c("FR", "GB", "NL", "BE", "EC", "LU", "IT", "CH",
              "MX", "DE", "IE", "QA", "ES", "AU", "US", "CA",
              "PT", "BS", "MA", "NO", "LB", "MU", "AT", "KW",
              "SE", "JP", "DK", "TR", "FI", "RU", "SG", "CN",
              "AE", "GH", "HK", "IN", "TN", "TW", "KR", "GG",
              "RO", "SI", "RS", "CY", "IL", "JE", "PL", "PA",
              "MT", "TH", "EE", "CO", "SA", "SK", "GR", "BH",
              "CW", "SM", "LT", "HU", "VG", "ZA", "CZ")


corr_pays<-data.frame(
  pays= c(rep("FR",100),niv1_mod),
  dep= c(c("75", "92", "40", "51", "64", "78", "57", "973", "94", "86", "74", "67", "88",
           "77", "34", "83", "24", "13", "47", "44","22", "11", "36", "23", "39", "81",
           "63", "21", "32", "46", "04", "84", "38", "68", "30", "62", "14", "60", "19", "48",
           "06", "971", "35", "12", "16", "42", "05", "2A", "09", "56", "972", "89", "28", 
           "15", "41", "01", "55", "37", "80", "69","73", "58", "02", "29", "65", "66", "87", 
           "85", "95", "27", "974", "07", "52", "25", "03", "71", "93", "2B", "26", "72",
           "70", "17", "59", "54", "49", "18", "79", "50", "33", "08", "43", "61", "31", 
           "90", "76", "82", "10", "45", "53", "91", "976",niv1_mod[niv1_mod != "FR"])
         
  )
)


hrc_pays <- rtauargus::write_hrc2(
  corr_pays,
  "hrc/pays_test.hrc", 
  adjust_unique_roots = TRUE
)

load("data/ca_test_3_hrc.RData")
source("R/passage_4_3_cas_1_non_hrc.R")
source("R/passage_4_3_cas_2_non_hrc.R")
source("R/choisir_sep.R")
source("R/cas_gen_4_3.R")
source("R/passage_4_3_cas_0_non_hrc.R")

totcode<-c(cj="Total",NUTS="Total", PAYS="Total",treff="Total")
dfs<-test_3hrc
nom_dfs <- "table_test_3_hrc"
sep_dir<-FALSE
hrc__files<-c(NUTS=hrc_nuts,PAYS=hrc_pays)

list_res2<-tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrc__files ,sep_dir)


source("R/fonction_general_choisit_sep_et_reduit_dim.R")
library(stringr)
source("test/test_nbs_tabs.R")

gen<-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode,hrc__files,sep_dir = TRUE,
                       hrc_dir = "output",select_hier = TRUE)

list_res2<-gen
list_tab2<-list_res2$tabs



####################
######TEST##########  LISTE DE TABLEAUX A 3 DIMENSION
####################

# On pose le secret primaire

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

# On pose le secret avec un grand tableau avec une hiérarchie et
# et on observe que la taille des tableuax est un problème 

# exemple_masq <- tab_multi_manager(
#   list_tables = liste_tabs_exemple,
#   list_explanatory_vars = list_vars ,
#   dir_name = "test_avec_rtauargus/hierarchie_2/1/mod",
#   totcode = c(ACTIVITY="Total",ident="Total",cj_pays="Total_Total"),
#   hrc = c(ACTIVITY=hrc_activity),
#   alt_hrc = list_res2$hrcs,
#   alt_totcode = list_res2$alt_tot,
#   value = "pizzas_tot",
#   maxscore = "pizzas_max",
#   freq = "nb_obs",
#   secret_var = "is_secret_prim",
#   
# )


list_tab2<-list_res2$tabs

freq<-"nb_obs"
value<-"pizzas_tot"

totcode<-c(cj="Total",`NUTS+++PAYS`="Total_Total", treff="Total")


#On récupère les varibales des différentes tables

var_cross<-paste(list_res2$vars[1],list_res2$vars[2],sep="_")
d<- intersect(names(list_res2$tabs$table_test_3_hrc1), names(totcode))

n<-length(list_tab2)
list_vars<-replicate(n,d,simplify=FALSE)
names(list_vars)<- c(paste0(nom_dfs,1:n,sep=""))


masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_vars ,
  dir_name = "test_avec_rtauargus/hierarchie_3/mod",
  totcode = totcode,
  hrc = hrc__files[!(names(hrc__files) %in% list_res2$vars)],
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim")

#A partir de 20000 L modular n'y arrive pas
#PUls de 100 lignes c long en dessous c plutot rapide comme on peut le voir avec
# ca_pizzas_4vars
#Modular a du mal avec des grandes hiérarchie




vecteur_tri <- c(11, 12, 14, 15, 16, 22, 61, 72, 81, 89, 91, 99, 101, 102, 103,
                 104, 105, 106, 107, 108, 109, 120, 131, 132, 133, 139, 141, 143,
                 151, 152, 161, 162, 171, 172, 181, 182, 192, 201, 202, 203, 204,
                 205, 206, 211, 212, 221, 222, 231, 232, 233, 234, 235, 236, 237,
                 239, 241, 242, 243, 244, 245, 251, 252, 253, 254, 255, 256, 257, 
                 259, 261, 262, 263, 264, 265, 266, 271, 273, 274, 275, 279, 281,
                 282, 283, 284, 289, 291, 292, 293, 301, 302, 303, 309, 310, 321,
                 322, 323, 324, 325, 329, 331, 332, 351, 352, 353, 360, 381, 382,
                 383, 390, 411, 412, 421, 422, 429, 431, 432, 433, 439, 451, 452,
                 453, 454, 461, 462, 463, 464, 465, 466, 467, 469, 471, 472, 473,
                 474, 475, 476, 477, 478, 479, 491, 493, 494, 501, 502, 503, 504,
                 511, 521, 522, 532, 551, 552, 553, 559, 561, 562, 563, 581, 582,
                 591, 592, 601, 602, 611, 612, 613, 619, 631, 639, 642, 643, 649,
                 651, 661, 662, 663, 681, 682, 683, 691, 692, 701, 702, 711, 712,
                 721, 722, 731, 732, 741, 742, 743, 749, 750, 771, 772, 773, 774,
                 781, 782, 783, 791, 799, 801, 802, 803, 811, 812, 813, 821, 822,
                 823, 829, 851, 852, 853, 854, 855, 856, 861, 862, 869, 871, 873,
                 879, 881, 889, 892, 910, 920, 932, 941, 949, 951, 952, 960)

a278<-c(vecteur_tri)
corr_act<-data.frame(
  a88=round(a278/10),
  a278= c(vecteur_tri)
)



hrc_act <- rtauargus::write_hrc2(
  corr_act,
  "hrc/act_test.hrc", 
  adjust_unique_roots = TRUE
)

load()
