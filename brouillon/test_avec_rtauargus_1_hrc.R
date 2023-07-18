load("data/test_4_var_1hrc.RData")
library("dplyr")
library(tictoc)
library("rtauargus")
loc_tauargus <- "Z:/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

source("R/passer_de_3_a_4_ou_5.R")
source("R/cas_gen_4_3.R")
source("R/format.R")
source("R/fonction_general_choisit_sep_et_reduit_dim.R")
source("R/choisir_sep.R")
source("R/passage_4_3_cas_0_non_hrc.R")
source("R/passage_4_3_cas_1_non_hrc.R")
source("R/passage_4_3_cas_2_non_hrc.R")
library(stringr)
str(test_4_var_1hrc)
hrc_activity<-"hrc/activity_2_niv.hrc"
hrcfiles<-hrc_activity
totcode<-c(treff="Total",cj="Total", ACTIVITY  ="Total",nuts1="Total")
dfs <- test_4_var_1hrc
nom_dfs <- "table_test_1_hrc"

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

lapply(liste_tabs_exemple, function(tab) list(sum(tab$is_secret_prim),sum(tab$is_secret_prim) * 100 / nrow(tab)))

tic()
exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/hierarchie_2/1/mod",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
)
t<-toc()

# > t<-toc()
# 1169.41 sec elapsed

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
      is_secret_5 ~ "D",
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

# # A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A               559   7250862.     16.4         1.57
# 2 B               157  14819116.      4.59        3.21
# 3 D               967  62258553.     28.3        13.5 
# 4 V              1735 377292683.     50.8        81.7 



tab_exemple<-test_4_var_1hrc%>% 
  mutate(
    is_secret_freq = nb_obs > 0 & nb_obs < 3,
    is_secret_dom = (pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot)
  ) %>% 
  mutate(
    is_secret_prim = is_secret_freq | is_secret_dom,
    nb_obs = ceiling(nb_obs)
  )
tab_exemple %>% count(is_secret_prim)

tic()

masq3<- tab_rtauargus(
  tab_exemple,
  files_name = "Tables",
  hrcfiles=c(ACTIVITY=hrcfiles),
  dir_name = "test_avec_rtauargus/hierarchie_2/1/mod/table",
  explanatory_vars = c("treff","cj","ACTIVITY","nuts1"),
  totcode = c(treff="Total",cj="Total", ACTIVITY  ="Total",nuts1="Total"),
  secret_var = "is_secret_prim",
  value = "pizzas_tot",
  freq = "nb_obs",
  verbose =FALSE
)

m<-toc()




masq_compt <- masq3 %>% 
    mutate(
      statut_final = case_when(
        is_secret_freq ~ "A",
        is_secret_dom ~ "B",
        TRUE ~ Status,
      )
    )

#nombre enlevé 

masq_compt %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(pizzas_tot)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )

# # A tibble: 4 × 5
# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A               559   7250862.     16.4         1.57
# 2 B               157  14819116.      4.59        3.21
# 3 D              1326  45456475.     38.8         9.85
# 4 V              1376 394094760.     40.3        85.4 

masq3<- tab_rtauargus(
  tab_exemple,
  files_name = "Tables",
  hrcfiles=c(ACTIVITY=hrcfiles),
  dir_name = "test_avec_rtauargus/hierarchie_2/1/hypercube/table",
  explanatory_vars = c("treff","cj","ACTIVITY","nuts1"),
  totcode = c(treff="Total",cj="Total", ACTIVITY  ="Total",nuts1="Total"),
  secret_var = "is_secret_prim",
  value = "pizzas_tot",
  freq = "nb_obs",
  verbose =FALSE,
  suppress="GH(1,100)"
)


# Error in batch file
# Error in file(file, "rt") : cannot open the connection
# In addition: Warning message:
#   In file(file, "rt") :
#   cannot open file 'test_avec_rtauargus/hierarchie_2/1/hypercube/table/Tables.csv': No such file or directory

tic()
exemple_masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple,
  list_explanatory_vars = list_res2$vars ,
  dir_name = "test_avec_rtauargus/hierarchie_2/1/hypercube",
  totcode = list_res2$totcode,
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = "pizzas_tot",
  maxscore = "pizzas_max",
  freq = "nb_obs",
  secret_var = "is_secret_prim",
  suppress="GH(1,100)")
t<-toc()

# > exemple_masq <- tab_multi_manager(
#   +   list_tables = liste_tabs_exemple,
#   +   list_explanatory_vars = list_res2$vars ,
#   +   dir_name = "test_avec_rtauargus/hierarchie_2/1/hypercube",
#   +   totcode = list_res2$totcode,
#   +   alt_hrc = list_res2$hrcs,
#   +   alt_totcode = list_res2$alt_tot,
#   +   value = "pizzas_tot",
#   +   maxscore = "pizzas_max",
#   +   freq = "nb_obs",
#   +   secret_var = "is_secret_prim",
#   +   suppress="GH(1,100)")
# --- Current table to treat:  table_test_1_hrc1 ---
#   Error in batch file
# Error in file(file, "rt") : cannot open the connection
# In addition: Warning message:
#   In file(file, "rt") :
#   cannot open file 'test_avec_rtauargus/hierarchie_2/1/hypercube/table_test_1_hrc1.csv': No such file or directory

#hypercube ne fonctionne pas car il a ajoute trop de secret (théorie)
