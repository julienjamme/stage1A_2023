
####################
######TEST##########
####################

# On pose le secret primaire

liste_tabs_exemple2 <- purrr::map(
  list_tab2,
  function(tab){
    tab %>% 
      mutate(
        is_secret_freq = nb_obs > 0 & nb_obs < 3,
        is_secret_dom = (value != 0) & (max > 0.85*value)
      ) %>% 
      mutate(
        is_secret_prim = is_secret_freq | is_secret_dom,
        nb_obs = ceiling(nb_obs)
      )})

freq<-"nb_obs"
value<-"value"

totcode<-c(ACTIVITY="Total",PERS_NUMBER_EMPL="Total_Total", GEO="Total")


#On récupère les varibales des différentes tables

var_cross<-paste(list_res2$vars[1],list_res2$vars[2],sep="_")
d<- intersect(names(list_res2$tabs$T1), names(totcode))

n<-length(list_tab2)
list_vars<-replicate(n,d,simplify=FALSE)
names(list_vars)<- c(paste0("T",1:n,sep=""))


#On regarde le secret

masq <- tab_multi_manager(
  list_tables = liste_tabs_exemple2,
  list_explanatory_vars = list_vars  ,
  dir_name = "test_avec_rtauargus/hierarchie_3/3/mod",
  totcode = totcode,
  hrc = hrcfiles[!(names(hrcfiles) %in% list_res2$vars)],
  alt_hrc = list_res2$hrcs,
  alt_totcode = list_res2$alt_tot,
  value = value,
  freq = freq,
  secret_var = "is_secret_prim")

res_list<-list(
  tabs=masq,
  hrcs=liste_res2$hrcs,
  vars=liste_res2$vars
)

tab_4<-passer_3_41(res_list,data)

tab_4_compt <- tab_4 %>% 
  mutate(
    statut_final = case_when(
      is_secret_freq ~ "A",
      is_secret_dom ~ "B",
      TRUE ~ Status,
    )
  )


#nombre enlevé 
tab_4_compt %>% 
  group_by(statut_final) %>% 
  summarise(
    n_cell = n(),
    val_cell = sum(nb_obs)
  ) %>%
  mutate(
    pc_n_cell = n_cell/sum(n_cell)*100,
    pc_val_cell = val_cell/sum(val_cell)*100
  )


# statut_final n_cell   val_cell pc_n_cell pc_val_cell
# <chr>         <int>      <dbl>     <dbl>       <dbl>
#   1 A              2147  21720282.     19.8         3.14
# 2 B               639  29244675.      5.89        4.22
# 3 D              5470 334455095.     50.4        48.3 
# 4 V              2598 307067737.     23.9        44.3 

tau<-tau_argus_4_3(list_res2,liste_tabs_exemple2,totcode,freq,value,hrc_files)
identical(tau,masq)

