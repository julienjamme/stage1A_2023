
library(dplyr)
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R")
source("R/passage_4_3_cas_2_non_hrc.R")
source("test/test_nbs_tabs.R")
library(tidyr)
######PREMIER EXEMPLE

# Données ###########

data <- expand.grid(
  ACT = c("Total","A","B","A1","A2"),
  SEX = c("Total","F","M"),
  GEO = c("Pays","Dep1","Dep2","Com11","Com12"),
  AGE = c("Ensemble","adulte","enfant","enfant1","enfant2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

dfs <- data %>% mutate(VALUE = runif(nrow(data)))

hrcfiles = c(ACT ="hrc/act_test3.hrc", GEO ="hrc/geo_test.hrc" , AGE ="hrc/age_test.hrc"  )

totcode<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="Total")

# pour execution ligne à ligne
nom_dfs <- "tab"
dir_name <-"output"
v2 <- "AGE"
v1 <- "SEX"

res <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name,sep="_")
res1<-format(res,nom_dfs)

(read.table(hrcfiles[[v2]]))
#On a bien 2 noeuds donc 2*2=4 tableaux

length(res1$tabs)



data <- expand.grid(
  ACT = c("Total","A","B","B1","B2","C","A1","A2"),
  SEX = c("Total","F","M"),
  GEO = c("Pays","Dep1","Dep2","Com11","Com12"),
  AGE = c("Ensemble","adulte","enfant","enfant1","enfant2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()



hrc_files = c(ACT ="hrc/act_test4.hrc", GEO ="hrc/geo_test.hrc" , AGE ="hrc/age_test.hrc"  )


totcode<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="Total")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "argus"
sep_dir<-TRUE
hrcfiles <- hrc_files
v2<-"ACT"
v1<-"SEX"
dir_name<- "output"
res <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name,sep ="_")
read.table(hrc_files[["ACT"]])
#On a bien 3 noeuds donc on a 6 tableaux

length(res$tabs)

data <- expand.grid(
  ACT = c("Total","A","B","B1","B2","A1","A2","A11","A12"),
  SEX = c("Total","F","M"),
  GEO = c("Pays","Dep1","Dep2","Com11","Com12"),
  AGE = c("Ensemble","adulte","enfant","enfant1","enfant2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()
# 
# corr_tab=expand.grid(
#   niv0="Total_Total",
#   niv1="F_Total","M_Total",
#   niv2=paste("F",unique(data$ACT[data$ACT != "Total"]),sep="_"),
# )
hrcfiles = c(ACT ="hrc/act_test2.hrc", GEO ="hrc/geo_test.hrc" , AGE ="hrc/age_test.hrc"  )

res <- passage_4_3_cas_1_non_hr(data, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)
res1<-format(res,nom_dfs)

read.table(hrcfiles[[v2]])
#On a bien 4 noeuds donc 2*4 =8 tableaux
length(res1$tabs)



#On vérifie les noms des différents fichiers
all(names(res1$tabs)==names(res1$hrcs))
all(names(res$tabs) == names(res$alt_tot))

lapply(res1$hrcs,function(tab){
  t<-names(res1$hrcs$tab) == "SEX_ACT"
  return (t)})

lapply(res1$alt_tot,function(tab){
  t<-names(res1$alt_tot$tab) == "SEX_ACT"
  return (t)})

#On vérifie que les sous_totaux sont les bons
res_sdc <- sdcHierarchies::hier_import(inp = hrcfiles[["ACT"]], from = "hrc", root = "Total") %>% 
  sdcHierarchies::hier_convert(as = "sdc")

codes_split <- lapply(
  res_sdc$dims,
  names
)
l<-c()
for (i in (1:length(codes_split))){
  l<-c(l,paste("Total",codes_split[[i]][1],sep="_"))
  l<-c(l,paste("Total",codes_split[[i]][1],sep="_"))
}
#on a les bons all_tot si t=True
for (i in (1:length(res1$alt_tot))){
  t<-unname(res1$alt_tot[[i]])==l[i]
  if (t==FALSE){return(t)}}
(t)
#On vérifie si on a toutes les lignes dans le tableaux

liste_tab<-lapply(res1$tabs,function(tab){
        tab<-separate(tab, SEX_ACT, into = c("SEX", "ACT"), sep = "_")
        return (tab)})

tab<- unique(bind_rows(liste_tab))

a2 <- full_join(tab,data, by = c("SEX", "ACT","AGE","GEO"))
                
verif<-sum(unlist(lapply(a2,function(col) sum(is.na(col))))) == 0
#On veut avoir TRUE 
(verif)
# Les deux tables sont-elles bien les tables attendues ?
l<-list()

s <- 0
# Tests de composition: ok pour les cellules
# La correspondance entre hrc et tableaux

l<-list()

for ( t in names(res1$tabs)){
  
  p<-res1$tabs[[t]] %>% 
    filter(SEX_ACT != res1$alt_tot[[t]]$SEX_ACT) %>% 
    pull(SEX_ACT) %>% 
    unique() %>% 
    sort() %>% 
    `==`(
      read.table(res1$hrcs[[t]]$SEX_ACT) %>% 
        mutate(V1 = gsub("@","",V1)) %>% 
        pull(V1) %>% 
        sort()
    ) %>% 
    all()
  l<-append(l,p)
}
(l) 
#On s'attend à avoir une liste de 8 éléments (length(res1$tabs)) contenant que des TRUE

# Les tables sont-elles les tables attendues

for (t in names(res1$tabs)) {
    
  hrc1<-read.table(res1$hrcs[[t]]$SEX_ACT) %>% 
    mutate(V1 = gsub("@","",V1)) %>% 
    pull(V1) 
  #on filtre selon les hiérarchie
  res_attendu_1 <- data %>%
    mutate(SEX_ACT = paste0(SEX, "_", ACT)) %>%
    filter(SEX_ACT %in% hrc1) %>%
    select(-SEX, -ACT)
  
  result <- res_attendu_1 %>%
    full_join(res1$tabs[[t]], by = c("SEX_ACT", "AGE", "GEO")) %>%
    is.na() %>%
    sum()
  
  s <- s + result
}
(s)

# Attendu = 0 pour aucune valeur manquante
#cad aucun pb d'appariementgit





