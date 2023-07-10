

library(dplyr)
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R")
library(tidyr)
library(rtauargus)
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R", encoding = "UTF-8")
source("R/format.R", encoding = "UTF-8")
######PREMIER EXEMPLE

# Données ########

data <- expand.grid(
  ACT = c("Total","A","B","A1","A2","A11","A12"),
  SEX = c("Total","F","M","F1","F2"),
  GEO = c("Pays","Dep1","Dep2","Com11","Com12"),
  AGE = c("Ensemble","adulte","enfant","enfant1","enfant2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()


hrcfiles = c(ACT ="hrc/act_test.hrc", GEO ="hrc/geo_test.hrc" , AGE ="hrc/age_test.hrc",SEX="hrc/sex_test.hrc"  )

totcode<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="Total")

# pour execution ligne à ligne
nom_dfs <- "tab"
dir_name <-"output"
v2 <- "AGE"
v1 <- "ACT"

res <- passage_4_3_cas_0_non_hr(data, nom_dfs,v1,v2,totcode,hrcfiles,dir_name,sep="_")
res1<-format(res,nom_dfs)

str(res1)
length(res1$tabs)

(read.table(hrcfiles[[v2]]))
#On a bien 2 noeuds
(read.table(hrcfiles[[v1]]))
#On a bien 3 noeuds

#On vérifie les noms des différents fichiers
all(names(res1$tabs)==names(res1$hrcs))
all(names(res$tabs) == names(res$alt_tot))

lapply(res1$hrcs,function(tab){
  t<-names(res1$hrcs$tab) == "ACT_AGE"
  return (t)})

lapply(res1$alt_tot,function(tab){
  t<-names(res1$alt_tot$tab) == "ACT_AGE"
  return (t)})

#On vérifie que les sous_totaux sont les bons
res_sdc <- sdcHierarchies::hier_import(inp = hrcfiles[["ACT"]], from = "hrc", root = "Total") %>% 
  sdcHierarchies::hier_convert(as = "sdc")

codes_split <- lapply(
  res_sdc$dims,
  names
)
res_sdc2 <- sdcHierarchies::hier_import(inp = hrcfiles[["AGE"]], from = "hrc", root = "Ensemble") %>% 
  sdcHierarchies::hier_convert(as = "sdc")

codes_split2 <- lapply(
  res_sdc2$dims,
  names
)

l<-c()

for (j in (1:length(codes_split))){
for (i in (1:length(codes_split2))){
  l<-c(l,paste(codes_split[[j]][1],codes_split2[[i]][1],sep="_"))
  l<-c(l,paste(codes_split[[j]][1],codes_split2[[i]][1],sep="_"))
}}
(l)
#on a les bons all_tot si t=True
for (i in (1:length(res1$alt_tot))){
  t<-unname(res1$alt_tot[[i]])==l[i]
  if (t==FALSE){return(t)}}
(t)
#On vérifie si on a toutes les lignes dans le tableaux

liste_tab<-lapply(res1$tabs,function(tab){
  tab<-separate(tab, ACT_AGE, into = c("ACT", "AGE"), sep = "_")
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
    filter(ACT_AGE != res1$alt_tot[[t]]$ACT_AGE) %>% 
    pull(ACT_AGE) %>% 
    unique() %>% 
    sort() %>% 
    `==`(
      read.table(res1$hrcs[[t]]$ACT_AGE) %>% 
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
#Tous les éléments des hiérarchies sont ils présents dans la table associés

for (t in names(res1$tabs)) {
  
  hrc1<-read.table(res1$hrcs[[t]]$ACT_AGE) %>% 
    mutate(V1 = gsub("@","",V1)) %>% 
    pull(V1) 
  #on filtre selon les hiérarchie
  res_attendu_1 <- data %>%
    mutate(ACT_AGE = paste0(ACT, "_", AGE)) %>%
    filter(ACT_AGE %in% hrc1) %>%
    select(-AGE, -ACT)
  
  result <- res_attendu_1 %>%
    full_join(res1$tabs[[t]], by = c("ACT_AGE", "GEO", "SEX")) %>%
    is.na() %>%
    sum()
  
  s <- s + result
}
(s)

# Attendu = 0 pour aucune valeur manquante
#cad aucun pb d'appariement






