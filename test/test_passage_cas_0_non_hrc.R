# Vider l'environnement global
rm(list = ls())


library(dplyr)
library(data.table)
library(stringr)
source(file = "R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/fonction_general_choisit_sep_et_reduit_dim.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")

# donnée 1 : première table ----------------------------------------------------------------

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrcfiles = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", 
             SEX = "hrc/hrc3.hrc", AGE = "hrc/exemple_1.hrc")


totcode<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

v1 <- "ACT"
v2 <- "GEO"

# pour supprimer les .hrc facilement
dir_name <- "output"


# test 1 ------------------------------------------------------------------

res <- passage_4_3_cas_0_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)

#on a le bon format
str(res$alt_tot)

format(res,nom_dfs,sep="_")

#test plus_petit_hrc 

identical("GEO",plus_petit_hrc(hrcfiles,totcode))
identical("ACT",plus_petit_hrc(hrcfiles,totcode))


#On a le bon nombre de tableau
length(res$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v=v1) * nb_noeuds(hrcfiles = hrcfiles, v=v2)

#Les fichiers hrcs sont stockés dans le bon endroit et nommé
dirname(res$hrcs$nom_data_frame_Total_Pays_ACT) == dir_name

#Les tables ont bien les bonnes modalités et les noms liant tableaux et hrcs sont les bons

c_ACT<- c("Total_Pays", "A_Pays", "B_Pays", "C_Pays", "A_Reg", "B_Reg", "C_Reg", "A_Reg_", "B_Reg_", "C_Reg_")


identical(sort(unique(res$tabs$nom_data_frame_Total_Pays_ACT$ACT_GEO)),sort(c_ACT))
read.table(res$hrcs$nom_data_frame_Total_Pays_ACT)


# donnée 2 : seconde table ----------------------------------------------------------------

data2 <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/age.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

v1<-"ACT"
v2<-"AGE"
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/exemple_1.hrc", 
              SEX = "hrc/hrc3.hrc", AGE = "hrc/age.hrc")

tot_code<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")
nom_dfs <- "nom_data_frame"
dir_name <- "output"


res2 <- passage_4_3_cas_0_non_hr(data2, nom_dfs,v1,v2,tot_code,hrc_files,dir_name)

#on a le bon format
str(res2)

#On a le bon nombre de tableau
length(res2$tabs) == 2 * nb_noeuds(hrcfiles = hrc_files, v=v1) * nb_noeuds(hrcfiles = hrc_files, v=v2)

#Les tables ont bien les bonnes modalités et les noms liant tableaux et hrcs sont les bons

c_AGE<- c( "A_18_25","Total_18_25" , "B_18_25", "C_18_25", "A_18", "B_18", "C_18", "A_19", "B_19", "C_19")

identical(sort(unique(res2$tabs$nom_data_frame_Total_18_25_ACT$ACT_AGE)),sort(c_AGE))
read.table(res2$hrcs$nom_data_frame_Total_18_25_ACT)


# donnée 3 ------------------------------------------------------------------

data2 <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/exemple_1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("LETTRE",read.table("hrc/age.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

v1<-"ACT"
v2<-"AGE"
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/exemple_1.hrc", 
              SEX = "hrc/hrc3.hrc", AGE = "hrc/age.hrc")

tot_code<-c(SEX="Total",AGE="LETTRE", GEO="Pays", ACT="Total")
nom_dfs <- "nom_data_frame"
dir_name <- "output"

# test 3 : test séparateur ------------------------------------------------------------------

res3 <- passage_4_3_cas_0_non_hr(data2, nom_dfs,v1,v2,tot_code,hrc_files,dir_name, sep = "+++")


# Les 5 premières lignes ont bien le séparateur
all(unlist(lapply(1:5, function(i) str_detect(data2[i,][[1]], "\\+++")))) == FALSE

all(unlist(lapply(1:5, function(i) str_detect(res3$tabs$nom_data_frame_Total_LETTRE_ACT$`ACT+++AGE`[i], "\\+++"))))

# il y a bien une colone avec le séparateur
any(str_detect(names(data2), "\\+++")) == FALSE
any(str_detect(names(res3$tabs$nom_data_frame_Total_LETTRE_ACT), "\\+++"))


# test 4 ------------------------------------------------------------------

data <- expand.grid(
  ACT = c("Total1", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
  GEO = c("Total2", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GA4","GB3","GB4","GB5","GB6","GB7","GB8","GB9","GA5","GA6","GA7","GA8","GA9","GA10","GA11","GB10"),
  SEX = c("Total3", "F", "M"),
  AGE = c("Total4", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
#nrow(data) #441 rows = 7*7*3^2

dfs <- data

hrc_act <- "test/test_cas_gen_4_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total1", nodes = c("A","B","C","D","E","F","G")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total2", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3","GA4","GA5","GA6","GA7","GA8","GA9","GA10","GA11")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4","GB5","GB6","GB7","GB8","GB9","GB10")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total3",AGE="Total4", GEO="Total2", ACT="Total1")

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo)
v1 = "ACT"
v2 = "GEO"
sep = "_"

res_passage_0 <- passage_4_3_cas_0_non_hr(dfs = data,nom_dfs = "tab",v1 = v1,v2 = v2,
                         totcode = totcode,hrcfiles = hrcfiles,
                         dir_name = "output",sep = '_')

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test3",
  v1 = v1,
  v2 = v2
)

l_tab <- length_tabs(dfs=data,
                     hrcfiles = hrcfiles,
                     v1 = v1,
                     v2 = v2)

tapp_tab <- lapply(res$tabs, nrow)
tapp_pass0 <- lapply(res_passage_0$tabs, nrow)

all(mapply(function(x, y) x == y, tapp_pass0, tapp_tab))

length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

all(mapply(function(x, y) x == y, l_tab, tapp_tab))

df <- data.frame(l_tab = unlist(l_tab), tapp_tab = unlist(tapp_tab))
df

# setdiff(unique(l_tab),unique(tapp_tab))
# 
# setdiff(unique(tapp_tab),unique(l_tab))

unique(res$tabs$tab_Total_GA_ACT$SEX)
unique(res$tabs$tab_Total_GA_ACT$AGE)
unique(res$tabs$tab_Total_GA_ACT$ACT_GEO)

new_col_name = paste0(v1, "_", v2)

tab_Total_GA_ACT <- expand.grid(
  ACT = c("Total","A","B","C","D","E","F","G"),
  GEO = c("GA", "GA1","GA2","GA3","GA4","GA5","GA6","GA7","GA8","GA9","GA10","GA11"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame() %>% 
  filter(ACT != "Total" | (ACT == "Total" & GEO == "GA")) %>% 
  mutate(!!new_col_name := paste(ACT, GEO, sep = "_")) %>% 
  select(-ACT,-GEO)

# tabi[[paste(v1, v2, sep = sep)]]<- paste(tabi[[v1]],tabi[[v2]],sep = sep)
nrow(tab_Total_GA_ACT)

str(res_passage_0$tabs$tab_Total_GA_ACT)

str(tab_Total_GA_ACT)

all(sort(unique(res$tabs$tab_Total_GA_ACT$SEX)) == sort(unique(tab_Total_GA_ACT$SEX)))
all(sort(unique(res$tabs$tab_Total_GA_ACT$AGE)) == sort(unique(tab_Total_GA_ACT$AGE)))

unique(res$tabs$tab_Total_GA_ACT$ACT_GEO)
length(unique(res$tabs$tab_Total_GA_ACT$ACT_GEO))

unique(tab_Total_GA_ACT$ACT_GEO)
length(unique(tab_Total_GA_ACT$ACT_GEO))

all(sort(unique(res$tabs$tab_Total_GA_ACT$ACT_GEO)) == sort(unique(tab_Total_GA_ACT$ACT_GEO)))


sum(duplicated(tab_Total_GA_ACT))


res$tabs$tab_Total1_GA_ACT[duplicated(res$tabs$tab_Total1_GA_ACT),]

setdiff(tab_Total_GA_ACT,res$tabs$tab_Total_GA_ACT %>% select(-VALUE))
setdiff(res$tabs$tab_Total_GA_ACT %>% select(-VALUE),tab_Total_GA_ACT)

dfs = data
nom_dfs = "tab"
totcode = totcode
hrcfiles = hrcfiles
sep_dir = TRUE
hrc_dir = "output"
v3=NULL
v4=NULL
vec_sep = c("\\+", "\\!", "\\?","\\:",
            "\\;","\\~","\\&","\\#")
select_hier = FALSE

res_cas_gen <- gen_tabs_5_4_to_3(dfs = data,nom_dfs = "tab",totcode = totcode,
                  hrcfiles = hrcfiles,sep_dir = TRUE,hrc_dir = "output",v1 = v1,v2 = v2)

tapp_tab_cas_gen <- lapply(res_cas_gen$tabs, nrow)
tapp_tab_cas_0 <- lapply(res_passage_0$tabs, nrow)

# Ce sont des tables de même longueur donc a priori c'est le même output
all(mapply(function(x, y) x == y, tapp_tab_cas_gen, tapp_tab_cas_0))

res_defusionne <- passer_a_4_ou_5(res_cas_gen)

names(res_cas_gen$tabs)
names(res_defusionne)

setdiff(res_defusionne$tab3, data)

sum(duplicated(res_defusionne$tab3))

sum(duplicated(res_defusionne$tab3 %>% select(-VALUE)))

sum(duplicated(data %>% select(-VALUE)))

sum(duplicated(data ))
