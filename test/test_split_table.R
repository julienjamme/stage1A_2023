library(dplyr)
library(stringr)

source("R/reduce_dims.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/length_tabs.R",encoding = "UTF-8")
source("R/chercher_combinaison_variable_a_fusionner.R",encoding = "UTF-8")
source("R/split_table.R",encoding = "UTF-8")
source("R/passer_de_3_a_4_ou_5.R")


# TEST 4 DIMENSIONS  -------------------------------------------------------------------------

# DONNEES

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2","A3"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2", "AGE3"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())

hrc_act <- "output/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "output/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
nom_dfs <- "nom"
hrcfiles <- c(ACT = hrc_act, GEO = hrc_geo)
hrc_dir <- "output"
dfs <- data
sep_dir <- TRUE


res1 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                         sep_dir = sep_dir,
                         hrc_dir = hrc_dir,
                         nb_tab = 'max')

max_1 <- max(sapply(res1$tabs, nrow))
max_1

LIMIT <- 110
max_1 > LIMIT
# split_tab 

res_f <- split_tab(res = res1,
                   var_fus = paste(res1$fus_vars[1],
                                   res1$fus_vars[2],
                                   sep = res1$sep),
                   LIMIT = LIMIT)

max_2 <- max(sapply(res_f$tabs, nrow))
max_2

# On a bien de plus petites tables si la limite n'était pas respectée
max_2 < max_1 | max_1 < LIMIT

# On observe si on split bien les tables
data_fus<-passer_a_4_ou_5(res_f$tabs,res_f)
dim(setdiff(data_fus,data))[1] == 0 #TRUE il ny aucune lignes différentes

# On observe si les variables sont les bonnes 

all(names(res_f$vars)==names(res_f$tabs))
all(sapply(1:length(res_f$tabs), function(i) {
  sort(res_f$vars[[i]]) == sort(names(res1$totcode)) &
    names(res_f$tabs[[i]]) == names(res1$tabs[[1]])
}))

names(res_f$tabs) # tab 2 a été splité


tab_non_changee <- lapply(res1$tabs, function(df) {
  if(nrow(df) < LIMIT) {
    return(df)
  } else {
    return(NULL)
  }
}) %>% 
  Filter(Negate(is.null), .) %>% # on enlève les NULL
  names

# les tables qui n'ont pas été touchée... 
# n'ont pas eu leur hrc altéré
all(sapply(tab_non_changee, function(x){
  all(
    names(res_f$hrcs$tab_non_changee) == names(res1$hrcs$tab_non_changee),
    all(sapply(names(res_f$hrcs$tab_non_changee), 
               function(y) res_f$hrcs$tab_non_changee[[y]] == res1$hrcs$tab_non_changee[[y]]))
  )
}))

# les tables qui n'ont pas été touchée...
# n'ont pas eu leur alt_tot altéré
all(sapply(tab_non_changee, function(x){
  all(
    names(res_f$alt_tot$tab_non_changee) == names(res1$alt_tot$tab_non_changee),
    all(sapply(names(res_f$alt_tot$tab_non_changee), 
               function(y) res_f$alt_tot$tab_non_changee[[y]] == res1$alt_tot$tab_non_changee[[y]]))
  )
}))

# totaux inchangés
alt_tot_to_1_to_4 <- res1$alt_tot[1:4]
alt_tot_to_8_to_12 <- res1$alt_tot[8:12]
alt_tot_to_6 <-res1$alt_tot[[6]]

# totaux créés, rajoutés à la fin
alt_tot_prevu5<- c("A_+_Total","A1_+_Total","A2_+_Total","A3_+_Total")
alt_tot_prevu7<- c("A_+_GA","A1_+_GA","A2_+_GA","A3_+_GA")

all(unname(unlist(res_f$alt_tot)) == unname(unlist(c(alt_tot_to_1_to_4,
                                                     alt_tot_to_6,
                                                     alt_tot_to_8_to_12,
                                                     alt_tot_prevu5,
                                                     alt_tot_prevu7)))
    )

# TEST 5 DIMENSIONS -  DEUX COUPLES DE VARIABLES FUISONNES -------------------------------------------------------------------------

#DONNEES 

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
                            hrc_dir = "test/test_fonction_general/test1",vec_sep =  c("\\___"),
                            nb_tab = 'min')
length(res5_3$tabs)
sapply(res5_3$tabs, nrow)

res5_3$fus_vars
res5_3$sep

# Première réduction  -------------------------------------------------------------------------

max_1 <- max(sapply(res5_3$tabs, nrow))
max_1

LIMIT <- 1000
max_1 > LIMIT
# split_tab 

res_f <- split_tab(res = res5_3,
                   var_fus = "AGE___ECO",
                   LIMIT = LIMIT)

max_2 <- max(sapply(res_f$tabs, nrow))
max_2

# On a bien de plus petites tables si la limite n'était pas respectée
max_2 < max_1 | max_1 < LIMIT

# On observe si on split bien les tables
data_fus<-passer_a_4_ou_5(res_f$tabs,res_f)
dim(setdiff(data_fus,data))[1] == 0 #TRUE il ny aucune lignes différentes

# On observe si les variables sont les bonnes 

all(names(res_f$vars)==names(res_f$tabs))
all(sapply(1:length(res_f$tabs), function(i) {
  sort(res_f$vars[[i]]) == sort(names(res5_3$totcode)) &
    names(res_f$tabs[[i]]) == names(res5_3$tabs[[1]])
}))

names(res_f$tabs) # les tabs 1, 3, 5,13,15 et 17 ont été splitées


tab_non_changee <- lapply(res5_3$tabs, function(df) {
  if(nrow(df) < LIMIT) {
    return(df)
  } else {
    return(NULL)
  }
}) %>% 
  Filter(Negate(is.null), .) %>% # on enlève les NULL
  names

# les tables qui n'ont pas été touchée... 
# n'ont pas eu leur hrc altéré
all(sapply(tab_non_changee, function(x){
  all(
    names(res_f$hrcs$tab_non_changee) == names(res5_3$hrcs$tab_non_changee),
    all(sapply(names(res_f$hrcs$tab_non_changee), 
               function(y) res_f$hrcs$tab_non_changee[[y]] == res5_3$hrcs$tab_non_changee[[y]]))
  )
}))

# les tables qui n'ont pas été touchée...
# n'ont pas eu leur alt_tot altéré
all(sapply(tab_non_changee, function(x){
  all(
    names(res_f$alt_tot$tab_non_changee) == names(res5_3$alt_tot$tab_non_changee),
    all(sapply(names(res_f$alt_tot$tab_non_changee), 
               function(y) res_f$alt_tot$tab_non_changee[[y]] == res5_3$alt_tot$tab_non_changee[[y]]))
  )
}))


# totaux inchangés
alt_tot_2 <-res5_3$alt_tot[[2]]
alt_tot_4 <-res5_3$alt_tot[[4]]
alt_tot_6_to_12 <- res5_3$alt_tot[6:12]
alt_tot_14 <-res5_3$alt_tot[[14]]
alt_tot_16 <-res5_3$alt_tot[[16]]
alt_tot_18_to_24 <-res5_3$alt_tot[18:24]

# On vérifie que les totaux inchangés sont bien ok
# on ne vérifie pas les totaux changés, on l'a fait dans le cas dimension 4
all(unname(unlist(res_f$alt_tot[1:18])) == unname(unlist(c(alt_tot_2,
                                                           alt_tot_4,
                                                           alt_tot_6_to_12,
                                                           alt_tot_14,
                                                           alt_tot_16,
                                                           alt_tot_18_to_24)))
)

# Seconde Réduction -------------------------------------------------------

max_2 <- max(sapply(res_f$tabs, nrow))
max_2

LIMIT <- 440

max_2 > LIMIT

res_f2 <-split_tab(res = res_f ,
                      var_fus = "ACT___GEO",
                      LIMIT = LIMIT)



max_3 <- max(sapply(res_f2$tabs, nrow))
max_3

# On a bien de plus petites tables si la limite n'était pas respectée
max_3 < max_2 | max_2 < LIMIT


# On observe si on split bien les tables
data_fus<-passer_a_4_ou_5(res_f2$tabs,res_f2)
dim(setdiff(data_fus,data))[1] == 0 #TRUE il ny aucune lignes différentes

# On observe si les variables sont les bonnes 

all(names(res_f2$vars)==names(res_f2$tabs))
all(sapply(1:length(res_f$tabs), function(i) {
  sort(res_f2$vars[[i]]) == sort(names(res5_3$totcode)) &
    names(res_f2$tabs[[i]]) == names(res5_3$tabs[[1]])
}))

names(res_f2$tabs) # les tabs 1, 3, 5,13,15 et 17 ont été splitées


tab_non_changee <- lapply(res_f$tabs, function(df) {
  if(nrow(df) < LIMIT) {
    return(df)
  } else {
    return(NULL)
  }
}) %>% 
  Filter(Negate(is.null), .) %>% # on enlève les NULL
  names

# les tables qui n'ont pas été touchée... 
# n'ont pas eu leur hrc altéré
all(sapply(tab_non_changee, function(x){
  all(
    names(res_f$hrcs$tab_non_changee) == names(res5_3$hrcs$tab_non_changee),
    all(sapply(names(res_f$hrcs$tab_non_changee), 
               function(y) res_f$hrcs$tab_non_changee[[y]] == res5_3$hrcs$tab_non_changee[[y]]))
  )
}))

# les tables qui n'ont pas été touchée...
# n'ont pas eu leur alt_tot altéré
all(sapply(tab_non_changee, function(x){
  all(
    names(res_f$alt_tot$tab_non_changee) == names(res5_3$alt_tot$tab_non_changee),
    all(sapply(names(res_f$alt_tot$tab_non_changee), 
               function(y) res_f$alt_tot$tab_non_changee[[y]] == res5_3$alt_tot$tab_non_changee[[y]]))
  )
}))

# les tables créés sont ajouté à la fin ce qui rend plus facile de les repérer
tab_split_deux_fois <- names(res_f2$tabs[55:126])

all(
  sapply(tab_split_deux_fois,
         is.null(res_f2$hrcs))
)

# TEST 5 DIMENSIONS ET UNE VARIABLES FUISONNES -------------------------------------------------------------------------

# DONNEES
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

res5_3_ <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                            vars_a_fusionner = c("AGE","ECO","ACT"),
                            sep_dir = sep_dir,
                            hrc_dir = hrc_dir)

res_test <- split_tab(res = res5_3,
                      var_fus = "ACT_+_AGE_+_ECO",
                      LIMIT = 1000)

