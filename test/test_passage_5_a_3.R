# Vider l'environnement global
rm(list = ls())

library(dplyr)
source(file = "R/passage_5_3.R", encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R", encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R", encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R", encoding = "UTF-8")
source("R/cas_gen_4_3.R", encoding = "UTF-8")
source("R/format.R", encoding = "UTF-8")
source("test/test_nbs_tabs.R", encoding = "UTF-8")

# Donnée 1 ----------------------------------------------------------------

data <- expand.grid(
  ACT = c(
    "KEBAB",
    read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  SEX = c(
    "Total",
    read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  GEO = c(
    "Pays",
    read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  AGE = c("Ensemble", "adulte", "enfant"),
  ECO = c("PIB", "riche", "pauvre"),
  stringsAsFactors = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc")

tot_code <-
  c(
    SEX = "Total",
    AGE = "Ensemble",
    GEO = "Pays",
    ACT = "KEBAB",
    ECO = "PIB"
  )

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

# obtention de V1 et v2
var_cat <- names(totcode)
var_sans_hier <- intersect(setdiff(names(dfs), names(hrcfiles)),
                           var_cat)
dfs_var_sans_hier <- subset(dfs, select = var_sans_hier)
# res_var<-get_2_smallest(hrcfiles,totcode)
# v1 <- names(res_var)[[1]]
# v2 <- names(res_var)[[2]]

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE
v1 = NULL
v2 = NULL
v3 = NULL
v4 = NULL
sep = "_"

# test 1 : arguments ------------------------------------------------------------------

res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)
all(sort(unlist(res$vars)) == sort(unlist(list("AGE","GEO","ACT","ECO"))))
length(res$tabs) == 4 * nb_noeuds(hrcfiles = hrcfiles, v="ACT") * 
                        nb_noeuds(hrcfiles = hrcfiles, v="GEO") *
                        nb_noeuds(hrcfiles = hrcfiles, v="AGE") * 
                        nb_noeuds(hrcfiles = hrcfiles, v="ECO")


# Vérification priorité var hierarchique
res2 <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name, select_hier = TRUE)
all(sort(unlist(res2$vars)) == sort(unlist(list("AGE","GEO","ACT","SEX"))))
length(res2$tabs) == 4 * nb_noeuds(hrcfiles = hrcfiles, v="ACT") * 
                         nb_noeuds(hrcfiles = hrcfiles, v="GEO") *
                         nb_noeuds(hrcfiles = hrcfiles, v="AGE") * 
                         nb_noeuds(hrcfiles = hrcfiles, v="SEX")

# C'est bon
# On a bien AGE et ACT selectionné pour 4 à 3. On remarque que v4 = ACT (et non v3)
# car pour que cas_1_nno_hrc fonctionne, il faut que v2 soit hrc, et v1 non hrc !

str(res$hrcs5_4)
str(res$hrcs4_3)
str(res$alt_tot5_4)
str(res$alt_tot4_3)

res<-format(res,nom_dfs)
# test séparateur
res_plusplus_ <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name, sep = "+++")

# Les 5 premières lignes ont bien le séparateur
all(unlist(lapply(1:5, function(i) str_detect(data[i,][[1]], "\\+++")))) == FALSE

all(unlist(lapply(1:5, function(i) str_detect(res_plusplus_$tabs$nom_data_frame_AGE_KEBAB_Pays_ACT$`AGE+++ECO`[i], "\\+++"))))
all(unlist(lapply(1:5, function(i) str_detect(res_plusplus_$tabs$nom_data_frame_AGE_KEBAB_Pays_ACT$`ACT+++GEO`[i], "\\+++"))))

# il y a bien une colone avec le séparateur
any(str_detect(names(data), "\\+++")) == FALSE
any(str_detect(names(res_plusplus_$tabs$nom_data_frame_AGE_KEBAB_Pays_ACT), "\\+++"))


(names(res_plusplus_$tabs$nom_data_frame_AGE_Total_Pays_ACT))

res_SEX_AGE_ECO <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE,hrc_dir = dir_name, v3 = "SEX",v4 = "AGE_ECO")
"SEX_AGE_ECO" %in% names(res_SEX_AGE_ECO$tabs$nom_data_frame_AGE_Total_Ensemble_PIB_SEX)
(names(res_SEX_AGE_ECO$tabs$nom_data_frame_AGE_Total_Ensemble_PIB_SEX))

# test des erreurs
res_ACT_ACT <-
  passer_de_5_a_3_var(
    dfs,
    nom_dfs,
    totcode,
    hrcfiles,
    sep_dir = TRUE,
    hrc_dir = dir_name,
    v3 = "ACT",
    v4 = "ACT"
  )
# Erreur. Vous essayez de fusionner une variable avec elle-même
res_MAVAR3 <-
  passer_de_5_a_3_var(
    dfs,
    nom_dfs,
    totcode,
    hrcfiles,
    sep_dir = TRUE,
    hrc_dir = dir_name,
    v3 = "MAVAR"
  )
# v3 n'est pas une variable catégorielle
res_MAVAR4 <-
  passer_de_5_a_3_var(
    dfs,
    nom_dfs,
    totcode,
    hrcfiles,
    sep_dir = TRUE,
    hrc_dir = dir_name,
    v4 = "MAVAR"
  )
# v4 n'est pas une variable catégorielle


# test 2 : Résultats ------------------------------------------------------

# bon format
str(res)


# nombre tableau cohérent
# Passage 5 -> 4
# AGE, ECO non hrc => 2 tableaux crées lors de leur fusion
# Passage 4 -> 3
# GEO, ACT var hrc moins de noeuds : 3, 2 => 3 * 2 combinaisons de hrc simple crée
# que l'on décline en 2 fois à chaque fois
# (à cause de la fusion des 2 vars pour tenir compte de la hierarchie créée)
# 2 * 3 * 2 * 2 = 24, le compte est bon
length(res$tabs)

#On vérifie les noms des différents fichiers
res <- format(res, nom_dfs)
#On vérifie les noms des différents fichiers
all(names(res$tabs) == names(res$hrcs))
all(names(res$tabs) == names(res$alt_tot))

lapply(res$hrcs, function(tab) {
  t <- "AGE_ECO" %in% names(tab)
  return(t)
})

lapply(res$hrcs, function(tab) {
  t <- "ACT_GEO" %in% names(tab)
  return(t)
})

lapply(res$alt_tot, function(tab) {
  t <- "AGE_ECO" %in% names(tab)
  return(t)
})

lapply(res$alt_tot, function(tab) {
  t <- "ACT_GEO" %in% names(tab)
  return(t)
})


#On vérifie que les sous_totaux sont les bons
res_sdc <-
  sdcHierarchies::hier_import(inp = hrcfiles[["ACT"]], from = "hrc", root = "KEBAB") %>%
  sdcHierarchies::hier_convert(as = "sdc")

res_sdc2 <-
  sdcHierarchies::hier_import(inp = hrcfiles[["GEO"]], from = "hrc", root = "Pays") %>%
  sdcHierarchies::hier_convert(as = "sdc")

codes_split <- lapply(res_sdc$dims,
                      names)

codes_split2 <- lapply(res_sdc2$dims,
                       names)

l <- c()



for (j in (1:length(codes_split))) {
  for (i in (1:length(codes_split2))) {
    l <- c(l, paste(codes_split[[j]][1], codes_split2[[i]][1], sep = "_"))
    l <- c(l, paste(codes_split[[j]][1], codes_split2[[i]][1], sep = "_"))
  }
}
#Les différents totaux
(l)

l <- rep(l, 2)
#on a les bons all_tot si t=True
for (i in (1:length(res$alt_tot))) {
  t <- unname(res$alt_tot[[i]][1]) == l[i]
  if (t == FALSE) {
    return(t)
  }
}
(t)


#LEs sous_totaux de la variable AGE_ECO
for (i in (1:length(res$alt_tot))) {
  t <- unname(res$alt_tot[[i]][2]) == "Ensemble_PIB"
  if (t == FALSE) {
    return(t)
  }
}
(t)



#On vérifie si on a toutes les lignes dans le tableaux

liste_tab <- lapply(res$tabs, function(tab) {
  tab <- separate(tab, ACT_GEO, into = c("ACT", "GEO"), sep = "_")
  tab <- separate(tab, AGE_ECO, into = c("AGE", "ECO"), sep = "_")
  return (tab)
})

tab <- unique(bind_rows(liste_tab)) %>%
  arrange(ECO, AGE, SEX, ACT, GEO, VALUE)


data_range <- data  %>% arrange(ECO, AGE, SEX, ACT, GEO)

a2 <-
  full_join(tab, data_range, by = c("ECO", "AGE", "SEX", "ACT", "GEO", "VALUE"))

verif <- unlist(lapply(a2, function(col)
  sum(is.na(col))))

(verif)
# Les deux tables sont-elles bien les tables attendues ?

s <- 0
# Tests de composition: ok pour les cellules
# La correspondance entre hrc et tableaux

l <- list()

for (t in names(res$tabs)) {
  p1 <- res$tabs[[t]] %>%
    filter(AGE_ECO != res$alt_tot[[t]]$AGE_ECO) %>%
    pull(AGE_ECO) %>%
    unique() %>%
    sort() %>%
    `==`(read.table(res$hrcs[[t]]$AGE_ECO) %>%
           mutate(V1 = gsub("@", "", V1)) %>%
           pull(V1) %>%
           sort()) %>%
    all()
  p2 <- res$tabs[[t]] %>%
    filter(ACT_GEO != res$alt_tot[[t]]$ACT_GEO) %>%
    pull(ACT_GEO) %>%
    unique() %>%
    sort() %>%
    `==`(read.table(res$hrcs[[t]]$ACT_GEO) %>%
           mutate(V1 = gsub("@", "", V1)) %>%
           pull(V1) %>%
           sort()) %>%
    all()
  l <- append(l, p1)
  l <- append(l, p2)
}
(l)
#On s'attend à avoir une liste de 48 éléments 2*(length(res$tabs)) contenant que des TRUE
l <- list()
# Les tables sont-elles les tables attendues


# Donnée 2 ----------------------------------------------------------------

# Test pour savoir si on peut fusionner 3 variables ensembles :)
# TEST AVEC 3 HRC
data <- expand.grid(
  ACT = c(
    "Total",
    read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  SEX = c(
    "Total",
    read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  GEO = c(
    "Total",
    read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  AGE = c("Ensemble", "adulte", "enfant"),
  ECO = c("Ensemble", "riche", "pauvre"),
  stringsAsFactors = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc3.hrc", GEO = "hrc/hrc3.hrc", SEX = "hrc/hrc3.hrc")

tot_code <-
  c(
    SEX = "Total",
    AGE = "Ensemble",
    GEO = "Total",
    ACT = "Total",
    ECO = "Ensemble"
  )

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

# obtention de V1 et v2
var_cat <- names(totcode)
var_sans_hier <- intersect(setdiff(names(dfs), names(hrcfiles)),
                           var_cat)
dfs_var_sans_hier <- subset(dfs, select = var_sans_hier)
# res_var<-get_2_smallest(hrcfiles,totcode)
# v1 <- names(res_var)[[1]]
# v2 <- names(res_var)[[2]]

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE

v1 <- NULL
v2 <- NULL
v3 <- NULL
v4 <- NULL

sep = "_"


# test 3 : 3 var fusionnées -----------------------------------------------

res5_4 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name)

# On vérifie que la variable fusionnée a moins de noeuds en moyenne que les autres variables
test_nb_tabs_3hrc(res5_4$hrcs, names(res5_4$hrcs)[1], res5_4$alt_tot)
test_nb_tabs_3hrc(res5_4$hrcs, names(res5_4$hrcs)[2], res5_4$alt_tot)

test_nb_tabs_3hrc(hrcfiles, names(hrcfiles)[1], totcode)
test_nb_tabs_3hrc(hrcfiles, names(hrcfiles)[2], totcode)
test_nb_tabs_3hrc(hrcfiles, names(hrcfiles)[3], totcode)

# On obtient bien
# Passage 4 à 3: chr [1:2] "AGE_ECO" "ACT"
res5_3 <-
  passer_de_5_a_3_var(dfs,
                      nom_dfs,
                      totcode,
                      hrcfiles,
                      sep_dir = TRUE,
                      hrc_dir = dir_name)


# Donnée 3 : 0 hrc --------------------------------------------------------

data <- expand.grid(
  ACT = c("Ensemble", "Est", "Ouest", "Est1", "Ouest1", "Est2", "Ouest2"),
  SEX = c("Ensemble", "Nord", "Sud", "Nord1", "Sud1", "Nord2"),
  GEO = c("Ensemble", "Oui", "Non", "Oui1", "Non1"),
  AGE = c("Ensemble", "adulte", "enfant"),
  ECO = c("Ensemble", "riche", "pauvre", "autre"),
  stringsAsFactors = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrcfiles <- NULL

totcode <-
  c(
    SEX = "Ensemble",
    AGE = "Ensemble",
    GEO = "Ensemble",
    ACT = "Ensemble",
    ECO = "Ensemble"
  )

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE
v1 <- "AGE"
v2 <- "ECO"
v3 <- "AGE_ECO"
v4 <- "GEO"

sep = "_"


# test 3 ------------------------------------------------------------------


res5_3 <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, sep_dir = sep_dir, 
                              hrc_dir = dir_name, v1 = v1,v2 = v2,
                              v3 = v3, v4 = v4)
str(res5_3)
# 14 hrc
# 6 AGE
# 8 ECO

res5_4 <-
  passer_de_4_a_3_var(dfs,
                      nom_dfs,
                      totcode,
                      hrcfiles,
                      sep_dir = TRUE,
                      hrc_dir = dir_name)

nb_noeuds <- lapply(names(res5_4$hrcs),
                    function(x)
                      test_nb_tabs_3hrc(res5_4$hrcs, x, res5_4$alt_tot))

rep(res5_4$hrcs[[1]], nb_noeuds[1])

unlist(lapply(1:length(res5_4$hrcs), function(i)
  rep(res5_4$hrcs[[i]], nb_noeuds[i])))



# Donnée 4 ----------------------------------------------------------------


data <- expand.grid(
  ACT = c(
    "Total",
    read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  SEX = c(
    "Total",
    read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  GEO = c(
    "Pays",
    read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
  ),
  AGE = c("Ensemble", "adulte", "enfant"),
  ECO = c("Ensemble", "riche", "pauvre", "autre"),
  stringsAsFactors = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc")

tot_code <-
  c(
    SEX = "Total",
    AGE = "Ensemble",
    GEO = "Pays",
    ACT = "Total",
    ECO = "Ensemble"
  )

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE
v1 = "AGE"
v2 = "GEO"
v3 = "ACT"
v4 = "SEX"
sep = "_"

# test 4 ------------------------------------------------------------------


res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name,
                           v1 = v1, v2 = v2,v3 = v3,v4 = v4)
str(res)
