library(dplyr)

source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")

# Donnée 1 : cas 2 var non hier -------------------------------------------

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
hrc_dir = "hrc_alt"
hier = TRUE
v1 = NULL
v2 = NULL


# test 1 ------------------------------------------------------------------

res <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir,
                           hrc_dir = "hrc_dir")
length(res$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v="SEX") * nb_noeuds(hrcfiles = hrcfiles, v="AGE")
all(sort(res$vars) == sort(unlist(list("AGE","SEX"))))

# On privilégie les var hierarchiques
res2 <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir,
                            hrc_dir = "hrc_dir", maximize_nb_tabs = TRUE)
length(res2$tabs) == 2 * nb_noeuds(hrcfiles = hrcfiles, v="ACT") * nb_noeuds(hrcfiles = hrcfiles, v="GEO")
all(sort(res2$vars) == sort(unlist(list("ACT","GEO"))))

# Vérification vis à vis de la selection des variables
res_ACT1 <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir, hrc_dir = "hrc_dir",v1 = "ACT" )
res_ACT1$vars[[1]] == "ACT"

res_ACT2 <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir, hrc_dir = "hrc_dir",v2 = "ACT" )
res_ACT2$vars[[2]] == "ACT"

res_ACT_GEO <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir, hrc_dir = "hrc_dir",v1 = "ACT", v2 = "GEO" )
res_ACT_GEO$vars[[1]] == "ACT" & res_ACT_GEO$vars[[2]] == "GEO"

# Test des erreurs
res_ACT_ACT <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir, hrc_dir = "hrc_dir",v1 = "ACT", v2 = "ACT" )
#Erreur. Vous essayez de fusionner une variable avec elle-même
res_MAVAR <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir, hrc_dir = "hrc_dir",v1 = "MAVAR" )
# v1 n'est pas une variable catégorielle
res_MAVAR <- passer_de_4_a_3_var(data,"mon_data_frame",totcode, hrcfiles,sep_dir = sep_dir, hrc_dir = "hrc_dir",v2 = "MAVAR" )
# v2 n'est pas une variable catégorielle

res2 <- passage_4_3_cas_2_non_hr(data,"mon_data_frame","SEX","AGE", totcode, dir_name = "hrc_dir")

identical(res,res2)
str(res)

# Donnée 2 : cas 1 var non hier -------------------------------------------


data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrcfiles = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc" )

totcode<-c(SEX="Total",AGE="Ensemble", GEO="Pays", ACT="Total")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir = TRUE
hrc_dir = "hrc_alt"

var_cat <- names(totcode)

var_sans_hier <- intersect(
  setdiff(names(dfs), names(hrcfiles)),
  var_cat
)

v1 <- var_sans_hier[1]
v2<- plus_petit_hrc(hrcfiles)
hier = TRUE
# v1 = NULL
# v2 = NULL


# test 2 ------------------------------------------------------------------

res <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles,sep_dir = sep_dir, hrc_dir = hrc_dir)
res2 <- passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,hrc_dir)

identical(res,res2)

str(res)

# On privilégie les var hierarchiques
nb_noeuds(hrcfiles[1], hrc_name=FALSE)
nb_noeuds(hrcfiles[2], hrc_name=FALSE)
nb_noeuds(hrcfiles[3], hrc_name=FALSE)
# -> on devrait choisir GEO et SEX si l'on privilégie le nombre de tableau (hier=TRUE)
res3 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles,sep_dir = sep_dir,
                            hrc_dir = hrc_dir, maximize_nb_tabs =TRUE)
# On a bien choisit SEX puis GEO (SEX a plus de noeuds que GEO donc a été choisis en premier)

# donnée 3 : cas 0 var non hier ----------------------------------------------------------------

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
dir_name <- "hrc_alt"


# test 3 ------------------------------------------------------------------

res <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE,hrc_dir = dir_name)
res2 <- passage_4_3_cas_0_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name)

identical(res,res2)

str(res)

# test 4 : cas hrcfiles = NULL --------------------------------------------


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
hrcfiles <- NULL
totcode<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir <- TRUE
dirname <- "output"
hrc_dir <- "output"

res <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE,hrc_dir = dir_name)
