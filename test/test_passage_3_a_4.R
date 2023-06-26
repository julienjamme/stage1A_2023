# Vider l'environnement global
rm(list = ls())


library(dplyr)
source("finaux/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:26]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")

tot_code<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

var_sans_hier <- names(tot_code)[1:2]
v1 <- var_sans_hier[1]
v2 <- var_sans_hier[2]

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "output"


res <- list(passage_4_3_cas_2_non_hr(data,nom_dfs,v1,v2, tot_code,dir_name))


dfs_union <- res[[1]][[1]][[1]]

######################################################
modalité_nom_var <- names(data)
modalité_SEXE <- unique(data$SEX)
modalité_AGE <- unique(data$AGE)
######################################################

#Helped by GPT
string <- "A_B_C_DDD_E_F"

# Diviser la chaîne en sous-chaînes en utilisant "_"
elements <- strsplit(string, "_")[[1]]

# Générer les différentes combinaisons possibles sous forme de liste de listes
combinations <- lapply(1:(length(elements)-1), function(i) {
  list(paste(elements[1:i], collapse = "_"), paste(elements[(i+1):length(elements)], collapse = "_"))
})

# Afficher les combinaisons
print(combinations)

###

generate_combinations <- function(string, separator="_") {
  # Diviser la chaîne en sous-chaînes en utilisant le séparateur
  elements <- strsplit(string, separator)[[1]]
  
  # Générer les différentes combinaisons possibles sous forme de liste de listes
  combinations <- lapply(1:(length(elements)-1), function(i) {
    list(paste(elements[1:i], collapse = separator), paste(elements[(i+1):length(elements)], collapse = separator))
  })
  
  return(combinations)
}

######################################################

division <- generate_combinations("SEX_AGE")

a_été_divisé <- function(liste_mot, liste_mod1, liste_mod2){
  return(liste_mot[[1]] %in% liste_mod1 & 
         liste_mot[[2]] %in% liste_mod2)
}

a_été_divisé(division[[1]], modalité_nom_var,modalité_nom_var)

division[[1]][[1]] %in% modalité_nom_var
