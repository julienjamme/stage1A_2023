
hrc <- "brouillon/ACT3.hrc"
total = "Big_Total"

#### playground

res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
  sdcHierarchies::hier_convert(as = "sdc")

noeuds <- lapply(res_sdc$dims,names)

# Obtenir le nombre de niveau
nb_level <- readLines(hrc) %>% sapply(function(x) sum(strsplit(x, "")[[1]] == "@")) %>% max

# Selectionner les différents niveaux
niveau_1 <- noeuds[sapply(noeuds, function(x) x[1] == total)]

niveau_2 <- noeuds[sapply(noeuds, function(x) x[1] %in% setdiff(unlist(niveau_0), total))]

niveau_3 <- noeuds[sapply(noeuds, function(x) x[1] %in% setdiff(unlist(niveau_1), unlist(niveau_0)))]

niveau_4 <- noeuds[sapply(noeuds, function(x) x[1] %in% setdiff(unlist(niveau_2), unlist(niveau_1)))]



# Fonctioooooooooon -------------------------------------------------------

# Fusion de level consécutif pour faire des hiérarchie de profondeur
# ex : faire des groupes {Région, Département, Commune} à partir de
# {Pays, Région, Département, Commune, Quartier}
fusionner_level_con <- function(level1,level2){
  
  new_level <- lapply(level1, 
                     function(x){for (i in 1:length(level2)){
                       
                       if (level2[[i]][1] %in% x){
                         x <- c(x, level2[[i]][2:length(level2[[i]])])
                       }
                     }
                       return(x)
                     }
  )
  
}


# Fusion de levels consécutifs pour faire des hiérarchie de profondeur
# input de la forme leveli, leveli+1,..., levelp
# ex : faire des groupes {Région, Département, Commune, Quartier} à partir de
# {Pays, Région, Département, Commune,Quartier}
fusionner_level_list <- function(...){
  list_level = list(...)
  niveaux <- list_level[[1]]
  for (i in 2:length(list_level)){
    niveaux <- fusionner_level_con(niveaux, list_level[[i]])
  }
  return(niveaux)
}

####

mod <- fusionner_level_list(niveau_0, niveau_1, niveau_2, niveau_3)

df <- as.data.frame(x = mod)
names(df) <- "mod"

# Par exemple si on veut filtrer sur les premiers niveaux
sapply(fusionner_level_list(niveau_1,niveau_2), function(x) df %>% filter(mod %in% x))

sapply(fusionner_level_list(niveau_2,niveau_3), function(x) df %>% filter(mod %in% x))

###

# Automatisation de la création de level_1, level_2, etc
extract_levels <- function(hrc, total) {
  # Import and convert the hierarchy
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  
  # Convert the list to a character vector
  noeuds <- lapply(res_sdc$dims, names)
  
  # Get the number of levels
  nb_level <- readLines(hrc) %>% sapply(function(x) sum(strsplit(x, "")[[1]] == "@")) %>% max
  
  # Initialize an empty list to hold all the levels
  levels <- vector("list", nb_level)
  names(levels) <- paste0("level_", seq(1, nb_level))
  
  # The first level is just the root
  levels[["level_1"]] <- noeuds[sapply(noeuds, function(x) x[1] == total)]
  
  # Calculate the other levels
  for (i in 1:nb_level) {
    current_names <- unique(unlist(levels[[paste0("level_", i)]]))
    parent_names <- if (i > 1) unique(unlist(levels[[paste0("level_", i - 1)]])) else total
    levels[[i+1]] <- noeuds[sapply(noeuds, 
                                   function(x) x[1] %in% setdiff(unlist(current_names), 
                                                                 unlist(parent_names)))]
    names(levels)[i+1] <- paste0("level_", i+1)
  }
  
  return(levels)
}

a <- extract_levels("hrc/test.hrc","A")
extract_levels("hrc/hrc1.hrc","Total")


# -------------------------------------------------------------------------


library(dplyr)
library(data.table)
library(stringr)
source(file = "R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/reduce_dims.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/length_tabs.R",encoding = "UTF-8")
source("R/nb_tab.R",encoding = "UTF-8")
source("R/chercher_combinaison_variable_a_fusionner.R",encoding = "UTF-8")
library(tictoc)

data <- expand.grid(
  ACT = c("KEBAB",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("PIB","riche","pauvre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))

hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc")

tot_code<-c(AGE="Ensemble", GEO="Pays", ACT="KEBAB", ECO = "PIB")

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "OPPO"

totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "test/test_fonction_general/test1"
hrc_dir <- dir_name
sep_dir <- TRUE


res4_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles, 
                            sep_dir= TRUE,
                            hrc_dir = "test/test_fonction_general/test1",vec_sep =  c("\\_+"),
                            nb_tab = 'min')

list_nrow <- lapply(res4_3$tabs,nrow)

LIMIT = 90
lapply(1:length(res4_3$tabs), function(x) 
  if (nrow(res4_3$tabs[[x]]) >= LIMIT){
    
    hrc <-  res4_3$hrcs[[x]]
    #print(hrc)
    level <- extract_levels(hrc = hrc[[1]],
                   total = res4_3$totcode[[names(hrc)]])
    
    res4_3$tabs[[x]] <- NULL
    return(TRUE)
  } else {
    return(FALSE)
  }
)

res4_3$tabs <- lapply(1:length(res4_3$tabs), function(x) {
  if (nrow(res4_3$tabs[[x]]) >= LIMIT){
    
    hrc <-  res4_3$hrcs[[x]]
    level <- extract_levels(hrc = hrc[[1]],
                            total = res4_3$totcode[[names(hrc)]])
    
    return(NULL)
  } else {
    return(res4_3$tabs[[x]])
  }
}
)

# Using Filter
res4_3$tabs <- Filter(Negate(is.null), res4_3$tabs)

tab <- res4_3$tabs[[1]]

hrc <-  res4_3$hrcs[[1]]
level <- extract_levels(hrc = hrc[[1]],
                        total = res4_3$totcode[[names(hrc)]])

level_u <- unlist(level, recursive  = FALSE)

new_var <- names(res4_3$hrcs[[1]])


# TODO: faire un codesplit intelligent en regroupant des niveaux
# pour ne pas être trop fin
# d'abord level0, level1...leveln
# puis level0, level1, level2...leveln


tab_splite <- lapply(
  level_u,
  function(codes){
    res <- tab %>% 
      filter(tab[[new_var]] %in% codes)
  }
)


###


# Collecte de variable hiérarchique créé
new_var <- names(res4_3$hrcs[[1]])

# Le split
tab_splite <- lapply(
  codes_split,
  function(codes){
    res <- dfs %>% 
      filter(dfs[[new_var]] %in% codes)
  }
)


# -------------------------------------------------------------------------

res4_3 <- gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles, 
                            sep_dir= TRUE,
                            hrc_dir = "test/test_fonction_general/test1",vec_sep =  c("\\_+"),
                            nb_tab = 'max')

tab <- res4_3$tabs[[1]]

hrc <-  res4_3$hrcs[[1]]
level <- extract_levels(hrc = hrc[[1]],
                        total = res4_3$totcode[[names(hrc)]])

level_u <- unlist(level, recursive  = FALSE)

new_var <- names(res4_3$hrcs[[1]])


# TODO: faire un codesplit intelligent en regroupant des niveaux
# pour ne pas être trop fin
# d'abord level0, level1...leveln
# puis level0, level1, level2...leveln


tab_splite <- lapply(
  level_u,
  function(codes){
    res <- tab %>% 
      filter(tab[[new_var]] %in% codes)
  }
)

# Rq : la variable nouvellement créé est forcément de profondeur 1 (level0 et level1)
# donc pas besoin de regrouper des niveaux :)

# La variable n'est plus hiérarchique
new_hrc <- NULL

# copiage hrc var caté
new_hrcfile <- rep(res4_3$hrcfile[[1]], times = length(level_u))
# -> bug s'il n'y a pas de var hier


new_vars <- rep(list(res4_3$vars[[1]]), times = length(level_u))

# pour totcode
new_altot <- lapply(level_u, function(x) return(x[[1]]))

# TODO: il faut trouver un nom aux fichiers créés...
# juste rajouter des 1...n au nom existant ?

# TODO: faire une fonction prenant un res en entré, une limite, une variable catégo
# et un booléen spécifiant si la variable est de départ ou a été créé par l'algo
# et qui renvoie le res avec des tabs spiltée si nécessaire

split_grandes_tables <- function(res,LIMIT, var, var_depart=TRUE){
  require(sdcHierarchies)
  totcode<-res$totcode
  hrcfiles<-res$hrcfile
  list_niv<-extract_levels(hrcfiles[[var]],totcode)
  hrc <- hrcfiles[[var]]
  i<-1
  while(i<length(list_niv) & Limit pas atteinte){
    
    
    total <- list_niv[[i]][1]
    
    res_sdc <- list_niv[[i]]
    
    # Code split nous donne les hiérarchies ainsi que les niveaux de la hiérarchie
    # Permet de selectionn un noeud de l'arbre et ses branches directes
    codes_split <- lapply(
      res_sdc$dims,
      names
    )
    
    ###########################
    # Réduction de hierarchie #
    ###########################
    
    liste_df_4_var_2_non_hr <- lapply(
      codes_split,
      function(codes){
        res <- dfs %>% 
          filter(dfs[[v2]] %in% codes)
      }
    )
    
    
    i<-i+1
  } 
  
  
  return(TRUE)
}
