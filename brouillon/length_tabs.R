# Fonction pour gérer l'importation de la hiérarchie
import_hierarchy <- function(hrcfile) {
  total <- "BIG_Total"
  res_sdc <- sdcHierarchies::hier_import(inp = hrcfile, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  # Stocker tous les ensembles de parents + enfant direct
  levels <- lapply(res_sdc$dims, names)
  return(levels)
}



#' entrée : entrée du cas gen nom trop long
#'
#' @param dfs 
#' @param hrcfiles 
#' @param v1 
#' @param v2 
#' @param totcode 
#'
#' @return (taille de tableaux)
#' @export
#'
#' @examples
length_tabs_4 <- function(dfs,hrcfiles,v1,v2){
  
  #les différentes modalités des 2 variables
  n_mod_v1<-length(unique(dfs[[v1]]))
  n_mod_v2<-length(unique(dfs[[v2]]))
  
  # 2 variables hiérarchiques fusionnées
  if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)){
    
    # La hierarchie de chaque variable
    level_v1 <- import_hierarchy(hrcfiles[[v1]])
    level_v2 <- import_hierarchy(hrcfiles[[v2]])
    
    # Stocke la somme des noeuds de v1_v2 de chaque table
    # On fait tout les croisements possible
    # entre v1 et v2
    # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
    
    # pour chacun de ses tableaux, il y a deux hierarchies possibles
    # une avec les totaux de v1, l'autre avec les totaux de v2
    
    # Le total de l'autre n'apparaît que dans le super total, d'où le -1, +1
    # Sachant que l'on commence par spliter v2
     nb_noeuds <- lapply(1:length(level_v1), function(i) {
      lapply(1:length(level_v2), function(j) {
        c((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1,
          length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1)
      })
    })
  
    
    # 2 variables non hierarchiques fusionnées
  } else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))){
    # On a qu'un tableau à la fin
    # qui peut avoir deux hierarchies
    # totaux sur v1, ou totaux sur v2
    nb_noeuds <- c((n_mod_v1 - 1) * n_mod_v2 + 1,
                   n_mod_v1 * (n_mod_v2 - 1) + 1)
    
    # 1 variable hier et une non hier
  } else {
    var_hier = ifelse(v1 %in% names(hrcfiles),v1,v2)
    mod_var_non_hier = ifelse(var_hier == v1,n_mod_v2,n_mod_v1)
    
    
    # analyse de la hierarchie de var_hier
    level_var_hier <- import_hierarchy(hrcfiles[[var_hier]])
    
    # On fait tout les croisements possible
    # entre v1 et v2
    # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
    
    # pour chacun de ses tableaux, il y a deux hierarchies possibles
    # une avec les totaux de v1, l'autre avec les totaux de v2
    nb_noeuds <- lapply(1:length(level_var_hier), function(i) {
      c(length(level_var_hier[[i]]) * (mod_var_non_hier - 1) + 1,
        (length(level_var_hier[[i]]) - 1) * mod_var_non_hier + 1)
    })
  }
  
  # Il faut maintenant multiplier par les modalités des variables non fusionnées
  
  liste_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1,v2))])
  
  mod_var_non_fusionnées <- lapply(liste_var_non_fusionnées, 
                                   function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_var_non_fusionnées))
  
  noeuds_tot <- lapply(unlist(nb_noeuds), function(x) x * prod_numbers)
  
  return(noeuds_tot)
}


#' entrée : entrée du cas gen nom trop long
#'
#' @param dfs 
#' @param hrcfiles 
#' @param v1 
#' @param v2 
#' @param totcode 
#'
#' @return (taille de tableaux)
#' @export
#'
#' @examples
length_tabs <- function(dfs,hrcfiles,v1,v2,v3,v4){
  
  # # 2 variables hiérarchiques fusionnées
  # if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)){
  #   
  #   level_v1 <- import_hierarchy(hrcfiles[[v1]])
  #   level_v2 <- import_hierarchy(hrcfiles[[v2]])
  #   
  #   # 2 variables non hierarchiques fusionnées
  # } else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))){
  #   
  #   level_v1 <- list(unique(dfs[[v1]]))
  #   level_v2 <- list(unique(dfs[[v2]]))
  #   
  #   # 1 variable hier et une non hier
  # } else {
  #   
  #   var_hier = ifelse(v1 %in% names(hrcfiles),v1,v2)
  #   var_non_hier = ifelse(var_hier == v1,v2,v1)
  #   
  #   level_var_hier <- import_hierarchy(hrcfiles[[var_hier]])
  #   
  #   level_v2 <- import_hierarchy(hrcfiles[[var_hier]])
  #   level_v1 <- list(unique(dfs[[var_non_hier]]))
  # }
  
  
  # On doit lister puis faire un unlist
  # sinon le ifelse renvoie le  premier élement de import_hierarchy (big total)
  # au lieu de renvoyer tous les noeuds
  level_v1 <- unlist(ifelse(v1 %in% names(hrcfiles),
                     list(import_hierarchy(hrcfiles[[v1]])),
                     list(list(unique(dfs[[v1]])))),
                     recursive = FALSE)
  
  level_v2 <- unlist(ifelse(v2 %in% names(hrcfiles),
                     list(import_hierarchy(hrcfiles[[v2]])),
                     list(list(unique(dfs[[v2]])))),
                     recursive = FALSE)
  
  # On split par rapport à v2 dans cas 1 non hrc donc il faut mettre dans l'ordre
  if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))){
    tmp <- level_v1
    level_v1 <- level_v2
    level_v2 <- tmp
  }
  
  # On fait tout les croisements possible
  # entre v1 et v2
  # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
  
  # pour chacun de ses tableaux, il y a deux hierarchies possibles
  # une avec les totaux de v1, l'autre avec les totaux de v2
  nb_noeuds <- lapply(1:length(level_v1), function(i) {
    lapply(1:length(level_v2), function(j) {
      c((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1,
        length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1)
    })
  })
  
  # Il faut maintenant multiplier par les modalités des variables non fusionnées
  
  liste_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1,v2))])
  
  mod_var_non_fusionnées <- lapply(liste_var_non_fusionnées, 
                                   function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_var_non_fusionnées))
  
  noeuds_tot <- lapply(unlist(nb_noeuds), function(x) x * prod_numbers)
  
  return(noeuds_tot)
}

#' entrée : entrée du cas gen nom trop long
#'
#' @param dfs 
#' @param hrcfiles 
#' @param v1 
#' @param v2 
#' @param totcode 
#'
#' @return (taille de tableaux)
#' @export
#'
#' @examples
length_tabs_5 <- function(dfs,hrcfiles,v1,v2,v3,v4){
  
  #les différentes modalités des 2 variables
  n_mod_v1<-length(unique(dfs[[v1]]))
  n_mod_v2<-length(unique(dfs[[v2]]))
  
  # 2 variables hiérarchiques fusionnées
  if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)){
    
    level_v1 <- import_hierarchy(hrcfiles[[v1]])
    level_v2 <- import_hierarchy(hrcfiles[[v2]])
    
    # 2 variables non hierarchiques fusionnées
  } else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))){
    
    level_v1 <- list(unique(dfs[[v1]]))
    level_v2 <- list(unique(dfs[[v2]]))
    
    # 1 variable hier et une non hier
  } else {
    
    var_hier = ifelse(v1 %in% names(hrcfiles),v1,v2)
    var_non_hier = ifelse(var_hier == v1,v2,v1)
    
    level_var_hier <- import_hierarchy(hrcfiles[[var_hier]])
    
    level_v2 <- import_hierarchy(hrcfiles[[var_hier]])
    level_v1 <- list(unique(dfs[[var_non_hier]]))
  }
  
  # On fait tout les croisements possible
  # entre v1 et v2
  # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
  
  # pour chacun de ses tableaux, il y a deux hierarchies possibles
  # une avec les totaux de v1, l'autre avec les totaux de v2
  nb_noeuds <- lapply(1:length(level_v1), function(i) {
    lapply(1:length(level_v2), function(j) {
      c((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1,
        length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1)
    })
  })
  
  # Il faut maintenant multiplier par les modalités des variables non fusionnées
  
  liste_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1,v2))])
  
  mod_var_non_fusionnées <- lapply(liste_var_non_fusionnées, 
                                   function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_var_non_fusionnées))
  
  noeuds_tot <- lapply(unlist(nb_noeuds), function(x) x * prod_numbers)
  
  return(noeuds_tot)
}



