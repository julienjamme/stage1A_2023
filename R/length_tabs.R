#' Calcul de la taille des tables généré a priori lors du passage
#' à 3 dimensions venant de dimension 4 ou 5
#'
#' @param dfs un data.frame
#' 
#' Variable dans le passage 5->4 ou 4->3
#' @param v1 la première variable fusionnée
#' @param v2 la seconde variable fusionnée
#' 
#' Variable dans le cas passage 4->3 dans le processus 4->3
#' ne pas indiquer v1_v2 si trois variables fusionée en une
#' @param v3 la troisième variable de départ fusionnée
#' @param v4 la quatrième variable de départ fusionnée
#' 
#' @param hrcfiles vecteur nommé des fichiers hrc relatifs aux variables
#' 
#' @return la liste des longeurs des tables créé lors de la réduction de dimension
#' @export
#'
#' @examples
length_tabs <- function(dfs,v1,v2,v3=NULL,v4=NULL,hrcfiles=NULL){
  
  # On a renseigné 4 variables -> cas 5 dimensions, 2 couples créés
  if (!is.null(v4)){
    return(length_tabs_5_4_var(dfs,hrcfiles,v1,v2,v3,v4))
    
  # On a renseigné 3 variables -> cas 5 dimensions, un trio fusionné
  } else if (!is.null(v3)){
    return(length_tabs_5_3_var(dfs,v1,v2,v3))
  
  # On a renseigné 2 variables -> cas 4 dimension
  } else {
    return(length_tabs_4(dfs,hrcfiles,v1,v2))
  }
}

# cas 4 dimensions
length_tabs_4 <- function(dfs,hrcfiles,v1,v2){

  # Récupération des regroupements {noeuds + branche}
  # en fonction de si la variable est hierarchique ou non
  
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
  # ainsi pour l'un des modalités, on ne fait pas de croisement aec son total
  # d'où le -1
  # et enfin on rajoute le super total, d'où le +1
  nb_rows <- lapply(1:length(level_v1), function(i) {
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
  
  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)
  
  return(nb_rows_tot)
}

# cas 5 dimensions, deux couples de variables fusionnés
length_tabs_5_4_var <- function(dfs,hrcfiles,v1,v2,v3,v4){
  
  # Récupération des regroupements {noeuds + branche}
  # en fonction de si la variable est hierarchique ou non
  
  # Passage 5-> 4
  
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
  
  # Passage 4 -> 3
  
  # On doit lister puis faire un unlist
  # sinon le ifelse renvoie le  premier élement de import_hierarchy (big total)
  # au lieu de renvoyer tous les noeuds
  level_v3 <- unlist(ifelse(v3 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v3]])),
                            list(list(unique(dfs[[v3]])))),
                     recursive = FALSE)
  
  level_v4 <- unlist(ifelse(v4 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v4]])),
                            list(list(unique(dfs[[v4]])))),
                     recursive = FALSE)
  
  
  # On split par rapport à v2 dans cas 1 non hrc donc il faut mettre dans l'ordre
  # ici v1 ~ v3 et v4 ~ v2 dans l'analogie
  if (!(v4 %in% names(hrcfiles)) & (v3 %in% names(hrcfiles))){
    tmp <- level_v3
    level_v3 <- level_v4
    level_v4 <- tmp
    
    tmp <- v3
    v3 <- v4
    v4 <- tmp
  }
  
  
  nb_rows <- lapply(1:length(level_v1), function(i) {
    lapply(1:length(level_v2), function(j) {
    
      # Une petite gymnastique est nécéessaire pour calculer la longueur des tableaux
      # dans l'ordre
      # Résultat vérifié empiriquement
      
      c(
        # Pour le 4->3, on met d'un côté les tableaux
        # la marge de v1_v2 ne prennant pas le total sur v1
        lapply(1:length(level_v3), function(k) {
          lapply(1:length(level_v4), function(l) {
            
            c( ((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1) * 
                 ((length(level_v3[[k]]) - 1) * length(level_v4[[l]]) + 1),
               
               ((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1) *
                 (length(level_v3[[k]]) * (length(level_v4[[l]]) - 1) + 1)
            )
          })
        }),
        
        # puis ceux ayant une marge de v1_v2 ne prennant pas de total sur v2
        lapply(1:length(level_v3), function(k) {
          lapply(1:length(level_v4), function(l) {
            
            c( (length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1) *
                 ((length(level_v3[[k]]) - 1) * length(level_v4[[l]]) + 1),
               
               (length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1) *
                 (length(level_v3[[k]]) * (length(level_v4[[l]]) - 1) + 1)
            )
          })
        })
      )
      
    })
  })

  # Il faut maintenant multiplier par les modalités des variables non fusionnées
  
  liste_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1,v2,v3,v4))])
  
  mod_var_non_fusionnées <- lapply(liste_var_non_fusionnées, 
                                   function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_var_non_fusionnées))
  
  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)
  
  return(nb_rows_tot)
}

# cas 5 dimensions, trois variables fusionnées en une
length_tabs_5_3_var <- function(dfs,v1,v2,v3){
  
  n_mod_v1 <- length(unique(dfs[[v1]]))
  n_mod_v2 <- length(unique(dfs[[v2]]))
  n_mod_v3 <- length(unique(dfs[[v3]]))
 
  
  
  nb_rows <- c(
                  1 + (n_mod_v3 - 1) * n_mod_v1,
                  1 + n_mod_v3 * (n_mod_v1 - 1),
                  
                  rep(c(1 + (n_mod_v3 - 1) * n_mod_v2,
                        1 + n_mod_v3 * (n_mod_v2 - 1))
                      , n_mod_v1),
                  
                  rep(c(1 + (n_mod_v3 - 1) * n_mod_v1,
                        1 + n_mod_v3 * (n_mod_v1 - 1))
                      , n_mod_v2 - 1)
  )

  # Il faut maintenant multiplier par les modalités des variables non fusionnées
  
  liste_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1,v2,v3,v4))])
  
  mod_var_non_fusionnées <- lapply(liste_var_non_fusionnées, 
                                   function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_var_non_fusionnées))
  
  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)
  
  return(nb_rows_tot)
}


#' Vérifie si les tableaux générés auront tous une taille convenable
#'
#' @param dfs un data.frame
#' 
#' Variable dans le passage 5->4 ou 4->3
#' @param v1 la première variable fusionnée
#' @param v2 la seconde variable fusionnée
#' 
#' Variable dans le cas passage 4->3 dans le processus 4->3
#' ne pas indiquer v1_v2 si trois variables fusionée en une
#' @param v3 la troisième variable de départ fusionnée
#' @param v4 la quatrième variable de départ fusionnée
#' 
#' @param hrcfiles vecteur nommé des fichiers hrc relatifs aux variables
#' @param LIMIT seul limite de taille de table toléré
#' 
#' @return la liste des longeurs des tables créé lors de la réduction de dimension
#' @export
#'
#' @examples
length_tabs_ok_tauargus <- function(dfs,v1,v2,v3=NULL,v4=NULL,hrcfiles=NULL,LIMIT=15000){
  return(!(max(unlist(length_tabs(dfs,v1,v2,v3=NULL,v4=NULL,hrcfiles=NULL))) > LIMIT))
}