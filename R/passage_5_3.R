#' Fonction passant de 5 à 3 variables catégorielles
#'
#' @param dfs data.frame à 5 variabls catégorielles (n >= 3 dans le cas général)
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param totcode vecteur normée des totaux pour les variables catégorielles
#' @param hrcfiles vecteur normée des hrc pour les variables catégorielles hierarchiques
#' @param sep_dir permet de forcer l'écriture des hrc dans un dossier séparé
#' par défault à FALSE
#' @param hrc_dir dossier où écrire les fichiers hrc si l'on force l'écriture
#' dans un nouveau dossier ou si aucun dossier n'est spécifié dans hrcfiles
#' @param v1 permet de forcer la valeur de la première variable à fusionner 
#' lors du passage de 5 à 4 dimensions, non spéficié par défault (NULL) 
#' @param v2 permet de forcer la valeur de la seconde variable à fusionner 
#' lors du passage de 5 à 4 dimensions, non spéficié par défault (NULL) 
#' @param v3 permet de forcer la valeur de la première variable à fusionner 
#' lors du passage de 4 à 3 dimensions, non spéficié par défault (NULL) 
#' @param v4 permet de forcer la valeur de la seconde variable à fusionner 
#' lors du passage de 4 à 3 dimensions, non spéficié par défault (NULL) 
#' 
#' @return liste(tabs, hrcs, vars)
#' tab : liste nommée des dataframes à 3 dimensions (n-2 dimensions dans le cas général)
#' doté de hiérarchies emboitées
#' hrc : liste nommée des hrc spécifiques à la variable crée via la fusion
#' alt_tot : liste nommée des totaux
#' vars : liste nommée de vecteur représentant les variables fusionnées
#' lors des deux étapes de réduction de dimensions
#' @export
#'
#' @examples
#' 
passer_de_5_a_3_var <- function(dfs, nom_dfs,totcode, hrcfiles, sep_dir = FALSE, hrc_dir = "hrc_alt",
                                v1 = NULL,v2 = NULL,v3 = NULL,v4 = NULL){
  
  # Mise à jour du dossier en sortie contenant les hiérarchie
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }
  
  # On enlève une dimension à notre dataframe de départ
  res_5_4 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, dir_name,
                                 v1 = v1, v2 = v2)
  # to do : supprimer les hrc de 5 à 4 puisque non utile pour la suite ?
  
  # Récupération des variables fusionnées
  v1f <- res_5_4$vars[[1]]
  v2f <- res_5_4$vars[[2]]
  new_var = paste(v1f, v2f, sep="_")
  
  # Mise à jour des totaux
  totcode2 <- totcode
  totcode2[[new_var]] <- paste(totcode[[v1f]],totcode[[v2f]], sep="_")
  totcode2 <- totcode2[!(names(totcode2) %in% c(v1f, v2f))]
  
  # Mise à jour des fichiers hrc
  hrcfiles2 <- hrcfiles
  hrcfiles2 <- hrcfiles2[!(names(hrcfiles2) %in% c(v1f, v2f))]
  
  # Les variables catégorielles sans hiérarchie dans nos tableaux à 4 dimensions
  var_cat <- names(totcode2)
  
  var_sans_hier <- intersect(
    setdiff(names(dfs), names(hrcfiles2)),
    var_cat
  )
  
  # Choix des variables pour le passage 4 -> 3 et vérification de celles renseignées en argument
  
  # Première variable pour le passage 4 à 3
  if (!is.null(v3)){
    if (!(v3 %in% var_cat)){
      stop(paste("v3 n'est pas une variable catégorielle, v3 = ", v3,
                 "Les variables catégorielles sont : ",paste(var_cat, collapse = ", ")), sep = "")
    }
  } else {
    # on choisit une variable en évitant v2
    v3 <- choisir_var(dfs = dfs[setdiff(names(dfs),v4)],
                      totcode = totcode2[setdiff(names(totcode2),v4)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v4)])
  }
  
  # Seconde variable pour le passage 4 à 3
  if (!is.null(v4)){
    if (!(v4 %in% var_cat)){
      stop(paste("v4 n'est pas une variable catégorielle, v4 = ", v4,
                 "Les variables catégorielles sont : ",paste(var_cat, collapse = ", ")), sep = "")
    }
    if (v3 == v4){
      stop("Erreur. Vous essayez de fusionner une variable avec elle-même")
    }
    
  } else {
    # on choisit une variable en évitant v1
    v4 <- choisir_var(dfs = dfs[setdiff(names(dfs),v3)],
                      totcode = totcode2[setdiff(names(totcode2),v3)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v3)])
  }
  
  appel_4_3_gen <- function(nom_dfsb){
    # Mise à jour des arguments de la fonction
    dfsb <- res_5_4$tabs[[nom_dfsb]]
    
    hrcfiles2b <-  c(hrcfiles2, res_5_4$hrcs[[nom_dfsb]])
    names(hrcfiles2b)[length(hrcfiles2b)] <- new_var
    
    passer_de_4_a_3_var(dfsb, nom_dfsb,totcode2, hrcfiles2b, sep_dir = TRUE, hrc_dir = dir_name,
                        v1 = v3, v2 = v4)
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res_5_3 <- lapply(
    names(res_5_4$tabs),
    appel_4_3_gen
  )
  
  tabs <- unlist(lapply(res_5_3, function(x) x$tabs), recursive = FALSE)
  hrcs <- unlist(lapply(res_5_3, function(x) x$hrcs), recursive = FALSE)
  alt_tot <- unlist(lapply(res_5_3, function(x) x$alt_tot), recursive = FALSE)
  
  vars1 <- res_5_4$vars
  vars2 <- res_5_3[[1]]$vars
  vars_tot <- list(vars1,vars2)
  names(vars_tot) <- c("Passage 5 à 4","Passage 4 à 3")
  
  return(list(tabs=tabs,
              hrcs=hrcs,
              alt_tot= alt_tot,
              vars=vars_tot)
        )
}
