# Compte le nombre de noeud dans un fichier hiérarchique
# 2 arguments sont attendus
# soit une liste nommée et une variable
# soit un hrc et hrc_name = FALSE
nb_noeuds <- function(hrcfiles, v = NULL, hrc_name = TRUE) {
  # Vérifie si la variable à un fichier hrc associé ou si hrc_names == FALSE
  if (hrc_name && !(v %in% names(hrcfiles)) || (!hrc_name && is.null(hrcfiles))) {
    # Variable non-hierarchique ou hrcfiles == NULL
    return(1)
  }
  
  # Prends le fichier spécifie si hrc_name = TRUE, sinon prend le hrc renseigné directement
  hrc <- ifelse(hrc_name, hrcfiles[[v]], hrcfiles)
  
  # Valeur non importante pour la suite
  total <- "Ceci_Est_Mon_Total"
  
  # Convertir en hierarchie
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>%
    sdcHierarchies::hier_convert(as = "sdc")
  
  # Renvoie le nombre de noeuds
  return(length(res_sdc$dims))
}

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
#' @param sep séparateur utilisé lors de la concaténation des variables
#' 
#' @return liste(tabs, hrcs5_4,hrcs4_3, alt_tot5_4,alt_tot4_3, vars)
#' tab : liste nommée des dataframes à 3 dimensions (n-2 dimensions dans le cas général)
#' doté de hiérarchies emboitées
#' hrcs5_4 : liste nommée des hrc spécifiques à la variable crée via la fusion
#'                lors du passage de 5 à 4 dimensions
#' hrcs4_3 : liste nommée des hrc spécifiques à la variable crée via la fusion
#'                lors du passage de 4 à 3 dimensions
#' alt_tot5_4 : liste nommée des totaux lors du passage de 5 à 4 dimensions
#' alt_tot4_3 : liste nommée des totaux lors du passage de 4 à 3 dimensions
#' vars : liste nommée de vecteur représentant les variables fusionnées
#' lors des deux étapes de réduction de dimensions
#' @export
#'
#' @examples
#' 
passer_de_5_a_3_var <- function(dfs, nom_dfs,totcode, hrcfiles = NULL, sep_dir = FALSE, hrc_dir = "hrc_alt",
                                v1 = NULL,v2 = NULL,v3 = NULL,v4 = NULL, sep = "_"){
  
  # Mise à jour du dossier en sortie contenant les hiérarchie
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }
  
  # On enlève une dimension à notre dataframe de départ
  res_5_4 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, dir_name,
                                 v1 = v1, v2 = v2, sep = sep)
  
  # Récupération des variables fusionnées
  v1f <- res_5_4$vars[[1]]
  v2f <- res_5_4$vars[[2]]
  new_var = paste(v1f, v2f, sep=sep)
  
  # Mise à jour des totaux
  totcode2 <- totcode
  totcode2[[new_var]] <- paste(totcode[[v1f]],totcode[[v2f]], sep=sep)
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
  
  # Nombre de noeud moyen de la nouvelle variable puisque
  # elle a un fichier hrc différent par tableau !
  nb_noeuds_new_var <- lapply(names(res_5_4$hrcs),
                              function(x) nb_noeuds(res_5_4$hrcs, x))
  nb_noeuds_moyen_new_var <- sum(unlist(nb_noeuds_new_var)) / length(res_5_4$hrcs)
  
  # Choix des variables pour le passage 4 -> 3 et vérification de celles renseignées en argument
  # On choisit dès maintenant v3 et v4 pour être sûr que la même variable
  # est créé au sein de tous les sous tableaux
  
  # Première variable pour le passage 4 à 3
  if (!is.null(v3)){
    if (!(v3 %in% var_cat)){
      stop(paste("v3 n'est pas une variable catégorielle, v3 = ", v3,
                 "Les variables catégorielles sont : ",paste(var_cat, collapse = ", ")), sep = "")
    }
  } else {
    # on choisit une variable en évitant v4
    v3 <- choisir_var(dfs = dfs[setdiff(names(dfs),v4)],
                      totcode = totcode2[setdiff(names(totcode2),v4)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v4)])
    
    # On regarde si la variable fusionnée à moins de noeuds que la variable selectionnée
    nb_noeuds_v3 <- nb_noeuds(hrcfiles2, v=v3)
    if (!is.null(v4)){
      # Nous devons faire deux if différents sinon NULL != new_var fait planter !
      if (v4 != new_var & nb_noeuds_v3 > nb_noeuds_moyen){
        v3 <- new_var
      }
      # Si v4 = NULL pas besoin de comparer v4 != new_var
    } else if (nb_noeuds_v3 > nb_noeuds_moyen_new_var){
      v3 <- new_var
    }
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
    # on choisit une variable en évitant v3
    v4 <- choisir_var(dfs = dfs[setdiff(names(dfs),v3)],
                      totcode = totcode2[setdiff(names(totcode2),v3)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v3)])
    
    # On regarde si la variable fusionnée à moins de noeuds que la variable selectionnée
    nb_noeuds_v4 <- nb_noeuds(hrcfiles2, v=v4)
    # Rq : v3 ne peut pas être NULL
    if (v3 != new_var & nb_noeuds_v4 > nb_noeuds_moyen_new_var){
      v4 <- new_var
    }
  }
  
  appel_4_3_gen <- function(nom_dfsb){
    # Mise à jour des arguments de la fonction
    dfsb <- res_5_4$tabs[[nom_dfsb]]
    
    hrcfiles2b <-  c(hrcfiles2, res_5_4$hrcs[[nom_dfsb]])
    names(hrcfiles2b)[length(hrcfiles2b)] <- new_var
    
    passer_de_4_a_3_var(dfsb, nom_dfsb,totcode2, hrcfiles2b, sep_dir = TRUE,
                        hrc_dir = dir_name,v1 = v3, v2 = v4, sep=sep)
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res_5_3 <- lapply(
    names(res_5_4$tabs),
    appel_4_3_gen
  )
  
  tabs <- unlist(lapply(res_5_3, function(x) x$tabs), recursive = FALSE)
  hrcs4_3 <- unlist(lapply(res_5_3, function(x) x$hrcs), recursive = FALSE)
  alt_tot4_3 <- unlist(lapply(res_5_3, function(x) x$alt_tot), recursive = FALSE)
  
  vars1 <- res_5_4$vars
  vars2 <- res_5_3[[1]]$vars # les variables fusionnées sont toujours les mêmes
  vars_tot <- list(vars1,vars2)
  names(vars_tot) <- c("Passage 5 à 4","Passage 4 à 3")
  
  # Mémorisation de res5_4
  
  # Cas on fusionne 4 variables différentes 
  if (!(new_var %in% c(v3,v4))){
    # On répète autant de fois  res5_4[i] que le tableau va créer 
    # de tableaux à 3 dimensions
    
    # Chaque tableau à 4 dimensions va créer le même nombre de tableau à 3 dimensions
    # car les variables selectionnées ont les mêmes modalités dans chacun d'eux
    nb_rep <- length(tabs) / length(res_5_4$tabs)
    hrcs5_4 <- as.list(unlist(lapply(res_5_4$hrcs,
                                     function(x) rep(x,nb_rep))))
    
    alt_tot5_4 <- as.list(unlist(lapply(res_5_4$alt_tot,
                                        function(x) rep(x,nb_rep))))
    
    # Si l'on fusionne 3 variables en une, le nombre de tableaux
    # créé par chaque table change !
  } else {
    # Stocke le nom de la variable new_var dans un nouvel objet
    fuse_var <- ifelse(v3 == new_var, v4, v3)
    
    # Calcule la valeur de nb_noeuds une fois pour chaque res_5_4$hrcs[[x]]
    # pour éviter de calculer deux fois la même quantité
    results <- lapply(1:length(res_5_4$hrcs), function(x) {
      nb_noeuds_val <- 2 * nb_noeuds(res_5_4$hrcs[[x]], hrc_name = FALSE) * nb_noeuds(hrcfiles2, fuse_var)
      
      # Utilisez la valeur calculée pour hrcs5_4 et alt_tot5_4
      list(
        hrcs = rep(res_5_4$hrcs[[x]], nb_noeuds_val),
        alt_tot = rep(res_5_4$alt_tot[[x]], nb_noeuds_val)
      )
    })
    
    # Extrait les valeurs pour hrcs5_4 et alt_tot5_4
    hrcs5_4 <- as.list(unlist(lapply(results, function(x) x$hrcs)))
    alt_tot5_4 <- as.list(unlist(lapply(results, function(x) x$alt_tot)))
  }
  
  return(list(tabs=tabs,
              hrcs5_4=hrcs5_4,
              hrcs4_3=hrcs4_3,
              alt_tot5_4=alt_tot5_4,
              alt_tot4_3=alt_tot4_3,
              vars=vars_tot)
  )
}