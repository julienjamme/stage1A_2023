# Renvoie la variable hierarchique avec le moins de noeuds (= sous totaux)
plus_petit_hrc <- function(hrcfiles, totcode) {
  v <- list()
  for (i in 1:length(hrcfiles)) {
    v <- append(v, test_nb_tabs_3hrc(hrcfiles, names(hrcfiles[i]), totcode))
  }
  indice_petit_hrc <- which.min(v)
  nom_plus_petit_hrc <- names(hrcfiles)[indice_petit_hrc]
  return(nom_plus_petit_hrc) 
}

# Renvoie la variable  avec le moins de modalité
plus_petit_mod <- function(dfs) {
  v <- list()
  for (colonne in dfs) {
    v <- append(v,length(unique(colonne)))
  }
  indice_petit_mod <- which.min(v)
  nom_plus_petit_mod <- names(dfs)[indice_petit_mod]
  return(nom_plus_petit_mod) 
}

# Choisir une variable catégorielle
# De préférence celle non hierarchique avec le moins de modalité
# A default la variable hierarchique avec le moins de noeuds
choisir_var_priorite_non_hierarchique <- function(dfs,totcode,hrcfiles){
  # Les variables catégorielles sans hiérarchie
  var_cat <- names(totcode)
  
  var_sans_hier <- intersect(
    setdiff(names(dfs), names(hrcfiles)),
    var_cat
  )
  
  n_vars_sans_hier<-length(var_sans_hier)
  
  # Principe: choisir en priorité les variables non hiérarchiques
  
  # si superieur à 1 on regarde les variables avec le moins de modalité
  # pour créer le moins de dataframe par la suite
  if (n_vars_sans_hier > 1){
    dfs_var_sans_hier <- subset(dfs,select = var_sans_hier)
    return (plus_petit_mod(dfs_var_sans_hier))
  }
  else if(n_vars_sans_hier == 1){
    return (var_sans_hier[1])
  }
  # Sinon on choisit la variable hierarchique avec le moins de sous totaux
  else {
    return (plus_petit_hrc(hrcfiles,totcode))
  }
}

# Renvoie la variable hiérarchique avec le plus de noeuds
plus_grand_hrc <- function(hrcfiles, totcode) {
  v <- list()
  for (i in 1:length(hrcfiles)) {
    v <- append(v, test_nb_tabs_3hrc(hrcfiles, names(hrcfiles[i]), totcode))
  }
  indice_grand_hrc <- which.max(v)
  nom_plus_grand_hrc <- names(hrcfiles)[indice_grand_hrc]
  return(nom_plus_grand_hrc)
}

# Renvoie la variable avec le plus de modalités
plus_grand_mod <- function(dfs) {
  v <- list()
  for (colonne in dfs) {
    v <- append(v, length(unique(colonne)))
  }
  indice_grand_mod <- which.max(v)
  nom_plus_grand_mod <- names(dfs)[indice_grand_mod]
  return(nom_plus_grand_mod)
}

# Choisir une variable catégorielle
# De préférence celle hiérarchique avec le plus de noeuds
# A défaut la variable non hiérarchique avec le plus de modalités
choisir_var_priorite_hierarchique <- function(dfs, totcode, hrcfiles) {
  # Principe: choisir en priorité les variables hiérarchiques
  
  # Si aucune variable hiérarchique, choisir variable non hiérarchique avec le plus de modalités
  if (length(hrcfiles) == 0) {
    return(plus_grand_mod(dfs[names(dfs) %in% names(totcode)]))
  # Sinon, choisir la variable hiérarchique avec le plus de sous-totaux
  } else {
    return(plus_grand_hrc(hrcfiles, totcode))
  }
}

choisir_var <- function(dfs, totcode, hrcfiles, select_hier = FALSE) {
  if(select_hier){
    return(choisir_var_priorite_hierarchique(dfs, totcode, hrcfiles))
  } else {
    return(choisir_var_priorite_non_hierarchique(dfs, totcode, hrcfiles))
  }
}

#' Fonction passant de 4 à 3 variables catégorielles
#'
#' @param dfs data.frame à 4 variabls catégorielles (n >= 2 dans le cas général)
#' @param nom_dfs nom du dataframe
#' @param totcode vecteur normée des totaux pourles variables catégorielles
#' @param hrcfiles vecteur normée des hrc pour les variables catégorielles hierarchiques
#' @param sep_dir permet de forcer l'écriture des hrc dans un dossier séparé
#' par défault à FALSE
#' @param hrc_dir dossier où écrire les fichiers hrc si l'on force l'écriture
#' dans un nouveau dossier ou si aucun dossier n'est spécifié dans hrcfiles
#' @param v1 permet de forcer la valeur de la première variable à fusionner, 
#' non spéficié par défault (NULL)
#' @param v2 permet de forcer la valeur de la seconde variable à fusionner
#' non spéficié par défault (NULL)
#' @param sep séparateur utilisé lors de la concaténation des variables
#' @param select_hier précise si l'on préfère selectionner les variables hiérarchiques avec
#' le plus de noeuds en priorité (hier=TRUE) ce qui génère plus de tableaux
#' mais de taille moins importante
#' ou bien les variables non hiérarchiques avec le moins de modalité (hier=FALSE)
#' pour créer le moins de tableau
#'
#' @return liste(tabs, hrcs, alt_tot, vars)
#' tab : liste nommée des dataframes à 3 dimensions (n-1 dimensions dans le cas général)
#' doté de hiérarchies emboitées
#' hrc : liste nommée des hrc spécifiques à la variable crée via la fusion
#' alt_tot : liste nommée des totaux
#' vars : liste nommée de vecteur représentant les variables fusionnées
#' lors des deux étapes de réduction de dimensions
#' @export
#'
#' @examples
passer_de_4_a_3_var <- function(dfs,nom_dfs,totcode,hrcfiles = NULL,
                                sep_dir = FALSE, hrc_dir = "hrc_alt",
                                v1 = NULL,v2 = NULL, 
                                sep = "_", select_hier = FALSE){
  
  # Mise à jour du dossier en sortie contenant les hiérarchie
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }
  
  # Les variables catégorielles sans hiérarchie
  var_cat <- names(totcode)
  
  var_sans_hier <- intersect(
                  setdiff(names(dfs), names(hrcfiles)),
                  var_cat
  )
  
  # Choix des variables et vérification de celles renseignées en argument
  
  n_vars_sans_hier <- 0 # Variable hierarchique selectionnée jusqu'à présent

  # Première variable
  if (!is.null(v1)){
    if (!(v1 %in% var_cat)){
      stop(paste("v1 n'est pas une variable catégorielle, v1 = ", v1,
           "Les variables catégorielles sont : ",paste(var_cat, collapse = ", ")), sep = "")
    }
  } else {
    # on choisit une variable en évitant v2
    v1 <- choisir_var(dfs = dfs[setdiff(names(dfs),v2)],
                      totcode = totcode[setdiff(names(totcode),v2)],
                      hrcfiles = hrcfiles[setdiff(names(hrcfiles),v2)],
                      select_hier = select_hier)
  }
  
  if (v1 %in% var_sans_hier){
    # Mise à jour du nombre de variable hier selectionnée
    n_vars_sans_hier <- n_vars_sans_hier + 1
  }
  
  # Seconde variable
  if (!is.null(v2)){
    if (!(v2 %in% var_cat)){
      stop(paste("v2 n'est pas une variable catégorielle, v2 = ", v2,
                 "Les variables catégorielles sont : ",paste(var_cat, collapse = ", ")), sep = "")
    }
    if (v1 == v2){
      stop("Erreur. Vous essayez de fusionner une variable avec elle-même")
    }

  } else {
    # on choisit une variable en évitant v1
    v2 <- choisir_var(dfs = dfs[setdiff(names(dfs),v1)],
                      totcode = totcode[setdiff(names(totcode),v1)],
                      hrcfiles = hrcfiles[!(names(hrcfiles) == v1)],
                      select_hier = select_hier)
  }
  
  if (v2 %in% var_sans_hier){
    # Mise à jour du nombre de variable hier selectionnée
    n_vars_sans_hier <- n_vars_sans_hier + 1
  }
  
  # On appelle la fonction correspondante
  
  # Cas 2 variables non hiérarchique
  if(n_vars_sans_hier == 2){
    return(passage_4_3_cas_2_non_hr(dfs, nom_dfs,v1,v2,totcode,dir_name,sep = sep))
  
  # Cas 1 variable non hiérarchique
  }else if(n_vars_sans_hier == 1){
    # Il faut que v2 soit hierarchique, v1 non hierarchique
    # Donc on met les variables dans le bon ordre
    if (v2 %in% var_sans_hier){
      tmp <- v2
      v2 <- v1
      v1 <- tmp
    }
    return(passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name,sep = sep))
    
  # Cas 0 variable non hiérarchique
  }else{
    return(passage_4_3_cas_0_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name, sep = sep))
  }
}