#' Passage de 4 à 3 variables via la fusion d'une variable hiérarchique
#' et une non hiérarchique
#'
#' @param dfs data.frame à 4 variables catégorielles (n >= 2 dans le cas général)
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param v1 variable catégorielle non hierarchique
#' @param v2 variable catégorielle hierarchique
#' @param totcode vecteur normée des totaux pour les variables catégorielles
#' @param hrcfiles vecteur nommé indiquant les fichiers hrc des variables 
#' hiérarchiques parmi les variables catégorielles de dfs
#' @param dir_name dossier où écrire les fichiers hrc
#' si aucun dossier n'est spécifié dans hrcfiles
#' @param sep séparateur utilisé lors de la concaténation des variables
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
passage_4_3_cas_1_bis <- function(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name, sep = "_") {
  
  #############################
  ## Création des code_split ##
  #############################
  hrc <- hrcfiles[[v2]]
  total <- totcode[[v2]]
  
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  
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
  # Nous avons maintenant des data.frames avec 2 variables non hierarchiques
  # nous pouvons donc appliquer la methode dédiée 
  
  # Mise à jour des arguments puis appel de la fonction cas_2_non_hrc
  # ...
  
  # Mise à jour des arguments puis appel de la fonction cas_2_non_hrc
  appel_4_3_non_hier <- function(dfs, i){
    if (i <= length(codes_split)) {
      totcode[v2] <- codes_split[[i]][1]
      nom_dfs <- paste(nom_dfs, totcode[v2], sep = "_")
      passage_4_3_cas_2_non_hr(dfs, nom_dfs, v1, v2, totcode, dir_name, sep = sep)
    } 
    else {
      print(paste("Index", i, "is out of bounds for codes_split."))
      return(NULL)
    }
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res <- lapply(seq_along(liste_df_4_var_2_non_hr), function(i) {
    appel_4_3_non_hier(liste_df_4_var_2_non_hr[[i]], i)
  })
  
  
  # On change l'objet pour qu'il soit le même que dans les autres cas
  tabs <- unlist(lapply(res, function(x) x$tabs), recursive = FALSE)
  hrcs <- unlist(lapply(res, function(x) x$hrcs), recursive = FALSE)
  alt_tot <- unlist(lapply(res, function(x) x$alt_tot), recursive = FALSE)
  return(
    list(
      tabs = tabs,
      hrcs = hrcs,
      alt_tot= alt_tot,
      vars = c(v1, v2))
  )
}
