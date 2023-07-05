#' Passage de 4 à 3 variables via la fusion e deux variables hiérarchiques
#'
#' @param dfs data.frame à 4 variables catégorielles (n >= 2 dans le cas général)
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param v1 variable catégorielle hierarchique
#' @param v2 variable catégorielle hierarchique
#' @param totcode vecteur normée des totaux pour les variables catégorielles
#' @param hrcfiles vecteur nommé indiquant les fichiers hrc des variables 
#' hiérarchiques parmi les variables catégorielles de dfs
#' @param dir_name dossier où écrire les fichiers hrc
#' si aucun dossier n'est spécifié dans hrcfiles
#' @param sep séparateur utilisé lors de la concaténation des variables
#'
#' @return liste(tabs, hrcs, vars)
#' tab : liste nommée des dataframes à 3 dimensions (n-1 dimensions dans le cas général)
#' doté de hiérarchies emboitées
#' hrc : liste nommée des hrc spécifiques à la variable crée via la fusion
#' alt_tot : liste nommée des totaux
#' vars : liste nommée de vecteur représentant les variables fusionnées
#' lors des deux étapes de réduction de dimensions
#' @export
#'
#' @examples
passage_4_3_cas_0_non_hr <- function(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name, sep = "_") {
  
  #############################
  ## Création des code_split ##
  #############################
  hrc1 <- hrcfiles[[v1]]
  total1 <- totcode[[v1]]
  
  #On crée le tableau donnant les niveaux de la hiérarchie
  res_sdc <- sdcHierarchies::hier_import(inp = hrc1, from = "hrc", root = total1) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  
  codes_split_1 <- lapply(
    res_sdc$dims,
    names
  )
  
  ###########################
  # Réduction de hierarchie #
  ###########################
  fonc_liste_df_4_var_1_non_hr <- function(codes_split,dfs){
    lapply(
      codes_split_1,
      function(codes){
        res <- dfs %>% 
          filter(dfs[[v1]] %in% codes)
      }
    )
  }
  
  liste_df_4_var_1_non_hr <- fonc_liste_df_4_var_1_non_hr(codes_split_1,dfs)
  # Nous avons maintenant des data.frames avec 1 variables non hierarchiques (v1)
  # nous pouvons donc appliquer la methode dédiée
  
  
  # Mise à jour des arguments puis appel de la fonction cas_1_non_hrc
  appel_4_3_1_non_hier <- function(dfs){
    # Mise à jour des arguments de la fonction
    totcode[v1] <- unique(dfs[[v1]])[1]
    nom_dfs <- paste(nom_dfs,totcode[v1],sep="_")
    
    passage_4_3_cas_1_non_hr(dfs,nom_dfs,v1,v2, totcode, hrcfiles, dir_name, sep = sep)
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res <- lapply(
    liste_df_4_var_1_non_hr,
    appel_4_3_1_non_hier
  )
  
  tabs <- unlist(lapply(res, function(x) x$tabs), recursive = FALSE)
  hrcs <- unlist(lapply(res, function(x) x$hrcs), recursive = FALSE)
  alt_tot <- unlist(lapply(res, function(x) x$alt_tot), recursive = FALSE)
  
  
  return(
    list(
      tabs = tabs,
      hrcs = hrcs,
      alt_tot=alt_tot,
      vars = c(v1, v2))
  )
}
