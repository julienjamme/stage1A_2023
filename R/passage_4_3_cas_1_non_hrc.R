library(dplyr)
source(file = "R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")

#' Passage d'un data.frame à 4 variables catégorielles
#' dont une non hierarchique
#' à une liste de data.frame à 3 variables catégorielles
#' doté de hierarchie non emboitées
#'
#' @param dfs data.frame à quatre variables catégorielles
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param v1 variable catégorielle non hierarchique
#' @param v2 variable catégorielle hierarchique
#' @param totcode vecteur nommé indiquant la modalité du total
#' pour chacune des 4 variables catégorielles de dfs
#' @param hrcfiles vecteur nommé indiquant les fichiers hrc des variables 
#' hiérarchiques parmi les 4 variables catégorielles de dfs
#' @param dir_name répertoire des fichiers hrc dans le cas où hrcfiles est vide
#'
#' @return une liste de data.frame à 3 variables catégorielles
#' doté de hierarchie non emboitées
#' @export
#'
#' @examples
passage_4_3_cas_1_non_hr <- function(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name) {
  
  #############################
  ## Création des code_split ##
  #############################
  hrc <- hrcfiles[[v2]]
  total <- totcode[[v2]]
  
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  #res_sdc$dims
  
  #Code split nous donne les hiérarchies ainsi que les niveaux de la hiérarchie 
  codes_split <- lapply(
    res_sdc$dims,
    names
  )
  
  
  
  
  
  #data <-unique(dfs[[v2]])
  
  #intersections <- lapply(codes_split, function(x) intersect(x, unique(data[[v2]])))
  #codes_split
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
  # nous pouvons donc appliquer la methode dédiée précédemment codée !
  
  
  # Mise à jour des arguments puis appel de la fonction cas_2_non_hrc
  appel_4_3_non_hier <- function(dfs){
    # Mise à jour des arguments de la fonction
    tot_code[v2] <- unique(dfs[[v2]])[1]
    nom_dfs <- paste(nom_dfs,tot_code[v2],sep="_")
    
    passage_4_3_cas_2_non_hr(dfs,nom_dfs,v1,v2, tot_code,dir_name)
  }

  # On transforme tous nos tableaux de 4 var en 3 var
  res <- lapply(
    liste_df_4_var_2_non_hr,
    appel_4_3_non_hier
  )
  # On change l'objet pour qu'il soit le même que dans les autres cas
  tabs <- unlist(lapply(res, function(x) x$tabs), recursive = FALSE)
  hrcs <- unlist(lapply(res, function(x) x$hrcs), recursive = FALSE)
  return(
    list(
      tabs = tabs,
      hrcs = hrcs,
      vars = c(v1, v2)
      )
  )
}

