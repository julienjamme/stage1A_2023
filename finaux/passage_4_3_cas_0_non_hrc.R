library(dplyr)
source(file = "finaux/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")

#' Passage d'un data.frame à 4 variables catégorielles
#' dont deux non hierarchique
#' à une liste de data.frame à 3 variables catégorielles
#' doté de hierarchie non emboitées
#'
#' @param dfs data.frame à quatre variables catégorielles
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param v1 variable catégorielle hierarchique
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
passage_4_3_cas_0_non_hr <- function(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name) {
  
  #############################
  ## Création des code_split ##
  #############################
  
  # Première variable
  hrc1 <- hrcfiles[[v1]]
  total1 <- totcode[[v1]]
  
  res_sdc <- sdcHierarchies::hier_import(inp = hrc1, from = "hrc", root = total1) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  
  codes_split_1 <- lapply(
    res_sdc$dims,
    names
  )
  
  # Seconde variable
  hrc2 <- hrcfiles[[v2]]
  total2 <- totcode[[v2]]
  
  res_sdc <- sdcHierarchies::hier_import(inp = hrc2, from = "hrc", root = total2) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  
  codes_split_2 <- lapply(
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
          filter(pull(., {{ v1 }}) %in% codes)
      }
    )
  }
  
  liste_df_4_var_1_non_hr <- fonc_liste_df_4_var_1_non_hr(codes_split_1,dfs)
  
  
  # Nous avons maintenant des data.frames avec 1 variables non hierarchiques
  # nous pouvons donc appliquer la methode dédiée précédemment codée !
  
  
  
  
  
  
  ###################
  ## A compléter ! ##
  ###################
  
  # Mise à jour des arguments puis appel de la fonction cas_2_non_hrc
  appel_4_3_1_non_hier <- function(dfs){
    # Mise à jour des arguments de la fonction
    tot_code[v1] <- unique(dfs[[v1]])[1]
    nom_dfs <- paste(nom_dfs,tot_code[v1],sep="_")
    
    passage_4_3_cas_1_non_hr(dfs,nom_dfs,v1,v2, tot_code, hrcfiles, dir_name)
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res <- lapply(
    liste_df_4_var_1_non_hr,
    appel_4_3_1_non_hier
  )
  
  res <- unlist(res, recursive = FALSE)
  
  return(res)
}

