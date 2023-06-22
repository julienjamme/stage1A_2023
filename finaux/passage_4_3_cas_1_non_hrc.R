library(dplyr)
source(file = "finaux/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")

#' Title
#'
#' @param dfs 
#' @param nom_dfs 
#' @param v1 
#' @param v2 
#' @param totcode 
#' @param hrcfiles 
#' @param dir_name 
#'
#' @return
#' @export
#'
#' @examples
passage_4_3_cas_1_non_hr <- function(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name) {
  # Rappel :
  # v1 non hier
  # v2 hier
  
  hrc <- hrcfiles[[v2]]
  total <- totcode[[v2]]
  
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  #res_sdc$dims
  
  codes_split <- lapply(
    res_sdc$dims,
    names
  )
  
  liste_df_4_var_2_non_hr <- lapply(
    codes_split,
    function(codes){
      res <- dfs %>% 
        filter(pull(., {{ v2 }}) %in% codes)
    }
  )
  # Nous avons maintenant des data.frames avec 2 variables non hierarchiques
  # nous pouvons donc appliquer la methode dédiée précédemment codée !
  
  
  # Mise à jour des arguments puis appel de la fonction cas_2_non_hrc
  appel_4_3_non_hier <- function(dfs){
    # Mise à jour des arguments de la fonction
    totcode[v2] <- unique(dfs[[v2]])[1]
    nom_dfs <- paste(nom_dfs,totcode[v2],sep="_")
    
    passage_4_3_cas_2_non_hr(data,nom_dfs,v1,v2, tot_code,dir_name)
  }

  # On transforme tous nos tableaux de 4 var en 3 var
  res <- lapply(
    liste_df_4_var_2_non_hr,
    appel_4_3_non_hier
  )
  
  return(res)
}

