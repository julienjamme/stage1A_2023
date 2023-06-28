
#aider par internet pour trouver nrow et which.min
plus_petit_hrc <- function(hrc_files) {
  dfs <- lapply(hrc_files, read.table)
  indice_petit_hrc <- which.min(sapply(dfs, nrow))
  nom_plus_petit_hrc <- names(hrc_files[indice_petit_hrc])
  return(nom_plus_petit_hrc)
}

get_2_smallest <- function(data){
  list_mod_n <- lapply(data, function(col) length(unique(col)))
  smallest_index <- which.min(list_mod_n)
  list_mod_n[smallest_index] <- NA
  sec_smallest_index <- which.min(list_mod_n)
  return(c(smallest_index,sec_smallest_index))
}
source(file = "finaux/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source(file = "finaux/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source(file = "finaux/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")

#' Passage d'un data.frame à 4 variables catégorielles
#' à une liste de data.frame à 3 variables catégorielles
#' doté de hierarchie non emboitées
#' 
#'
#' @param dfs data.frame à 4 variabls catégorielles
#' @param nom_dfs nom du data.frame qui modifera les noms des hrc en output
#' @param totcode liste des totaux liés au data.frame
#' @param hrcfiles liste des fichiers hrc liés au data.frame
#' @param sep_dir permet de forcer l'export dans hrc_dir
#' @param hrc_dir dossier utilisé pour l'écriture lors de l'expot
#'
#' @return une liste de data.frame à 3 variables catégorielles
#' doté de hierarchie non emboitées
#' @export
#'
#' @examples
passer_de_4_a_3_var <- function(dfs,nom_dfs,totcode, hrcfiles, sep_dir = FALSE, hrc_dir = "hrc_alt"){
  
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

  n_vars_sans_hier<-length(var_sans_hier)
  
  #si superieur à 3 je dis que je regarde celui avec les plus petites moda
  if (n_vars_sans_hier > 2){
    dfs_var_sans_hier <- subset(dfs,select = var_sans_hier)
    res<-get_2_smallest(dfs_var_sans_hier)
    v1 <- names(res)[[1]]
    v2 <- names(res)[[2]]
    # to do : le transformer en une liste d'un seul élément
    return(passage_4_3_cas_2_non_hr(dfs, nom_dfs,v1,v2,totcode,dir_name))
  }
  
  # 1 - Sélectionner les colonnes à fusionner
  #Principe: choisir en priorité les variables non hiérarchiques

  if(n_vars_sans_hier <= 2){
    v1 <- var_sans_hier[1]
    v2 <- var_sans_hier[2]
  }

  # Test le nb de variables non hier récupérées
  if(n_vars_sans_hier == 2){
    # to do : le transformer en une liste d'un seul élément
    return(passage_4_3_cas_2_non_hr(dfs, nom_dfs,v1,v2,totcode,dir_name))
    
  }else if(n_vars_sans_hier == 1){
    # Aller chercher une des 3 variables hierarchiques
    # de préférence celle avec le moins de modalités
    v2 <- plus_petit_hrc(hrcfiles)

    return(passage_4_3_cas_1_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name))
  }else{
    #cas ou que des var hier
    # Aller chercher deux des 4 var hier
    # de préférence celles avec le moins de modalités
    v1 <- plus_petit_hrc(hrcfiles)
    
    # on enlève la var trouvé pour v1 pour trouver v2
    hrc_files_2 <- hrc_files[setdiff(names(hrc_files), v1)]
    v2 <- plus_petit_hrc(hrc_files_2)
    
    return(passage_4_3_cas_0_non_hr(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name))
  }

}

