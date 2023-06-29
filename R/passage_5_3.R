
source(file = "R/cas_gen_4_3.R",encoding = "UTF-8")

#' Title
#'
#' @param dfs 
#' @param nom_dfs 
#' @param totcode 
#' @param hrcfiles 
#' @param sep_dir 
#' @param hrc_dir 
#'
#' @return
#' @export
#'
#' @examples
passer_de_5_a_3_var <- function(dfs, nom_dfs,totcode, hrcfiles, sep_dir = FALSE, hrc_dir = "hrc_alt"){
  
  # Mise à jour du dossier en sortie contenant les hiérarchie
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }
  
  res_5_4 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, dir_name)
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
  
  appel_4_3_gen <- function(nom_dfsb){
    # Mise à jour des arguments de la fonction
    dfsb <- res_5_4$tabs[[nom_dfsb]]
    
    hrcfiles2b <-  c(hrcfiles2, res_5_4$hrcs[[nom_dfsb]])
    names(hrcfiles2b)[length(hrcfiles2b)] <- new_var
    
    passer_de_4_a_3_var(dfsb, nom_dfsb,totcode2, hrcfiles2b, sep_dir = TRUE, hrc_dir = dir_name )
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res_5_3 <- lapply(
    names(res_5_4$tabs),
    appel_4_3_gen
  )
  
  tabs <- unlist(lapply(res_5_3, function(x) x$tabs), recursive = FALSE)
  hrcs <- unlist(lapply(res_5_3, function(x) x$hrcs), recursive = FALSE)
  
  vars1 <- res_5_4$vars
  vars2 <- res_5_3[[1]]$vars
  vars_tot <- list(vars1,vars2)
  names(vars_tot) <- c("Passage 5 à 4","Passage 4 à 3")
  
  
  return(list(tabs=tabs,
              hrcs=hrcs,
              vars=vars_tot)
        )
}

