passer_de_4_a_3_var <- function(data_4vars, hrc_files){
  
  n_col <- ncol(data_4vars)
  n_to_paste <- 2
  
  # 1 - Sélectionner les colonnes à fusionner
  #Principe: choisir en priorité les variables non hiérarchiques
  var_sans_hier <- setdiff(names(data_4vars), names(hrc_files))
  v1 <- var_sans_hier[1]
  v2 <- var_sans_hier[2]
  n_var_sans_hier <- length(var_sans_hier)
  # Test le nb de variables non hier récupérées
  if(n_var_sans_hier == 2){
    # data_4vars %>% 
    # mutate(NEWVAR = paste(data_4vars[[v1]], data_4vars[[v2]], sep = "_")) %>% 
    # tidyr::unite(col = "NEWVAR", all_of(var_sans_hier), sep = "_")
    
    data_4vars$NEWVAR <- paste(data_4vars[[v1]], data_4vars[[v2]], sep = "_")
    return(list(data_4vars))
  }else if(n_var_sans_hier == 1){
    # Aller chercher une des 3 variables hierarchiques 
    # de préférence celle avec le moins de modalités
    # puis faire un split sur la var qui n'est pas hierarchique
    # puis coller les deux variables dans chaque sous-table créée par le split
    return(list())
  }else{ #cas ou que des var hier
    # Aller chercher deux des 4 var hier
    # de préférence celles avec le moins de modalités
    # creer la liste des codes split 
    # puis faire un split sur la var qui n'est pas hierarchique
    # puis coller les deux variables dans chaque sous-table créée par le split
    return(list())
  }
  
}