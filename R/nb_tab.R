# Fonction pour gérer l'importation de la hiérarchie
import_hierarchy <- function(hrcfile) {
  total <- "BIG_Total"
  res_sdc <- sdcHierarchies::hier_import(inp = hrcfile, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  # Stocker tous les ensembles de parents + enfant direct
  levels <- lapply(res_sdc$dims, names)
  return(levels)
}


#' Calcul le nombre de table généré lors de la fusion de 3 variables
#' dans le passage de 5 à 3 dimensions
#'
#' @param v1 première variable fusionnée de 5 à 4
#' @param v2 seconde variable fusionnéee de 5 à 4
#' @param v3 troisième variable à fusionner (variable se fusionnant à v1 et v2 à défault si v4 non spécifié)
#' @param v4 quatrième variable à fusionner (avec v3)
#' @param hrcfiles liste nommée des fichiers hrc
#' @param data data.frame
#'
#' @return
#' @export
#' 
#' TODO : généraliser e cas 3 variables en une ?
#' @examples
calculer_nb_tab <- function(v1,v2,v3 = NULL, v4=NULL,hrcfiles=NULL, data=NULL){
  
  # Cas dimension 5 : 2 couples créés
  if (!is.null(v4)){
    return(4 * nb_noeuds(hrcfiles = hrcfiles, v=v1) * 
               nb_noeuds(hrcfiles = hrcfiles, v=v2) * 
               nb_noeuds(hrcfiles = hrcfiles, v=v3) * 
               nb_noeuds(hrcfiles = hrcfiles, v=v4))
    
  # Cas dimension 5 : un triplet fusionné
  } else if (!is.null(v3)){
    
    # 2 variables hiérarchiques fusionnées  
    if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)){
      
      # La hierarchie de chaque variable
      level_v1 <- import_hierarchy(hrcfiles[[v1]])
      level_v2 <- import_hierarchy(hrcfiles[[v2]])
      
      # Stocke la somme des noeuds de v1_v2 de chaque table
      # On fait tout les croisements possible
      # entre v1 et v2
      # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
      
      # pour chacun de ses tableaux, il y a deux hierarchies possibles
      # une avec les totaux de v1, l'autre avec les totaux de v2
      # le nombre de noeuds est égal à leur nombre de modalité
      nb_noeuds <- sum(sapply(1:length(level_v1), function(i) {
                        sum(sapply(1:length(level_v2), function(j) {
                            length(level_v1[[i]]) + length(level_v2[[j]])
                            }))
                      }))
    
    # 2 variables non hierarchiques fusionnées
    } else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))){
      # On a qu'un tableau à la fin
      # qui peut avoir deux hierarchies
      # totaux sur v1, ou totaux sur v2
      # le nombre de noeuds équivaut au nombre de modalité
      nb_noeuds <- length(unique(data[[v1]])) + length(unique(data[[v2]]))
      
    # 1 variable hier et une non hier
    } else {
      var_hier = ifelse(v1 %in% names(hrcfiles),v1,v2)
      mod_var_non_hier = ifelse(var_hier == v1,
                                length(unique(data[[v2]])),
                                length(unique(data[[v1]])))
      
      
      # analyse de la hierarchie de var_hier
      level_var_hier <- import_hierarchy(hrcfiles[[var_hier]])
    
      # On fait tout les croisements possible
      # entre v1 et v2
      # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
      
      # pour chacun de ses tableaux, il y a deux hierarchies possibles
      # une avec les totaux de v1, l'autre avec les totaux de v2
      # le nombre de noeuds est égal à leur nombre de modalité
      nb_noeuds <- sum(sapply(1:length(level_var_hier), function(i) {
                      length(level_var_hier[[i]]) + mod_var_non_hier
                      }))
    }
    
    # nb_noeuds correspondant au nombre de table qu'il faut créer
    # pour rendre v1_v2 non hierarchique
    # pour chacun de ces tables, il faut rendre v3 non hierarchique
    # on on crée autant de table que sa hierarchie a de noeuds
    # enfin pour chaque table créé, deux hierarchies sont possibles
    # les totaux sur v1_v2 et les totaux sur v3
    return(2 * nb_noeuds * nb_noeuds(hrcfiles, v=v3))
    
  # Cas dimension 4
  } else {
    return(2 * nb_noeuds(hrcfiles = hrcfiles, v=v1) * 
               nb_noeuds(hrcfiles = hrcfiles, v=v2))
  }
}
