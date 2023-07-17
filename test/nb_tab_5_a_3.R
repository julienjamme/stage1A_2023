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
#' @param v3 variable qui est fusionnée avec v1_v2 de 4 à 3
#' @param hrcfiles liste nommée des fichiers hrc
#' @param n_mod_v1 nombre de modalité de la variable 1
#' @param n_mod_v2 nombre de modalité de la variable 2
#'
#' @return
#' @export
#' 
#' @examples
#' data <- expand.grid(
#' ACT = c(
#'   "Total",
#'   read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
#' ),
#' SEX = c(
#'   "Total",
#'   read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
#' ),
#' GEO = c(
#'   "Pays",
#'   read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
#' ),
#' AGE = c("Ensemble", "adulte", "enfant"),
#' ECO = c("Ensemble", "riche", "pauvre", "autre"),
#' stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = runif(nrow(data)))
#' hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc")
#' 
#' tot_code <-
#'   c(
#'     SEX = "Total",
#'     AGE = "Ensemble",
#'     GEO = "Pays",
#'     ACT = "Total",
#'     ECO = "Ensemble"
#'   )
#' 
#' # pour execution ligne à ligne
#' dfs <- data
#' nom_dfs <- "nom_data_frame"
#' 
#' 
#' totcode <- tot_code
#' hrcfiles <- hrc_files
#' 
#' dir_name <- "output"
#' hrc_dir <- dir_name
#' sep_dir <- TRUE
#' sep = "_"
#' 
#' # Cas fusion deux variable hier
#' v1="ACT"
#' v2="GEO"
#' v3="ECO"
#' v4="ACT_GEO"
#' hrcfiles=hrcfiles
#' n_mod_v1 = length(unique(data[[v1]]))
#' n_mod_v2 = length(unique(data[[v2]]))
#' 
#' res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name,
#'                            v1 = v1, v2 = v2,v3 = v3,v4 = v4)
#' 
#' length(res$tabs) == calculer_nb_tab(v1=v1,
#'                                     v2=v2,
#'                                     v3=v3,
#'                                     hrcfiles=hrcfiles)
#' 
#' 
#' # Cas fusion deux var non hier
#' v1="AGE"
#' v2="ECO"
#' 
#' # On fait comme si SEX n'était pas hier
#' v3="SEX"
#' hrcfiles=c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc")
#' v4="AGE_ECO"
#' n_mod_v1 = length(unique(data[[v1]]))
#' n_mod_v2 = length(unique(data[[v2]]))
#' 
#' res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name,
#'                            v1 = v1, v2 = v2,v3 = v3,v4 = v4)
#' 
#' length(res$tabs) == calculer_nb_tab(v1=v1,
#'                                     v2=v2,
#'                                     v3=v3,
#'                                     n_mod_v1=n_mod_v1,
#'                                     n_mod_v2=n_mod_v2)
#' 
#' # Cas fusion une var hier et une var non hier
#' 
#' v1="ACT"
#' v2="ECO"
#' v3="GEO"
#' v4="ECO_ACT"
#' hrcfiles=hrcfiles
#' n_mod_v1 = length(unique(data[[v1]]))
#' n_mod_v2 = length(unique(data[[v2]]))
#' 
#' res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name,
#'                            v1 = v1, v2 = v2,v3 = v3,v4 = v4)
#' 
#' length(res$tabs) == calculer_nb_tab(v1=v1,
#'                                     v2=v2,
#'                                     v3=v3,
#'                                     hrcfiles=hrcfiles,
#'                                     n_mod_v1=n_mod_v1,
#'                                     n_mod_v2=n_mod_v2)
calculer_nb_tab <- function(v1,v2,v3, hrcfiles=NULL, n_mod_v1=NULL, n_mod_v2=NULL){
  
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
    nb_noeuds <- n_mod_v1 + n_mod_v2
    
  # 1 variable hier et une non hier
  } else {
    var_hier = ifelse(v1 %in% names(hrcfiles),v1,v2)
    mod_var_non_hier = ifelse(var_hier == v1,n_mod_v2,n_mod_v1)
    
    
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
}
