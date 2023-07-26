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
#' library(dplyr)
#' 
#' source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
#' 
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   SEX = c("Total", "F", "M","F1","F2","M1","M2"),
#'   AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB","Ménages","Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>% 
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1:n())
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' hrc_sex <- "output/hrc_SEX.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("F","M")) %>% 
#'   sdcHierarchies::hier_add(root = "F", nodes = c("F1","F2")) %>% 
#'   sdcHierarchies::hier_add(root = "M", nodes = c("M1","M2")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)
#' 
#' res <- passage_4_3_cas_0_non_hr(dfs = data,
#'                                 nom_dfs = "nom_dfs",
#'                                 v1 = "ACT",v2 = "SEX",
#'                                 totcode = c(ACT = "Total",SEX = "Total",
#'                                             AGE = "Total",ECO = "PIB"),
#'                                 hrcfiles = c(ACT = hrc_act, SEX = hrc_sex),
#'                                 dir_name = "output")
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

  # Mise à jour des arguments puis appel de la fonction cas_2_non_hrc
  appel_4_3_non_hier <- function(dfs, i){
    if (i <= length(codes_split_1)) {
      totcode[v1] <- codes_split_1[[i]][1]
      nom_dfs <- paste(nom_dfs, totcode[v1], sep = "_")
      passage_4_3_cas_1_non_hr(dfs,nom_dfs,v1,v2, totcode, hrcfiles, dir_name, sep = sep)
    } 
    else {
      print(paste("Index", i, "is out of bounds for codes_split."))
      return(NULL)
    }
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res <- lapply(seq_along(liste_df_4_var_1_non_hr), function(i) {
    appel_4_3_non_hier(liste_df_4_var_1_non_hr[[i]], i)
  })
  
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