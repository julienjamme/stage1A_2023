# Compte le nombre de noeud dans un fichier hiérarchique
# 2 arguments sont attendus
# soit une liste nommée et une variable
# soit un hrc et hrc_name = FALSE
nb_noeuds <- function(hrcfiles, v = NULL, hrc_name = TRUE) {
  # Vérifie si la variable à un fichier hrc associé ou si hrc_names == FALSE
  if (hrc_name && !(v %in% names(hrcfiles)) || (!hrc_name && is.null(hrcfiles))) {
    # Variable non-hierarchique ou hrcfiles == NULL
    return(1)
  }
  
  # Prends le fichier spécifie si hrc_name = TRUE, sinon prend le hrc renseigné directement
  hrc <- ifelse(hrc_name, hrcfiles[[v]], hrcfiles)
  
  # Valeur non importante pour la suite
  total <- "Ceci_Est_Mon_Total"
  
  # Convertir en hierarchie
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>%
    sdcHierarchies::hier_convert(as = "sdc")
  
  # Renvoie le nombre de noeuds
  return(length(res_sdc$dims))
}

#' Fonction passant de 5 à 3 variables catégorielles
#'
#' @param dfs data.frame à 5 variabls catégorielles (n >= 3 dans le cas général)
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param totcode vecteur normée des totaux pour les variables catégorielles
#' @param hrcfiles vecteur normée des hrc pour les variables catégorielles hierarchiques
#' @param sep_dir permet de forcer l'écriture des hrc dans un dossier séparé
#' par défault à FALSE
#' @param hrc_dir dossier où écrire les fichiers hrc si l'on force l'écriture
#' dans un nouveau dossier ou si aucun dossier n'est spécifié dans hrcfiles
#' @param v1 permet de forcer la valeur de la première variable à fusionner 
#' lors du passage de 5 à 4 dimensions, non spéficié par défault (NULL) 
#' @param v2 permet de forcer la valeur de la seconde variable à fusionner 
#' lors du passage de 5 à 4 dimensions, non spéficié par défault (NULL) 
#' @param v3 permet de forcer la valeur de la première variable à fusionner 
#' lors du passage de 4 à 3 dimensions, non spéficié par défault (NULL) 
#' @param v4 permet de forcer la valeur de la seconde variable à fusionner 
#' lors du passage de 4 à 3 dimensions, non spéficié par défault (NULL)
#' @param sep séparateur utilisé lors de la concaténation des variables
#' @param select_hier précise si l'on préfère selectionner les variables hiérarchiques avec
#' le plus de noeuds en priorité (hier=TRUE) ce qui génère plus de tableaux
#' mais de taille moins importante
#' ou bien les variables non hiérarchiques avec le moins de modalité (hier=FALSE)
#' pour créer le moins de tableau
#' @param verbose print les différentes étapes de la fonction pour avertir
#' l'utilisateur de l'avancement principalement pour la fonction générale
#' gen_tabs_5_4_to_3()
#' 
#' @return liste(tabs, hrcs5_4,hrcs4_3, alt_tot5_4,alt_tot4_3, vars)
#' tab : liste nommée des dataframes à 3 dimensions (n-2 dimensions dans le cas général)
#' doté de hiérarchies emboitées
#' hrcs5_4 : liste nommée des hrc spécifiques à la variable crée via la fusion
#'                lors du passage de 5 à 4 dimensions
#' hrcs4_3 : liste nommée des hrc spécifiques à la variable crée via la fusion
#'                lors du passage de 4 à 3 dimensions
#' alt_tot5_4 : liste nommée des totaux lors du passage de 5 à 4 dimensions
#' alt_tot4_3 : liste nommée des totaux lors du passage de 4 à 3 dimensions
#' vars : liste nommée de vecteur représentant les variables fusionnées
#' lors des deux étapes de réduction de dimensions
#' @export
#'
#' @examples
#' 
#' library(dplyr)
#' 
#' source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3.R",encoding = "UTF-8")
#' source("R/passage_5_3.R",encoding = "UTF-8")
#' 
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
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
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
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
#' # Résultats de la fonction
#' res1 <- passer_de_5_a_3_var(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   v1 = "ACT",
#'   v2 = "AGE",
#'   v3 = "SEX",
#'   v4 = "ECO"
#' )
#' 
#' res2 <- passer_de_5_a_3_var(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   verbose = TRUE
#' )
passer_de_5_a_3_var <- function(dfs, nom_dfs,totcode, hrcfiles = NULL, 
                                sep_dir = FALSE, hrc_dir = "hrc_alt",
                                v1 = NULL,v2 = NULL,v3 = NULL,v4 = NULL, 
                                sep = "_", select_hier = FALSE,
                                verbose = FALSE){
  
  # Mise à jour du dossier en sortie contenant les hiérarchie
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }
  
  # On enlève une dimension à notre dataframe de départ
  res_5_4 <- passer_de_4_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, dir_name,
                                 v1 = v1, v2 = v2, sep = sep, select_hier = select_hier)
  if (verbose){
    print(paste(length(res_5_4$tabs),"tables créés"))
    print(c("Réduction de 4 à 3..."))
  }
  
  # Récupération des variables fusionnées
  v1f <- res_5_4$vars[[1]]
  v2f <- res_5_4$vars[[2]]
  new_var = paste(v1f, v2f, sep=sep)
  
  # Mise à jour des totaux
  totcode2 <- totcode
  totcode2 <- totcode2[!(names(totcode2) %in% c(v1f, v2f))]
  # totcode2[[new_var]] <- 1
  
  # Mise à jour des fichiers hrc
  hrcfiles2 <- hrcfiles
  hrcfiles2 <- hrcfiles2[!(names(hrcfiles2) %in% c(v1f, v2f))]
  
  # Les variables catégorielles sans hiérarchie dans nos tableaux à 4 dimensions
  var_cat <- c(names(totcode2),new_var)
  
  var_sans_hier <- intersect(
    setdiff(names(dfs), names(hrcfiles2)),
    var_cat
  )
  
  # Choix des variables pour le passage 4 -> 3 et vérification de celles renseignées en argument
  # On choisit dès maintenant v3 et v4 pour être sûr que la même variable
  # est créé au sein de tous les sous tableaux
  
  # Première variable pour le passage 4 à 3
  if (!is.null(v3)){
    if (!(v3 %in% var_cat)){
      stop(paste("v3 n'est pas une variable catégorielle, v3 = ", v3,
                 "Les variables catégorielles sont : ",paste(var_cat, collapse = ", ")), sep = "")
    }
  } else {
    # on choisit une variable en évitant v4
    v3 <- choisir_var(dfs = dfs[setdiff(names(dfs),v4)],
                      totcode = totcode2[setdiff(names(totcode2),v4)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v4)],
                      select_hier = select_hier)
    
    # On regarde si la variable fusionnée à moins de noeuds que la variable selectionnée
    nb_noeuds_v3 <- nb_noeuds(hrcfiles2, v=v3)
    if (!is.null(v4)){
      # Nous devons faire deux if différents sinon NULL != new_var fait planter !
      if (v4 != new_var & select_hier == TRUE){
        v3 <- new_var
      }
      # Si v4 = NULL pas besoin de comparer v4 != new_var
    } else if (select_hier == TRUE){
      v3 <- new_var
    }
  }
  
  # Seconde variable pour le passage 4 à 3
  if (!is.null(v4)){
    if (!(v4 %in% var_cat)){
      stop(paste("v4 n'est pas une variable catégorielle, v4 = ", v4,
                 "Les variables catégorielles sont : ",paste(var_cat, collapse = ", ")), sep = "")
    }
    if (v3 == v4){
      stop("Erreur. Vous essayez de fusionner une variable avec elle-même")
    }
    
  } else {
    # on choisit une variable en évitant v3
    v4 <- choisir_var(dfs = dfs[setdiff(names(dfs),v3)],
                      totcode = totcode2[setdiff(names(totcode2),v3)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v3)],
                      select_hier = select_hier)
    
    # On regarde si la variable fusionnée à moins de noeuds que la variable selectionnée
    nb_noeuds_v4 <- nb_noeuds(hrcfiles2, v=v4)
    # Rq : v3 ne peut pas être NULL
    if (v3 != new_var & select_hier == TRUE){
      v4 <- new_var
    }
  }
  
  appel_4_3_gen <- function(nom_dfsb){
    # Mise à jour des arguments de la fonction
    dfsb <- res_5_4$tabs[[nom_dfsb]]
    
    hrcfiles2b <-  c(hrcfiles2, res_5_4$hrcs[[nom_dfsb]])
    names(hrcfiles2b)[length(hrcfiles2b)] <- new_var
    
    totcode2[[new_var]] <- res_5_4$alt_tot[[nom_dfsb]]
    
    passer_de_4_a_3_var(dfsb, nom_dfsb,totcode2, hrcfiles2b, sep_dir = TRUE,
                        hrc_dir = dir_name,v1 = v3, v2 = v4, 
                        sep=sep)
  }
  
  # On transforme tous nos tableaux de 4 var en 3 var
  res_5_3 <- lapply(
    names(res_5_4$tabs),
    appel_4_3_gen
  )
  
  tabs <- unlist(lapply(res_5_3, function(x) x$tabs), recursive = FALSE)
  hrcs4_3 <- unlist(lapply(res_5_3, function(x) x$hrcs), recursive = FALSE)
  alt_tot4_3 <- unlist(lapply(res_5_3, function(x) x$alt_tot), recursive = FALSE)
  
  vars1 <- res_5_4$vars
  vars2 <- res_5_3[[1]]$vars # les variables fusionnées sont toujours les mêmes
  vars_tot <- list(vars1,vars2)
  names(vars_tot) <- c("five_to_three","four_to_three")
  
  # Mémorisation de res5_4
  
  # Cas on fusionne 4 variables différentes 
  if (!(new_var %in% c(v3,v4))){
    # On répète autant de fois  res5_4[i] que le tableau va créer 
    # de tableaux à 3 dimensions
    
    # Chaque tableau à 4 dimensions va créer le même nombre de tableau à 3 dimensions
    # car les variables selectionnées ont les mêmes modalités dans chacun d'eux
    nb_rep <- length(tabs) / length(res_5_4$tabs)
    hrcs5_4 <- as.list(unlist(lapply(res_5_4$hrcs,
                                     function(x) rep(x,nb_rep))))
    
    alt_tot5_4 <- as.list(unlist(lapply(res_5_4$alt_tot,
                                        function(x) rep(x,nb_rep))))
    
    # Si l'on fusionne 3 variables en une, le nombre de tableaux
    # créé par chaque table change !
  } else {
    # Stocke le nom de la variable qui n'est pas new_var dans un nouvel objet
    non_fused_var <- ifelse(v3 == new_var, v4, v3)
    
    # Calcule la valeur de nb_noeuds une fois pour chaque res_5_4$hrcs[[x]]
    # pour éviter de calculer deux fois la même quantité
    results <- lapply(1:length(res_5_4$hrcs), function(x) {
      nb_noeuds_val <- 2 * nb_noeuds(res_5_4$hrcs[[x]], hrc_name = FALSE) * nb_noeuds(hrcfiles2, non_fused_var)
      
      # Utilisez la valeur calculée pour hrcs5_4 et alt_tot5_4
      list(
        hrcs = rep(res_5_4$hrcs[[x]], nb_noeuds_val),
        alt_tot = rep(res_5_4$alt_tot[[x]], nb_noeuds_val)
      )
    })
    
    # Extrait les valeurs pour hrcs5_4 et alt_tot5_4
    hrcs5_4 <- as.list(unlist(lapply(results, function(x) x$hrcs)))
    alt_tot5_4 <- as.list(unlist(lapply(results, function(x) x$alt_tot)))
  }
  
  return(list(tabs=tabs,
              hrcs5_4=hrcs5_4,
              hrcs4_3=hrcs4_3,
              alt_tot5_4=alt_tot5_4,
              alt_tot4_3=alt_tot4_3,
              vars=vars_tot)
  )
}