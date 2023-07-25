#' Fonction générale choissisant le bon séparateur
#' puis appliquant la réduction de dimension
#'
#' @param dfs data.frame à 4 ou 5 variabls catégorielles
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param totcode vecteur normée des totaux pour les variables catégorielles
#' @param hrcfiles vecteur normée des hrc pour les variables catégorielles hierarchiques
#' @param sep_dir permet de forcer l'écriture des hrc dans un dossier séparé
#' par défault à FALSE
#' @param hrc_dir dossier où écrire les fichiers hrc si l'on force l'écriture
#' dans un nouveau dossier ou si aucun dossier n'est spécifié dans hrcfiles
#' @param vars_a_fusionner NULL ou vecteur des variables à fusionner :
#' 2 en dimension 4 ; 3 ou 4 en dimension 5
#' @param nb_tab stratégie à suivre pour choisir les variables automatiquement :
#' min : minimiser le nombre de table;
#' max : maximise le nombre de table;
#' smart : minimiser le nombre de table sous la contrainte de leur nombre de ligne
#' @param LIMIT nombre de ligne maximale autorisé dans le cas smart
#' @param vec_sep vecteur des séparateurs candidats à utiliser
#'
#' @return liste(tabs, hrcs, alt_tot, vars, sep, totcode, hrcfiles, fus_vars)
#' tab : liste nommée des dataframes à 3 dimensions
#' doté de hiérarchies emboitées
#' hrcs : liste nommée des hrc spécifiques aux variables créés
#'            lors de la fusion pour passer en dimension 3
#' alt_tot : liste nommée des totaux spécifiques aux variables créés
#'              lors de la fusion pour passer en dimension 3
#' vars : variable catégorielles des dataframes en sortie
#' sep : séparateur utilisé pour lier les variables
#' totcode : vecteur nommé des totaux pour toutes les variables catégorielles
#' hrcfiles : liste nommée des hrc spécifiques aux variables créés
#'              lors de la fusion pour passer en dimension 3
#' fus_vars : liste nommée de vecteur représentant les variables fusionnées
#'            lors des deux étapes de réduction de dimensions
#' @export
#'
#' @examples
#' library(dplyr)
#' source("R/passage_5_3.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3.R",encoding = "UTF-8")
#' source("R/choisir_sep.R",encoding = "UTF-8")
#' source("R/format.R",encoding = "UTF-8")
#' source("R/length_tabs.R",encoding = "UTF-8")
#' source("R/nb_tab.R",encoding = "UTF-8")
#' source("R/chercher_combinaison_variable_a_fusionner.R",encoding = "UTF-8")
#' 
#' # Exemples dimension 4
#' 
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1)
#' 
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' 
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' # Résultat de la fonction en forc_ant des variabels à fusionner
#' res1 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   vars_a_fusionner = c("ACT","GEO")
#' )
#' 
#' # Résultat de la fonction (minimise le nombre de tableau créé par défault)
#' res2 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output"
#' )
#' 
#' # Résultat de la fonction (minimise le nombre de tableau créé par défault)
#' res3 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   LIMIT = 1250,
#'   nb_tab = "smart"
#' )
#' 
#' # Exemple dimension 5
#' 
#' data <- expand.grid(
#'   ACT = c("Total_A", paste0("A", seq(1,5),"_"),paste0("A1_", seq(1,7)),paste0("A2_", seq(1,9))),
#'   GEO = c("Total_G", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GB3","GB4"),
#'   SEX = c("Total_S", "F", "M","F1","F2","M1","M2"),
#'   AGE = c("Ensemble", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB","Ménages","Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>% 
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1:n())
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", seq(1,5),"_")) %>% 
#'   sdcHierarchies::hier_add(root = "A1_", nodes = paste0("A1_", seq(1,7))) %>% 
#'   sdcHierarchies::hier_add(root = "A2_", nodes = paste0("A2_", seq(1,9))) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total_G", nodes = c("GA","GB")) %>% 
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3")) %>% 
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
#' 
#' # Résultat de la fonction
#' res4 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total_S",AGE="Ensemble", GEO="Total_G", ACT="Total_A", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'   sep_dir = TRUE,
#'   hrc_dir = "output"
#' )
gen_tabs_5_4_to_3<-function(
    dfs,
    nom_dfs,
    totcode,
    hrcfiles = NULL,
    sep_dir = FALSE, 
    hrc_dir="hrc_alt",
    vars_a_fusionner = NULL,
    vec_sep = c("\\_+_", "\\_!_", "\\_?_"),
    nb_tab = "min",
    LIMIT = 15000
){
  
  dfs <- as.data.frame(dfs)
  
  if (!is.character(nom_dfs)){
    stop("nom_dfs doit être une chaine de caractère")
  }
  
  if (any(!names(totcode) %in% names(dfs))){
    stop("Au moins une modalité de totcode n'est pas présent dans dfs !")
  }
  
  if (!(length(totcode) %in% c(4,5))){
    stop("Veullez entrer un dataframe à 4 ou 5 variables catégorielles !")
  }
  
  if (length(totcode) == 4 & !length(vars_a_fusionner) %in% c(0,2)){
    stop("Dans le cas à 4 dimensions, veuillez spécifier 2 variables ou bien laisser vars_a_fusionner à NULL !")
  }
  
  if (length(totcode) == 5 & !length(vars_a_fusionner) %in% c(0,3,4)){
    stop("Dans le cas à 5 dimensions, veuillez spécifier 2 ou 3 variables ou bien laisser vars_a_fusionner à NULL !")
  }
  
  if (any(!names(hrcfiles) %in% names(dfs))){
    stop("Au moins une modalité de hrcfiles n'est pas présent dans dfs !")
  }
  
  if (!is.logical(sep_dir)){
    stop("sep_dir doit être logique")
  }
  
  if (!is.character(hrc_dir)){
    stop("hrc_dir doit être une chaine de caractère")
  }

  if (!nb_tab %in% c('min','max','smart')){
    stop("nb_tab doit être égale à 'min', 'max' ou 'smart' !")
  }
  
  if (!is.null(vars_a_fusionner)){
    if (any(!vars_a_fusionner %in% names(totcode))){
      stop("vars_a_fusionner contient au moins une variable n'étant pas dans totcode !")
    }
  }
  
  LIMIT <- as.numeric(LIMIT)
  
  
  # Choix du séparateur
  data_var_cat <- dfs[names(dfs) %in% names(totcode)]
  sep <- choisir_sep(data_var_cat,vec_sep)
  
  if (length(totcode) == 5){
    # L'utilisateur a spécifié les variables à fusionner
    if (length(vars_a_fusionner) == 3){
      v1 <- vars_a_fusionner[[1]]
      v2 <- vars_a_fusionner[[2]]
      v3 <- vars_a_fusionner[[3]]
      v4 <- paste(v1,v2,sep=sep)
      
    } else if (length(vars_a_fusionner) == 4){
      v1 <- vars_a_fusionner[[1]]
      v2 <- vars_a_fusionner[[2]]
      v3 <- vars_a_fusionner[[3]]
      v4 <- vars_a_fusionner[[4]]
      
      # L'utilisateur n'a pas spécifié les variables à fusionner
      # Nous devons les calculer
    } else {

      if (nb_tab == 'smart'){
        # Proposition de varibales
        choix_3_var <- choisir_var_a_fusionner_general(dfs=data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 3,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        
        choix_4_var <- choisir_var_a_fusionner_general(dfs = data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 4,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        
        # Nombre de tableau généré par chaque proposition
        choix_3_var_nb_tab <- calculer_nb_tab(v1 = choix_3_var[[1]],
                                              v2 = choix_3_var[[2]],
                                              v3 = choix_3_var[[3]],
                                              hrcfiles=hrcfiles,
                                              data=dfs)
        
        choix_4_var_nb_tab <- calculer_nb_tab(v1 = choix_4_var[[1]],
                                              v2 = choix_4_var[[2]],
                                              v3 = choix_4_var[[3]],
                                              v4 = choix_4_var[[4]],
                                              hrcfiles=hrcfiles,
                                              data=dfs)
        
        # On choisit la meilleure proposition
        if (choix_3_var_nb_tab < choix_4_var_nb_tab){
          v1 <- choix_3_var[[1]]
          v2 <- choix_3_var[[2]]
          v3 <- choix_3_var[[3]]
          v4 <- paste(v1,v2,sep=sep)
        } else {
          v1 <- choix_4_var[[1]]
          v2 <- choix_4_var[[2]]
          v3 <- choix_4_var[[3]]
          v4 <- choix_4_var[[4]]
        }
      
      # Retour à l'implémentation primitive pour minimiser ou maximiser
      # le nombre de tableaux
      # Puisque la vieille implémentation n'est pas si mauvaise et est
      # plus rapide que de calculer la taille et le nombre de tableau généré
      # ce qui prend ~10se
      } else {
        v1 <- NULL
        v2 <- NULL
        v3 <- NULL
        v4 <- NULL
        select_hier <- if (nb_tab == 'max') TRUE else FALSE
      }
    }
    res<-passer_de_5_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             v1=v1,v2=v2,v3=v3,v4=v4,
                             sep=sep,
                             select_hier = select_hier)
    
    return(format(res,nom_dfs,sep,totcode,hrcfiles))
    
  } else if (length(totcode) == 4){
    
    # L'utilisateur a spécifié les variables à fusionner
    if (length(vars_a_fusionner) == 2){
      v1 <- vars_a_fusionner[[1]]
      v2 <- vars_a_fusionner[[2]]
      
      # L'utilisateur n'a pas spécifié les variables à fusionner
      # Nous devons les calculer
    } else {
      
      if (nb_tab == 'smart'){ 
        choix_2_var <- choisir_var_a_fusionner_general(dfs=data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 2,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        v1 <- choix_2_var[[1]]
        v2 <- choix_2_var[[2]]
        
      # Retour à l'implémentation primitive pour minimiser ou maximiser
      # le nombre de tableaux
      # Puisque la vieille implémentation n'est pas si mauvaise et est
      # plus rapide que de calculer la taille et le nombre de tableau généré
      # pour gagner quelques dixièmes
      } else {
        v1 <- NULL
        v2 <- NULL
        select_hier <- if (nb_tab == 'max') TRUE else FALSE
      }
    }

    res<-passer_de_4_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             v1=v1,v2=v2,
                             sep=sep,
                             select_hier = select_hier)
    
    return(format(res,nom_dfs,sep,totcode,hrcfiles))
  }
}
