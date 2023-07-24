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
#' @param vars_a_fusionner NULL ou vecteur des variables à fusionner 
#' (2 en dimension 4, 3 ou 4 en dimension 5)
#' @param nb_tab stratégie à suivre pour choisir les variables automatiquement :
#' min : minimiser le nombre de table;
#' max : maximise le nombre de table;
#' smart : minimise le nombre de table sous la contrainte de leur nombre de ligne
#' @param LIMIT nombre de ligne maximale autorisé dans le cas smart
#' @param vec_sep vecteur des séparateurs candidats à utiliser
#'
#' @return liste(tabs, hrcs, alt_tot, vars, sep)
#' tab : liste nommée des dataframes à 3 dimensions
#' doté de hiérarchies emboitées
#' hrcs : liste nommée des hrc spécifiques aux variables créés
#'            lors de la fusion pour passer en dimension 3
#' alt_tot : liste nommée des totaux spécifiques aux variables créés
#'              lors de la fusion pour passer en dimension 3
#' vars : liste nommée de vecteur représentant les variables fusionnées
#'            lors des deux étapes de réduction de dimensions
#' sep : séparateur utilisé pour lier les variables
#' @export
#'
#' @examples
#' TODO exemples
gen_tabs_5_4_to_3<-function(
    dfs,
    nom_dfs,
    totcode,
    hrcfiles = NULL,
    sep_dir=FALSE, 
    hrc_dir="hrc_alt",
    vars_a_fusionner = NULL,
    vec_sep = c("\\_+_", "\\_!_", "\\_?_"),
    nb_tab = "min",
    LIMIT = 15000
){
  
  dfs <- as.data.frame(dfs)
  
  if (any(!names(hrcfiles) %in% names(dfs))){
    stop("Au moins une modalité de hrcfiles n'est pas présent dans dfs !")
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
  
  if (!nb_tab %in% c('min','max','smart')){
    stop("nb_tab doit être égale à 'min', 'max' ou 'smart' !")
  }
  
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
        tic("Choix des variables")
        # Proposition de varibales
        choix_3_var <- choisir_var_a_fusionner_general(dfs=data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 3,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        
        print(choix_3_var)
        choix_4_var <- choisir_var_a_fusionner_general(dfs = data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 4,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        print(choix_4_var)
        
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
        temps_choisir_var <- toc(log = TRUE)
        print(temps_choisir_var$callback_msg)
      
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
    tic("Passage de 5 à 3")
    res<-passer_de_5_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             v1=v1,v2=v2,v3=v3,v4=v4,
                             sep=sep,
                             select_hier = select_hier)
    temps_reduc_dim <- toc()
    print(temps_reduc_dim$callback_msg)
    
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
        tic("Choix des variables")
        choix_2_var <- choisir_var_a_fusionner_general(dfs=data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 2,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        v1 <- choix_2_var[[1]]
        v2 <- choix_2_var[[2]]
        temps_choisir_var <- toc(log = TRUE)
        print(temps_choisir_var$callback_msg)
        
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
    tic("Passage de 4 à 3")
    res<-passer_de_4_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             v1=v1,v2=v2,
                             sep=sep,
                             select_hier = select_hier)
    temps_reduc_dim <- toc()
    print(temps_reduc_dim$callback_msg)
    
    
    return(format(res,nom_dfs,sep,totcode,hrcfiles))
  }
}