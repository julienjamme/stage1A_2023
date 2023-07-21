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
#' @param v1 permet de forcer la valeur de la première variable à fusionner 
#' lors du passage de la première réduction de dimension, non spéficié par défault (NULL) 
#' @param v2 permet de forcer la valeur de la seconde variable à fusionner 
#' lors du passage de la première réduction de dimension, non spéficié par défault (NULL) 
#' @param v3 permet de forcer la valeur de la première variable à fusionner 
#' lors du passage de la seconde réduction de dimension, non spéficié par défault (NULL) 
#' @param v4 permet de forcer la valeur de la seconde variable à fusionner 
#' lors du passage de la seconde réduction de dimension, non spéficié par défault (NULL) 
#' @param vec_sep vecteur des séparateurs candidats à utiliser
#' @param select_hier précise si l'on préfère selectionner les variables hiérarchiques avec
#' le plus de noeuds en priorité (hier=TRUE) ce qui génère plus de tableaux
#' mais de taille moins importante
#' ou bien les variables non hiérarchiques avec le moins de modalité (hier=FALSE)
#' pour créer le moins de tableau
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
gen_tabs_5_4_to_3<-function(dfs,nom_dfs,totcode,hrcfiles = NULL,
                            sep_dir=FALSE,hrc_dir="hrc_alt",
                            v1=NULL,v2=NULL,v3=NULL,v4=NULL,
                            vec_sep = c("\\+", "\\!", "\\?","\\:",
                                        "\\;","\\~","\\&","\\#"),
                            select_hier = FALSE){
  
  # On épure les fichiers hrc et les totaux au cas où
  # ils concernent des variables qui n'existent pas
  hrcfiles <-  hrcfiles[names(hrcfiles) %in% names(dfs)]
  totcode <-  totcode[names(totcode) %in% names(dfs)]
  
  # Choix du séparateur
  data_var_cat <- dfs[names(dfs) %in% names(totcode)]
  sep <- choisir_sep(data_var_cat,vec_sep)
  
  if (length(totcode) == 5){
    res<-passer_de_5_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             v1=v1,v2=v2,v3=v3,v4=v4,
                             sep=sep,
                             select_hier=select_hier)
    
    return(format(res,nom_dfs,sep,totcode,hrcfiles))
    
  } else if (length(totcode) == 4){
    res<-passer_de_4_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             v1=v1,v2=v2,
                             sep=sep,
                             select_hier=select_hier)
    
    return(format(res,nom_dfs,sep,totcode,hrcfiles))
  }
}