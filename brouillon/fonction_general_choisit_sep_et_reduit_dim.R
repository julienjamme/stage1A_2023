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
#' @param liste_sep liste des séparateurs candidats à utiliser
#'
#' @return liste(tabs, hrcs, alt_tot, vars)
#' tab : liste nommée des dataframes à 3 dimensions
#' doté de hiérarchies emboitées
#' hrcs : liste nommée des hrc spécifiques aux variables créés
#'            lors de la fusion pour passer en dimension 3
#' alt_tot : liste nommée des totaux spécifiques aux variables créés
#'              lors de la fusion pour passer en dimension 3
#' vars : liste nommée de vecteur représentant les variables fusionnées
#'            lors des deux étapes de réduction de dimensions
#' @export
#'
#' @examples
gen_tabs_5_4_to_3<-function(dfs,nom_dfs,totcode,hrcfiles = NULL,sep_dir=FALSE,
                        hrc_dir="hrc_alt",
                        liste_sep = c("\\+", "\\!", "\\?","\\:",
                                      "\\;","\\~","\\&","\\#")){
  
  data_var_cat <- dfs[names(dfs) %in% names(totcode)]
  
  sep <- choisir_sep(data_var_cat,liste_sep )
    
  if (length(totcode)==5){
    res<-passer_de_5_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             sep=sep)
    
    return(format(res,nom_dfs))
  }
  if (length(totcode)==4){
    res<-passer_de_4_a_3_var(dfs=dfs,
                             nom_dfs=nom_dfs,
                             totcode=totcode,
                             hrcfiles=hrcfiles,
                             sep_dir=sep_dir, 
                             hrc_dir=hrc_dir,
                             sep=sep)
    
    return(format(res,nom_dfs))
  }
}
