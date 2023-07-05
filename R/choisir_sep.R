#' Choix du séparateur
#'
#' @param data un dataframe ne contenant que des variables catégorielles
#' @param liste_sep un vecteur des séparateurs à tester
#' Les séparateurs doivent pouvoir être appliqué avec str_detect sous peine d'erreur
#' (càd : éviter les regex pattern)
#' @return un séparateur de liste_sep présent dans aucune des modalités s'il existe
#' sinon renvoie NULL
#' @export
#'
#' @examples
choisir_sep <- function(data, liste_sep = c(".++.", ".?:?.", "€$/")){

  liste_mod = unique(unlist(lapply(data, unique)))
  n_mod <- length(liste_sep)
  
  i = 0
  is_in_mod = TRUE
  while (i < n_mod & is_in_mod){
    i <- i + 1
    sep <- liste_sep[i]
    is_in_mod = sum(unlist(lapply(liste_mod, function(x) str_detect(x,sep))))
  }
  
  # On a un sépérateur qui fonctionne !
  if (i <= n_mod){
    return(liste_sep[i])
  }
  # Tous les séparateurs sont présents dans les modalités
  return (NULL)
}