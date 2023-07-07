#' Choix du séparateur
#'
#' @param data un dataframe ne contenant que des variables catégorielles
#' @param liste_sep un vecteur des séparateurs à tester
#' Les séparateurs doivent être précédés de "\\" et doivent être compatible
#' avec l'utilisation de str_detect
#' @return un séparateur de liste_sep présent dans aucune des modalités s'il existe
#' sinon renvoie NULL
#' 
#' @export
#'
#' @examples
choisir_sep <- function(data, liste_sep = c("\\+", "\\!", "\\?","\\:",
                                            "\\;","\\~","\\&",
                                            "\\#")){
  liste_var<-names(data)
  liste_mod<-unique(unlist(lapply(data, unique)))
  liste_mod<-c(liste_mod,liste_var)
  n_sep <- length(liste_sep)
  
  i = 0
  is_in_mod = TRUE
  while (i <= n_sep & is_in_mod){
    i <- i + 1
    sep <- liste_sep[i]
    is_in_mod = sum(unlist(lapply(liste_mod, function(x) str_detect(x,sep)))) > 0
  }
  
  # On a un sépérateur qui fonctionne !
  if (i  <= n_sep){
    # On enlève les \ devant
    sep <- str_sub(liste_sep[i], start = 2, end = 2)
    
    # On renvoie le triple du séparateur
    sep <- rep(sep, times = 3)
    return(paste0(sep, collapse = ""))
  } else {
    # Tous les séparateurs sont présents dans les modalités
    # warning("Aucun séparateur trouvé. Veuillez tester d'autres valeurs")
    # le faire sans les \\
    return (paste0(liste_sep, collapse = ""))
  }
}
