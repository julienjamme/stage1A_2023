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
#' library(dplyr)
#' data <- expand.grid(
#'   AGE = c("+","ça va ?","Mais non, Pas possible !!!"),
#'   ECO = c("ça fait 5€"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#' 
#' liste_sep = c("\\+", "\\!", "\\?","\\$","\\£",
#'               "\\€","\\:","\\;","\\~","\\&",
#'               "\\#")
#' choisir_sep(data,liste_sep = liste_sep) == "$"
#' 
#' 
#' liste_sep = c("\\+", "\\!", "\\?")
#' choisir_sep(data,liste_sep = liste_sep)
#' 
#' choisir_sep(data)
choisir_sep <- function(data, liste_sep = c("\\_+_", "\\_!_", "\\_?_")){
  require(stringr)
  
  liste_var<-names(data)
  liste_mod<-unique(unlist(lapply(data, unique)))
  liste_mod<-c(liste_mod,liste_var)
  n_sep <- length(liste_sep)
  
  i = 0
  is_in_mod = TRUE
  while (i <= n_sep & is_in_mod){
    i <- i + 1
    sep <- liste_sep[i]
    is_in_mod = sum(unlist(lapply(liste_mod, function(x) stringr::str_detect(x,sep)))) > 0
  }
  
  # On a un sépérateur qui fonctionne !
  if (i  <= n_sep){
    # On enlève les \ devant
    sep <- stringr::str_sub(liste_sep[i], start = 2)
    
    # On renvoie le triple du séparateur
    return(paste0(sep, collapse = ""))
  } else {
    return(paste(rep("_+",4), collapse = ""))
    # # Tous les séparateurs sont présents dans les modalités
    # # On concatène tous les séparateurs pour construire un super séparateur
    # return(stringr::str_flatten(
    #   lapply(liste_sep,
    #          function(x) stringr::str_sub(x, start = 2)
    #   )
    # )
    # )
  }
}
