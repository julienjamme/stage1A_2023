#' Separator Selection
#'
#' @param data a dataframe containing only categorical variables
#' @param liste_sep a vector of separators to test
#' The separators must be preceded by "\\" and must be compatible
#' with the use of str_detect
#' @return a separator from liste_sep that is not present in any of the modalities if it exists,
#' otherwise returns NULL
#' 
#' @export
#' 
#' @examples
#' library(dplyr)
#' library(stringr)
#' 
#' source("R/choisir_sep.R")
#' 
#' data <- expand.grid(
#'   AGE = c("+", "How are you?", "No, Not possible!!!"),
#'   ECO = c("It costs 5€"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#' 
#' liste_sep = c("\\+", "\\!", "\\?")
#' 
#' # All separators appear in the modalities
#' choisir_sep(data, liste_sep = liste_sep)
#' 
#' choisir_sep(data)
choisir_sep <- function(
    data,
    liste_sep = c("\\_+_", "\\_!_",
                  "\\_?_", "___", "_z_z_z_z")
                  )
{
  require(stringr)
  
  liste_var <- names(data)
  liste_mod <- unique(unlist(lapply(data, unique)))
  liste_mod <- c(liste_mod, liste_var)
  n_sep <- length(liste_sep)
  
  i = 0
  is_in_mod = TRUE
  while (i <= n_sep & is_in_mod) {
    i <- i + 1
    sep <- liste_sep[i]
    is_in_mod = sum(unlist(lapply(liste_mod, function(x) stringr::str_detect(x, sep)))) > 0
  }
  
  # We have a working separator!
  if (i <= n_sep) {
    # Remove the "\" in front of the separator
    sep <- stringr::str_sub(liste_sep[i], start = 2)
    
    # Return the concatenated separator thrice
    return(paste0(sep,
                  collapse = ""))
  } else {
    # Return a default separator (four underscores)
    return(paste(rep("_+", 4),
                 collapse = ""))
  }
}
