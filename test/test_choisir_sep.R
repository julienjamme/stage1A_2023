library(stringr)

source("R/choisir_sep.R")

data <- expand.grid(
  AGE = c("+","ça va ?","Mais non, Pas possible !!!"),
  ECO = c("ça fait 5€"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

# test choisir le bon séparateur lorsqu'il existe
liste_sep = c("\\+", "\\!", "\\?","\\$","\\£",
              "\\€","\\:","\\;","\\~","\\&",
              "\\#")
choisir_sep(data,liste_sep = liste_sep) == "$"

# test aucun séparateur disponible
liste_sep = c("\\+", "\\!", "\\?")
choisir_sep(data,liste_sep = liste_sep) == "_+_+_+_+"

data <- expand.grid(
  AGE = c("ça va ?","Mais non, Pas possible !!!"),
  ECO = c("ça fait 5€"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

# test choisir le bon séparateur lorsqu'il existe
liste_sep = c("\\+", "\\!", "\\?","\\$","\\£",
              "\\€","\\:","\\;","\\~","\\&",
              "\\#")
choisir_sep(data,liste_sep = liste_sep) == "+"
