

#'
#' @param res est le résultat de la fonction qui fait passer un tableaux à 4
#' varibales catégorielles à 3 . Il est de la forme list( tabs,hrcs,vars )
#' avec tabs une liste composant les différents dataframes nommées
#' et crées à partir de 
#' hrcs une liste de characters nommés contenant les chemins des hiérarchies
#' et vars un vecteur contenant les Deux variables fusionnées que l'on va vouloir 
#' défuisonnées 
#' @param data le dataframe donné par l'utilisatuer à l'entrée#'
#' @return un dataframe a 4 variables catégorielles contenant toutes les 
#' données 


library(stringr)


passer_3_41 <- function(res, data) {
  #On récupère les variables et leur modalités
  var1 <- res$vars[1]
  var2 <- res$vars[2]
  
  mod1 <- unique(data[[var1]])
  mod2 <- unique(data[[var2]])
  var_cross <- paste(var1, var2, sep = "_")
  #On récupère les noms des différents tableaux et
 
  
  nom_des_dfs<-names(res$tabs)
  #On initialise la boucle avec list_tabs pour stocker les tableaux changés 
  separateur <- "_"
  list_tabs <- list()
  
  #On regarde tous les tableaux possibles
  for (nom in nom_des_dfs) {
    var <- list()  # liste qui conserve les variables du tableaux demandés
    #On observe les termes séparées par _ 
    
    for (chaine in res$tabs[[nom]][[var_cross]]) {
      positions <- str_locate_all(chaine, separateur)[[1]][,"start"]
      var_poss <- lapply(positions, function(pos) {
        partie1 <- str_sub(chaine, end = pos - 1)
        partie2 <- str_sub(chaine, start = pos + 1)
        list(partie1, partie2)
      })
      # On regarde si ses termes sont des modalités de nos variables
      var <- append(var, lapply(var_poss, function(x) {
        if (x[[1]] %in% mod1 & x[[2]] %in% mod2) {
          return(c(x[[1]], x[[2]]))
        }
      }))
    }
    #On crée nos collonnes avec les bons noms et on supprime la collone des variables fusionées 
    res$tabs[[nom]][[var1]] <- unlist(lapply(var, function(x) x[[1]]), recursive = FALSE)
    res$tabs[[nom]][[var2]] <- unlist(lapply(var, function(x) x[[2]]), recursive = FALSE)
    res$tabs[[nom]][[var_cross]] <- NULL
    res$tabs[[nom]][[var1]] <- unname(res$tabs[[nom]][[var1]])
    res$tabs[[nom]][[var2]] <- unname(res$tabs[[nom]][[var2]])
    
    
  }
  # On récupère 
  l <- unlist(list(res$tabs),recursive = FALSE)
  
  
  ca2<-do.call(rbind,l)
  
  
  
    
  return (unique(ca2))
}



