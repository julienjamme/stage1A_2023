#' Title
#'
#' @param res resultat de la fusion de variable composéee d'une liste de liste de tableaux,
#' une liste de fichiers hiérarchique, une liste de sous_totaux associées à ses fichiers,
#' et une liste de vectuer de variables ou un vectuer de variables selon la taille de base
#' du dataframes
#' @param nom_dfs le nom du dataframes entré
#'
#' @return Une liste de liste de tableaux nommé,une liste de hrcs de mêmes noms
#' que le tableaux associées,une liste de sous_toutaux nommé de même façon que les hrcs
#' avec eb outre le nom de la variable associées aux sous_totaux , et la liste
#' des variables fusionnées ou vecteur selon la taille du tableau d'entrée

#' @examples library(dplyr)
#'
#' source("R/cas_gen_4_3.R",encoding = "UTF-8")
#' source("R/format.R",encoding = "UTF-8")
#'
#'
#' # Test 1 data avec aucune var hier -----------------------------
#'
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1)
#' nrow(data) #81 rows = 3^4
#'
#' totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
#'
#'
#' res <- passer_de_4_a_3_var(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = totcode,
#'   hrcfiles = NULL,
#'   sep_dir = TRUE,
#'   hrc_dir = "hrc_alt"
#' )
#' format(res,"nom_dfs"tab) #### A CORRIGER
#'

format <- function(res, nom_dfs, sep, totcode, hrcfiles) {
  if (class(res$vars[1]) == "character") {
    return(format4(res, nom_dfs, sep, totcode, hrcfiles))
  }
  if (class(res$vars) == "list") {
    return(format5(res, nom_dfs, sep, totcode, hrcfiles))
  }
}

#Format pour les tableaux à 4 variables

format4 <- function(res, nom_dfs, sep, totcode, hrcfiles) {
  #Données
  
  v1 <- res$vars[1]
  v2 <- res$vars[2]
  tabs <- res$tabs
  n <- length(tabs)
  var_cross <- paste(v1, v2, sep = sep)
  
  if (v1 %in% names(totcode)) {
    tot1 <- totcode[[v1]]
  } else
    tot1 <- paste(res$fus_vars[1], res$fus_vars[2], sep = sep)
  if (v2 %in% names(totcode)) {
    tot2 <- totcode[[v2]]
  } else
    tot2 <- paste(res$fus_vars[1], res$fus_vars[2], sep = sep)
  
  tot_cross <- paste(tot1, tot2, sep = sep)
  
  d <- intersect(names(res$tabs[[1]]), names(totcode))
  p <- totcode[names(totcode) %in% d]
  names(tot_cross) <- var_cross
  totcode_2 <- c(p, tot_cross)
  
  v <- c(d, var_cross)
  list_vars <- replicate(n, v, simplify = FALSE)
  names(list_vars) <- c(paste0(nom_dfs, 1:n, sep = ""))
  
  names(tabs) <- c(paste0(nom_dfs, 1:n, sep = ""))
  
  
  #Noms des hrcs
  res2 <- setNames(
    lapply(
      seq_along(res$tabs), 
      function(i) setNames(list(res$hrcs[[i]]), var_cross) 
    ),
    paste(nom_dfs, seq_along(res$tabs), sep = "")
  )
  if (length(hrcfiles)==0){res2<-NULL}
  #Noms des sous_totaux
  res3 <- setNames(
    lapply(
      seq_along(res$tabs),
      function(i) setNames(list(res$alt_tot[[i]]), var_cross)
    ),
    paste(nom_dfs, seq_along(res$tabs), sep = "")
  )
  
  
  return (
    list(
      tabs = tabs,
      hrcs = res2,
      alt_tot = res3,
      vars = list_vars,
      sep = sep,
      totcode = totcode_2,
      hrcfile = hrcfiles[!(names(hrcfiles) %in% res$vars)],
      fus_vars = res$vars
    )
  )
  
}

#Format pour les tableaux à 5 variables

format5 <- function(res, nom_dfs, sep, totcode, hrcfiles) {
  if (class(res$vars) == "list") {
    #On récupère les différentes variables
    v1 <- res$vars[[2]][1]
    v2 <- res$vars[[2]][2]
    v3 <- res$vars[[1]][1]
    v4 <- res$vars[[1]][2]
    var_cross <- paste(v1, v2, sep = sep)
    var_cross2 <- paste(v3, v4, sep = sep)
    
    # On fusionne 3 variables en une
    # Donc les infos relatifs à deux variables fusionnées lors de 5->4
    # ne nous sont plus utiles puisque la variable n'existe plus en dimension 3
    if (var_cross2 %in% c(v1, v2)) {
      res2 <- list(
        tabs = res$tabs,
        hrcs = res$hrcs4_3,
        alt_tot = res$alt_tot4_3,
        vars = res$vars[[2]],
        sep = sep,
        fus_vars = c(v3, v4)
      )
      res2 <- format(res2, nom_dfs, sep, totcode, hrcfiles)
      
      # On garde l'information des variables fusionnés à chaque étape
      res2$fus_vars<-res$vars
      return(res2)
    }
    
    
    tot_cross <- paste(totcode[[v1]], totcode[[v2]], sep = sep)
    tot_cross2 <- paste(totcode[[v3]], totcode[[v4]], sep = sep)
    tabs <- res$tabs
    d <- intersect(names(res$tabs[[1]]), names(totcode))
    p <- totcode[names(totcode) %in% d]
    
    names(tot_cross) <- var_cross
    names(tot_cross2) <- var_cross2
    totcode_2 <- c(p, tot_cross, tot_cross2)
    
    n <- length(res$tabs)
    v <- c(d, var_cross, var_cross2)
    list_vars <- replicate(n, v, simplify = FALSE)
    names(list_vars) <- c(paste0(nom_dfs, 1:n, sep = ""))
    names(tabs) <- c(paste0(nom_dfs, 1:n, sep = ""))
    
    #Noms des hrcs
    
    res2 <- setNames(lapply(seq_along(res$tabs), function(i) {
      list1 <- setNames(list(res$hrcs4_3[[i]]), var_cross)
      list2 <- setNames(list(res$hrcs5_4[[i]]), var_cross2)
      c(list1, list2)
    }),
    paste(nom_dfs, seq_along(res$tabs), sep = ""))
    if (length(hrcfiles)==0){res2<-NULL}
    
    #Noms des sous_totaux
    
    res3 <- setNames(lapply(seq_along(res$tabs), function(i) {
      list1 <- setNames(list(res$alt_tot4_3[[i]]), var_cross)
      list2 <- setNames(list(res$alt_tot5_4[[i]]), var_cross2)
      c(list1, list2)
    }),
    paste(nom_dfs, seq_along(res$tabs), sep = ""))
    
  }
  return (
    list(
      tabs = tabs,
      hrcs = res2,
      alt_tot = res3,
      vars = list_vars,
      sep = sep,
      totcode = totcode_2,
      hrcfile = hrcfiles[!(names(hrcfiles) %in% names(totcode_2))],
      fus_vars = res$vars
    )
  )
}
