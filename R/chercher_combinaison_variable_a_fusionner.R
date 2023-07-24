#' Fonction général choisissant les variables à fusionner
#' pour limiter le nombre de tableau généré tout en s'assurant de ne pas généré
#' des tableaux trop grands
#'
#' @param dfs data.frame
#' @param totcode vector nommé des totaux pour les variables catégorielles
#' @param hrcfiles vector nommé des fichiers hrc pour les variables catégorielles
#' @param nb_var nombre de variable à fusionner
#' @param nb_tab stratégie à suivre pour choisir les variables automatiquement :
#' min : minimiser le nombre de table;
#' max : maximise le nombre de table;
#' smart : minimise le nombre de table sous la contrainte de leur nombre de ligne
#' @param LIMIT nombre de ligne maximale autorisé dans le cas smart
#'
#' @return
#' @export
#'
#' @examples
choisir_var_a_fusionner_general <- function(dfs,totcode,hrcfiles=NULL, nb_var = 4, nb_tab = "min", LIMIT=150) {
  
  # Cas 2 couplese en dimension 5
  if (nb_var == 4){
    result_comb <- generer_deux_paires(totcode)
    
    # Cas trio en dimension 5
  } else if (nb_var == 3){
    result_comb <- generer_triplet(totcode)
    
    # cas dimension 4
  } else {
    result_comb <- generer_une_paire(totcode)
  }
  
  return(choisir_var_a_fusionner(dfs = dfs,
                                 result_comb = result_comb,
                                 hrcfiles = hrcfiles,
                                 LIMIT = LIMIT,
                                 nb_tab = nb_tab))
}

choisir_var_a_fusionner <- function(dfs,result_comb,hrcfiles=NULL, LIMIT=150, nb_tab = "smart"){
  
  res_func <- lapply(result_comb, function(x) length_tabs(
                                                     dfs=data,
                                                     v1=x[1],
                                                     v2=x[2],
                                                     v3=x[3],
                                                     v4=x[4],
                                                     hrcfiles=hrcfiles))
  
  # Récupération du max de ligne et du nombre de tableau créé
  res_max <- sapply(res_func, function(x) max(unlist(x)))
  res_len <- sapply(res_func, function(x) length(unlist(x)))
  
  # Créer un dataframe pour pouvoir mieux filtrer après
  df <- data.frame(res_max = res_max, res_len = res_len)
  
  # On souvegarde le numéro de ligne enrajoutant une colonne
  df$original_index <- seq(nrow(df))
  
  # Cas minimiser le nombre de tableau
  if (nb_tab == "min"){
    min_nb_tab <-  min(df$res_len)
    filtered_df <- df[df$res_len == min_nb_tab, ]
    
    # indice du tableau filtré
    min_index <- which.min(filtered_df$res_max)
    # Imprimer l'indice original
    i <- filtered_df$original_index[min_index]
    
    return(result_comb[[i]])
  
  # Cas maximiser le nombre de tableau
  } else if (nb_tab == "max"){
    max_nb_tab <-  max(df$res_len)
    filtered_df <- df[df$res_len == max_nb_tab, ]
    
    # indice du tableau filtré
    min_index <- which.min(filtered_df$res_max)
    # Imprimer l'indice original
    i <- filtered_df$original_index[min_index]
    
    return(result_comb[[i]])
    
  # Cas 'smart' : maximisation sous contrainte de limite de taille
  } else {
    # On filtre sur la condition du max
    filtered_df <- df[df$res_max < LIMIT, ]
    
    # Si au moins un cas satisfait à cela
    if (nrow(filtered_df) > 0){
      # indice du tableau filtré
      min_index <- which.min(filtered_df$res_len)
      
      # Imprimer l'indice original
      i <- filtered_df$original_index[min_index]
      
      return(result_comb[[i]])
      
    } else {
      # warning("Pas de résultat en dessous de la limite spécifiée :
      #        Nous renvoyons la solution la plus proche de celle voulue.")
      
      # on renvoie le résultat ayant le moins de tableau parmis ceux
      # ayant les tableaux les moins longs
      min_res_max <- min(df$res_max)
      filtered_df <- df[df$res_max == min_res_max, ]
      
      # indice du tableau filtré
      min_index <- which.min(filtered_df$res_len)
      
      # Imprimer l'indice original
      i <- filtered_df$original_index[min_index]
      return(result_comb[[i]])
    }
  }
}

generer_une_paire <- function(totcode){
  # Récupération des variables catégorielles du dataframe
  var_cat <- names(totcode)
  
  # Utiliser combn pour obtenir toutes les combinaisons de deux éléments
  comb <- combn(var_cat,2)
  
  # Transformer les résultats en une liste de vecteurs
  result <- split(t(comb), seq(ncol(comb)))
  
  return(result)
}

generer_deux_paires <- function(totcode) {
  # Récupération des variables catégorielles du dataframe
  var_cat <- names(totcode)
  
  # Obtenir toutes les combinaisons de quatre éléments
  comb <- combn(var_cat, 4)
  
  # Pour chaque combinaison, obtenir deux paires disjointes
  result <- lapply(seq(ncol(comb)), function(i) {
    quad <- comb[,i]
    pair_comb <- t(combn(quad, 2))
    
    # Créer deux paires disjointes pour chaque combinaison
    pairs <- lapply(seq(nrow(pair_comb)), function(j) {
      pair1 <- pair_comb[j,]
      pair2 <- setdiff(quad, pair1)
      
      # Convertir les paires en chaînes
      pair1_str <- paste(sort(pair1), collapse = ",")
      pair2_str <- paste(sort(pair2), collapse = ",")
      
      # Créer une chaîne qui représente les deux paires
      both_pairs_str <- paste(sort(c(pair1_str, pair2_str)), collapse = ",")
      return(both_pairs_str)
    })
    return(pairs)
  })
  
  # Aplatir le résultat
  result <- unlist(result, recursive = FALSE)
  
  # Supprimer les doublons
  unique_pairs <- unique(result)
  
  # Convertir les chaînes en vecteurs
  result <- lapply(unique_pairs, function(pair_str) {
    pairs <- strsplit(pair_str, ",")[[1]]
    return(pairs)
  })
  
  return(result)
}

generer_triplet <- function(totcode) {
  # Récupération des variables catégorielles du dataframe
  var_cat <- names(totcode)
  
  # Obtenir toutes les combinaisons de trois éléments
  comb <- combn(var_cat, 3)
  
  # Transformer le résultat en une liste de vecteurs
  result <- split(t(comb), seq(ncol(comb)))
  
  return(result)
}
