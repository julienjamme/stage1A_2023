library(tidyr)

passer_a_4_ou_5 <- function(res){
  
  # séparateur pour le separate (pour éviter les erreurs regex)
  # Pour l'instant seul le cas triple séparateur est pris en compte
  sep <- res$sep
  sep_regex <- gsub("([+])", "\\\\\\1", sep)
  
  # Ancien tableau à 4 dimensions
  if (class(res$fus_vars) == "character") {
    v1 = res$fus_vars[1]
    v2 = res$fus_vars[2]
    v1_v2 = paste(v1,v2,sep=sep)
    
    function_sep <- function(df){ df %>% 
      tidyr::separate(v1_v2, into = c(v1,v2), sep = sep_regex)
    }
    
  # Ancien tableau à 5 dimensions
  } else {
    v1 = res$fus_vars$`Passage 5 à 4`[1]
    v2 = res$fus_vars$`Passage 5 à 4`[2]
    v1_v2 = paste(v1,v2,sep=sep)
    
    v3 = res$fus_vars$`Passage 4 à 3`[1]
    v4 = res$fus_vars$`Passage 4 à 3`[2]
    
    # Deux couples ont été créés
    if (!(v1_v2 == v3 | v1_v2 == v4)){
      v3_v4 = paste(v3,v4,sep=sep)
      
      function_sep <- function(df){ df %>% 
        tidyr::separate(v1_v2, into = c(v1,v2), sep = sep_regex) %>% 
        tidyr::separate(v3_v4, into = c(v3,v4), sep = sep_regex)
      }
    
    # Trois variables fusionnées en une
    } else {
      v1_v2_v3 = paste(v3,v4,sep=sep)
      
      function_sep <- ifelse(v1_v2 == v3,
                             function(df){df %>% tidyr::separate(v1_v2_v3, 
                                      into = c(v1,v2,v4), sep = sep_regex)},
                             function(df){df %>% tidyr::separate(v1_v2_v3, 
                                      into = c(v3,v1,v2), sep = sep_regex)})
    }
  }
  return(purrr::map(
                  res$tabs,
                  function_sep)
        )
}


passer_a_4_ou_5_r_base <- function(res){
  
  sep <- res$sep
  sep_regex <- gsub("([+])", "\\\\\\1", sep)
  
  if (class(res$fus_vars) == "character") {
    v1 = res$fus_vars[1]
    v2 = res$fus_vars[2]
    v1_v2 = paste(v1, v2, sep=sep)
    
    function_sep <- function(df) {
      splits <- strsplit(df[[v1_v2]], split=sep_regex)
      df[[v1]] <- sapply(splits, `[`, 1)
      df[[v2]] <- sapply(splits, `[`, 2)
      df[[v1_v2]] <- NULL
      df
    }
    
  } else {
    v1 = res$fus_vars$`Passage 5 à 4`[1]
    v2 = res$fus_vars$`Passage 5 à 4`[2]
    v1_v2 = paste(v1, v2, sep=sep)
    
    v3 = res$fus_vars$`Passage 4 à 3`[1]
    v4 = res$fus_vars$`Passage 4 à 3`[2]
    
    if (!(v1_v2 == v3 | v1_v2 == v4)){
      v3_v4 = paste(v3, v4, sep=sep)
      
      function_sep <- function(df) {
        splits <- strsplit(df[[v1_v2]], split=sep_regex)
        df[[v1]] <- sapply(splits, `[`, 1)
        df[[v2]] <- sapply(splits, `[`, 2)
        df[[v1_v2]] <- NULL
        splits <- strsplit(df[[v3_v4]], split=sep_regex)
        df[[v3]] <- sapply(splits, `[`, 1)
        df[[v4]] <- sapply(splits, `[`, 2)
        df[[v3_v4]] <- NULL
        df
      }
      
    } else {
      v1_v2_v3 = paste(v3, v4, sep=sep)
      
      function_sep <- ifelse(v1_v2 == v3,
                             function(df){
                               splits <- strsplit(df[[v1_v2_v3]], split=sep_regex)
                               df[[v1]] <- sapply(splits, `[`, 1)
                               df[[v2]] <- sapply(splits, `[`, 2)
                               df[[v4]] <- sapply(splits, `[`, 3)
                               df[[v1_v2_v3]] <- NULL
                               df},
                             function(df){
                               splits <- strsplit(df[[v1_v2_v3]], split=sep_regex)
                               df[[v3]] <- sapply(splits, `[`, 1)
                               df[[v1]] <- sapply(splits, `[`, 2)
                               df[[v2]] <- sapply(splits, `[`, 3)
                               df[[v1_v2_v3]] <- NULL
                               df})
    }
  }
  
  return(lapply(res$tabs, function_sep))
}
