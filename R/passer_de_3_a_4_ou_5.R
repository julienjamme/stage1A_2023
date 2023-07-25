
#' @param masq liste contenant les différents tableaux avec le secret posé
#' @param res résultat donné contenant le séparateur et les variables fusionnées
#'
#' @return une table a 4 ou 5 variables catégorielles
#' @export
#'
#' @examples
#' #' library(dplyr)
#' 
#' source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
#' source("R/cas_gen_4_3.R",encoding = "UTF-8")
#' source("R/passage_5_3.R",encoding = "UTF-8")
#' source("R/format.R",encoding = "UTF-8")
#' source("R/passer_de_3_a_4_ou_5.R",encoding = "UTF-8")
#' source("R/reduce_dims.R",encoding = "UTF-8")
#' source("R/choisir_sep.R",encoding = "UTF-8")
#' source("test/test_nbs_tabs.R",encoding = "UTF-8")
#' 
#' # Test dimension 4 --------------------------------------------------------
#' 
#' data <- expand.grid(
#'   ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
#'   GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
#'   SEX = c("Total",LETTERS[1:5]),
#'   AGE = c("Total",LETTERS[21:25]),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = runif(nrow(data)))
#' #hrcfiles = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")
#' hrcfiles = c(GEO = "hrc/hrc2.hrc")
#' totcode<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
#' 
#' 
#' 
#' dfs <- data
#' nom_dfs <- "nom_data_frame"
#' sep_dir = TRUE
#' hrc_dir = "hrc_alt"
#' 
#' res4_3 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles)
#' 
#' res3_4 <- passer_a_4_ou_5(masq=res4_3$tabs,res=res_4_3)
#' 
#' str(res3_4)
#' 
#' 




#Fonction permettant la séparation de la variable fusionnées v1_v2_v3 en v1,v2,v3
separer5_3 <- function(df, v1, v2, v3, sep_regex) {
    splits <- strsplit(df[[v1_v2_v3]], split = sep_regex)
    df[[v3]] <- sapply(splits, `[`, 1)
    df[[v1]] <- sapply(splits, `[`, 2)
    df[[v2]] <- sapply(splits, `[`, 3)
    df[[v1_v2_v3]] <- NULL
  df
}

#Fonction permettant la séparation de la variable fusionnées v1_v2 en V1 et v2 
separer4_3 <- function(df, v1, v2, v1_v2, sep_regex) {
  splits <- strsplit(df[[v1_v2]], split = sep_regex)  
  df[[v1]] <- sapply(splits, `[`, 1)
  df[[v2]] <- sapply(splits, `[`, 2)
  df[[v1_v2]] <- NULL
  df
}

passer_a_4_ou_5 <- function(masq, res) {
  
  require(stringr)
  sep <- res$sep
  sep_regex <- gsub("([+])", "\\\\\\1", sep)
  # Cas à 4 variables catégorielles
  
  masq_liste_empilee <- unique(do.call("rbind",  unname(masq)))
  
  if (class(res$fus_vars) == "character") {
  # données
  v1 <- res$fus_vars[1]
  v2 <- res$fus_vars[2]
  
  v1_v2 <- paste(v1, v2, sep = sep)
  
  result <- separer4_3(masq_liste_empilee, v1, v2,v1_v2, sep_regex) 
  return(result)}

  # Cas à 5 dimensions 
  # Les variables 
  
  v1<-res$fus_vars$five_to_three[1]
  v2<-res$fus_vars$five_to_three[2]
  v3<-res$fus_vars$four_to_three[1]
  v4<-res$fus_vars$four_to_three[2]
  v1_v2 <- paste(v1, v2, sep = sep)
  
if (!(v1_v2 == v3 | v1_v2 == v4)) {
  # Cas fusion entre 3 variables différentes
  v3_v4 <- paste(v3, v4, sep = sep)
  
  split1 <- separer4_3(masq_liste_empilee, v1, v2, v1_v2, sep_regex)
  result <- separer4_3(split1, v3, v4, v3_v4, sep_regex)
  
} else {
  # Cas de fusion avec une variable déjà fusionnée
  v1_v2_v3 <- paste(v1, v2, v3, sep = sep)  
  
  function_sep <- ifelse(v1_v2 == v3,
                         function(df) {
                           separer5_3(df, v2, v4,v1, sep_regex)  
                         },
                         function(df) {
                           separer5_3(df, v1, v2, v3, sep_regex)
                         })
  result<<-function_sep(masq_liste_empilee)
}

return(result)
}