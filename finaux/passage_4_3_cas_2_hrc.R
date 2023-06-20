
#' Title
#'
#' @param dfs data.frame à quatre variables catégorielles
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param totcode vecteur nommé indiquant la modalité du total
#' pour chacune des 4 variables catégorielles de dfs
#' @param hrcfiles vecteur nommé indiquant les fichiers hrc des variables 
#' hiérarchiques parmi les 4 variables catégorielles de dfs
#' @param hrc_dir répertoire des fichiers hrc dans le cas où hrcfiles est vide
#'
#' @return Liste de 3 éléments:
#' - liste de deux data.frames à 3 variables (avec fusion)
#' - liste des fichiers hrc de chaque var cat. hier des dataframes construits
#' - vecteur des variables traitées
#' @export
#' TODO: 
#' - trouver des noms de tableaux robustes
#' - faire une fonction pour la construction des tables de correspondance
#' - modifier l'objet retourné
#' - pour les fichiers hrc créés, trouver un nom robuste et les sauvegarder dans 
#' le même répertoire que les hrs des var hierarchiques (si pas de var hier, 
#' les déposer dans un répertoire "hrc")
#' - les tables renvoyées ont 3 colonnes
#' - généraliser le choix des 2 variables à fusionner en comparant le nb de 
#' modalités des variables non hierarchiques.
#' @examples
#' library(dplyr)
#' data <- expand.grid(
#'   ACT = c("Total",read.table("hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
#'   GEO = c("Total",read.table("hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
#'   SEX = c("Total",LETTERS[7:12]),
#'   AGE = c("Total",LETTERS[15:25]),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   as.data.frame()
#' hrc_files = c(ACT = "hrc1.hrc", GEO = "hrc2.hrc")
#' names(hrc_files[1])
#' 
#' tot_code<-c(SEX="Total",AGE="Total")
#' res <- passage_4_3_cas_2hr(data,tot_code, hrc_files)
passage_4_3_cas_2_non_hr <- function(dfs, nom_dfs,totcode, hrcfiles) {
  
  # Nom du dossier où se trouvent les variables hiérarchiques
  if(length(hrcfiles) != 0){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }
  
  
  
  #  l'indixes des deux variables avec le moins de modalités
  
  get_2_smallest <- function(data){
    list_mod_n <- lapply(data, function(col) length(unique(col)))
    smallest_index <- which.min(list_mod_n)
    list_mod_n[smallest_index] <- NA
    sec_smallest_index <- which.min(list_mod_n)
    return(c(smallest_index,sec_smallest_index))
  }
  
  
  # les variables sans hiérarchie
  var_sans_hier <- setdiff(names(dfs), names(hrcfiles))
  #si superieur à 3 je dis que je regarde celui avec les plus petites moda
  n_vars_sans_hier<-length(var_sans_hier)
  
  res<-get_2_smallest(dfs)
    
  
  
  v1 <- names(res)[[1]]
  v2 <- names(res)[[2]]
  # les différents totaux
  var1_total <- totcode[v1]
  var2_total <- totcode[v2]
  #les différentes modalités des 2 variables
  mods1 <- unique(dfs[[v1]])
  mods2 <- unique(dfs[[v2]])
  
  var1_mods_hors_tot <- mods1[mods1 != var1_total]
  var2_mods_hors_tot <- mods2[mods2 != var2_total]
  
  var1_mods_n <- length(var1_mods_hors_tot)
  var2_mods_n <- length(var2_mods_hors_tot)
  
  # Construction des niveaux pour la table de correspondance
  tab1_niv1 <- expand.grid(
    v1 = sort(rep(var1_mods_hors_tot, var2_mods_n)),
    v2 = var2_total,
    stringsAsFactors = FALSE
  ) %>% as.data.frame()
  
  tab1_niv1$v3 <- paste(tab1_niv1$v1, tab1_niv1$v2, sep = "_")
  
  tab1_niv2 <- expand.grid(
    v1 = var1_mods_hors_tot,
    v2 = var2_mods_hors_tot,
    stringsAsFactors = FALSE
  ) %>% as.data.frame()
  
  tab1_niv2 <- tab1_niv2[order(tab1_niv2$v1, tab1_niv2$v2), ]
  tab1_niv2$v3 <- paste(tab1_niv2$v1, tab1_niv2$v2, sep = "_")
  
  tab1_corresp <- data.frame(
    Niv1 = tab1_niv1$v3,
    Niv2 = tab1_niv2$v3,
    stringsAsFactors = FALSE
  )
  #Construction du tableau 
  
  tab1 <- dfs[(dfs[[v1]] != var1_total) | (dfs[[v1]] == var1_total & dfs[[v2]] == var2_total), ]
  tab1[[paste(v1, v2, sep = "_")]]<- paste(tab1[[v1]],tab1[[v2]],sep="_")
  
  tab1[[v1]]<-NULL
  tab1[[v2]]<-NULL
  
  # Construction des niveaux pour la table de correspondance
  
  tab2_niv1 <- expand.grid(
    v1 = sort(rep(var2_mods_hors_tot, var1_mods_n)),
    v2 = var1_total,
    stringsAsFactors = FALSE
  ) %>% as.data.frame()
  
  tab2_niv1$v3 <- paste(tab2_niv1$v1, tab2_niv1$v2, sep = "_")
  
  tab2_niv2 <- expand.grid(
    v1 = var2_mods_hors_tot,
    v2 = var1_mods_hors_tot,
    stringsAsFactors = FALSE
  ) %>% as.data.frame()
  
  tab2_niv2 <- tab2_niv2[order(tab2_niv2$v1, tab2_niv2$v2), ]
  tab2_niv2$v3 <- paste(tab2_niv2$v1, tab2_niv2$v2, sep = "_")
  
  tab2_corresp <- data.frame(
    Niv1 = tab2_niv1$v3,
    Niv2 = tab2_niv2$v3,
    stringsAsFactors = FALSE
  )
  
  #Construction de tab2
  
  tab2 <- dfs[(dfs[[v2]] != var2_total) | (dfs[[v2]] == var2_total & dfs[[v1]] == var1_total), ]
  tab2[[paste(v1, v2, sep = "_")]]<- paste(tab2[[v1]],tab2[[v2]],sep="_")
  
  tab2[[v1]]<-NULL
  tab2[[v2]]<-NULL
  
  #Construction des hiérarchies (cela ne marche pas quand je le mets dans la fonction )
  
  
  
  
  hrc_tab1 <- rtauargus::write_hrc2(tab1_corresp, 
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",nom_dfs,v1,sep = "_"),".hrc",sep="")
  )
  #names(hrc_tab1) <- paste(nom_dfs,v1, sep="_")
  hrc_tab2 <- rtauargus::write_hrc2(tab2_corresp, 
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",nom_dfs,v2,sep = "_"),".hrc",sep="")
  )
  
  #names(hrc_tab2) <- paste(nom_dfs,v2, sep="_")
  
  tabs <- list(tab1, tab2)
  names(tabs) <- c(paste(nom_dfs,v1, sep="_"),paste(nom_dfs,v2, sep="_"))
  hrcs <- list(hrc_tab1, hrc_tab2)
  names(hrcs) <- names(tabs)
  
  return(
    list(
      tabs = tabs,
      hrcs = hrcs,
      vars = c(v1, v2)
    )
  )
}

