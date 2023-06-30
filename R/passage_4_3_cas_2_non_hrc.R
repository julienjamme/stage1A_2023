library(dplyr)

#' Passage de 4 à 3 variables via la fusion de deux variables non hiérarchiques
#'
#' @param dfs data.frame à 4 variables catégorielles (n >= 2 dans le cas général)
#' @param nom_dfs nom du data.frame dans la liste fournie par l'utilisateur
#' @param v1 variable catégorielle non hierarchique
#' @param v2 variable catégorielle non hierarchique
#' @param totcode vecteur nommé indiquant la modalité du total
#' pour chacune des variables catégorielles de dfs
#' @param hrcfiles vecteur nommé indiquant les fichiers hrc des variables 
#' hiérarchiques parmi les variables catégorielles de dfs
#' @param dir_name répertoire des fichiers hrc dans le cas où hrcfiles est vide
#'
#' @return une liste de data.frame à 3 variables catégorielles
#' doté de hierarchie emboitées (n-1 dans le cas général)
#' @export
#'
#' @examples
passage_4_3_cas_2_non_hr <- function(dfs, nom_dfs,v1,v2,totcode,dir_name) {
  
  # les différents totaux
  var1_total <- totcode[v1]
  var2_total <- totcode[v2]
  
  #les différentes modalités des 2 variables
  mods1 <- unique(dfs[[v1]])
  mods2 <- unique(dfs[[v2]])
  
  var1_mods_hors_tot <- mods1[mods1 != var1_total]
  var2_mods_hors_tot <- mods2[mods2 != var2_total]
  
  # nombre de modalité pour chaque var
  var1_mods_n <- length(var1_mods_hors_tot)
  var2_mods_n <- length(var2_mods_hors_tot)
  
  
  # to do : enlever la généralisation de fonciton
  # car le code en devient moins lisible
  creation_table_3_var <- function(i){
    # Introduction des notations :
    # soit i = 1, j = 2
    # soit i = 2, j = 1

    # Construction des niveaux pour la table de correspondance
    
    # Création du niveau 1 hier
    var_j_total <- get(
      paste("var",3-i,"_total",sep=""))
    
    var_i_mods_hors_tot <- get(
      paste("var",i,"_mods_hors_tot",sep=""))
    
    var_j_mods_hors_tot <- get(
      paste("var",3-i,"_mods_hors_tot",sep=""))
    
    var_j_mods_n <- get(
      paste("var",3-i,"_mods_n",sep=""))
    
    tabi_nv1 <- expand.grid(
               v1 = sort(rep(var_i_mods_hors_tot, var_j_mods_n)),
               v2 = var_j_total,
               stringsAsFactors = FALSE
               ) %>% as.data.frame()
    
    vi <- paste("v",i,sep="")
    vj <- paste("v",3-i,sep="")
    
    tabi_nv1$v3 <- paste(tabi_nv1[[vi]], tabi_nv1[[vj]], sep = "_")
    
    # Création du niveau 2 hier
    tabi_nv2 <- expand.grid(
      v1 = var_i_mods_hors_tot,
      v2 = var_j_mods_hors_tot,
      stringsAsFactors = FALSE
    ) %>% as.data.frame()
    
    tabi_nv2 <- tabi_nv2[order(tabi_nv2$v1, tabi_nv2$v2), ]
    
    tabi_nv2$v3 <- paste(tabi_nv2[[vi]], tabi_nv2[[vj]], sep = "_")
    
    # Création table de correspondance
    tabi_corresp <- data.frame(
      Niv1 = tabi_nv1$v3,
      Niv2 = tabi_nv2$v3,
      stringsAsFactors = FALSE
    )

    # Construction de tabi
    vi <- get(
      paste("v",i,sep=""))
    vj <- get(
      paste("v",3-i,sep=""))
    var_i_total <- get(
      paste("var",i,"_total",sep=""))

    tabi <- dfs[(dfs[[vi]] != var_i_total) | 
                  (dfs[[vi]] == var_i_total & dfs[[vj]] == var_j_total), ]
    tabi[[paste(v1, v2, sep = "_")]]<- paste(tabi[[v1]],tabi[[v2]],sep="_")
    
    tabi[[v1]]<-NULL
    tabi[[v2]]<-NULL
    
    return(list(tabi,tabi_corresp))
  }
  
  res1 <- creation_table_3_var(1)
  tab1 <- res1[[1]]
  tab1_corresp <- res1[[2]]
  
  res2 <- creation_table_3_var(2)
  tab2 <- res2[[1]]
  tab2_corresp <- res2[[2]]
  
  #Construction des hiérarchies
  
  # to do :
  # utiliser file.path() ?
  # ne pas écrire si le fichier existe déjà ?
  
  hrc_tab1 <- rtauargus::write_hrc2(tab1_corresp, 
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",nom_dfs,v1,sep = "_"),".hrc",sep="")
  )

  hrc_tab2 <- rtauargus::write_hrc2(tab2_corresp, 
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",nom_dfs,v2,sep = "_"),".hrc",sep="")
  )
  
  tabs <- list(tab1, tab2)
  names(tabs) <- c(paste(nom_dfs,v1, sep="_"),paste(nom_dfs,v2, sep="_"))
  hrcs <- list(hrc_tab1, hrc_tab2)
  names(hrcs) <- names(tabs)
  total_total = paste(totcode[v1],totcode[v2],sep="_")
  alt_tot=list(total_total,total_total)
  names(alt_tot)<- names(tabs)
   
  return(
    list(
        tabs = tabs,
        hrcs = hrcs,
        vars = c(v1, v2),
        alt_tot=alt_tot
    )
  )
}

