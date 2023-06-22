
#' Passage d'un tableau de 4 variables à 3 variables
#' dans lme cas où il y a 2 variables non hierarchique
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
#' - S'intéresser au cas où la profondeur de la hierarchie est 'n'
#' @examples
#' library(dplyr)
#' 
#' data <- expand.grid(
#'   ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
#'   GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
#'   SEX = c("Total",LETTERS[7:12]),
#'   AGE = c("Total",LETTERS[15:25]),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = runif(nrow(data)))
#' hrc_files = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")
#' 
#' tot_code<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
#' 
#' var_sans_hier <- names(tot_code)[1:2]
#' v1 <- var_sans_hier[1]
#' v2 <- var_sans_hier[2]
#' 
#' res <- passage_4_3_cas_2_non_hr(data,nom_dfs,v1,v2, tot_code,dir_name)
passage_4_3_cas_2_non_hr <- function(dfs, nom_dfs,v1,v2,totcode,dir_name) {
  
  # les différents totaux
  var1_total <- totcode[v1]
  var2_total <- totcode[v2]
  #print(var2_total)
  
  #les différentes modalités des 2 variables
  mods1 <- unique(dfs[[v1]])
  mods2 <- unique(dfs[[v2]])
  #print(mods2)
  
  var1_mods_hors_tot <- mods1[mods1 != var1_total]
  var2_mods_hors_tot <- mods2[mods2 != var2_total]
  #print(var2_mods_hors_tot)
  
  # nombre de modalité pour chaque var
  var1_mods_n <- length(var1_mods_hors_tot)
  var2_mods_n <- length(var2_mods_hors_tot)
  
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
    
    tabi_nv1$v3 <- paste(tabi_nv1$v1, tabi_nv1$v2, sep = "_")
    
    # Création du niveau 2 hier
    tabi_nv2 <- expand.grid(
      v1 = var_i_mods_hors_tot,
      v2 = var_j_mods_hors_tot,
      stringsAsFactors = FALSE
    ) %>% as.data.frame()
    
    tabi_nv2 <- tabi_nv2[order(tabi_nv2$v1, tabi_nv2$v2), ]
    tabi_nv2$v3 <- paste(tabi_nv2$v1, tabi_nv2$v2, sep = "_")
    
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
    tabi[[paste(v1, v2, sep = "_")]]<- paste(tabi[[vi]],tabi[[vj]],sep="_")
    
    tabi[[v1]]<-NULL
    tabi[[v2]]<-NULL
    
    return(list(tabi,tabi_corresp))
  }
  
  res1 <- creation_table_3_var(1)
  tab1 <- res1[[1]]
  tab1_corresp <- res1[[2]]
  #print(tab1_corresp)
  
  res2 <- creation_table_3_var(2)
  tab2 <- res2[[1]]
  tab2_corresp <- res2[[2]]
  #print(tab2_corresp)
  
  #Construction des hiérarchies (cela ne marche pas quand je le mets dans la fonction )
  
  # to do :
  # utiliser file.path() ?
  # ne pas écrire si le fichier existe déjà ?
  
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

