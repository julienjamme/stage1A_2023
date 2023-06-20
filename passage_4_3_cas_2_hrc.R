

passage_4_3_cas_2hr <- function(dfs, totcode, hrcfiles) {
  # les variables sans hiérarchie
  
  var_sans_hier <- setdiff(names(dfs), names(hrcfiles))
  v1 <- var_sans_hier[1]
  v2 <- var_sans_hier[2]
  # les différents totaux
  var1_total <- totcode[v1]
  var2_total <- totcode[v2]
  #les différentes modalités des 2 variables
  mods1 <- unique(dfs[[v1]])
  mods2 <- unique(dfs[[v2]])
  
  var1_mods_hors_tot <- mods1[mods1 != var1_total]
  var2_mods_hors_tot <- mods2[mods2 != var2_total]
  
  var1_mods <- length(var1_mods_hors_tot)
  var2_mods <- length(var2_mods_hors_tot)
  
  # JConstruction des niveaux pour la table de correspondance
  tab1_niv1 <- expand.grid(
    v1 = sort(rep(var1_mods_hors_tot, var2_mods)),
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
  # Construction des niveaux pour la table de correspondance
  
  tab2_niv1 <- expand.grid(
    v1 = sort(rep(var2_mods_hors_tot, var1_mods)),
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
  
  #Construction des hiérarchies (cela ne marche pas quand je le mets dans la fonction )
  
  #hrc_tab1<-write_hrc2(tab1_corresp,file_name = "/hrc.tab1.hrc")
  #hrc_tab2<-write_hrc2(tab1_corresp2,file_name = "/hrc.tab2.hrc")
  
  return(list(tab1="tab1", tab2="tab2",tab1_corresp="tab1_corresp",tab2_corresp="tab2_corresp"))
  
  
}

