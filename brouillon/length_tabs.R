# Fonction pour gérer l'importation de la hiérarchie
import_hierarchy <- function(hrcfile) {
  total <- "BIG_Total"
  res_sdc <- sdcHierarchies::hier_import(inp = hrcfile, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  # Stocker tous les ensembles de parents + enfant direct
  levels <- lapply(res_sdc$dims, names)
  return(levels)
}



length_tabs<-function(dfs,hrcfiles,v1,v2,totcode){

# les différents totaux
var1_total <- totcode[v1]
var2_total <- totcode[v2]

#les différentes modalités des 2 variables
mods1 <- unique(dfs[[v1]])
mods2 <- unique(dfs[[v2]])

n_mod_v2<-length(mods1)
n_mod_v1<-length(mods2)

var1_mods_hors_tot <- mods1[mods1 != var1_total]
var2_mods_hors_tot <- mods2[mods2 != var2_total]

# nombre de modalité pour chaque var
var1_mods_n <- length(var1_mods_hors_tot)
var2_mods_n <- length(var2_mods_hors_tot)


# 2 variables hiérarchiques fusionnées
if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)){
  
  # La hierarchie de chaque variable
  level_v1 <- import_hierarchy(hrcfiles[[v1]])
  level_v2 <- import_hierarchy(hrcfiles[[v2]])
  
  # Stocke la somme des noeuds de v1_v2 de chaque table
  # On fait tout les croisements possible
  # entre v1 et v2
  # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
  
  # pour chacun de ses tableaux, il y a deux hierarchies possibles
  # une avec les totaux de v1, l'autre avec les totaux de v2
  # le nombre de noeuds est égal à leur nombre de modalité
 nb_noeuds <- lapply(1:length(level_v1), function(i) {
  lapply(1:length(level_v2), function(j) {
    c((length(level_v1[[i]]) - 1) * length(level_v2[[j]]),
      length(level_v1[[i]]) * (length(level_v2[[j]])-1))
  })
})

  
  # 2 variables non hierarchiques fusionnées
} else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))){
  # On a qu'un tableau à la fin
  # qui peut avoir deux hierarchies
  # totaux sur v1, ou totaux sur v2
  # le nombre de noeuds équivaut au nombre de modalité
  nb_noeuds <- c((n_mod_v1 - 1) * n_mod_v2,
                  n_mod_v1 * (n_mod_v2 -1))
  
  # 1 variable hier et une non hier
} else {
  var_hier = ifelse(v1 %in% names(hrcfiles),v1,v2)
  mod_var_non_hier = ifelse(var_hier == v1,n_mod_v2,n_mod_v1)
  
  
  # analyse de la hierarchie de var_hier
  level_var_hier <- import_hierarchy(hrcfiles[[var_hier]])
  
  # On fait tout les croisements possible
  # entre v1 et v2
  # => représente les tableaux créés lors de la création de v1_v2 à l'étape 5->4
  
  # pour chacun de ses tableaux, il y a deux hierarchies possibles
  # une avec les totaux de v1, l'autre avec les totaux de v2
  # le nombre de noeuds est égal à leur nombre de modalité
  nb_noeuds <- lapply(1:length(level_var_hier), function(i) {
    c((length(level_var_hier[[i]])-1) * mod_var_non_hier,
      length(level_var_hier[[i]]) * (mod_var_non_hier - 1))
  })
}
# le 1500 <15000 which >15000 et 
 return(nb_noeuds)
}
