



passer_3_4(list_df,list_var,dir_name){
  
  
  
  
  
  
  
  
  
}


###
###
# Découpage des différentes possibilités
###
###
# source : mon ami GPT
chaine <- "ma_variable_nom_tres_complexe"
sous_chaine <- "_"

# Trouver toutes les positions de la sous-chaîne dans la chaîne
positions <- gregexpr(sous_chaine, chaine)[[1]]

# Générer toutes les possibilités de division
possibilites <- list()
for (pos in positions) {
  partie1 <- substr(chaine, 1, pos - 1)
  partie2 <- substr(chaine, pos + nchar(sous_chaine), nchar(chaine))
  possibilites[[pos]] <- list(partie1, partie2)
}

possibilites <- Filter(function(x) !is.null(x), possibilites)

# elem = un élément de possibilité
# fonction qui regarde si elem[1] est une possibilité de la var 1
# et elem[2] une possibilité de var 2

# Requirement : la fonction au dessus ne renvoie TRU que pour un seul élément
# ie : les variables n'ont pas des noms bizarres


# to do : dans le code_split, faire un intersect avec les modalités présentes