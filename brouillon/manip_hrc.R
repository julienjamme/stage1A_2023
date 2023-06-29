

load(file="/hrc/activity.hrc")

library("dplyr")
library("rtauargus")

library("stringr")

loc_tauargus <- "C:/Users/ZOW2JK/Downloads/TauArgus4.2.4b2/TauArgus4.2.4b2/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)
write_hrc2(activity_corr_table, file_name = "hrc/activity.hrc")

list_act<-list(read.table("hrc/activity.hrc"))
act<-(read.table("hrc/activity.hrc"))

str(readLines("hrc/activity.hrc"))
# Liste initiale

str_count("@@A111","@")
length(act$V1)
str(act$V1[1])
act[[1]]
# Dataframe initiale

# Listes de niveaux
niv0 <- vector("list")
niv1 <- vector("list")
niv2 <- vector("list")

# Variables pour le comptage des @
nb_at <- 0

# Parcours des éléments du dataframe
for (i in 1:length(act$V1)) {
  elements <- act$V1[i]
  nb_at <- str_count(element,"@")
  # Parcours des éléments de la colonne
    if (nb_at == 0) {
        niv0 <- c(niv0, element)}
    if (nb_at == 1){niv1 <- c(niv1, element)
    }
        
    if (nb_at == 2){niv2 <- c(niv2, element)}
        
      }
    }

niv0 <- vector("list")
niv1 <- vector("list")
niv2 <- vector("list")

# Variables pour le comptage des @
nb_at <- 0

# Parcours des éléments du dataframe
for (i in 1:length(act$V1)) {
  elements <- act$V1[i]
  #je peux peut etre reemplacer cela avec la fonction gregex pour rendre le package indépendant de stringr
  nb_at <- str_count(elements, "@") 
  
  # Parcours des éléments de la colonne
  if (nb_at == 0) {
    niv0 <- c(niv0, elements)
  }
  if (nb_at == 1) {
    niv1 <- c(niv1, elements)
  }
  if (nb_at == 2) {
    niv2 <- c(niv2, elements)
  }
}

#pas encore tester
separer_niveaux <- function(liste_elements) {
  niv0 <- vector("list")
  niv1 <- vector("list")
  niv2 <- vector("list")
  
  for (i in 1:length(liste_elements)) {
    elements <- liste_elements[i]
    nb_at <- str_count(elements, "@")
    
    if (nb_at == 0) {
      niv0 <- c(niv0, elements)
    } else if (nb_at == 1) {
      niv1 <- c(niv1, elements)
    } else if (nb_at == 2) {
      niv2 <- c(niv2, elements)
    }
  }
  
  resultats <- list(niv0 = niv0, niv1 = niv1, niv2 = niv2)
  return(resultats)
}















































"""

ARBRE

"""
















construire_arbre <- function(corr_table, niveau = 1) {
  # Initialisation de l'arbre avec la racine "ALL"
# Exemple d'utilisation avec la corr_table corr_tab5
corr_tab5 <- tibble(
  niv1 = c(rep("ALL", 3)),
  niv2 = c(rep("A1", 2), "A2"),
  niv3 = c("A11", "A12", "A2")
)

arbre2 <- construire_arbre(corr_tab5)

# Affichage de l'arbre
print(arbre2)









# Exemple d'utilisation avec la corr_tab5


names(arbre)[[4]] <- "A2"
names(arbre)[[3]] <- "A1"
# Aff

corr_tab3 <- tibble(
  niv0 = "ALL",
  niv1 = c(rep("A",2),rep("B",3), "C"),
  niv2 = c("A1","A2","B1",rep("B2",2), "C"),
  niv3 = c("A1","A2","B1", "B21", "B22", "C")
)
corr_tab4 <- tibble(
  niv1 = c(rep("A",2),rep("B",5), "C"),
  niv2 = c("A1","A2","B1",rep("B2",4), "C"),
  niv3 = c("A1","A2","B1", rep("B21",2),rep("B22",2), "C"),
  niv4 = c("A1","A2","B1", "B211","B212" ,"B221","B222", "C"))

corr_tab5 <-tibble(
  niv1 = c("1",2,3,4,5),
  niv2 = c("11",14 ,21,23,31,32))


tree[1]
tree[2]

tree<-list( list(1,list(11,14)),list(2,list(21,23)),list(3,list(31:32))
            
corr_tab4 <- tibble(
              niv1 = c(rep("ALL",5)),
              niv2 = c(rep("A1",3),"A2","A3"),
              niv3 = c("A11","A12","A13")
              
corr_tab5 <- tibble(
                niv1 = c(rep("ALL",3)),
                niv2 = c(rep("A1",2),"A2"),
                niv3 = c("A11","A12","A2")
                
arbre <- list("ALL",
                      list("A1","A2","A3"),
                      list("A11","A12","A13"),
                      list("A22","A24"))
          
                
names(arbre[[3]])<- "A1" 

arbre$A1
arbre$A2

crea_names=

creation_hierarchie


arbre2 <- list("ALL",
list( "A1", "A2" ),
list("A11","A12"))
                
"""
# Fonction récursive pour construire les listes à partir de l'arbre
construire_listes <- function(arbre, prefix = "") {
  result <- list()  # Liste résultat
  
  # Parcourir les éléments de l'arbre
  for (i in seq_along(arbre)) {
    element <- arbre[[i]]
    
    # Vérifier si l'élément est une liste
    if (is.list(element)) {
      # Appeler récursivement la fonction pour l'élément de la liste
      sous_liste <- construire_listes(element, prefix = paste0(prefix, i))
      
      # Ajouter les éléments de la sous-liste à la liste résultat
      result <- c(result, sous_liste)
    } else {
      # L'élément est une valeur individuelle, l'ajouter à la liste résultat
      result <- c(result, paste0(prefix, element))
    }
  }
  
  return(result)
}

# Exemple d'utilisation avec l'arbre donné
arbre <- list("ALL",
              list("A1", "A2", "A3"),
              list("A11", "A12", "A13"),
              list("A22", "A24"))

# Appeler la fonction pour construire les listes
resultat <- construire_listes(arbre)

"""


>>>>>>> d0ee274a5d80e633ebae8a62156263ba8623a836
