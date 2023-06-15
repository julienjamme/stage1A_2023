
# Cas 1 -------------------------------------------------------------------

tab <- expand.grid(
  cj = paste0("cj", c(1:6,"T")),
  tr = paste0("tr", c(1:5,"T")),
  stringsAsFactors = FALSE
)

tab <- as.data.frame(tab)
str(tab)

tab$cj_tr <- paste(tab$cj, tab$tr, sep = "_")

split(tab, tab$tr)

# cj1_tr1 => cj1_trT et cjT_tr1



# Cas 2 -------------------------------------------------------------------
corr_tab <- data.frame(
  cjB = c(11:14,21:23,31:32,41,51,61)
)
corr_tab$cjA <- round(corr_tab$cjB/10)

tab2 <- expand.grid(
  cj = paste0("cj", c(unique(corr_tab$cjA),corr_tab$cjB,"T")),
  tr = paste0("tr", c(1:5,"T")),
  stringsAsFactors = FALSE
)

tab2 <- as.data.frame(tab2)
str(tab2)

tab2$cj_tr <- paste(tab2$cj, tab2$tr, sep = "_")
split(tab2, tab2$tr)


# Cas table triée selon leur table de correspondance -------------------------------------------------------------

library(dplyr)


# Exemple de table de correspondance pour les variables "A" et "B"
corr_tab_Ab <- tibble(
  niv1 = c(rep("A1",5),rep("A2",2),"A3"),
  niv2 = c(rep("A11",3),"A12","A2","A21","A3"),
  niv3 = c("A1","A11","A111","A112","A12","A2","A21","A3")
)

corr_tab_B <- tibble(
  niv1 = c(rep("B1",5),rep("B2",2),"B3"),
  niv2 = c("B1",rep("B11",3),"B12","B2","B21","B3"),
  niv3 = c("B1","B11","B111","B112","B12","B2","B21","B3")
)

# Création du croisement cartésien entre A et B, sans oublier le total
tab_c <- expand.grid(
  A = c(c("A1","A11","A111","A112","A12","A2","A21","A3"),"AT"),
  B = c(c("B1","B11","B111","B112","B12","B2","B21","B3"),"BT"),
  stringsAsFactors = FALSE
)
tab_c <- as.data.frame(tab_c)

# tab_c <- tab_c[order(tab_c$A,decreasing = TRUE),]
# sensible à l'ordre !

# Création du croisement cartésien table de corrélation A et B avec ajout du total
tab_c_corr1 <- expand.grid(
  corr_A = c(corr_tab_Ab$niv1,"AT"),
  corr_B = c(corr_tab_B$niv1,"BT"),
  stringsAsFactors = FALSE
)
tab_c_corr1 <- as.data.frame(tab_c_corr1)

# str(tab_c)

tab_c$A_B <- paste(tab_c$A, tab_c$B, sep = "_")

split(tab_c, tab_c_corr1$corr_A)


# case table non triée selon leur table de correspondance ------------------------------------------------------------------


# Exemple de table de correspondance pour les variables "A" et "B"
# Enlève les niveaux non nécessaires par rapport au cas précédent
corr_tab_Ab <- tibble(
  niv1 = c(rep("A1",3),rep("A2",2),"A3"),
  niv2 = c(rep("A11",2),"A12","A21","A22","A3"),
  niv3 = c("A111","A112","A12","A21","A22","A3")
)

corr_tab_B <- tibble(
  niv1 = c(rep("B1",3),rep("B2",2),"B3"),
  niv2 = c(rep("B11",2),"B12","B21","B22","B3"),
  niv3 = c("B111","B112","B12","B21","B22","B3")
)


# Création du croisement cartésien entre A et B, sans oublier le total
tab_c <- expand.grid(
  A = c(c("A111","A112","A12","A21","A22","A3"),"AT"),
  B = c(c("B111","B112","B12","B21","B22","B3"),"BT"),
  stringsAsFactors = FALSE
)
tab_c <- as.data.frame(tab_c)

# tab_c <- tab_c[order(tab_c$A,decreasing = TRUE),]
# sensible à l'ordre !

# Création du croisement cartésien table de corrélation A et B avec ajout du total
tab_c_corr1 <- expand.grid(
  corr_A = c(corr_tab_Ab$niv1,"AT"),
  corr_B = c(corr_tab_B$niv1,"BT"),
  stringsAsFactors = FALSE
)
tab_c_corr1 <- as.data.frame(tab_c_corr1)

# str(tab_c)

tab_c$A_B <- paste(tab_c$A, tab_c$B, sep = "_")

split(tab_c, tab_c_corr1$corr_A)


tab_c_shuffle = tab_c[sample(1:nrow(tab_c)), ]

tmp <- tab_c_shuffle[order(tab_c_shuffle$B, tab_c_shuffle$A),]

split(tmp, tab_c_corr1$corr_A)
# -> ça marche


################################################
################################################
# Deuxième teste avec des hierarchies différentes
################################################
################################################


# Exemple de table de correspondance pour les variables "A" et "B"
# Enlève les niveaux non nécessaires par rapport au cas précédent
corr_tab_Ab <- tibble(
  niv1 = c(rep("A1",3),rep("A2",2),"A3"),
  niv2 = c(rep("A11",2),"A12","A21","A22","A3"),
  niv3 = c("A111","A112","A12","A21","A22","A3")
)

corr_tab_B <- tibble(
  niv1 = c(rep("B1",2),rep("B2",2),"B3",rep("B4",2)),
  niv2 = c("B11","B12","B21","B22","B3","B41","B42"),
)


# Création du croisement cartésien entre A et B, sans oublier le total
# Ajout de tous les sous totaux :)

sous_totA <- setdiff(union(corr_tab_Ab$niv1,corr_tab_Ab$niv2), 
                     corr_tab_Ab$niv3)

sous_totB <- setdiff(corr_tab_B$niv1, 
                     corr_tab_B$niv2)


tab_c <- expand.grid(
  A = c(corr_tab_Ab$niv3,sous_totA,"AT"),
  B = c(corr_tab_B$niv2,sous_totB,"BT"),
  stringsAsFactors = FALSE
)

tab_c <- as.data.frame(tab_c)

# Création du croisement cartésien table de corrélation A et B avec ajout du total
tab_c_corr1 <- expand.grid(
  corr_A = c(corr_tab_Ab$niv1,sous_totA,"AT"),
  corr_B = c(corr_tab_B$niv1,sous_totB,"BT"),
  stringsAsFactors = FALSE
)
tab_c_corr1 <- as.data.frame(tab_c_corr1)

tab_c_corr2 <- expand.grid(
  corr_A = c(corr_tab_Ab$niv1,sous_totA,"AT"),
  corr_B = c(corr_tab_B$niv1,sous_totB,"BT"),
  stringsAsFactors = FALSE
)
tab_c_corr2 <- as.data.frame(tab_c_corr2)

# str(tab_c)

tab_c$A_B <- paste(tab_c$A, tab_c$B, sep = "_")

split_table<- split(tab_c, tab_c_corr1$corr_A)


# rbind ???

# to do :
# il faudrait ne pas spiter par rapport à tous les sous totaux
# mais uniquement les sous totaux grossier (niveau 1 + total)
# -> trouver comment ne pas trop split
# ou bien -> trouver comment merge les sous totaux



# -> idée non bonne (poubelle)
# on passe à l'idée d'après (cf ci-dessous)



#############################################################################
#############################################################################
#############################################################################
#############################################################################
# Autre approche : utilisation de colonne supplémentaire représentant
# les niveaux de correspondances
#############################################################################
#############################################################################
#############################################################################
#############################################################################

library(dplyr)

# table de correspondance de la table A
corr_tab_Ab <- tibble(
  niv1 = c(rep("A1",3),rep("A2",2),"A3"),
  niv2 = c(rep("A11",2),"A12","A21","A22","A3"),
  niv3 = c("A111","A112","A12","A21","A22","A3")
)


# Création de la table A à partir de la table de correpondance
tabA <- Reduce(union, corr_tab_A)

# Création de colonne correspondant aux niveaux de correspondances
# comme dans une table de correspondance
# sans redondance des niveaux -> utilisation de ""

#fait à la main (laborieux)
# ne marche que si tabA n'est pas un dataframe de base

tabA <- as.data.frame(tabA) %>%
  mutate(level1 = case_when(
    tabA %in% corr_tab_Ab$niv1 ~ tabA,
    tabA %in% corr_tab_Ab$niv2 ~ corr_tab_Ab$niv1[match(tabA, corr_tab_Ab$niv2)],
    tabA %in% corr_tab_Ab$niv3 ~ corr_tab_Ab$niv1[match(tabA, corr_tab_Ab$niv3)]
  )) %>%
  mutate(level2 = case_when(
    tabA %in% corr_tab_Ab$niv1 ~ "",
    tabA %in% corr_tab_Ab$niv2 ~ tabA,
    tabA %in% corr_tab_Ab$niv3 ~ corr_tab_Ab$niv2[match(tabA, corr_tab_Ab$niv3)]
  )) %>%
  mutate(level3 = case_when(
    tabA %in% corr_tab_Ab$niv1 ~ "",
    tabA %in% corr_tab_Ab$niv2 ~ "",
    tabA %in% corr_tab_Ab$niv3 ~ tabA
  ))

tabA

###############################################################################

# table de correspondance de la table Ab
# afin d'avoir un autre exemple empirique
corr_tab_Ab <- tibble(
  niv1 = c(rep("A1",5),rep("A2",2),"A3"),
  niv2 = c(rep("A11",4),"A12","A21","A22","A3"),
  niv3 = c(rep("A111",3),"A112","A12","A21","A22","A3"),
  niv4 = c(rep("A1111",2),"A1112","A112","A12","A21","A22","A3"),
  niv5 = c("A11111","A11112","A1112","A112","A12","A21","A22","A3")
)

# Création de la table Ab à partir de la table de correpondance
tabAb <- Reduce(union, corr_tab_Ab)

tabAb <- as.data.frame(tabAb) %>%
  mutate(level1 = case_when(
    tabAb %in% corr_tab_Ab$niv1 ~ tabAb,
    tabAb %in% corr_tab_Ab$niv2 ~ corr_tab_Ab$niv1[match(tabAb, corr_tab_Ab$niv2)],
    tabAb %in% corr_tab_Ab$niv3 ~ corr_tab_Ab$niv1[match(tabAb, corr_tab_Ab$niv3)],
    tabAb %in% corr_tab_Ab$niv4 ~ corr_tab_Ab$niv1[match(tabAb, corr_tab_Ab$niv4)],
    tabAb %in% corr_tab_Ab$niv5 ~ corr_tab_Ab$niv1[match(tabAb, corr_tab_Ab$niv5)]
  )) %>%
  mutate(level2 = case_when(
    tabAb %in% corr_tab_Ab$niv1 ~ "",
    tabAb %in% corr_tab_Ab$niv2 ~ tabAb,
    tabAb %in% corr_tab_Ab$niv3 ~ corr_tab_Ab$niv2[match(tabAb, corr_tab_Ab$niv3)],
    tabAb %in% corr_tab_Ab$niv4 ~ corr_tab_Ab$niv2[match(tabAb, corr_tab_Ab$niv4)],
    tabAb %in% corr_tab_Ab$niv5 ~ corr_tab_Ab$niv2[match(tabAb, corr_tab_Ab$niv5)]
  )) %>%
  mutate(level3 = case_when(
    tabAb %in% corr_tab_Ab$niv1 ~ "",
    tabAb %in% corr_tab_Ab$niv2 ~ "",
    tabAb %in% corr_tab_Ab$niv3 ~ tabAb,
    tabAb %in% corr_tab_Ab$niv4 ~ corr_tab_Ab$niv3[match(tabAb, corr_tab_Ab$niv4)],
    tabAb %in% corr_tab_Ab$niv5 ~ corr_tab_Ab$niv3[match(tabAb, corr_tab_Ab$niv5)]
  )) %>%
  mutate(level4 = case_when(
    tabAb %in% corr_tab_Ab$niv1 ~ "",
    tabAb %in% corr_tab_Ab$niv2 ~ "",
    tabAb %in% corr_tab_Ab$niv3 ~ "",
    tabAb %in% corr_tab_Ab$niv4 ~ tabAb,
    tabAb %in% corr_tab_Ab$niv5 ~ corr_tab_Ab$niv4[match(tabAb, corr_tab_Ab$niv5)]
  )) %>%
  mutate(level5 = case_when(
    tabAb %in% corr_tab_Ab$niv1 ~ "",
    tabAb %in% corr_tab_Ab$niv2 ~ "",
    tabAb %in% corr_tab_Ab$niv3 ~ "",
    tabAb %in% corr_tab_Ab$niv4 ~ "",
    tabAb %in% corr_tab_Ab$niv5 ~ tabAb
  ))

tabAb


# to do : trouver comment le faire dynamiquement
# + trouver comment le faire à partir d'un .hrc
# et non pas d'une table de correspondance !



# Recherche empirique d'une formule générale de split :

# groupes de niveau 1
tabA %>% 
  filter(level2 == "")

tabAb %>% 
  filter(level2 == "")

tabAb %>% 
  filter(level2 == "") %>% 
  split(.$level3)

# groupes de niveau 2
tabA %>%
  filter(level3 == "") %>%
  split(.$level1)


tabAb %>%
  filter(level3 == "") %>%
  split(.$level1)

tabAb %>%
  filter(level3 == "" & level2 != "") %>%
  split(.$level1)

# groupes de niveau 3
tabA %>% 
  filter(level3 != "" | (level3 == "" & level2 != "")) %>% 
  split(.$level2)

tabAb %>% 
  filter(  level2 != "" & level4 == "" & level3 != "") %>% 
  split(.$level2)

# groupes de niveau 4
tabAb %>% 
  filter( level5 == "" & level3 != "" ) %>% 
  split(.$level3)


# même chose mais en accédant aux colonnes avec des variables
levelip1 <- paste0("level", as.character(4+1))
levelim1 <- paste0("level", as.character(4-1))

tabAb %>% 
  filter(pull(., {{ levelip1 }}) == "" & pull(., {{ levelim1 }}) != "") %>% 
  split(., pull(., {{ levelim1 }}))

# groupes de niveau 5
tabAb %>% 
  filter( level4 != "" & level5 != "") %>% 
  split(.$level4)



# formule générale :

# group 1 : 
#tabA %>% 
#  filter(level1 == "") %>% 
#  select(tabA)

# group i from 2 to n-1 :
#tabAb %>% 
#  filter( leveli+1 == "" & leveli-1 != "" ) %>% 
#  split(.$leveli-1)

# group n (si n != 1) :
#tabAb %>% 
#  filter( leveln-1 != "" ) %>% 
#  split(.$leveln-1)


# Reminder :
# ctrl shift c pour commenter plusieurs lignes
# ctrl shft r pour section de code
# ctrl + alt + shift + r faire le skelette d'une fonction


#' Fonction de découpage en sous tableaux hierarchique
#'
#' @param table Un dataframe contenant les niveaux hierarchique level1 ... leveln
#' nommé de la sorte et des "" pour les sous-niveaux "trop fin"
#'
#'# Exemple de dataframe dans le format requis :
# > tabAb
#      tabAb level1 level2 level3 level4 level5
# 1       A1     A1                            
# 2       A2     A2                            
# 3       A3     A3                            
# 4      A11     A1    A11                     
# 5      A12     A1    A12                     
# 6      A21     A2    A21                     
# 7      A22     A2    A22                     
# 8     A111     A1    A11   A111              
# 9     A112     A1    A11   A112              
# 10   A1111     A1    A11   A111  A1111       
# 11   A1112     A1    A11   A111  A1112       
# 12  A11111     A1    A11   A111  A1111 A11111
# 13  A11112     A1    A11   A111  A1111 A11112
# 14 A_Total
#'
#' @param n Le nombre de niveau hierarchique
#'
#' @return une liste de liste de dataframe
#'        ces différents dataframe conservent la hierarchie de la table de départ
#'        et minimise le nombre de doublon (uniquement les sous totaux 2 à 2 emboités)
#'
#' @examples
# > res <- decouper_en_sous_tableaux(tabAb, 5)
# > res
# [[1]]
# [[1]][[1]]
#       tabAb level1 level2 level3 level4 level5
# 1      A1     A1                            
# 2      A2     A2                            
# 3      A3     A3     
# 4 A_Total
# 
# 
# [[2]]
# [[2]]$A1
#    tabAb level1 level2 level3 level4 level5
# 1    A1     A1                            
# 4   A11     A1    A11                     
# 5   A12     A1    A12                     
# 
# [[2]]$A2
#    tabAb level1 level2 level3 level4 level5
# 2    A2     A2                            
# 6   A21     A2    A21                     
# 7   A22     A2    A22                     
# 
# 
# [[3]]
# [[3]]$A11
#    tabAb level1 level2 level3 level4 level5
# 1   A11     A1    A11                     
# 5  A111     A1    A11   A111              
# 6  A112     A1    A11   A112              
# 
# 
# [[4]]
# [[4]]$A111
#   tabAb level1 level2 level3 level4 level5
# 1  A111     A1    A11   A111              
# 3 A1111     A1    A11   A111  A1111       
# 4 A1112     A1    A11   A111  A1112       
# 
# 
# [[5]]
# [[5]]$A1111
#    tabAb level1 level2 level3 level4 level5
# 1  A1111     A1    A11   A111  A1111       
# 3 A11111     A1    A11   A111  A1111 A11111
# 4 A11112     A1    A11   A111  A1111 A11112
#'
#'
decouper_en_sous_tableaux <- function(table, n) {
  sous_tableaux <- list()  # Create an empty list to store the sub-tables
  
  for (i in 1:n) {
      
    # création des différentes sous-tables
    if (i == 1) {
      levelip1 <- paste0("level", as.character(i+1))
      
      sous_table <- table %>%
        filter(pull(., {{ levelip1 }}) == "")
      
      # on met la sous_table sous le même format que
      # dans les cas else if/else pour harmoniser
      sous_table <- list(sous_table)
      
    } else if (i < n) {
      levelim1 <- paste0("level", as.character(i-1))
      levelip1 <- paste0("level", as.character(i+1))
      
      sous_table <- table %>% 
        filter(pull(., {{ levelip1 }}) == "" &
               pull(., {{ levelim1 }}) != "") %>% 
        split(., pull(., {{ levelim1 }}))
      
    } else {
      levelim1 <- paste0("level", as.character(i-1))
      
      sous_table <- table %>%
        filter(pull(., {{ levelim1 }}) != "") %>%
        split(., pull(., {{ levelim1 }}))
    }
    
    
    # le code au dessus génère des tables à une seule ligne
    # pour les extrémités de l'arbre
    # nous les enlevons -> elle ne donne aucune information
    
    # cas : liste de dataframe
    
    # les data.frame sont reconnus comme des listes
    # donc on n'utilise que les listes ne sont pas des dataframes...
    if (!is.data.frame(sous_table)){
      j <- 1
      while (j <= length(sous_table)) {
        nl <- nrow(sous_table[[j]]) # nombre de ligne du dataframe
        if (nl == 1) {
          sous_table[[j]] <- NULL
          # on enlève les dataframe d'une seule ligne
        } else {
          j <- j + 1
          # on n'incrémente que si on ne supprime rien
          # car supprimer décale les élements de la liste
        }
      }
    }
    #print(sous_table)
    sous_tableaux[[i]] <- sous_table  # Add the sub-table to the list
  }
  return(sous_tableaux)
}

# vérification que tout marche bien :)
res <- decouper_en_sous_tableaux(tabAb, 5)

res[1]
res[3]

class(res[1])
class(res[3])

class(res[[1]])
class(res[[3]])


#' Création d'une liste de dataframe
#'
#' @param res liste de liste de dataframe
#'            même format que l'output de la fonction decouper_en_sous_tableaux
#'
#' @return une liste de dataframe
#'
#' @examples
#' 
#' Exemple de tableau en entrée : (issue de decouper_en_sous_tableaux)
# > res2
# [[1]]
# [[1]][[1]]
#     tabAb level1 level2 level3 level4 level5
# 1     A1     A1                            
# 2     A2     A2                            
# 3     A3     A3 
# 4 A_Total 
# 
# 
# [[2]]
# [[2]]$A1
#    tabAb level1 level2 level3 level4 level5
# 1    A1     A1                            
# 4   A11     A1    A11                     
# 5   A12     A1    A12                     
# 
# [[2]]$A2
#    tabAb level1 level2 level3 level4 level5
# 2    A2     A2                            
# 6   A21     A2    A21                     
# 7   A22     A2    A22                     
# 
# 
# [[3]]
# [[3]]$A11
#    tabAb level1 level2 level3 level4 level5
# 1   A11     A1    A11                     
# 5  A111     A1    A11   A111              
# 6  A112     A1    A11   A112              
# 
# 
# [[4]]
# [[4]]$A111
#    tabAb level1 level2 level3 level4 level5
# 1  A111     A1    A11   A111              
# 3 A1111     A1    A11   A111  A1111       
# 4 A1112     A1    A11   A111  A1112       
# 
# 
# [[5]]
# [[5]]$A1111
#    tabAb level1 level2 level3 level4 level5
# 1  A1111     A1    A11   A111  A1111       
# 3 A11111     A1    A11   A111  A1111 A11111
# 4 A11112     A1    A11   A111  A1111 A11112
#
#
#
# Sortie associée :
# > res3 <- forme_liste_dataf(res2)
# > res3
# [[1]]
#     tabAb level1 level2 level3 level4 level5
# 1     A1     A1                            
# 2     A2     A2                            
# 3     A3     A3  
# 4 A_Total
# 
# $A1
#    tabAb level1 level2 level3 level4 level5
# 1    A1     A1                            
# 4   A11     A1    A11                     
# 5   A12     A1    A12                     
# 
# $A2
#    tabAb level1 level2 level3 level4 level5
# 2    A2     A2                            
# 6   A21     A2    A21                     
# 7   A22     A2    A22                     
# 
# $A11
#    tabAb level1 level2 level3 level4 level5
# 1   A11     A1    A11                     
# 5  A111     A1    A11   A111              
# 6  A112     A1    A11   A112              
# 
# $A111
#    tabAb level1 level2 level3 level4 level5
# 1  A111     A1    A11   A111              
# 3 A1111     A1    A11   A111  A1111       
# 4 A1112     A1    A11   A111  A1112       
# 
# $A1111
#    tabAb level1 level2 level3 level4 level5
# 1  A1111     A1    A11   A111  A1111       
# 3 A11111     A1    A11   A111  A1111 A11111
# 4 A11112     A1    A11   A111  A1111 A11112
forme_liste_dataf <- function(res){
  liste_dataframe <- list()
  for (i in 1:length(res)){
    liste_i <- res[[i]]
    for (j in 1:length(liste_i)){
        # print(c("type = ",class(liste_i[[j]])))
        liste_dataframe <- append(liste_dataframe, liste_i[j])
      }
    }
  return(liste_dataframe)
}
# to do : passer en lapply pour gagner en rapidité


res2 <- res

# res3[[i]] est bien un dataframe !
# et res3 une liste
res3 <- forme_liste_dataf(res2)
class(res3)
class(res3[[1]])

#####
# test emprique avec total global


tabAb

# Ajout du total global !
total_ligne <- c("A_Total", "","","","","")
tabAb_avec_total <- rbind(tabAb,total_ligne)

res_total <- decouper_en_sous_tableaux(tabAb_avec_total, 5)
res_total2 <- forme_liste_dataf(res_total)

# -> fonctionne bien avec les totaux globaux !
# il faut juste que level1=...=leveln = "" pour la ligne du total !

#' Enlever les colonnes superflus (leveli)
#'
#' @param table dataframecontenant les colonnes niveau1...niveaun
#' @param n nombre de niveau total
#'
#' @return dataframe privé des colonnes level1...leveln
#'
#' @examples
#' > tabAb_avec_total
#     tabAb level1 level2 level3 level4 level5
# 1       A1     A1                            
# 2       A2     A2                            
# 3       A3     A3                            
# 4      A11     A1    A11                     
# 5      A12     A1    A12                     
# 6      A21     A2    A21                     
# 7      A22     A2    A22                     
# 8     A111     A1    A11   A111              
# 9     A112     A1    A11   A112              
# 10   A1111     A1    A11   A111  A1111       
# 11   A1112     A1    A11   A111  A1112       
# 12  A11111     A1    A11   A111  A1111 A11111
# 13  A11112     A1    A11   A111  A1111 A11112
# 14 A_Total                                   
#
#
#> tabAb_avec_total_sans_col <- enlever_colonne(tabAb_avec_total,5)
#> tabAb_avec_total_sans_col
#     tabAb
# 1       A1
# 2       A2
# 3       A3
# 4      A11
# 5      A12
# 6      A21
# 7      A22
# 8     A111
# 9     A112
# 10   A1111
# 11   A1112
# 12  A11111
# 13  A11112
# 14 A_Total
#
enlever_colonne <- function(table,n){
  for (i in 1:n){
    level_i = paste0("level", as.character(i))
    table <- table %>% 
      select(-{{level_i}})
  }
  return(table)
}


tabAb_avec_total <- rbind(tabAb,total_ligne)


ma_col <- "level1"
tabAb_avec_total %>% 
  select(- {{ma_col}} )


tabAb_avec_total_sans_col <- enlever_colonne(tabAb_avec_total,5)


res_total2[1]
data_t <- enlever_colonne(res_total2[[1]],n)

###
res_total2
res_total3 <- res_total2

# enlever toutes les colonnes superflus dans tous les dataframes !
res_total4 <- lapply(res_total3, function(x) enlever_colonne(x, 5))

# -> it works !
