# Pose du secret statistique sur des tableaux à grande dimension

## Contexte 

### Principe du secret statistique

Le secret statistique impose de masquer les cellules avec peu de contributeurs (2 ou moins) ou avec un contributeur majoritaire (80%+ de la valeur). On appelle cela le secret primaire.

Cette tâche est relativement facile.

Pour éviter que l’on puisse retrouve le secret primaire par des calculs (via notamment les sous totaux), on masque d’autres cellules. On appelle cela le secret secondaire.


### Limite à la pose du secret secondaire

Plusieurs algorithmes existent pour la pose du secret secondaire. Ils résolvent des problèmes de minimisation. Ces algorithmes fonctionnent très bien en dimensions 3 (ie 3 variables catégorielles croisées) mais peuvent ne pas aboutir en dimension 4 ou 5.


Ainsi, nous avons l’idée de passer un problème de dimension 4 ou 5 en sous problèmes de dimension 3 afin de poser le secret secondaire. 

## Principe

Nous fusionons plusieurs variables afin de passer en 3 dimensions. Cependant ce processus crée des hiérarchies non emboitées.

Ainsi nous créons une liste de sous tableaux afin de ne générer que des hiérarchies emboitées.

## Implémentation

### Créations de sous problèmes en 3 dimensions

Deux fonctions principales sont implémentées afin de passer un tableau de 4 ou 5 dimensions en sous problème à 3 dimensions.

```R
#' Fonction passant de 4 à 3 variables catégorielles
#'
#' @param dfs data.frame à 4 variabls catégorielles (n >= 2 dans le cas général)
#' @param nom_dfs nom du dataframe
#' @param totcode vecteur normée des totaux pourles variables catégorielles
#' @param hrcfiles vecteur normée des hrc pour les variables catégorielles hierarchiques
#' @param sep_dir permet de forcer l'écriture des hrc dans un dossier séparé
#' par défault à FALSE
#' @param hrc_dir dossier où écrire les fichiers hrc si l'on force l'écriture
#' dans un nouveau dossier ou si aucun dossier n'est spécifié dans hrcfiles
#'
#' @return une liste de data.frame à 3 variables catégorielles
#' doté de hierarchie emboitées (n-1 dans le cas général)
passer_de_4_a_3_var <- function(dfs,nom_dfs,totcode, hrcfiles, sep_dir = FALSE, hrc_dir = "hrc_alt")


#' Fonction passant de 5 à 3 variables catégorielles
#'
#' @param dfs data.frame à 5 variabls catégorielles (n >= 3 dans le cas général)
#' @param nom_dfs nom du dataframe
#' @param totcode vecteur normée des totaux pourles variables catégorielles
#' @param hrcfiles vecteur normée des hrc pour les variables catégorielles hierarchiques
#' @param sep_dir permet de forcer l'écriture des hrc dans un dossier séparé
#' par défault à FALSE
#' @param hrc_dir dossier où écrire les fichiers hrc si l'on force l'écriture
#' dans un nouveau dossier ou si aucun dossier n'est spécifié dans hrcfiles
#' 
#' @return liste(tabs, hrcs, vars)
#' tab : liste nommée des dataframes à 3 dimensions (n-2 dimensions dans le cas général)
#' doté de hiérarchies emboitées
#' hrc : liste nommée des hrc spécifiques à la variable crée via la fusion
#' vars : liste nommée de vecteur représentant les variables fusionnées
#' lors des deux étapes de réduction de dimensions
passer_de_5_a_3_var <- function(dfs, nom_dfs,totcode, hrcfiles, sep_dir = FALSE, hrc_dir = "hrc_alt")

```

### Pose du secret

Nous pouvons après poser le secret via rtauargus.

```R
# Exemple à insérer
```

### Formation du tableau initiale

Nous pouvons enfin recréer la forme initiale à 4 ou 5 dimensions.

```R
# Exemple à insérer
```

