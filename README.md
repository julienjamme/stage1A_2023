# Pose du secret statistique sur des tableaux à grande dimension

## Contexte 

### Principe du secret statistique

Le secret statistique impose de masquer les cellules avec peu de contributeurs (2 ou moins) ou avec un contributeur majoritaire (85%+ de la valeur). On appelle cela le secret primaire.

Cette tâche est relativement facile.

Pour éviter que l’on puisse retrouve le secret primaire par des calculs (via notamment les sous totaux), on masque d’autres cellules. On appelle cela le secret secondaire.


### Limite à la pose du secret secondaire

Plusieurs algorithmes existent pour la pose du secret secondaire. Ils résolvent des problèmes de minimisation. Ces algorithmes fonctionnent très bien en dimensions 3 (ie 3 variables catégorielles croisées) mais peuvent ne pas aboutir en dimension 4 ou 5.


Ainsi, nous avons l’idée de passer un problème de dimension 4 ou 5 en sous problèmes de dimension 3 afin de poser le secret secondaire. 
## Principe

Nous fusionons plusieurs variables afin de passer en 3 dimensions. Cependant ce processus crée des hiérarchies non emboitées. 

Ainsi nous créons une liste de sous tableaux afin de ne générer que des hiérarchies emboitées.

## Principe

Le principe est de fusionner deux variables en une. Par exemple en fusionnant les variables SEX et AGE en une variable SEX_AGE.

Prenons les variables SEX et AGE telles que les modalités de SEX sont Total, Femme et Homme et les modalités de AGE sont Ensemble, Adulte et Eenfant, avec Total la somme des modalité Femme et Homme, et Ensemble la somme des modalités Adulte et Enfant.

Alors les modalités de SEX_AGE sont des combinaisons du type Total_Ensemble, Femme_Adulte, Femme_Enfant, Homme_Enfant,Homme_Adulte.

Néanmoins, cette fusion naive crée des hierarchies non emboité.

Par exemple Total_Ensemble = Total_Adulte + Total_Enfant  mais on a aussi
            Total_Ensemble = Femme_Ensemble + Homme_Ensemble

Pour ne pas perdre cette hierarchie, nous créons deux tableaux : l’un a pour marge SEX, l’autre AGE.

| Modalité premier tableau |  |		Modalité deuxième tableau |
| ------------------------ |- |---------------------------- |
| Total_Ensemble           |  |	Total_Ensemble              |
| Femme_Ensemble           | 	|	Total_Adulte                |
| Femme_Adulte             | 	|	Femme_Adulte                |
| Femme_Enfant             |	|	Homme_Adulte                |
| Homme_Ensemble           | 	|	Total_Enfant                |
| Homme_Adulte             |	|	Femme_Enfant                |
| Homme_Enfant             |	|	Homme_Enfant                |

Lorsqu’il y a des hierarchies, nous décomposons ses hierarchies en plusieurs tableau à variable non hiérarchique.

Par exemple pour une variable GEO prenant des modalités France, toutes les Régions, tous les Départements, nous créons un tableau avec la France et toutes les régions, ensuites ,pour chacunes des Régions, un tableau avec la Région et chacun des départements qui la composent.

## Implémentation

### Fonction principale

Nous avons élaboré une fonction **gen_tabs_5_4_to_3** afin de passer des tableaux de 5 ou 4 dimensions en 3 dimensions.

#### Input

Les principaux éléments pris en entrée sont :

- **dfs** : data.frame à 4 ou 5 variables catégorielles

- **nom_dfs** : nom du data.frame dans la liste fournie par l'utilisateur

- **totcode** : vecteur normée des totaux pour les variables catégorielles

Des arguments optionnels supplémentaires peuvent être renseignés :

- **hrcfiles** : vecteur normée des hrc pour les variables catégorielles hiérarchiques

- **sep_dir** : permet de forcer l'écriture des hrc dans un dossier séparé (par défaut à FALSE)

- **hrc_dir** : dossier où écrire les fichiers hrc si l'on force l'écriture dans un nouveau dossier ou si aucun dossier n'est spécifié dans hrcfiles

- **v1** : permet de forcer la valeur de la première variable à fusionner lors du passage de la première réduction de dimension

- **v2** : permet de forcer la valeur de la seconde variable à fusionner lors du passage de la première réduction de dimension

- **v3** : permet de forcer la valeur de la première variable à fusionner lors du passage de la seconde réduction de dimension

- **v4** : permet de forcer la valeur de la seconde variable à fusionner lors du passage de la seconde réduction de dimension

- **vec_sep** : vecteur des séparateurs candidats à utiliser. Par défault, nous essayon "+", "!", "?", ":", ";", "~", "&" et "#". A noter que nous utilisons en préfixe un double backslash ("\\") pour échapper les caractères spéciaux.

- **select_hier** : précise si l'on préfère sélectionner les variables hiérarchiques avec le plus de noeuds en priorité (hier=TRUE), ce qui génère plus de tableaux mais de taille moins importante, ou bien les variables non hiérarchiques avec le moins de modalités (hier=FALSE) pour créer le moins de tableau possible

#### Output

L'élement retourné est une liste contenant les éléments : 

- **tab** : liste nommée des dataframes à 3 dimensions doté de hiérarchies emboitées

- **hrcs** : liste nommée des hrc spécifiques aux variables créés lors de la fusion pour passer en dimension 3

- **alt_tot** : liste nommée des totaux spécifiques aux variables créés lors de la fusion pour passer en dimension 3

- **vars** : liste nommée de vecteur représentant les variables fusionnées lors des deux étapes de réduction de dimensions

- **sep** : séparateur utilisé pour lier les variables

La sortie est formatée de manière à être utilisable avec le package rtauargus.

#### Exemple d'utilisation

Exemple en supposant l'existence des fichiers hrc : *hrc/hrc2.hrc*, *hrc/hrc_REG_deep_3.hrc* et *hrc/hrc3.hrc*.

```R
library(dplyr)

data <- expand.grid(
  ACT = c("KEBAB",read.table("hrc/hrc2.hrc") %>% 
              mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",read.table("hrc/hrc3.hrc") %>% 
              mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Pays",read.table("hrc/hrc_REG_deep_3.hrc") %>% 
              mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  AGE = c("Ensemble","adulte","enfant"),
  ECO = c("PIB","riche","pauvre"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))

hrcfiles <- c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc" )

totcode <-c (SEX="Total",AGE="Ensemble", GEO="Pays", ACT="KEBAB", ECO = "PIB")

nom_dfs <- "nom_data_frame"

res5_3 <- gen_tabs_5_4_to_3(dfs=data,
                            nom_dfs=nom_dfs,
                            totcode=totcode,
                            hrcfiles=hrcfiles,
                            sep_dir=TRUE,
                            hrc_dir="output")
```

La fonction fonctionne également avec les tibbles. Elle renvoie dès lors une liste de tibble.

### Fonctionnement de la réduction de dimension - fonctions internes

#### Principe de base : fusionner deux variables hiérarchiques

La fusion est créé via la fonction **passage_4_3_cas_2_non_hr**. Elle créé les deux tableaux créés lors de la fusion ainsi que leur hiérarchie et les transforme en fichier .hrc.

#### Cas lorsqu'il y a des variables hiérarchiques

Lorsque nous voulons fusionner deux variables hiérarchiques, nous divions le tableau en plusieurs tableaux non hiérarchiques. Pour ce faire, en représentant la hierarchie sous forme d’arbre, nous selectionons les modalités par paquet {noeuds + branches}. Cela permet de créer plus de problèmes mais de difficulté moindre.

Ce passage est fait grâce aux fonctions **passage_4_3_cas_1_non_hr** et **passage_4_3_cas_0_non_hr** prenant respectivement 1 ou 2 variables hiérarchiques à fusionner en entrée.

Ainsi **passage_4_3_cas_1_non_hr** et **passage_4_3_cas_0_non_hr** appellent respectivement **passage_4_3_cas_2_non_hr** et **passage_4_3_cas_1_non_hr**.

Par exemple lors de l’éxécution de **passage_4_3_cas_0_non_hr**, celui_ci appelle **passage_4_3_cas_1_non_hr** qui lui même appelle **passage_4_3_cas_2_non_hr**.

#### Fonction global permettant de passer de de 4 ou 5 dimensions à 3.

La fonction **passer_de_4_a_3_var** permet de passer de 4 à 3 dimensions. 
Elle prend en entré un data.frame, choisit les variables à fusionner
en fonction de ses arguments (afin de minimiser le nombre de tableau ou bien la taille de ces derniers),
et applique les fonctions précédentes correspondantes.

La fonction **passer_de_5_a_3_var** permet de passer de 5 à 3 dimensions.
Elle applique successivement la fonction précédente.

#### Fonction générale

La fonction **gen_tabs_5_4_to_3** permet de généraliser le processus en choisissant notamment
le séparateur à utiliser (avec la fonction **choisir_sep** afin d'éviter d'utiliser "_" 
s'il est déjà présent de base dans le tableau par exemle),
appelle une des fonctions précédentes en fonction de la dimension du tableau initiale,
et formate la sortie (avec la fonction **format**) afin qu'elle soit utilisable directement avec rtauargus.


### Fonctions annexes

Plusieurs fonctions annexes sont disponibles permettant notamment de compter le nombre de tableau créés (voir section ci-dessous)

La fonction **nb_noeuds** permet de compter le nombre de noeuds d’une variable. 
La fonction **calculer_nb_tab** permet de compter le nombre de tableau créés lors de la fusion de 3 variables en une.

## Nombre de tableaux créés

### Fusion de deux variables en une

#### Dimension 4

Lors du passage de 4 dimensions à 3 dimensions et du passage de 5 dimensions à 3 dimensions via la création de deux couples de variables (ie : les variables AGE,SEXE,GEO,ECO => AGE_SEX et GEO_ECO), la formule est très simple.

Afin de rendre une variables hiérarchiques non hierarchiques, nous créons un nombre de tableau égal au nombre de noeud de celle-ci. Une fois que nous avons rendu une variable hiérarchique, sur chacun de ses sous tableaux, nous appliquons le même procédés à la deuxième variable.

Enfin, pour chaque combinaison de tableau créé, deux hierarchies sont créés pour tenir compte des marges par rapport à la première ou la seconde variable.

Nous obtenons donc dans le cas à 4 dimensions au départ:

$$\textit{nombre de tableau} = 2 \times \textit{nombre_noeuds_v1} \times \textit{nombre_noeuds_v2}$$

où v1 et v2 correpondent aux variables fusionnées et nombre_noeuds_v1 et nombre_noeuds_v2 le nombre de noeud dans leur hierarchique respective.

Par exemple dans le cas de la fusion de GEO et SEX avec GEO = {Pays, Région 1, Région 2, Commune 11, Commune 12, Commune 13, Commune 21, Commune 22,Commune 23,Commune 24} et SEX = {Ensemble, Femme, Homme} avec les hierarchies implicites habituelles (ie Pays = Région 1 + Région 2, Région 1 = Commune 11 + Commune 21 + Commune 22, etc), nous obtenons : $6 = 2 \times 3 \times 1$ tableaux.

En effet GEO a trois noeuds (France, Région 1, Région 2) et SEX étant non hierarchique, qu’un seul (Ensemble).

#### Dimension 5

Et dans le cas à 5 dimensions au départ, nous itérons le processus aux tableaux intermédiaires à 4 dimensions, donc nous obtenons :

$$\textit{nombre de tableau} = 4 \times \textit{nombre_noeuds_v1} \times \textit{nombre_noeuds_v2} 
\times \textit{nombre_noeuds_v3} \times \textit{nombre_noeuds_v4}$$

où v1…v4 correpondent aux variables fusionnées et nombre_noeuds_v1…nombre_noeuds_v4 le nombre de noeud dans leur hierarchique respective.

### Fusion de trois variables en une

Lors du passage de 5 dimensions en 3 dimensions, il est possible de fusionner trois variables en une. Par exemple à partir des variables SEX, GEO et AGE, créer la variable SEX_GEO_AGE.

Supposons que dans le passage de 5 à 4 dimensions, nous créons la variable SEX_GEO, puis que lors du passage 4 à 3 nous créons la variable SEX_GEO_AGE.

Le problème ici est que la variable SEX_GEO est hiérarchique mais ne contient pas le même nombre de noeuds dans chacun des tableaux à 4 dimensions. Il nous est donc pas possible d’effectuer un simple produit comme dans les formules précédentes.

Comptons tout d’abord le nombre de noeuds lié à la variable SEX_GEO.

Il y a un tableau différent par croisement des modalité SEX et GEO. Ensuite, il y a deux hierarchiques créés : une sur les marges de SEX, l’autre sur GEO.

La hierarchie sur les marges de AGE a autant de noeuds qu’AGE a de modalités, et de façon analogue pour SEX.

Nous obtenons alors :

$$\textit{nombre de tableau lié à v1_v2} = 
\sum_{i\in\textit{{noeud + branche v1}}} \sum_{j\in\textit{{noeud + branche v2}}} \textit{nb_mod_i + nb_mod_j}$$

où nb_mod_i et nb_mod_j correpondent au nombre de modalité respectif du regroupement i et j. Par exemple si i = {France, Région 1, Région 2} alors nb_mod_i = 3.

Ensuite, pour passer à 3 dimensions, nous obtenons :

$$
\textit{nombre de tableau} = 2 \times \textit{nombre de tableau lié à v1_v2} \times \textit{nombre_noeuds_v3}
$$

$$
=2 \times \textit{nombre_noeuds_v3} \times \sum_{i\in\textit{{noeud + branche v1}}}
\sum_{j\in\textit{{noeud + branche v2}}}
\textit{nb_mod_i + nb_mod_j}
$$

Dans l’exemple de SEX_GEO_AGE, nous obtenons :

$$\textit{nombre de tableau lié à SEXE_GEO} =  (3+3)+(3+4)+(3+5) = 21$$

$$\textit{nombre de tableau total} = 2 \times 1 \times 42 = 42$$

Puisque SEX n’est pas hierarchique donc on ne considère que {Ensemble,Homme,Femme} (qui a donc trois modalités), GEO a trois noeuds : {France, Région 1, Région 2}, {Région 1, Commune 11, Commune 12, Commune 13}, {Région 2, Commune 21, Commune 22,Commune 23,Commune 24} ayant respectivement 3, 4 et 5 modalités.

Pour finir AGE étant non hiérarchique, elle n’a qu’un seul noeud.

Nous obtenons donc 42 tableaux lors de la création de SEX_GEO_AGE. Par construction la variable SEX_GEO engendre des hiérarchies avec beaucoup de noeuds donc la fusion de trois variables en une est rare si l’on choisit de minimiser le nombre de table créée.

### Revenir à la dimension initiale

La fonction **passer_a_4_ou_5** permet de transformer la sortie à 3 dimensions en liste de tableau de 4 ou 5 dimensions.

#### Input

Le résultat de gen_tabs_5_4_to_3.

#### Output

La liste des tableaux en entrée reformés en 4 ou 5 dimensions.

## Utilisation avec rtauargus

WIP
