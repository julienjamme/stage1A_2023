# Ordonner une table sur plusieurs variables
# On veut trier la table ca_pizzas_4vars sur ses 4 premières variables
load("data/ca_pizzas_4vars.RData")
library(dplyr)
library(data.table)

# en R de base
baseR_order <- ca_pizzas_4vars

for(j in 4:1){
  #On souhaite que le tri principal soit effectué sur la première variable
  #il est donc fat en dernier
  baseR_order <- baseR_order[order(baseR_order[[j]]),]
}
head(baseR_order)

# avec dplyr
dplyr_order <- ca_pizzas_4vars %>% arrange(across(1:4))
head(dplyr_order)

# avec data.table
datatable_order <- as.data.table(ca_pizzas_4vars)
setorderv(datatable_order, colnames(datatable_order)[1:4])

head(datatable_order)


# on peut tester que les trois tables sont bien triées dans le même ordre
# Pour cela on accolle les trois tables et on vérifie que les valeurs 
#lignes à lignes sont bien égales

purrr::imap_dfc(
  list(b = baseR_order, d = dplyr_order, dt = datatable_order),
  function(df, name){
    df %>% 
      select(1:4) %>% 
      rename_with(~paste0(.,"_",name))
  }
) %>%
  mutate(
    ACTIVITY_TEST = (ACTIVITY_b != ACTIVITY_d) | 
      (ACTIVITY_d != ACTIVITY_dt) | (ACTIVITY_b != ACTIVITY_dt),
    NUTS23_TEST = (NUTS23_b != NUTS23_d) | 
      (NUTS23_d != NUTS23_dt) | (NUTS23_d != NUTS23_dt),
    treff_TEST = (treff_b != treff_d) |
      (treff_d != treff_dt) | (treff_b != treff_dt),
    cj_TEST = (cj_b != cj_d) |
      (cj_d != cj_dt) | (cj_b != cj_dt)
  ) %>% 
  summarise(
    across(ends_with("TEST"), sum)
  )
# Attendu: 0,0,0,0





