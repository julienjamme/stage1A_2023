calculer_secret <- function(data, type = ""){
  if (is.logical(data$is_secret_final)) {
    data <- data %>% 
      mutate(
        statut_final = case_when(
          is_secret_freq ~ "A",
          is_secret_dom ~ "B",
          is_secret_final ~ "D",
          TRUE ~ "V"
        ))
  } else {
    data <- data %>% 
      mutate(
        statut_final = is_secret_final)}
    
  data %>% 
    group_by(statut_final) %>% 
    summarise(
      n_cell = n(),
      val_cell = sum(pizzas_tot)
    ) %>%
    mutate(
      pc_n_cell = round(n_cell/sum(n_cell)*100,1),
      pc_val_cell = round(val_cell/sum(val_cell)*100,1)
    ) %>% 
    mutate(METHODE = type) %>% 
    relocate(METHODE, .before = statut_final)
  
}


test_part_secret <-function(data, quant){
  
  seuil <- quantile(data$nb_obs, probs = quant/100)
  
  data_sp <- data %>% 
    mutate(
      is_secret_freq=(nb_obs > 0 & nb_obs < seuil),
      is_secret_dom = FALSE, #(pizzas_tot != 0) & (pizzas_max > 0.85*pizzas_tot),
      pizzas_tot= abs(pizzas_tot)
    ) %>% 
    mutate(
      is_secret_prim =  is_secret_freq ,
      nb_obs = ceiling(nb_obs)
    )
  
  nom_table <- paste0("table_test_",quant)
  totcodes <- c(A10="Total",type_distrib="Total",treff="Total",cj="Total")
  
  root_dir <- "test_avec_rtauargus/hierarchie_2/0/mod"
  dir_1table <- file.path(root_dir, "table", quant)
  dir_liste <- file.path(root_dir, "liste", quant)
  
  tic()
  masq_1table <- tab_rtauargus(
    data_sp,
    files_name = nom_table,
    explanatory_vars = c("treff","A10","type_distrib","cj"),
    dir_name = dir_1table,
    totcode = totcodes,
    value = "pizzas_tot",
    freq = "nb_obs",
    secret_var = "is_secret_prim",
    verbose = FALSE
  ) %>% 
    rename(is_secret_final = last_col())
  t_1table <- toc()
  
  list_tables <- gen_tabs_5_4_to_3( 
    data_sp,
    nom_table,
    totcodes,
    sep_dir = TRUE,
    hrc_dir = "output",
    vec_sep = c("\\___")
  )
  
  tic()
  masq_liste <- tab_multi_manager(
    list_tables = list_tables$tabs,
    list_explanatory_vars = list_tables$vars ,
    dir_name = dir_liste,
    totcode = list_tables$totcode,
    alt_hrc = list_tables$hrcs,
    alt_totcode = list_tables$alt_tot,
    value = "pizzas_tot",
    maxscore = "pizzas_max",
    freq = "nb_obs",
    secret_var = "is_secret_prim",
  )
  t_liste <- toc()
  
  masq_liste_empilee <- unique(do.call("rbind", unique(masq_liste))) %>% 
    rename(is_secret_final = last_col())
  
  return(list(stats_1table=masq_1table,stats_liste=masq_liste_empilee))

  
 
}
