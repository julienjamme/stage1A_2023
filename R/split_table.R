#' Title
#'
#' @param res 
#' @param var_fus 
#' @param LIMIT 
#'
#' @return
#' @export
#'
#' @examples
#' 



split_tab <- function(res, var_fus, LIMIT) {
  # table to split because they are too big
  
  res$to_split <- sapply(res$tabs, function(x) nrow(x) > LIMIT)
  table_a_gerer <-names(res$to_split[res$to_split == TRUE]) 
  
  all_tot_stock <- list()
  tabs2 <- list()
  list_vars <- list()
  list_alt_hrcs <- list()
  for (t in table_a_gerer) {
    hrc <- res$hrcs[[t]][[var_fus]]
    total <- res$alt_tot[[t]][[var_fus]]
    autre_totaux <-
      res$alt_tot[[t]][names(res$alt_tot[[t]]) != (var_fus)]
    
    res_sdc <-
      sdcHierarchies::hier_import(inp = hrc,
                                  from = "hrc",
                                  root = total) %>%
      sdcHierarchies::hier_convert(as = "sdc")
    
    # Code split nous donne les hiérarchies ainsi que les niveaux de la hiérarchie
    # Permet de selectionn un noeud de l'arbre et ses branches directes
    
    codes_split <- lapply(res_sdc$dims,
                          names)
    n <- length(codes_split)
    ###########################
    # Réduction de hierarchie #
    ###########################
    
    tabs <- lapply(codes_split,
                   function(codes) {
                     res <- res$tabs[[t]] %>%
                       filter(res$tabs[[t]][[var_fus]] %in% codes)
                   })
    
    noms <- lapply(1:n, function(i)
      paste(t, i, sep = "_"))
    
    liste_alt_tot <- setNames(lapply(1:n, function(i) {
      totali <- c(codes_split[[i]][1])
      totali <- setNames(list(totali), var_fus)
      totali <- c(totali, autre_totaux)
      return(totali)
    }),
    noms)
    
    names(tabs) <- noms
    
    tabs2 <- append(tabs2, tabs)
    all_tot_stock <- append(all_tot_stock, liste_alt_tot)
    var <- replicate(n, list(res$vars[[1]]))
    list_add <- replicate(n, list(res$vars[[1]]))
    names(list_add) <- noms
    list_vars <- append(list_vars, list_add)
    
    res$hrcs[[t]][[var_fus]] <- NULL
    
    if (length(res$hrcs[[t]]) != 0) {
      hrc_e <- list(res$hrcs[[t]])
      
      names(hrc_e) <- names(res$hrcs[[t]])
      
      alt_hrcs <- replicate(n, hrc_e)
      names(alt_hrcs) <- noms
      
      list_alt_hrcs <-
        append(list_alt_hrcs, alt_hrcs)
    } else {
      list_alt_hrcs <- NULL
    }
    
  }
  
  table <- names(res$tabs[!(names(res$tabs) %in% table_a_gerer)])
  
  tabs_tot <- append(res$tabs[table], tabs2)
  alt_tot <- append(res$alt_tot[table],all_tot_stock)
  vars <- append(res$vars[table], list_vars)
  
  hrcs <- append(list_alt_hrcs, res$hrcs[table]) # Je prends tous les hrcs présents
  
  if (length(hrcs) == 0) {
    hrcs <- NULL }
  
  res = list(
    tabs = tabs_tot,
    vars = vars,
    sep = res$sep,
    hrcs = hrcs,
    totcode = res$totcode,
    alt_tot = alt_tot,
    hrcfile = res$hrcfile,
    fus_vars = res$fus_vars
  )
  return(res)
}
