

tau_argus_4_3<-function(list_res,totcode,freq,value){
  
  
  
  freqs<-as.character(freq)
  values<-as.character(value)
  list_tab<-list_res$tabs
  n<-length(list_tab)
  #Pour le moment après voir avec les modalités
  res$vars
  var_cross<-paste(res$vars[1],res$vars[2],sep="_")

   d<- intersect(names(list_res$tabs$T1), names(totcode))

 
  v<-c(d,var_cross)
    
    # Secret primaire
    
    liste_secret <- lapply(list_tab, function(tab) {
      tab$is_secret_freq <- tab$nb_obs > 0 & tab$nb_obs < 3
      tab$is_secret_dom <- (tab$pizzas_tot != 0) & (tab$pizzas_max > 0.85 * tab$pizzas_tot)
      tab$is_secret_prim <- tab$is_secret_freq | tab$is_secret_dom

      return(tab)
    })
    
    list_vars<-replicate(n,v,simplify=FALSE)
    
    names(list_vars)<- c(paste0("T",1:n,sep=""))
    
    masq <- tab_multi_manager(
    list_tables = liste_secret,
    list_explanatory_vars = list_vars  ,
    dir_name = "test_avec_rtauargus.R",
    totcode = totcode,
    hrc = c(ACTIVITY=hrc_activity,NUTS23=hrc_nuts),
    alt_hrc = list_res$hrcs,
    alt_totcode = list_res$alt_tot,
    value = values,
    freq = freqs,
    secret_var = "is_secret_prim",
    
  )
  
  return(masq)
}