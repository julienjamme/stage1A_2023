





#' tau_argus_4_3
#'
#' @param list_res liste de 3 éléments : 
#'      - une liste de tableaux de 3 variables catégorielles
#'      - une liste 
#' @param liste_secret liste_res après avoir appliqué le secret primaire 
#' @param totcode liste contenant les totaux 
#' @param freq  nom de la colonne contenant les fréquences
#' @param value nom de la colonne contenant les valeurs
#'
#' @return une liste avec de tableaux avec le secret posé
#' 
#'
tau_argus_4_3<-function(list_res,liste_secret,totcode,freq,value="FALSE",hrcfiles){
  
    
  
    #Initilaisation
  
    list_tab<-list_res$tabs
    n<-length(list_tab)
    
    
    totcode2 <- totcode
    totcode2[[new_var]] <- paste(totcode[["ACT"]],totcode[["ACT"]], sep="_")
    totcode2 <- totcode2[!(names(totcode2) %in% c("ACT", "AGE"))]
    #On récupère les varibales des différentes tables

    var_cross<-paste(list_res$vars[1],list_res$vars[2],sep="_")
    d<- intersect(names(list_res$tabs$T1), names(totcode))
    
    list_vars<-replicate(n,d,simplify=FALSE)
    names(list_vars)<- c(paste0("T",1:n,sep=""))
    
    # On pose le secret avec tauargus
    #Je vérifie si le tableaux est un tableaux de fréquence
    
   
    #faire un test de value existe et tableaux de fréquences
    masq <- tab_multi_manager(
    list_tables = liste_secret,
    list_explanatory_vars = list_vars  ,
    dir_name = "test_avec_rtauargus",
    totcode = totcode2,
    hrc = hrcfiles[!(names(hrcfiles) %in% c("ACT", "AGE"))], 
    alt_hrc = list_res$hrcs,
    alt_totcode = list_res$alt_tot,
    value = value,
    freq = freq,
    secret_var = "is_secret_prim")
    
   
     
  return(masq)
}