library(dplyr)

passage_4_3_cas_1_non_hr <- function(dfs, nom_dfs,v1,v2,totcode,hrcfiles,dir_name) {
  # Rappel :
  # v1 non hier
  # v2 hier
  
  hrc <- hrcfiles[[v2]]
  total <- totcode[[v2]]
  
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  res_sdc$dims
  
  codes_split <- lapply(
    res_sdc$dims,
    names
  )
  length(codes_split)
  
  
  codes <- codes_split[[1]]
  tab_liste <- lapply(
    codes_split,
    function(codes){
      dfs %>% 
        filter({{v2}} %in% codes) %>% 
        mutate(V1V2 = paste(v1, v2, sep = "_")) %>% 
        select(-{{v1}},-{{v2}})
    }
  )
  length(tab_liste)
  
  
  
  
  ################# Brouiloon ###################
  # intuitivement on peut créer n arbre où n = profondeur de la hier
  # en comptant le total
  
  
  # pour i allant de 1 à n-1
  # on split à une profondeur i  par rapport à la var hier 
  # que l'on exprime comme total par rapport à la var non hier
  
  # + un tableau où on split dès le début par rapport à la var non hier
  # puis n-1 fois sur la variable hier
  
  
  
  return(TRUE)
}

