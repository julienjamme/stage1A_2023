


test_nb_tabs_4hrc<-function(hrcfiles,v2,v1,totcode){




hrc <- hrcfiles[[v2]]
total <- totcode[[v2]]

res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
  sdcHierarchies::hier_convert(as = "sdc")
#res_sdc$dims

#Code split nous donne les hiérarchies ainsi que les niveaux de la hiérarchie 
codes_split <- lapply(
  res_sdc$dims,
  names
)

hrc1 <- hrcfiles[[v1]]
total1 <- totcode[[v1]]

#On crée le tableau donnant les niveaux de la hiérarchie

res_sdc <- sdcHierarchies::hier_import(inp = hrc1, from = "hrc", root = total1) %>% 
  sdcHierarchies::hier_convert(as = "sdc")

codes_split_1 <- lapply(
  res_sdc$dims,
  names
)

return(2*(length(codes_split)+length(codes_split_1)))}

test_nb_tabs_3hrc<-function(hrcfiles,v2,totcode){
  
  
  
  
  hrc <- hrcfiles[[v2]]
total <- totcode[[v2]]

res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>% 
  sdcHierarchies::hier_convert(as = "sdc")
#res_sdc$dims
codes_split <- lapply(
  res_sdc$dims,
  names
)


return(2*(length(codes_split)))}
