library(dplyr)

res_sdc <- sdcHierarchies::hier_import(inp = "hrc1.hrc", from = "hrc", root = "Total") %>% 
  sdcHierarchies::hier_convert(as = "sdc")
res_sdc$dims

codes_split <- lapply(
  res_sdc$dims,
  names
)
length(codes_split)

data <- expand.grid(
  V1 = c("Total",read.table("hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  V2 = c("Total",read.table("hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  V3 = c("Total",LETTERS[7:12]),
  V4 = c("Total",LETTERS[15:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

str(data)

tab_liste <- lapply(
  codes_split,
  function(codes){
    data %>% 
      filter(V1 %in% codes) %>% 
      mutate(V1V2 = paste(V1, V2, sep = "_")) %>% 
      select(-V1,-V2)
  }
)
length(tab_liste)










