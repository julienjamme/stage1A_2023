library(dplyr)

source("fonction_split_tables.R", encoding = "UTF-8")

data <- expand.grid(
  ACT = c("Total",read.table("hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[7:12]),
  AGE = c("Total",LETTERS[15:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()
hrc_files = c(ACT = "hrc1.hrc", GEO = "hrc2.hrc")


res <- passer_de_4_a_3_var(data, hrc_files)