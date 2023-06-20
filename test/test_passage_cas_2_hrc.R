library(dplyr)
source("passage_4_3_cas_2_hrc.R",encoding = "UTF-8")



data <- expand.grid(
  ACT = c("Total",read.table("hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[7:12]),
  AGE = c("Total",LETTERS[15:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()
data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc1.hrc", GEO = "hrc2.hrc")
names(hrc_files[1])

tot_code<-c(SEX="Total",AGE="Total")

res <- passage_4_3_cas_2hr(data,tot_code, hrc_files)

plus_petit_hrc(hrc_files)

