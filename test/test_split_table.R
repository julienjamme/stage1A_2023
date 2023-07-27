source("R/reduce_dims.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/choisir_sep.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/length_tabs.R",encoding = "UTF-8")
source("R/chercher_combinaison_variable_a_fusionner.R",encoding = "UTF-8")
source("R/split_table.R",encoding = "UTF-8")

data <- expand.grid(
  ACT = c("Total",read.table("hrc/hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total",LETTERS[1:5]),
  AGE = c("Total",LETTERS[21:25]),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
#hrcfiles = c(ACT = "hrc/hrc1.hrc", GEO = "hrc/hrc2.hrc")
hrcfiles = c(GEO = "hrc/hrc2.hrc")
totcode<-c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")


# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"
sep_dir = TRUE
hrc_dir = "test/test_fonction_general/test2"

res1 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                           sep_dir = sep_dir,
                           hrc_dir = hrc_dir,
                           nb_tab = 'min')

max(sapply(res1$tabs, nrow))

res2 <-gen_tabs_5_4_to_3(dfs,nom_dfs,totcode ,hrcfiles,
                           sep_dir = sep_dir,
                           hrc_dir = hrc_dir,
                           nb_tab = 'max')

max(sapply(res2$tabs, nrow))

res_f <- split_tab(res = res1,
                   var_fus = paste(res1$fus_vars[1],
                                    res1$fus_vars[2],
                                    sep = res1$sep),
                   LIMIT = 1500)

max(sapply(res_f$tabs, nrow))
