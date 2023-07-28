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
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) #441 rows = 7*7*3^2

hrc_act <- "output/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "output/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
nom_dfs <- "nom"
hrcfiles <- c(ACT = hrc_act, GEO = hrc_geo)
hrc_dir <- "output"
dfs <- data
sep_dir <- TRUE

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
                   LIMIT = 150)

max(sapply(res_f$tabs, nrow))
