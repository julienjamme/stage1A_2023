library(dplyr)

source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("R/length_tabs.R")
source("R/nb_tab.R")
source("R/chercher_combinaison_variable_a_fusionner.R")


# -------------------------------------------------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) #441 rows = 7*7*3^2

hrc_act <- "test/test_cas_gen_4_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

hrcfiles <- c(ACT = hrc_act, GEO = hrc_geo)

choisir_var_a_fusionner_general(dfs=data,
                        totcode,
                        hrcfiles,
                        LIMIT=1,
                        nb_var = 2,
                        nb_tab = 'smart')

# Cohérent : on choisit deux var hier
choisir_var_a_fusionner_general(dfs=data,
                                totcode,
                                hrcfiles,
                                nb_var = 2,
                                nb_tab = 'max')

# Cohérent : on choisit deux var non hier
choisir_var_a_fusionner_general(dfs=data,
                                totcode,
                                hrcfiles,
                                nb_var = 2,
                                nb_tab = 'min')


length_tabs(dfs = data,v1 = "ACT",v2="GEO", totcode=totcode)

# -------------------------------------------------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
  SEX = c("Total", "F", "M","F1","F2","M1","M2"),
  AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) # 7203 rows = 7**4*3

hrc_act <- "test/test_cas_gen_5_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_5_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_age <- "test/test_cas_gen_5_output/test3/hrc_AGE.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("AGE1","AGE2")) %>% 
  sdcHierarchies::hier_add(root = "AGE1", nodes = c("AGE11","AGE12")) %>% 
  sdcHierarchies::hier_add(root = "AGE2", nodes = c("AGE21","AGE22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_age, row.names = F, col.names = F, quote = F)

hrc_sex <- "test/test_cas_gen_5_output/test3/hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("F","M")) %>% 
  sdcHierarchies::hier_add(root = "F", nodes = c("F1","F2")) %>% 
  sdcHierarchies::hier_add(root = "M", nodes = c("M1","M2")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB")

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age, SEX = hrc_sex)

choisir_var_a_fusionner_general(dfs = data,
                                totcode = totcode,
                                hrcfiles = hrcfiles,
                                LIMIT = 15000,
                                nb_var = 4)
