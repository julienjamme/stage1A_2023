library(dplyr)

source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")
source("brouillon/length_tabs.R")


# cas 1 : dimension 4 - 2 non hier ----------------------------------------


data <- expand.grid(
  ACT = c("Total", "A", "B","C"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M","T","G"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
#nrow(data) #81 rows = 3^4

dfs <- data
totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test1",
  v1 = "SEX",
  v2 = "AGE"
)

hrcfiles = NULL
v1 = "SEX"
v2 = "AGE"

l_tab <- length_tabs(dfs=data,
            hrcfiles = NULL,
            v1 = "SEX",
            v2 = "AGE")

tapp_tab <- lapply(res$tabs, nrow)

all(mapply(function(x, y) x == y, l_tab, tapp_tab))


# cas 2 : dimension 4 - 1 non hier ----------------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
#nrow(data) #189 rows = 7*3^3

dfs <- data

hrc_act <- "test/test_cas_gen_4_output/test2/hrc_ACT.hrc"

sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test2",
  v1 = "ACT",
  v2 = "GEO"
)

v1 = "ACT"
v2 = "GEO"
hrcfiles = c(ACT = hrc_act)

l_tab <- length_tabs(dfs=data,
                     hrcfiles = hrcfiles,
                     v1 = v1,
                     v2 = v2)
tapp_tab <- lapply(res$tabs, nrow)

all(mapply(function(x, y) x == y, l_tab, tapp_tab))



# cas 3 : dimension 4 - 2 non hier -----------------------------------------------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2","B3"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GA4","GB3"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
#nrow(data) #441 rows = 7*7*3^2

dfs <- data

hrc_act <- "test/test_cas_gen_4_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3","GA4")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo)
v1 = "ACT"
v2 = "GEO"

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output//test3",
  v1 = v1,
  v2 = v2
)

l_tab <- length_tabs(dfs=data,
                     hrcfiles = hrcfiles,
                     v1 = "ACT",
                     v2 = "GEO")

tapp_tab <- lapply(res$tabs, nrow)

all(mapply(function(x, y) x == y, l_tab, tapp_tab))


