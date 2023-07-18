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
  ACT = c("Total", "A", "B","C","D"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M","T","G","Q","A"),
  AGE = c("Total", "AGE1", "AGE2","AGE3","AGE4", "AGE5", "AGE6","AGE7","AGE8"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
#nrow(data) #81 rows = 3^4

dfs <- data
totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

hrcfiles = NULL
v1 = "SEX"
v2 = "AGE"

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = hrcfiles,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test1",
  v1 = v1,
  v2 = v2
)



l_tab <- length_tabs(dfs=data,
            hrcfiles = NULL,
            v1 = "SEX",
            v2 = "AGE")

tapp_tab <- lapply(res$tabs, nrow)

length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

all(mapply(function(x, y) x == y, l_tab, tapp_tab))


# cas 2 : dimension 4 - 1 non hier ----------------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
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

sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

v1 = "ACT"
v2 = "GEO"

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test2",
  v1 = v1,
  v2 = v2
)


hrcfiles = c(ACT = hrc_act)

l_tab <- length_tabs(dfs=data,
                     hrcfiles = hrcfiles,
                     v1 = v1,
                     v2 = v2)
tapp_tab <- lapply(res$tabs, nrow)

length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

all(mapply(function(x, y) x == y, l_tab, tapp_tab))


# cas 2b : dimension 4 - 1 non hier ----------------------------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
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

sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

v1 = "GEO"
v2 = "ACT"

# Résultat de la fonction
res <- passer_de_4_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = c(ACT = hrc_act),
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_4_output/test2",
  v1 = v1,
  v2 = v2
)


hrcfiles = c(ACT = hrc_act)

l_tab <- length_tabs(dfs=data,
                     hrcfiles = hrcfiles,
                     v1 = v1,
                     v2 = v2)
tapp_tab <- lapply(res$tabs, nrow)

length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

all(mapply(function(x, y) x == y, l_tab, tapp_tab))

# cas 3 : dimension 4 - 2 hier : ne passe pas, à corriger --------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GA4","GB3","GB4","GB5","GB6","GB7","GB8","GB9","GA4","GA5","GA6","GA7","GA8","GA9","GA10","GA11","GB10"),
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
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3","GA4","GA5","GA6","GA7","GA8","GA9","GA10","GA11")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4","GB5","GB6","GB7","GB8","GB9","GB10")) %>% 
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
  hrc_dir = "test/test_cas_gen_4_output/test3",
  v1 = v1,
  v2 = v2
)

l_tab <- length_tabs(dfs=data,
                     hrcfiles = hrcfiles,
                     v1 = v1,
                     v2 = v2)

tapp_tab <- lapply(res$tabs, nrow)

length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

all(mapply(function(x, y) x == y, l_tab, tapp_tab))

df <- data.frame(l_tab = unlist(l_tab), tapp_tab = unlist(tapp_tab))
df

# setdiff(unique(l_tab),unique(tapp_tab))
# 
# setdiff(unique(tapp_tab),unique(l_tab))

unique(res$tabs$tab_Total_GA_ACT$SEX)
unique(res$tabs$tab_Total_GA_ACT$AGE)
unique(res$tabs$tab_Total_GA_ACT$ACT_GEO)

tab_Total_GA_ACT <- expand.grid(
  ACT = c("Total","A","B","C","D","E","F","G"),
  GEO = c("GA", "GA1","GA2","GA3","GA4","GA5","GA6","GA7","GA8","GA9","GA10","GA11"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame() %>% 
  filter(ACT != "Total" | (ACT == "Total" & GEO == "GA"))

nrow(tab_Total_GA_ACT)


# cas 4 : dimension 5 - 2 couples de variables - 4 non hier -----------------

data <- expand.grid(
  ACT = c("Total", "A", "B","C"),
  GEO = c("Total", "G1", "G2","G3","G4"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total","5","10","25","50","75","Dead"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)
nrow(data) #243 rows = 3^5

dfs <- data
totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB")

hrcfiles = NULL
v1 = "ACT"
v2 = "GEO"
v3 = "SEX"
v4 = "ECO"

# Résultat de la fonction
res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test1",
  v1=v1,v2=v2,v3=v3,v4=v4
)

tapp_tab <- lapply(res$tabs, nrow)

l_tab <- length_tabs_5_4_var(dfs = data,
                              hrcfiles = hrcfiles,v1 = v1,v2 = v2,v3 = v3,v4 = v4)

all(mapply(function(x, y) x == y, l_tab, tapp_tab))


# cas 5 : dimension 5 - 2 couples de variables - 2 non hier/ 2 hier -----------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2","B3","B4","A3"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GB3","GB4"),
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
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_5_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB")

#hrcfiles = c(ACT = hrc_act, GEO = hrc_geo)
hrcfiles = c(ACT = hrc_act) # marche
# hrcfiles = c(GEO = hrc_geo) # marche pas

v1 = "ACT"
v2 = "SEX"
v3 = "AGE"
v4 = "GEO"

# Résultat de la fonction
res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = hrcfiles,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test3",
  v1 = v1,
  v2 = v2,
  v3 = v3,
  v4 = v4
)

length(res$tabs)
tapp_tab <- lapply(res$tabs, nrow)

l_tab <- length_tabs_5_4_var(dfs = data,
                             hrcfiles = hrcfiles,v1 = v1,v2 = v2,v3 = v3,v4 = v4)

all(mapply(function(x, y) x == y, l_tab, tapp_tab))

length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

df <- data.frame(l_tab = unlist(l_tab), tapp_tab = unlist(tapp_tab))
df
