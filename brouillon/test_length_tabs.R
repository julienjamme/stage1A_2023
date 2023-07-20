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

# On fait attention à engendrer des tables ayant toutes
# un nombre de ligne différent afin d'éviter les faux positifs
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

# On fait attention à engendrer des tables ayant toutes
# un nombre de ligne différent afin d'éviter les faux positifs
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

# On fait attention à engendrer des tables ayant toutes
# un nombre de ligne différent afin d'éviter les faux positifs
length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

all(mapply(function(x, y) x == y, l_tab, tapp_tab))

# cas 3 : dimension 4 - 2 hier --------------------

data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5","I","J","K","L","M","N"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GA4","GB3","GB4","GB5","GB6","GB7","GB8","GB9","GA5","GA6","GA7","GA8","GA9","GA10","GA11","GC","GD","GE","GF","GH","GA12","GA13","GA14","GA15"),
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
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G","I","J","K","L","M","N")) %>% 
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_4_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB","GC","GD","GE","GF","GH")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3","GA4","GA5","GA6","GA7","GA8","GA9","GA10","GA11","GA12","GA13","GA14","GA15")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4","GB5","GB6","GB7","GB8","GB9")) %>% 
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

# On fait attention à engendrer des tables ayant toutes
# un nombre de ligne différent afin d'éviter les faux positifs
length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))

all(mapply(function(x, y) x == y, l_tab, tapp_tab))

# cas 4 : dimension 5 - 2 couples de variables - 4 non hier -----------------

data <- expand.grid(
  ACT = c("Total", paste0("A", seq(1,5))),
  GEO = c("Total", paste0("G", seq(1,15))),
  SEX = c("Total", "F", "M","A"),
  AGE = c("Total",paste0("E", seq(1,25))),
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

# On fait attention à engendrer des tables ayant toutes
# un nombre de ligne différent afin d'éviter les faux positifs
length(l_tab)
length(unique(l_tab))
length(tapp_tab)
length(unique(tapp_tab))


# cas 5 : dimension 5 - 2 couples de variables - 2 non hier/ 2 hier -----------

data <- expand.grid(
  ACT = c("Total_A", paste0("A", seq(1,5),"_"),paste0("A1_", seq(1,7)),paste0("A2_", seq(1,9))),
  GEO = c("Total_G", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GB3","GB4"),
  SEX = c("Total_S", "F", "M","F1","F2","M1","M2"),
  AGE = c("Ensemble", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) # 7203 rows = 7**4*3

hrc_act <- "test/test_cas_gen_5_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", seq(1,5),"_")) %>% 
  sdcHierarchies::hier_add(root = "A1_", nodes = paste0("A1_", seq(1,7))) %>% 
  sdcHierarchies::hier_add(root = "A2_", nodes = paste0("A2_", seq(1,9))) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_5_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total_G", nodes = c("GA","GB")) %>% 
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3")) %>% 
  sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total_S",AGE="Ensemble", GEO="Total_G", ACT="Total_A", ECO = "PIB")

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo)

v1 = "ACT"
v2 = "AGE"

v3 = "GEO"
v4 = "SEX"

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
l_reel <- lapply(res$tabs, nrow)

l_predict <- length_tabs_5_4_var(dfs = data,
                                 hrcfiles = hrcfiles,v1 = v1,v2 = v2,v3 = v3,v4 = v4)

all(mapply(function(x, y) x == y, l_reel, l_predict))

# On fait attention à engendrer des tables ayant toutes
# un nombre de ligne différent afin d'éviter les faux positifs
length(l_reel)
length(unique(l_reel))
length(l_predict)
length(unique(l_predict))

# cas 6 : dimension 5 - 2 couples de variables - 4 hier -----------

data <- expand.grid(
  ACT = c("Total_A", paste0("A", seq(1,5),"_"),paste0("A1_", seq(1,7))),
  GEO = c("Total_G", paste0("G", seq(1,9),"_"),paste0("G1_", seq(1,11))),
  SEX = c("Total_S", paste0("S", seq(1,17),"_"),paste0("S1_", seq(1,19))),
  AGE = c("Total_O", paste0("O", seq(1,13),"_"),paste0("O1_", seq(1,15))),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())
nrow(data) # 7203 rows = 7**4*3

hrc_act <- "test/test_cas_gen_5_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", seq(1,5),"_")) %>% 
  sdcHierarchies::hier_add(root = "A1_", nodes = paste0("A1_", seq(1,7))) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_5_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total_G", nodes = paste0("G", seq(1,9),"_")) %>% 
  sdcHierarchies::hier_add(root = "G1_", nodes = paste0("G1_", seq(1,11))) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_age <- "test/test_cas_gen_5_output/test3/hrc_AGE.hrc"
sdcHierarchies::hier_create(root = "Total_O", nodes = paste0("O", seq(1,13),"_")) %>% 
  sdcHierarchies::hier_add(root = "O1_", nodes = paste0("O1_", seq(1,15))) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_age, row.names = F, col.names = F, quote = F)

hrc_sex <- "test/test_cas_gen_5_output/test3/hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total_S", nodes = paste0("S", seq(1,17),"_")) %>% 
  sdcHierarchies::hier_add(root = "S1_", nodes = paste0("S1_", seq(1,19))) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)

totcode <- c(SEX="Total_S",AGE="Total_O", GEO="Total_G", ACT="Total_A", ECO = "PIB")

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age, SEX = hrc_sex)

dfs = data
nom_dfs = "tab"
sep_dir = TRUE
hrc_dir = "test/test_cas_gen_5_output/test3"
v1 = "ACT"
v2 = "GEO"
v3 = "SEX"
v4 = "AGE"

# Résultat de la fonction

res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = hrcfiles,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test3",
  v1 = "ACT",
  v2 = "GEO",
  v3 = "SEX",
  v4 = "AGE"
)

length(res$tabs)
l_reel <- lapply(res$tabs, nrow)

l_predict <- length_tabs_5_4_var(dfs = data,
                                 hrcfiles = hrcfiles,v1 = v1,v2 = v2,v3 = v3,v4 = v4)

all(mapply(function(x, y) x == y, l_reel, l_predict))

length(l_reel)
length(unique(l_reel))
length(l_predict)
length(unique(l_predict))


# cas 7 : dimension 5 - 3 var non hier fusionnées ------------------

data <- expand.grid(
  ACT = c("Total_A","A1","A2","A3","A11","A12","A21","A22"),
  GEO = c("Total_G","G1","G2","G3","G11","G12","G21","G22"),
  SEX = c("Total_S","S1","S2","S3","S11","S12","S21","S22"),
  AGE = c("Total_0"),
  ECO = c("PIB"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)

dfs <- data

hrc_act <- "test/test_cas_gen_5_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total_A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "A1", nodes = c("A11","A12")) %>% 
  sdcHierarchies::hier_add(root = "A2", nodes = c("A21","A22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_5_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total_G", nodes = c("G1","G2","G3")) %>% 
  sdcHierarchies::hier_add(root = "G1", nodes = c("G11","G12")) %>% 
  sdcHierarchies::hier_add(root = "G2", nodes = c("G21","G22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_sex <- "test/test_cas_gen_5_output/test3/hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total_S", nodes = c("S1","S2","S3")) %>% 
  sdcHierarchies::hier_add(root = "S1", nodes = c("S11","S12")) %>% 
  sdcHierarchies::hier_add(root = "S2", nodes = c("S21","S22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age, SEX = hrc_sex)

totcode <- c(SEX="Total_S",AGE="Total_0", GEO="Total_G", ACT="Total_A", ECO = "PIB")

# Résultat de la fonction
res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test2",
  v1 = "ACT",
  v2 = "GEO",
  v3 = "SEX",
  v4 = "ACT_GEO"
)

length(res$tabs)
l_reel <- lapply(res$tabs, nrow)

l_predict <- length_tabs_5_3_var(dfs = data,
                                 v1 = v1,v2 = v2,v3 = v3)

all(mapply(function(x, y) x == y, l_reel, l_predict))

length(l_reel)
length(unique(l_reel))
length(l_predict)
length(unique(l_predict))


# cas 8 : dimension 5 - 3 var non hier fusionnées autre exemple ------------------

data <- expand.grid(
  ACT = c("Total_A","A1","A2","A3","A11","A12","A13"),
  GEO = c("Total_G","G1","G2","G3","G11","G12","G21","G22","G31","G32"),
  SEX = c("Total_S","S1","S2","S3","S11","S12","S21","S22"),
  AGE = c("Total_0"),
  ECO = c("PIB"),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()

data <- data %>% mutate(VALUE = 1)

dfs <- data

hrc_act <- "test/test_cas_gen_5_output/test3/hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total_A", nodes = c("A1","A2","A3")) %>% 
  sdcHierarchies::hier_add(root = "A1", nodes = c("A11","A12","A13")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_act, row.names = F, col.names = F, quote = F)

hrc_geo <- "test/test_cas_gen_5_output/test3/hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total_G", nodes = c("G1","G2","G3")) %>% 
  sdcHierarchies::hier_add(root = "G1", nodes = c("G11","G12")) %>% 
  sdcHierarchies::hier_add(root = "G2", nodes = c("G21","G22")) %>% 
  sdcHierarchies::hier_add(root = "G3", nodes = c("G31","G32")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)

hrc_sex <- "test/test_cas_gen_5_output/test3/hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total_S", nodes = c("S1","S2","S3")) %>% 
  sdcHierarchies::hier_add(root = "S1", nodes = c("S11","S12")) %>% 
  sdcHierarchies::hier_add(root = "S2", nodes = c("S21","S22")) %>% 
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>% 
  mutate(levels = substring(paste0(level,name),3)) %>% 
  select(levels) %>% 
  write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)

hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age, SEX = hrc_sex)

totcode <- c(SEX="Total_S",AGE="Total_0", GEO="Total_G", ACT="Total_A", ECO = "PIB")

# Résultat de la fonction
res <- passer_de_5_a_3_var(
  dfs = data,
  nom_dfs = "tab",
  totcode = totcode, 
  hrcfiles = NULL,
  sep_dir = TRUE,
  hrc_dir = "test/test_cas_gen_5_output/test2",
  v1 = "ACT",
  v2 = "GEO",
  v3 = "ACT_GEO",
  v4 = "SEX"
)

length(res$tabs)
l_reel <- lapply(res$tabs, nrow)

l_predict <- length_tabs_5_3_var(dfs = data,
                                 v1 = v1,v2 = v2,v3 = v3)

all(mapply(function(x, y) x == y, l_reel, l_predict))

length(l_reel)
length(unique(l_reel))
length(l_predict)
length(unique(l_predict))

