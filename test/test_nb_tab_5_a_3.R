source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
source("R/cas_gen_4_3.R",encoding = "UTF-8")
source("R/passage_5_3.R",encoding = "UTF-8")
source("R/format.R",encoding = "UTF-8")
source("test/test_nbs_tabs.R",encoding = "UTF-8")
source("test/nb_tab_5_a_3.R",encoding = "UTF-8")
source("R/nb_tab.R")

data <- expand.grid(
ACT = c(
  "Total",
  read.table("hrc/hrc2.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
),
SEX = c(
  "Total",
  read.table("hrc/hrc3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
),
GEO = c(
  "Pays",
  read.table("hrc/hrc_REG_deep_3.hrc") %>% mutate(V1 = gsub("@?", "", V1, perl = TRUE)) %>% pull(V1)
),
AGE = c("Ensemble", "adulte", "enfant"),
ECO = c("Ensemble", "riche", "pauvre", "autre"),
stringsAsFactors = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = runif(nrow(data)))
hrc_files = c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc", SEX = "hrc/hrc3.hrc")

tot_code <-
  c(
    SEX = "Total",
    AGE = "Ensemble",
    GEO = "Pays",
    ACT = "Total",
    ECO = "Ensemble"
  )

# pour execution ligne à ligne
dfs <- data
nom_dfs <- "nom_data_frame"


totcode <- tot_code
hrcfiles <- hrc_files

dir_name <- "output"
hrc_dir <- dir_name
sep_dir <- TRUE
sep = "_"

# Cas fusion deux variable hier
v1="ACT"
v2="GEO"
v3="ECO"
v4="ACT_GEO"
hrcfiles=hrcfiles
n_mod_v1 = length(unique(data[[v1]]))
n_mod_v2 = length(unique(data[[v2]]))

res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name,
                           v1 = v1, v2 = v2,v3 = v3,v4 = v4)

length(res$tabs) == calculer_nb_tab(v1=v1,
                                    v2=v2,
                                    v3=v3,
                                    hrcfiles=hrcfiles)


# Cas fusion deux var non hier
v1="AGE"
v2="ECO"

# On fait comme si SEX n'était pas hier
v3="SEX"
hrcfiles=c(ACT = "hrc/hrc2.hrc", GEO = "hrc/hrc_REG_deep_3.hrc")
v4="AGE_ECO"
n_mod_v1 = length(unique(data[[v1]]))
n_mod_v2 = length(unique(data[[v2]]))

res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name,
                           v1 = v1, v2 = v2,v3 = v3,v4 = v4)

length(res$tabs) == calculer_nb_tab(v1=v1,
                                    v2=v2,
                                    v3=v3,
                                    n_mod_v1=n_mod_v1,
                                    n_mod_v2=n_mod_v2)

# Cas fusion une var hier et une var non hier

v1="ACT"
v2="ECO"
v3="GEO"
v4="ECO_ACT"
hrcfiles=hrcfiles
n_mod_v1 = length(unique(data[[v1]]))
n_mod_v2 = length(unique(data[[v2]]))

res <- passer_de_5_a_3_var(dfs,nom_dfs,totcode, hrcfiles, sep_dir = TRUE, hrc_dir = dir_name,
                           v1 = v1, v2 = v2,v3 = v3,v4 = v4)

length(res$tabs) == calculer_nb_tab(v1=v1,
                                    v2=v2,
                                    v3=v3,
                                    hrcfiles=hrcfiles,
                                    n_mod_v1=n_mod_v1,
                                    n_mod_v2=n_mod_v2)