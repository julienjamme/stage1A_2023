library(dplyr)

data <- expand.grid(
  ACT = c("Total",read.table("hrc1.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  GEO = c("Total",read.table("hrc2.hrc") %>% mutate(V1 = gsub("@?","",V1, perl = TRUE)) %>% pull(V1)),
  SEX = c("Total","F","M"),
  AGE = c("Total",paste0("A",1:5)),
  stringsAsFactors = FALSE
) %>% 
  as.data.frame()


age_mods <- unique(data$AGE)
sex_mods <- unique(data$SEX)

unique(data[, c("SEX","AGE")])

# Sous-totaux par SEX
tab1 <- data %>% 
  filter( (SEX != "Total") | (SEX == "Total" & AGE == "Total"))

unique(tab1[,c("SEX","AGE")]) %>% 
  mutate(SEX_AGE = paste(SEX,AGE, sep="_"))

sex_total <- "Total"
age_total <- "Total"

sex_mods_hors_tot <- sex_mods[sex_mods != sex_total]
age_mods_hors_tot <- age_mods[age_mods != age_total]

sex_mods_n <- length(sex_mods_hors_tot)
age_mods_n <- length(age_mods_hors_tot)

n_feuilles_croisees <- (age_mods_n)*(sex_mods_n)

Niv0 => Niv1 => Niv2
TOT_TOT => F_TOT => F_A1
TOT_TOT => F_TOT => F_A2
TOT_TOT => F_TOT => F_A3
TOT_TOT => M_TOT => M_A1
TOT_TOT => M_TOT => M_A2
TOT_TOT => M_TOT => M_A3

tab1_niv0 <- rep(
  paste(sex_total, age_total, sep = "_"), 
  n_feuilles_croisees
)
tab1_niv1 <- expand.grid(
  v1 = sort(rep(sex_mods_hors_tot, age_mods_n)),
  v2 = age_total,
  stringsAsFactors = FALSE
) %>% as.data.frame() %>% 
  mutate(v3 = paste(v1, v2, sep = "_")) %>% 
  pull(v3)
  
tab1_niv2 <- expand.grid(
  v1 = sex_mods_hors_tot,
  v2 = age_mods_hors_tot,
  stringsAsFactors = FALSE
) %>% 
  as.data.frame() %>%
  arrange(v1,v2) %>% 
  mutate(v3 = paste(v1, v2, sep = "_")) %>% 
  pull(v3)
  
tab1_corresp <- data.frame(
  # Niv0 = tab1_niv0,
  Niv1 = tab1_niv1,
  Niv2 = tab1_niv2,
  stringsAsFactors = FALSE
)

rtauargus::write_hrc2(tab1_corresp, "tab1.hrc")



# Sous-totaux par AGE
tab2 <- data %>% 
  filter( (AGE != "Total") | (SEX == "Total" & AGE == "Total"))
