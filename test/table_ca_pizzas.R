load("data/ca_pizzas_4vars.RData")

str(ca_pizzas_4vars)
str(corr_act)
str(corr_nuts)

hrc_activity <- rtauargus::write_hrc2(
  corr_act, 
  "hrc/activity_2_niv.hrc", 
  adjust_unique_roots = TRUE
)
hrc_nuts <- rtauargus::write_hrc2(
  corr_nuts,
  "hrc/nuts23.hrc", 
  adjust_unique_roots = TRUE
)
