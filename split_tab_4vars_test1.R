
# Cas 1 -------------------------------------------------------------------

tab <- expand.grid(
  cj = paste0("cj", c(1:6,"T")),
  tr = paste0("tr", c(1:5,"T")),
  stringsAsFactors = FALSE
)

tab <- as.data.frame(tab)
str(tab)

tab$cj_tr <- paste(tab$cj, tab$tr, sep = "_")

split(tab, tab$tr)

# cj1_tr1 => cj1_trT et cjT_tr1



# Cas 2 -------------------------------------------------------------------
corr_tab <- data.frame(
  cjB = c(11:14,21:23,31:32,41,51,61)
)
corr_tab$cjA <- round(corr_tab$cjB/10)

tab2 <- expand.grid(
  cj = paste0("cj", c(unique(corr_tab$cjA),corr_tab$cjB,"T")),
  tr = paste0("tr", c(1:5,"T")),
  stringsAsFactors = FALSE
)

tab2 <- as.data.frame(tab2)
str(tab2)

tab2$cj_tr <- paste(tab2$cj, tab2$tr, sep = "_")
split(tab2, tab2$tr)
