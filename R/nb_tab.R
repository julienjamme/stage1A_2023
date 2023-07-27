# Function to manage the import of the hierarchy
import_hierarchy <- function(hrcfile) {
  total <- "BIG_Total"
  res_sdc <- sdcHierarchies::hier_import(inp = hrcfile, from = "hrc", root = total) %>% 
    sdcHierarchies::hier_convert(as = "sdc")
  # Store all sets of parent + direct child
  levels <- lapply(res_sdc$dims, names)
  return(levels)
}

#' Calculate the number of tables generated when merging 3 variables
#' in the transition from 5 to 3 dimensions
#'
#' @param v1 first variable to be merged
#' @param v2 second variable to be merged
#' @param v3 third variable to be merged (variable that will be merged with v1 and v2 if v4 is not specified)
#' @param v4 fourth variable to be merged (with v3)
#' @param hrcfiles named list of hrc files
#' @param data data.frame (used only in the case where a trio is formed)
#'
#' @return an integer representing the number of tables generated
#' @export
#'
#' TODO: Generalize the case of 3 variables into one?
#'
#' @examples
#' library(dplyr)
#'
#' source("R/passage_5_3.R", encoding = "UTF-8")
#' source("R/nb_tab.R")
#'
#' # Dimension 4
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1)
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#'
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1", "A2")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1", "B2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' # 1 pair created
#' calculer_nb_tab(v1 = "ACT", v2 = "GEO",
#'                 hrcfiles = c(ACT = hrc_act))
#'
#' # Dimension 5
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
#'   SEX = c("Total", "F", "M", "F1", "F2", "M1", "M2"),
#'   AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB", "Ménages", "Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1:n())
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1", "A2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("GA", "GB")) %>%
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1", "GA2")) %>%
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1", "GB2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' hrc_sex <- "output/hrc_SEX.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("F", "M")) %>%
#'   sdcHierarchies::hier_add(root = "F", nodes = c("F1", "F2")) %>%
#'   sdcHierarchies::hier_add(root = "M", nodes = c("M1", "M2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_sex, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' # Trio merged
#' calculer_nb_tab(data = data,
#'                 v1 = "ACT", v2 = "GEO", v3 = "SEX",
#'                 hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex))
#'
#' # 2 pairs created
#' calculer_nb_tab(v1 = "ACT", v2 = "GEO",
#'                 v3 = "SEX", v4 = "EXO",
#'                 hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex))
calculer_nb_tab <- function(
  v1,
  v2,
  v3 = NULL,
  v4 = NULL,
  hrcfiles = NULL,
  data = NULL)
{
  # Case dimension 5: 2 couples created
  if (!is.null(v4)) {
    return(4 * nb_noeuds(hrcfiles = hrcfiles, v = v1) * 
             nb_noeuds(hrcfiles = hrcfiles, v = v2) * 
             nb_noeuds(hrcfiles = hrcfiles, v = v3) * 
             nb_noeuds(hrcfiles = hrcfiles, v = v4))
    
    # Case dimension 5: one triplet merged
  } else if (!is.null(v3)) {
    
    # 2 hierarchical variables merged
    if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)) {
      
      # The hierarchy of each variable
      level_v1 <- import_hierarchy(hrcfiles[[v1]])
      level_v2 <- import_hierarchy(hrcfiles[[v2]])
      
      # Store the sum of nodes of v1_v2 for each table
      # We consider all possible combinations between v1 and v2
      # => represents the tables created during the creation of v1_v2 in the 5->4 step
      
      # For each of these tables, there are two possible hierarchies
      # one with the totals of v1, and the other with the totals of v2
      # the number of nodes is equal to their number of modalities
      nb_noeuds_var <- sum(sapply(1:length(level_v1), function(i) {
        sum(sapply(1:length(level_v2), function(j) {
          length(level_v1[[i]]) + length(level_v2[[j]])
        }))
      }))
      
      # 2 non-hierarchical variables merged
    } else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))) {
      # There is only one table in the end
      # which can have two hierarchies
      # totals on v1, or totals on v2
      # the number of nodes is equivalent to the number of modalities
      nb_noeuds_var <- length(unique(data[[v1]])) + length(unique(data[[v2]]))
      
      # 1 hierarchical variable and 1 non-hierarchical variable merged
    } else {
      var_hier <- ifelse(v1 %in% names(hrcfiles), v1, v2)
      mod_var_non_hier <- ifelse(var_hier == v1,
                                 length(unique(data[[v2]])),
                                 length(unique(data[[v1]])))
      
      # Analysis of the hierarchy of var_hier
      level_var_hier <- import_hierarchy(hrcfiles[[var_hier]])
      
      # We consider all possible combinations between v1 and v2
      # => represents the tables created during the creation of v1_v2 in the 5->4 step
      
      # For each of these tables, there are two possible hierarchies
      # one with the totals of v1, and the other with the totals of v2
      # the number of nodes is equal to their number of modalities
      nb_noeuds_var <- sum(sapply(1:length(level_var_hier), function(i) {
        length(level_var_hier[[i]]) + mod_var_non_hier
      }))
    }
    
    # nb_noeuds corresponds to the number of tables that need to be created
    # to make v1_v2 non-hierarchical
    # for each of these tables, v3 needs to be made non-hierarchical
    # and we create as many tables as its hierarchy has nodes
    # finally, for each created table, two hierarchies are possible
    # totals on v1_v2 and totals on v3
    return(2 * nb_noeuds_var * nb_noeuds(hrcfiles, v = v3))
    
    # Case dimension 4
  } else {
    return(2 * nb_noeuds(hrcfiles = hrcfiles, v = v1) * 
             nb_noeuds(hrcfiles = hrcfiles, v = v2))
  }
}
