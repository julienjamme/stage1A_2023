#' Calculation of the table sizes generated a priori during the reduction of dimension
#' from 4 or 5 dimensions to 3 dimensions
#' 
#' @param dfs a data.frame
#' 
#' Variable in the 5->4 or 4->3 step
#' @param v1 the first merged variable
#' @param v2 the second merged variable
#' 
#' Variable in the case of 4->3 passage in the 4->3 process
#' do not specify v1_v2 if three variables are merged into one
#' @param v3 the third original variable to be merged
#' @param v4 the fourth original variable to be merged
#' 
#' @param hrcfiles named vector of hrc files related to the variables
#' 
#' @return a list of the lengths of the tables created during the dimension reduction
#' @export
#' 
#' TODO: review the case of a merged trio
#' Verify if the case of 3 variables, with at least one hierarchical variable, is correct
#' It seems correct, but the output is not well "sorted"
#' 
#' @examples
#' library(dplyr)
#' 
#' source("R/length_tabs.R")
#' source("R/nb_tab.R")
#' 
#' # Dimension 4
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1)
#' 
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' 
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' # Function results
#' 
#' res1 <- length_tabs(dfs = data,
#'                     hrcfiles = c(ACT = hrc_act),
#'                     totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total"),
#'                     v1 = "ACT",
#'                     v2 = "GEO")
#'                     
#' # Dimension 5                   
#' data <- expand.grid(
#'   ACT = c("Total_A", paste0("A", seq(1,5),"_"),paste0("A1_", seq(1,7)),paste0("A2_", seq(1,9))),
#'   GEO = c("Total_G", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GB3","GB4"),
#'   SEX = c("Total_S", "F", "M","F1","F2","M1","M2"),
#'   AGE = c("Ensemble", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB","Ménages","Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>% 
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1:n())
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", seq(1,5),"_")) %>% 
#'   sdcHierarchies::hier_add(root = "A1_", nodes = paste0("A1_", seq(1,7))) %>% 
#'   sdcHierarchies::hier_add(root = "A2_", nodes = paste0("A2_", seq(1,9))) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total_G", nodes = c("GA","GB")) %>% 
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3")) %>% 
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
#' 
#' res2 <- length_tabs(dfs = data,
#'                     hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'                     totcode = c(SEX="Total_S",AGE="Ensemble", GEO="Total_G",
#'                                 ACT="Total_A", ECO = "PIB"),
#'                     v1 = "ACT",v2 = "AGE",
#'                     v3 = "GEO",v4 = "SEX")
#' 
#' # Warning : The ouput in case of hierarchical variables
#' # is not in the right order                    
#' res3 <- length_tabs(dfs = data,
#'                     hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'                     totcode = c(SEX="Total_S",AGE="Ensemble", GEO="Total_G",
#'                                 ACT="Total_A", ECO = "PIB"),
#'                     v1 = "ACT",v2 = "AGE",v3 = "GEO")
length_tabs <- function(
  dfs,
  v1,
  v2,
  v3 = NULL,
  v4 = NULL,
  totcode,
  hrcfiles = NULL)
{
  # To generalize the function to handle NA for an external function
  v3 <- if (!is.null(v3) && is.na(v3)) NULL else v3
  v4 <- if (!is.null(v4) && is.na(v4)) NULL else v4
  
  # If 4 variables are specified -> 5 dimensions case, 2 couples are created
  if (!is.null(v4)) {
    return(length_tabs_5_4_var(dfs = dfs,
                               hrcfiles = hrcfiles,
                               v1 = v1, v2 = v2,
                               v3 = v3, v4 = v4,
                               totcode = totcode))
    
    # If 3 variables are specified -> 5 dimensions case, a trio is merged
  } else if (!is.null(v3)) {
    return(length_tabs_5_3_var(dfs = dfs,
                               hrcfiles = hrcfiles,
                               v1 = v1, v2 = v2, v3 = v3,
                               totcode = totcode))
    
    # If 2 variables are specified -> 4 dimensions case
  } else {
    return(length_tabs_4(dfs = dfs,
                         hrcfiles = hrcfiles,
                         v1 = v1, v2 = v2,
                         totcode = totcode))
  }
}

# case : 4 dimensions
length_tabs_4 <- function(dfs,v1,v2,totcode,hrcfiles=NULL){
  
  # Retrieval of groupings {nodes + branch} 
  # based on whether the variable is hierarchical or not
  
  # We need to list and then unlist
  # otherwise the ifelse returns the first element of import_hierarchy (big total)
  # instead of returning all the nodes
  level_v1 <- unlist(ifelse(v1 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v1]])),
                            list(list(unique(dfs[[v1]])))),
                     recursive = FALSE)
  
  level_v2 <- unlist(ifelse(v2 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v2]])),
                            list(list(unique(dfs[[v2]])))),
                     recursive = FALSE)
  
  # If case 1 non hrc (not hierarchical) and v2 in hrcfiles, then we need to reorder
  if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
    tmp <- level_v1
    level_v1 <- level_v2
    level_v2 <- tmp
  }
  
  # We do all possible combinations between v1 and v2
  # which represents the tables created during the creation of v1_v2 in the 5->4 step
  
  # For each of these tables, there are two possible hierarchies
  # one with the totals of v1, and the other with the totals of v2
  # thus, for one of the modalities, we do not make any combination with its total
  # hence the -1
  # and finally, we add the grand total, hence the +1
  nb_rows <- lapply(1:length(level_v1), function(i) {
    lapply(1:length(level_v2), function(j) {
      c((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1,
        length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1)
    })
  })
  
  # Now we need to multiply by the modalities of the non-merged variables
  
  list_non_merged_vars <- names(totcode[!(names(totcode) %in% c(v1, v2))])
  
  mod_non_merged_vars <- lapply(list_non_merged_vars,
                                function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_non_merged_vars))
  
  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)
  
  return(nb_rows_tot)
}

# case : 5 dimensions, two pairs of merged variables
length_tabs_5_4_var <- function(dfs, v1, v2, v3, v4, totcode, hrcfiles = NULL) {
  
  # Retrieve groupings {nodes + branches} based on whether the variable is hierarchical or not, transitioning from 5 dimensions to 4 dimensions.
  
  # List and then unlist the results; ifelse returns all nodes instead of just the first one.
  level_v1 <- unlist(ifelse(v1 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v1]])),
                            list(list(unique(dfs[[v1]])))),
                     recursive = FALSE)
  
  level_v2 <- unlist(ifelse(v2 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v2]])),
                            list(list(unique(dfs[[v2]])))),
                     recursive = FALSE)
  
  # Swap level_v1 and level_v2 in case v2 is not hierarchical but v1 is (to maintain order).
  if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
    tmp <- level_v1
    level_v1 <- level_v2
    level_v2 <- tmp
  }
  
  level_v3 <- unlist(ifelse(v3 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v3]])),
                            list(list(unique(dfs[[v3]])))),
                     recursive = FALSE)
  
  level_v4 <- unlist(ifelse(v4 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v4]])),
                            list(list(unique(dfs[[v4]])))),
                     recursive = FALSE)
  
  # Swap level_v3 and level_v4 in case v4 is not hierarchical but v3 is (to maintain order).
  if (!(v4 %in% names(hrcfiles)) & (v3 %in% names(hrcfiles))) {
    tmp <- level_v3
    level_v3 <- level_v4
    level_v4 <- tmp
    
    tmp <- v3
    v3 <- v4
    v4 <- tmp
  }
  
  # Calculate the length of resulting 4-dimensional datasets for each combination of variables.
  
  nb_rows <- lapply(1:length(level_v1), function(i) {
    lapply(1:length(level_v2), function(j) {
      
      c(
        lapply(1:length(level_v3), function(k) {
          lapply(1:length(level_v4), function(l) {
            
            # A formula to calculate the length of the arrays.
            c( ((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1) * 
                 ((length(level_v3[[k]]) - 1) * length(level_v4[[l]]) + 1),
               
               ((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1) *
                 (length(level_v3[[k]]) * (length(level_v4[[l]]) - 1) + 1)
            )
          })
        }),
        
        lapply(1:length(level_v3), function(k) {
          lapply(1:length(level_v4), function(l) {
            
            c( (length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1) *
                 ((length(level_v3[[k]]) - 1) * length(level_v4[[l]]) + 1),
               
               (length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1) *
                 (length(level_v3[[k]]) * (length(level_v4[[l]]) - 1) + 1)
            )
          })
        })
      )
      
    })
  })
  
  # Calculate the total number of rows by multiplying with the unique modalities of non-merged variables.
  
  list_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1, v2, v3, v4))])
  
  mod_var_non_fusionnées <- lapply(list_var_non_fusionnées, 
                                   function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_var_non_fusionnées))
  
  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)
  
  return(nb_rows_tot)
}

# case : 5 dimensions, three variables merged into one
length_tabs_5_3_var <- function(dfs, v1, v2, v3, totcode, hrcfiles = NULL) {
  
  # Case of at least one hierarchical variable
  if (length(setdiff(names(hrcfiles), c(v1, v2, v3))) != length(hrcfiles)) {
    
    # WARNING
    # This case is a work in progress (WIP)
    # Only the different lengths of modalities are calculated
    # But we do not know specifically the length of table i, for example
    # However, this is not currently critical
    # All modalities appear the correct number of times, but not in the correct order
    
    # Transition from 5 dimensions to 4 dimensions
    
    # List and then unlist the results; ifelse returns all nodes instead of just the first one.
    level_v1 <- unlist(ifelse(v1 %in% names(hrcfiles),
                              list(import_hierarchy(hrcfiles[[v1]])),
                              list(list(unique(dfs[[v1]])))),
                       recursive = FALSE)
    
    level_v2 <- unlist(ifelse(v2 %in% names(hrcfiles),
                              list(import_hierarchy(hrcfiles[[v2]])),
                              list(list(unique(dfs[[v2]])))),
                       recursive = FALSE)
    
    # Swap level_v1 and level_v2 if v2 is not hierarchical but v1 is (to maintain order).
    if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
      tmp <- level_v1
      level_v1 <- level_v2
      level_v2 <- tmp
    }
    
    # Transition from 4 dimensions to 3 dimensions
    
    # List and then unlist the results; ifelse returns all nodes instead of just the first one.
    level_v3 <- unlist(ifelse(v3 %in% names(hrcfiles),
                              list(import_hierarchy(hrcfiles[[v3]])),
                              list(list(unique(dfs[[v3]])))),
                       recursive = FALSE)
    
    
    nb_rows <- lapply(1:length(level_v1), function(i) {
      
      lapply(1:length(level_v3), function(k) {
        
        c( (length(level_v1[[i]]) - 1) * length(level_v3[[k]]) + 1,
           length(level_v1[[i]]) * (length(level_v3[[k]]) - 1) + 1
        )
      })
      
      lapply(1:length(level_v2), function(j) {
        lapply(1:length(level_v3), function(k) {
          
          c(
            rep(c((length(level_v2[[j]]) - 1) * length(level_v3[[k]]) + 1,
                  length(level_v2[[j]]) * (length(level_v3[[k]]) - 1) + 1
            ),
            times = length(level_v1[[i]])
            ),
            
            rep(c((length(level_v1[[i]]) - 1) * length(level_v3[[k]]) + 1,
                  length(level_v1[[i]]) * (length(level_v3[[k]]) - 1) + 1
            ),
            times = length(level_v2[[j]])
            )
          )
        })
      })
    })
    
    # Case of 3 non-hierarchical variables: exact result (the length of table i is known)
  } else {
    
    n_mod_v1 <- length(unique(dfs[[v1]]))
    n_mod_v2 <- length(unique(dfs[[v2]]))
    n_mod_v3 <- length(unique(dfs[[v3]]))
    
    nb_rows <- c(
      1 + (n_mod_v3 - 1) * n_mod_v1,
      1 + n_mod_v3 * (n_mod_v1 - 1),
      
      rep(c(1 + (n_mod_v3 - 1) * n_mod_v2,
            1 + n_mod_v3 * (n_mod_v2 - 1))
          , n_mod_v1),
      
      rep(c(1 + (n_mod_v3 - 1) * n_mod_v1,
            1 + n_mod_v3 * (n_mod_v1 - 1))
          , n_mod_v2 - 1)
    )
  }
  
  # Calculate the total number of rows by multiplying with the unique modalities of non-merged variables.
  
  list_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1, v2, v3))])
  
  mod_var_non_fusionnées <- lapply(list_var_non_fusionnées, 
                                   function(x)  length(unique(dfs[[x]])))
  
  prod_numbers <- prod(unlist(mod_var_non_fusionnées))
  
  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)
  
  return(nb_rows_tot)
}
