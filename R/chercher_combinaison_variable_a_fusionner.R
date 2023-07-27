#' General function to choose variables to merge,
#' limiting the number of generated tables while ensuring not to generate
#' tables that are too large.
#' 
#' @param dfs data.frame
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc files for categorical variables
#' @param nb_var number of variables to merge
#' @param nb_tab strategy to follow for choosing variables automatically:
#'   - 'min': minimize the number of tables;
#'   - 'max': maximize the number of tables;
#'   - 'smart': minimize the number of tables under the constraint of their row count.
#' @param LIMIT maximum allowed row count in the 'smart' case
#' 
#' @return A list of vectors representing the chosen variables to merge
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' source("R/chercher_combinaison_variable_a_fusionner.R")
#' source("R/length_tabs.R")
#' source("R/nb_tab.R")
#' 
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "GA", "GB", "GA1", "GA2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>% 
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1:n())
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>% 
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>% 
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
#' 
#' totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
#' 
#' hrcfiles <- c(ACT = hrc_act, GEO = hrc_geo)
#' 
#' # Consistent: choose two hierarchical variables
#' res1 <- choisir_var_a_fusionner_general(dfs = data,
#'                                         totcode = totcode,
#'                                         hrcfiles = hrcfiles,
#'                                         nb_var = 2,
#'                                         nb_tab = 'max')
#' res1
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res1[1], v2 = res1[2])))
#' 
#' # Consistent: choose two non-hierarchical variables
#' res2 <- choisir_var_a_fusionner_general(dfs = data,
#'                                 totcode = totcode,
#'                                 hrcfiles = hrcfiles,
#'                                 nb_var = 2,
#'                                 nb_tab = 'min')
#' res2
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res2[1], v2 = res2[2])))
#' 
#' res3 <- choisir_var_a_fusionner_general(dfs = data,
#'                                 totcode = totcode,
#'                                 hrcfiles = hrcfiles,
#'                                 LIMIT = 200,
#'                                 nb_var = 2,
#'                                 nb_tab = 'smart')
#' res3
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res3[1], v2 = res3[2])))
#' 
#' # Obtains 147, which is well below 200
#' 
#' res4 <- choisir_var_a_fusionner_general(dfs = data,
#'                                 totcode = totcode,
#'                                 hrcfiles = hrcfiles,
#'                                 LIMIT = 5,
#'                                 nb_var = 2,
#'                                 nb_tab = 'smart')
#' res4
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res4[1], v2 = res4[2])))
#' 
#' # Receives a warning: unable to reach the announced value
#' # There are 63 rows (equivalent to the max 
#' # -> this is what reduces the table size)
#' # And the warning announces 63 rows, which is consistent with the output
choisir_var_a_fusionner_general <- function(
    dfs,
    totcode,
    hrcfiles = NULL,
    nb_var = 4,
    nb_tab = "min",
    LIMIT = 150)
{
  # Case of 2 pairs in dimension 5
  if (nb_var == 4){
    result_comb <- generer_deux_paires(totcode)
    
    # Case of a triplet in dimension 5
  } else if (nb_var == 3){
    result_comb <- generer_triplet(totcode)
    
    # Case of dimension 4
  } else {
    result_comb <- generer_une_paire(totcode)
  }
  
  return(choisir_var_a_fusionner(dfs = dfs,
                                 result_comb = result_comb,
                                 totcode = totcode,
                                 hrcfiles = hrcfiles,
                                 LIMIT = LIMIT,
                                 nb_tab = nb_tab))
}

choisir_var_a_fusionner <- function(
    dfs,
    result_comb,
    totcode,
    hrcfiles = NULL,
    LIMIT = 150,
    nb_tab = "smart")
{
  # Calculate the number of tables and maximum rows for each combination of variables
  res_func <- lapply(result_comb, function(x) length_tabs(
    dfs = data,
    v1 = x[1],
    v2 = x[2],
    v3 = x[3],
    v4 = x[4],
    totcode = totcode,
    hrcfiles = hrcfiles))
  
  # Get the maximum rows and number of created tables
  res_max <- sapply(res_func, function(x) max(unlist(x)))
  res_len <- sapply(res_func, function(x) length(unlist(x)))
  
  # Create a dataframe for better filtering
  df <- data.frame(res_max = res_max, res_len = res_len)
  
  # Save the row number by adding a column
  df$original_index <- seq(nrow(df))
  
  # Case: minimize the number of tables
  if (nb_tab == "min"){
    min_nb_tab <-  min(df$res_len)
    filtered_df <- df[df$res_len == min_nb_tab, ]
    
    # Get the index of the filtered table
    min_index <- which.min(filtered_df$res_max)
    # Print the original index
    i <- filtered_df$original_index[min_index]
    
    return(result_comb[[i]])
    
    # Case: maximize the number of tables
  } else if (nb_tab == "max"){
    max_nb_tab <-  max(df$res_len)
    filtered_df <- df[df$res_len == max_nb_tab, ]
    
    # Get the index of the filtered table
    min_index <- which.min(filtered_df$res_max)
    # Print the original index
    i <- filtered_df$original_index[min_index]
    
    return(result_comb[[i]])
    
    # Case: 'smart' - maximize under the constraint of the size limit
  } else {
    # Filter based on the maximum rows condition
    filtered_df <- df[df$res_max < LIMIT, ]
    
    # If at least one case satisfies this condition
    if (nrow(filtered_df) > 0){
      # Get the index of the filtered table
      min_index <- which.min(filtered_df$res_len)
      
      # Print the original index
      i <- filtered_df$original_index[min_index]
      
      return(result_comb[[i]])
      
    } else {
      # Return the result with the fewest tables among those
      # with the shortest tables
      min_res_max <- min(df$res_max)
      warning(c("
      The limit of ",LIMIT," cannot be achieved.
      The largest table has ",min_res_max," rows."))
      
      filtered_df <- df[df$res_max == min_res_max, ]
      
      # Get the index of the filtered table
      min_index <- which.min(filtered_df$res_len)
      
      # Print the original index
      i <- filtered_df$original_index[min_index]
      return(result_comb[[i]])
    }
  }
}

generer_une_paire <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  var_cat <- names(totcode)
  
  # Use combn to get all combinations of two elements
  comb <- combn(var_cat, 2)
  
  # Transform the results into a list of vectors
  result <- split(t(comb), seq(ncol(comb)))
  
  return(result)
}

generer_deux_paires <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  var_cat <- names(totcode)
  
  # Get all combinations of four elements
  comb <- combn(var_cat, 4)
  
  # For each combination, obtain two disjoint pairs
  result <- lapply(seq(ncol(comb)), function(i) {
    quad <- comb[, i]
    pair_comb <- t(combn(quad, 2))
    
    # Create two disjoint pairs for each combination
    pairs <- lapply(seq(nrow(pair_comb)), function(j) {
      pair1 <- pair_comb[j, ]
      pair2 <- setdiff(quad, pair1)
      
      # Convert the pairs to strings
      pair1_str <- paste(sort(pair1), collapse = ",")
      pair2_str <- paste(sort(pair2), collapse = ",")
      
      # Create a string representing both pairs
      both_pairs_str <- paste(sort(c(pair1_str, pair2_str)), collapse = ",")
      return(both_pairs_str)
    })
    return(pairs)
  })
  
  # Flatten the result
  result <- unlist(result, recursive = FALSE)
  
  # Remove duplicates
  unique_pairs <- unique(result)
  
  # Convert the strings back to vectors
  result <- lapply(unique_pairs, function(pair_str) {
    pairs <- strsplit(pair_str, ",")[[1]]
    return(pairs)
  })
  
  return(result)
}

generer_triplet <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  var_cat <- names(totcode)
  
  # Get all combinations of three elements
  comb <- combn(var_cat, 3)
  
  # Transform the result into a list of vectors
  result <- split(t(comb), seq(ncol(comb)))
  
  return(result)
}
