doMeanSD <- function(data, factors, foldChange = F){
  
  posResponses <- lapply(data, is.numeric) %>% unlist() %>% which(.)
  restFactors <- data %>% dplyr::select(-all_of(posResponses)) %>% colnames() 
  posFactors <- restFactors %in% factors %>% which(.)
  factors <- restFactors[posFactors] # Fix order of factors 
  
  # Main effects: means and SD
  tableMeans = tableSD = list()
  for (i in 1:length(posFactors)) {
    tableMeans[[i]] <- data %>% group_by(data %>% dplyr::select(posFactors[i])) %>% summarise_all(funs(mean)) %>% t() %>% as.data.frame() %>% 
      purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
    tableSD[[i]] <- data %>% group_by(data %>% dplyr::select(posFactors[i])) %>% summarise_all(funs(sd)) %>% t() %>% as.data.frame() %>% 
      purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
  }
  
  # Interaction effect (number of factors is > 1): means and SD
  if (length(posFactors) > 1) {
    tableMeans[[i+1]] <- data %>% group_by(data %>% dplyr::select(all_of(posFactors))) %>% summarise_all(funs(mean)) %>% unite("Interactions", 1:2, remove = T, sep = " x ") %>% 
      t() %>% as.data.frame() %>% purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
    tableSD[[i+1]] <- data %>% group_by(data %>% dplyr::select(all_of(posFactors))) %>% summarise_all(funs(sd)) %>% unite("Interactions", 1:2, remove = T, sep = " x ") %>%
      t() %>% as.data.frame() %>% purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
  }
  
  # For decimal fit -> extract a vector with the lower mean values of each effect. Extracted here to prevent problems if 
  # the means are changed (eg. when using foldchange).
  minMean <- tableMeans %>% lapply(., function(x) apply(x, 2, as.numeric) %>% abs() %>% apply(., 1, min)) # Absolute value for negative variables
  minMean <- do.call(cbind, minMean) %>% apply(., 1, function(x){ # Avoid problems in rows with all the values = 0
    if (sum(x[x>0]) == 0){0} else{min(x[x>0])}})
  maxMean <- tableMeans %>% lapply(., function(x) apply(x, 2, as.numeric) %>% apply(., 1, max)) # Extracts max means (for the nex function)
  
  # Foldchanges relative to the maximum mean value of each effect
  if (foldChange == T) {
    for (i in 1:length(tableSD)) {tableSD[[i]] <- apply(tableSD[[i]], 2, as.numeric) / maxMean[[i]]} # Foldchange of dev
    tableMeans <- tableMeans %>% lapply(., function(x) apply(x, 2, as.numeric) %>% apply(., 1, function(x) x/max(x)) %>% t())# %>% round(2)) # Foldchange means & round(2)
  }
  
  # Decimal fit (continue)
  # Assign minMean to a vector from the min non-zero value across all mean effects
  decimals <- minMean %>% replace(., .<= 0.0001, 5/100000) %>% replace(., .> 0.0001 & .<= 0.001, 4/100000) %>% replace(., .> 0.001 & .<= 0.08, 3/100000) %>% 
    replace(., .> 0.08 & .<= 2, 2/100000) %>% replace(., .> 2 & .<= 15, 1/100000) %>% replace(., .> 15, 0)
  decimals <- decimals * 100000
  if (foldChange == T) {decimals <- decimals %>% replace(., is.numeric(.), 2)}
  
  return(list("tableMeans" = tableMeans, "tableSD" = tableSD, "decimals" = as.data.frame(decimals)))
}
