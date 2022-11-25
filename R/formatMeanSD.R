formatMeanSD <- function(output_mean, output_SD, decimalsFitted){
  tableMeans = output_mean
  tableSD = output_SD
  
  # Fitting format
  tableMeans <- lapply(tableMeans, function(x){
    for(i in 1:nrow(x)){x[i,] <- x[i,] %>% as.numeric() %>% formatC(., format = "f", digits = decimalsFitted[i,])}
    return(x)
  })
  tableSD <- lapply(tableSD, function(x){
    for(i in 1:nrow(x)){x[i,] <- x[i,] %>% as.numeric() %>% formatC(., format = "f", digits = decimalsFitted[i,])}
    return(x)
  })
  
  return(list("tableMeans" = tableMeans, "tableSD" = tableSD))
}