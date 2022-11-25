compileANOVA <- function(tableMeans, tableSD, posHoc, pvaluesAst, showSD = F){
  # Building the final table
  for (i in 1:length(posHoc)) {
    # Add SD
    if (showSD == T) {tableMaking <- tableMeans[[i]] %>% paste.matrix(., tableSD[[i]], sep = " ± ")}else
    {tableMaking <- tableMeans[[i]]}
    
    tableMaking <- tableMaking %>% paste.matrix(., posHoc[[i]], sep = "") %>% 
      cbind(., pvaluesAst[,i]) 
    colnames(tableMaking) <- c(tableMeans[[i]] %>% colnames(.), "p-value")
    if (i == 1) {
      tableANOVA <- tableMaking
    }else{
      tableANOVA <- cbind(tableANOVA, tableMaking)
    }
    rownames(tableANOVA) <- rownames(pvaluesAst) # Original rownames (since in mean_sd are changed to prevent caption problems)
  }
  return(tableANOVA)
}