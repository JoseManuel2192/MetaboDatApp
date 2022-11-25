ANOVA <- function(data, factors, doInteractions = T){
  
  posResponses <- lapply(data, is.numeric) %>% unlist() %>% which(.)
  responses <- colnames(data)[posResponses]
  responsesRenamed <- paste0("Feat_", 1:length(responses)); colnames(data)[posResponses] = responsesRenamed # Changed to avoid naming problems
  restFactors <- data %>% dplyr::select(-all_of(posResponses)) %>% colnames() 
  posFactors <- restFactors %in% factors %>% which(.)
  factors <- restFactors[posFactors] # Fix order of factors 
  ini <- length(restFactors) + 1
  fin <- ncol(data)
  
  anovaResults <- list()
  for(i in ini:fin){
    variable <- responsesRenamed[i - ini + 1]
    
    # Get formula
    mainEffects <- paste0(factors, "+")[-length(factors)] %>% append(factors[length(factors)])
    if (doInteractions == F | length(factors) == 1) {
      .f <- c(variable, "~", mainEffects) %>% paste0(., collapse = " ") %>% as.formula()
    }else{
      interactions <- paste0(factors, "*")[-length(factors)] %>% append(factors[length(factors)])
      .f <- c(variable, "~", mainEffects, "+", interactions) %>% paste0(. ,collapse = " ") %>% as.formula()
    }
    
    # AOV
    modelAOV <- aov(.f, data = data)
    
    # ANOVA
    anovaResults[[i - ini + 1]] <- rstatix::anova_summary(modelAOV) 
    rmPosHoc <- which(anovaResults[[i - ini + 1]] %>% dplyr::select(p) > 0.05) # Pos-hoc letter to be removed
    
    # Main Effects
    posHocFeat <- list()
    for (j in 1:length(factors)) {
      posHocFeat[[j]] <- HSD.test(modelAOV, factors[j], group = T, console = F)$groups %>% mutate(rownames(.)) %>%
        setNames(., c("Feat", "groups", "names")) %>% arrange(., names) %>% t() %>% as.tibble() %>% slice(2)
      if (j %in% rmPosHoc) { # Remove letters from pos-hoc when p-value > 0.05
        posHocFeat[[j]] <- posHocFeat[[j]] %>% mutate_all(funs(str_replace(., ., " ")))
      }
    }
    
    # Interactions
    if (doInteractions == T & length(factors) > 1) {
      posHocFeat[[j+1]] <- HSD.test(modelAOV, factors, group = T, console = F)$groups %>% mutate(rownames(.)) %>%
        setNames(., c("Feat", "groups", "names")) %>% arrange(., names) %>% t() %>% as.tibble() %>% slice(2)
      if (c(j+1) %in% rmPosHoc) { # Remove letters from pos-hoc when p-value > 0.05
        posHocFeat[[j+1]] <- posHocFeat[[j+1]] %>% mutate_all(funs(str_replace(., ., " ")))
      }
    }
    
    # rbind in a list separated by effect type
    if (i == ini) {
      posHoc <- posHocFeat
    }else{
      posHoc <- map2(posHoc, posHocFeat, rbind)
    }
  }
  names(anovaResults) <- responsesRenamed
  
  # p-values to astherics
  pvaluesAst <- pvalues <- lapply(anovaResults, function(x) x%>% as_tibble() %>% dplyr::select(p)) %>% sapply(., unlist) %>% t() %>% round(.,3) 
  pvaluesAst[pvalues <= 0.05 & pvalues > 0.01] <- "*"
  pvaluesAst[pvalues <= 0.01 & pvalues > 0.001] <- "**"
  pvaluesAst[pvalues <= 0.001] <- "***"
  pvaluesAst[pvalues > 0.05] <- "ns"
  if (length(factors) == 1){pvaluesAst <- t(pvaluesAst)}
  
  rownames(pvaluesAst) <- responses
  
  return(list("posHoc" = posHoc, "pvaluesAst" = pvaluesAst))
}
