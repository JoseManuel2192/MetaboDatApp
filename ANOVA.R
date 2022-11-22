

automaticANOVA = function(data, nfactors, setModel, block, printSD = F, allPairwise = "Tukey.HSD", 
                     alpha, decimal, Print.foldchange = F, decimalFoldchange = 1, minValueFoldchange = -1.087767){
  

  
  # mainEffects <- paste0(factor, "+")[-length(factor)] %>% append(factor[length(factor)])
  # if (doInteractions == F | length(factor) == 1) {
  #   .f <- paste0(factor, " ~ ", variable) %>% as.formula()
  # }else{
  #   interactions <- paste0(factor, "*")[-length(factor)] %>% append(factor[length(factor)])
  #   .f <- c(mainEffects, "+", interactions, "~", variable) %>% paste0(collapse = " ") %>% as.formula()
  # }
  # 
  #' Check if there is a block and reorder data
  blockexist = 0
  
  factorNames = colnames(data)[1:nfactors]
  if (blockexist == 1) {
    nfactors = nfactors - 1
    blockPos = which(factorNames == block)
    factorsPos = which(factorNames != block)
    
    orderedFactors = data[,c(blockPos, factorsPos)]
    data[,1:ncol(orderedFactors)] = orderedFactors
    colnames(data)[1:ncol(orderedFactors)] = colnames(orderedFactors)
    colnames(data)[1] = "block"
    printBlock = c("BLOCK FITTED")
  }else{
    orderedFactors = data[,1:nfactors]
    printBlock = c("BLOCK WAS NOT TESTED")
  }
  
  #' Rename factors
  namesPosib = c("factorA", "factorB", "factorC", "factorD",
                 "factorE", "factorF")
  
  #' colnames(data)[which(colnames(orderedFactors) != block)]
  for (i in 1:nfactors) {
    if (colnames(data)[1] == "block") {
      posic = i + 1
    }else{
      posic = i
    }
    colnames(data)[posic] = namesPosib[(i)]
  }

  ######################################################################################
  #' Starting iteration to fit ANOVA 
  ######################################################################################
  AICoptimiz = ANOVA = allANOVA = factorGroupsOrdered = groupsOrdered = meansOrdered = 
    stdOrdered = signifSave = rowsSaved = results = list() 
  colnamesResults = c()
  factors = data[,1:totalfactors]
  
  for (i in c(1:c(ncol(data)-totalfactors))) {
    compound = setNames(as.list((data[i + totalfactors])),"compound")
    newdata=c(compound,factors)
    
    #' ANOVA
    require(memisc)
    if (nfactors == 1) {
      one.way.A = aov(compound ~ factorA, data = newdata)
      
      if (blockexist == 1) {
        one.way.A.block = aov(compound ~ factorA + block, data = newdata)
        model.set = list(one.way.A, one.way.A.block)
        model.names <- c("one.way.A","one.way.A.block")
      }else{
        model.set = list(one.way.A)
        model.names <- c("one.way.A")
      }
    }
    
    if (nfactors == 2) {
      one.way.A = aov(compound ~ factorA, data = newdata)
      one.way.B = aov(compound ~ factorB, data = newdata)
      FO = aov(compound ~ factorA + factorB, data = newdata)
      interaction = aov(compound ~ factorA * factorB, data = newdata)
      
      if (blockexist == 1) {
        one.way.A.block = aov(compound ~ factorA + block, data = newdata)
        one.way.B.block = aov(compound ~ factorB + block, data = newdata)
        FO.block = aov(compound ~ factorA + factorB + block, data = newdata)
        interaction.block = aov(compound ~ factorA * factorB + block, data = newdata)
        
        model.set = list(one.way.A, one.way.B, FO, interaction, one.way.A.block,
                         one.way.B.block, FO.block, interaction.block)
        model.names <- c("one.way.A", "one.way.B", "FO", "interaction", "one.way.A.block",
                         "one.way.B.block", "FO.block", "interaction.block")
      }else{
        model.set = list(one.way.A, one.way.B, FO, interaction)
        model.names <- c("one.way.A", "one.way.B", "FO", "interaction")
      }
    }
    
    if (nfactors == 3) {
      one.way.A = aov(compound ~ factorA, data = newdata)
      one.way.B = aov(compound ~ factorB, data = newdata)
      one.way.C = aov(compound ~ factorC, data = newdata)
      FO = aov(compound ~ factorA + factorB + factorC, data = newdata)
      interaction = aov(compound ~ factorA * factorB * factorC, data = newdata)
      
      if (blockexist == 1) {
        one.way.A.block = aov(compound ~ factorA + block, data = newdata)
        one.way.B.block = aov(compound ~ factorB + block, data = newdata)
        one.way.C.block = aov(compound ~ factorC + block, data = newdata)
        FO.block = aov(compound ~ factorA + factorB + factorC + block, data = newdata)
        interaction.block = aov(compound ~ factorA * factorB * factorC + block, data = newdata)
        
        model.set = list(one.way.A, one.way.B, one.way.C, FO, interaction, one.way.A.block,
                         one.way.B.block, one.way.C.block, FO.block, interaction.block)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "FO", "interaction", "one.way.A.block",
                        "one.way.B.block", "one.way.C.block", "FO.block", "interaction.block")
      }else{
        model.set = list(one.way.A, one.way.B, one.way.C, FO, interaction)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "FO", "interaction")
      }
    }
    
    if (nfactors == 4) {
      one.way.A = aov(compound ~ factorA, data = newdata)
      one.way.B = aov(compound ~ factorB, data = newdata)
      one.way.C = aov(compound ~ factorC, data = newdata)
      one.way.D = aov(compound ~ factorD, data = newdata)
      FO = aov(compound ~ factorA + factorB + factorC + factorD, data = newdata)
      interaction = aov(compound ~ factorA * factorB * factorC * factorD, data = newdata)
      
      if (blockexist == 1) {
        one.way.A.block = aov(compound ~ factorA + block, data = newdata)
        one.way.B.block = aov(compound ~ factorB + block, data = newdata)
        one.way.C.block = aov(compound ~ factorC + block, data = newdata)
        one.way.D.block = aov(compound ~ factorD + block, data = newdata)
        FO.block = aov(compound ~ factorA + factorB + factorC + factorD + block, data = newdata)
        interaction.block = aov(compound ~ factorA * factorB * factorC * factorD + block, data = newdata)
        
        
        model.set = list(one.way.A, one.way.B, one.way.C, one.way.D, FO, interaction, one.way.A.block,
                         one.way.B.block, one.way.C.block, one.way.D.block, FO.block, interaction.block)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "one.way.D", "FO", "interaction", "one.way.A.block",
                        "one.way.B.block", "one.way.C.block", "one.way.D.block", "FO.block", "interaction.block")
      }else{
        model.set = list(one.way.A, one.way.B, one.way.C, one.way.D, FO, interaction)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "one.way.D", "FO", "interaction")
      }
    }
    
    if (nfactors == 5) {
      one.way.A = aov(compound ~ factorA, data = newdata)
      one.way.B = aov(compound ~ factorB, data = newdata)
      one.way.C = aov(compound ~ factorC, data = newdata)
      one.way.D = aov(compound ~ factorD, data = newdata)
      one.way.E = aov(compound ~ factorE, data = newdata)
      FO = aov(compound ~ factorA + factorB + factorC + factorD + factorE, 
               data = newdata)
      interaction = aov(compound ~ factorA * factorB * factorC * factorD * factorE, 
                        data = newdata)
      
      if (blockexist == 1) {
        one.way.A.block = aov(compound ~ factorA + block, data = newdata)
        one.way.B.block = aov(compound ~ factorB + block, data = newdata)
        one.way.C.block = aov(compound ~ factorC + block, data = newdata)
        one.way.D.block = aov(compound ~ factorD + block, data = newdata)
        one.way.E.block = aov(compound ~ factorE + block, data = newdata)
        FO.block = aov(compound ~ factorA + factorB + factorC + factorD + factorE + block, 
                       data = newdata)
        interaction.block = aov(compound ~ factorA * factorB * factorC * factorD * factorE + block, 
                                data = newdata)
        
        
        model.set = list(one.way.A, one.way.B, one.way.C, one.way.D, one.way.E, FO, interaction, one.way.A.block,
                         one.way.B.block, one.way.C.block, one.way.D.block, one.way.E.block, FO.block, interaction.block)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "one.way.D", "one.way.E", "FO", "interaction", "one.way.A.block",
                        "one.way.B.block", "one.way.C.block", "one.way.D.block", "one.way.E.block", "FO.block", "interaction.block")
      }else{
        model.set = list(one.way.A, one.way.B, one.way.C, one.way.D, "one.way.E", FO, interaction)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "one.way.D", "one.way.E", "FO", "interaction")
      }
    }
    
    if (nfactors == 6) {
      one.way.A = aov(compound ~ factorA, data = newdata)
      one.way.B = aov(compound ~ factorB, data = newdata)
      one.way.C = aov(compound ~ factorC, data = newdata)
      one.way.D = aov(compound ~ factorD, data = newdata)
      one.way.E = aov(compound ~ factorE, data = newdata)
      one.way.F = aov(compound ~ factorF, data = newdata)
      FO = aov(compound ~ factorA + factorB + factorC + factorD + factorE
               + factorF, data = newdata)
      interaction = aov(compound ~ factorA * factorB * factorC * factorD * factorE 
                        * factorF, data = newdata)
      
      if (blockexist == 1) {
        one.way.A.block = aov(compound ~ factorA, data = newdata)
        one.way.B.block = aov(compound ~ factorB, data = newdata)
        one.way.C.block = aov(compound ~ factorC, data = newdata)
        one.way.D.block = aov(compound ~ factorD, data = newdata)
        one.way.E.block = aov(compound ~ factorE, data = newdata)
        one.way.F.block = aov(compound ~ factorF, data = newdata)
        FO.block = aov(compound ~ factorA + factorB + factorC + factorD + factorE
                       + factorF, data = newdata)
        interaction.block = aov(compound ~ factorA * factorB * factorC * factorD * factorE 
                                * factorF, data = newdata)
        
        
        model.set = list(one.way.A, one.way.B, one.way.C, one.way.D, one.way.E, one.way.F, FO, interaction, one.way.A.block,
                         one.way.B.block, one.way.C.block, one.way.D.block, one.way.E.block, one.way.F.block, FO.block, interaction.block)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "one.way.D", "one.way.E", "one.way.F", "FO", "interaction", "one.way.A.block",
                        "one.way.B.block", "one.way.C.block", "one.way.D.block", "one.way.E.block", "one.way.F.block", "FO.block", "interaction.block")
      }else{
        model.set = list(one.way.A, one.way.B, one.way.C, one.way.D, one.way.E, one.way.F, FO, interaction)
        model.names = c("one.way.A", "one.way.B", "one.way.C", "one.way.D", "one.way.E", "one.way.F", "FO", "interaction")
      }
    }
    
    require(AICcmodavg)
    #' From these results, it appears that the two.way model is the best fit. 
    #' The two-way model has the lowest AIC value, and 71% of the AIC weight, 
    #' which means that it explains 71% of the total variation in the dependent 
    #' variable that can be explained by the full set of models.
    #' delta: Less than 2, this indicates there is substantial evidence to support the 
    #' candidate model (i.e., the candidate model is almost as good as the best model)
    
    #' AIC optimization
    AICmodels = aictab(modnames = model.names, cand.set = model.set)
    AICoptimiz[[i]] = AICmodels 
    
    #' Select the best model. The more complex with deltaAIC <= 2
    deltaAIC = as.data.frame(AICmodels)[,4]
    kAIC = as.data.frame(AICmodels)[,2]
    if (length(deltaAIC[deltaAIC <= 2]) == 1) {
      orderBestModel = 1
    }else{
      models = 1:length(deltaAIC[deltaAIC <= 2])
      orderBestModel = which(kAIC[models] == max(kAIC[models]))
    }
    bestModel = as.data.frame(AICmodels)[orderBestModel,1]
    
    #' Option to manually select the model
    if (exists("setModel")) {
      if (length(which(setModel == as.data.frame(AICmodels)[,1])) == 0) {
        bestModel = as.data.frame(AICmodels)[orderBestModel,1]
        modelWarning = c("The model selected is wrong. Optimization on the basis of AIC")
      } else{
        bestModel = setModel
      }
    }else{
      modelWarning = "Models optimized on the basis of the AIC"
    }
    selectedModel = model.set[which(model.names == bestModel)]
    ANOVA[[i]] = summary(selectedModel[[1]])
    
    ANOVAcombinations = list()
    for (k in 1:length(model.names)) {
      ANOVAcombinations[[k]] = summary(model.set[which(model.names == model.names[k])][[1]])
    }
    allANOVA[[i]] = ANOVAcombinations
    
    #' p-value
    pValue = as.data.frame(ANOVA[[i]][[1]])[,5]
    pValue = pValue[!is.na(pValue)]
    signific = c()
    for (j in 1:length(pValue)) {
      if (pValue[j]<=0.001){signific[j]="***"}else{
        if (pValue[j]<=0.01 & pValue[j]>0.001){signific[j]="**"}else{
          if (pValue[j]<=0.05 & pValue[j]>0.01){signific[j]="*"}else{
            if (pValue[j]>0.05){signific[j]="ns"}else{}
          }
        }
      }
    }
    
    #' Identifying factors selected in AIC
    factorSelected = c()
    rowNames = gsub(" ", "", rownames(ANOVA[[i]][[1]]))
    lastPosic = which(rowNames == "Residuals") - 1
    for (j in 1:lastPosic) {
      factorSelected = c(factorSelected, rowNames[j])
    }
    if (length(which(factorSelected == "block")) == 1) {
      factorSelected = factorSelected[1:c(length(factorSelected)-1)]
      signific = signific[1:c(length(signific)-1)]
    }
    
    #' Identifying for the rest instead of ns or ***
    signifFinal = rep("Not Fitted", nfactors)
    currentFactor = namesPosib[1:nfactors]
    signifFinal[which(currentFactor == factorSelected)] = signific
    signifSave[[i]] = signifFinal
    
    #' All pairwise comparison
    require(agricolae)
    cv=cv.model(selectedModel[[1]])
    mean=mean(c(do.call("cbind",compound)))
    df=df.residual(selectedModel[[1]])
    MSerror<-deviance(selectedModel[[1]])/df
    
    #' Taking the factors to arrange in alphabetic order
    if (!exists("alpha")) {
      alpha = 0.05
    }
    groups = means = std = factorGroups = orderedFactors = c()
    for (j in which(colnames(data) == "factorA"):ncol(factors)) {
      
      #' Selecting the pairwise comparison. 4 avalaible
      if(allPairwise == c("LSD")){
        lsd=LSD.test(compound, MSerror = MSerror, DFerror = df, 
                     trt = factors[,j], alpha = 0.05)
      }
      if(allPairwise == c("duncan")){
        lsd=duncan.test(compound, MSerror = MSerror, DFerror = df, 
                        trt = factors[,j], alpha = 0.05)
      }
      if(allPairwise == c("Tukey.HSD")){
        lsd=HSD.test(compound, MSerror = MSerror, DFerror = df, 
                     trt = factors[,j], alpha = 0.05)
        
      }
      if(allPairwise == c("scheffe")){
        lsd=scheffe.test(compound, MSerror = MSerror, DFerror = df, 
                         trt = factors[,j], alpha = 0.05)
      }
  
      # aa=selectedModel[[1]]; aa$terms
      # colnames(as.matrix(selectedModel[[1]]))
      # interaction <- with(compound, interaction(factors[,c(2,3)]))
      # 
      # 
      # aaa=HSD.test(compound, interaction, MSerror = MSerror, DFerror = df, 
      #          alpha = 0.05)
      # 
      
      
      
      groups = c(groups, lsd$groups$groups)
      means = c(means, lsd$means$compound)
      
      #' Fit decimals automatically
      require(memisc)
      decimals = memisc::cases(
        abs(as.numeric(min(means[means != 0]))) <= 0.0001  -> 5,
        abs(as.numeric(min(means[means != 0]))) <= 0.001 & min(means[means != 0]) > 0.0001  -> 4,
        abs(as.numeric(min(means[means != 0]))) <= 0.08 & min(means[means != 0]) > 0.001 -> 3, 
        abs(as.numeric(min(means[means != 0]))) <= 2 & min(means[means != 0]) > 0.01 -> 2, 
        abs(as.numeric(min(means[means != 0]))) <= 15 & min(means[means != 0]) > 2 -> 1, 
        abs(as.numeric(min(means[means != 0]))) >= 15  -> 0 
      )
      
      #' If decimals are manually fitted
      if (exists("decimal")) {
        if (is.numeric(decimal)) {
          decimals = decimal
        }
      }
      
      means = formatC(round(as.numeric(means), decimals), format = "f", digits = decimals)
      std = formatC(round(c(as.numeric(std), lsd$means$std), decimals), format = "f", digits = decimals)
      factorGroups = formatC(c(factorGroups, rownames(lsd$groups)) , format = "f", digits = decimals)
      orderedFactors = formatC(c(orderedFactors, levels(as.factor(factors[,j][[1]]))), format = "f", digits = decimals)
    }
    vectorOrderedFactors = c()
    for (j in 1:length(orderedFactors)) {
      vectorOrderedFactors = c(vectorOrderedFactors,
                               which(factorGroups == orderedFactors[j]))
    }
    groupsOrdered[[i]] = groups[vectorOrderedFactors]
    meansOrdered[[i]] = means
    stdOrdered[[i]] = std
    
    #' Print data as fold-change
    if (Print.foldchange == T) {
      inicFactors = 1
      for (j in 1:nfactors) {
        neachFactor = length(levels(as.factor(data[,which(colnames(data) == currentFactor[j])][[1]])))
        inicFactors = c(inicFactors, c(inicFactors[j] + neachFactor))
      }
      meansfold.order = c()
      for (k in 1:c(length(inicFactors)-1)) {
        foldchange.transf = meansOrdered[[i]][inicFactors[k]:c(inicFactors[k+1]-1)]
        for (l in 1:length(foldchange.transf)) {
          foldchange.transf = as.numeric(foldchange.transf)
          
          if (min(foldchange.transf) <= minValueFoldchange) {
            minFoldchange = min(foldchange.transf[foldchange.transf != min(foldchange.transf)])
          }else{
            minFoldchange = min(foldchange.transf)
          }
          foldchange.transf = foldchange.transf/minFoldchange
        }
        meansfold.order = c(meansfold.order, foldchange.transf)
      }
      meansOrdered[[i]] = formatC(round(as.numeric(meansfold.order), decimalFoldchange), format = "f", digits = decimalFoldchange)
    }
    
    #' Print data with or without SD
    # printSD = ifelse(!exists("printSD"), T, printSD)
    if (printSD == T) {
      results[[i]] = paste(meansOrdered[[i]], " ? "  , gsub(" ", "", paste(stdOrdered[[i]], groupsOrdered[[i]])))
      nsSustit = paste(meansOrdered[[i]], " ? "  , gsub(" ", "", paste(stdOrdered[[i]])))
    }else{
      results[[i]] = gsub(" ", "", paste(meansOrdered[[i]], groupsOrdered[[i]]))
      nsSustit = meansOrdered[[i]]
    }
    
    #' Cleaning non-significant letters
    inicFactors = 1
    for (j in 1:nfactors) {
      neachFactor = length(levels(as.factor(data[,which(colnames(data) == currentFactor[j])][[1]])))
      inicFactors = c(inicFactors, c(inicFactors[j] + neachFactor))
    }
    rows = c()
    for (j in 1:nfactors) {
      if (signifFinal[j] == "ns" ) {
        results[[i]][c(inicFactors[j] : c(inicFactors[j+1] - 1))] = nsSustit[c(inicFactors[j] : c(inicFactors[j+1] - 1))]
      }
      if (signifFinal[j] == "Not Fitted" ) {
        results[[i]][c(inicFactors[j] : c(inicFactors[j+1] - 1))] = ""
      }
      rows = c(rows, results[[i]][c(inicFactors[j] : c(inicFactors[j+1] - 1))], 
               signifSave[[i]][j])
      if (i == 1) {
        colnamesResults = c(colnamesResults, orderedFactors[c(inicFactors[j] : c(inicFactors[j+1] - 1))], "p-value")
      }
      
      if (j == nfactors) {
        rows = c(rows, bestModel)
        if (i == 1) {
          colnamesResults = c(colnamesResults, "ModelFitted")
        }
      }
      rowsSaved[[i]] = rows
    }
  }
  matrixResults = matrix(ncol = length(rowsSaved[[1]]), nrow = length(rowsSaved))
  for (i in 1:length(rowsSaved)) {
    matrixResults[i,] = rowsSaved[[i]]
  }
  colnames(matrixResults) = gsub(" ", "", colnamesResults)
  rownames(matrixResults) = colnames(data[,-c(1:totalfactors)])
  
  #' Return the results
  printBlock
  # modelWarning
  returnResults = list("matrixResults" = matrixResults, "AICoptimiz" = AICoptimiz, 
                       "ANOVA" = ANOVA, "allANOVA" = allANOVA)
  returnResults$allANOVA = setNames(returnResults$allANOVA, colnames(data)[-c(1:totalfactors)])
  returnResults$AICoptimiz = setNames(returnResults$AICoptimiz, colnames(data)[-c(1:totalfactors)])
  return(returnResults)
}




#' Hacer medias y desviaciones. Dejar opci?n a elegir sin desviacion
#' Opcion foldchange en lugar de medias 
#' Incluir opcion LOD y LOQ
#' Completar que se pueda elegir optimizar o elegir modelo (testear funcionamiento)
#' Optimizar modelo: ahora lo hace eligiendo el modelo mas complejo con deltaAIC < 2. 
#' Permitir que tambien lo haga para el modelo optimo (primero de la lista)
#' Completar modelwarning para 
#' Introducir interacciones (p-value y pos-hoc)
#' A?adir en la lista ANOVA que se guarden los que no se han hecho tambien
#' Ajustar que los decimales bien: si acaba en 0 lo esta eliminando
#' REVISAR INTERACCIONES: EN GENERAL TODOS LOS MODELOS


