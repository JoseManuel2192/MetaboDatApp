library(mixOmics)
my.plsda <- mixOmics::plsda(X,Y,ncomp = 6)
perf.plsda <- mixOmics::perf(my.plsda, validation = "loo", progressBar = T)
plot(perf.plsda)
backgraound <- background.predict(my.plsda, comp.predicted = 2, dist = "mahalanobis.dist")

plotIndiv(my.plsda, comp = 1:2, group = Y, ind.names = F, background = backgraound, legend = T, title = "plsda")


#############################################################################################################
# PLS-DA construction
#############################################################################################################

# ----------------- Load inputs ----------------- #
library(caret); library(tidyverse); library(plyr); library(splitTools); library(foreach); library(magrittr); library(sjmisc)
data <- read_excel("./data.xlsx")
Y <- data$Style %>% as.factor()
X <- data[,-c(1:7)] %>% scale()
ncompMax <- 6 # Max number of components to optimize

type <- "Mfold" # Mfold / loo

typePartition <- "stratified" # stratified, basic, grouped, blocked

folds = 4
rep = 10

classErrorPlot <- "Andalusian" # (For class error rate)

# ----------------- Partitions ----------------- #
my.partition <- doPartition(Y, type = type, typePartition = typePartition, folds = folds, rep = rep)
folds <- my.partition$folds
rep <- my.partition$rep
# ----------------- PLS-DA perf: obtain assignation matrix ----------------- #
plsdaPerf <- plsdaCV(X, Y, rep = rep, partition = my.partition$partition, ncompMax = ncompMax)

# ----------------- Convert assignation matrix into diagnostics ----------------- #
diagnostics <- list()
for (i in 1:rep) {
  diagnostics[[i]] <- getErrors(assignations = purrr:::map(plsdaPerf, i), Y = Y)
}

# ----------------- Get diagnostic means ----------------- #
BERmean <- do.call(rbind, purrr::map(diagnostics, 1)) %>% group_by(Component) %>% summarise_each(.,funs(mean))
classErrormean <- do.call(rbind, purrr::map(diagnostics, 2)) %>% dplyr::group_by(distance, class) %>% summarise_each(.,funs(mean)) %>% arrange(class) %>%
  filter(class == classErrorPlot) %>% dplyr::select(-class) %>% remove_rownames %>% column_to_rownames(var="distance") %>% rotate_df(rn = "Component") %>% 
  slice(-1) %>% as_tibble() %>% mutate(., c_across(2:4, as.factor())) 
Accuracymean <- do.call(rbind, purrr::map(diagnostics, 3)) %>% group_by(Component) %>% summarise_each(.,funs(mean))

# ----------------- Plot each diagnostic mean ----------------- #
plotDiagnostic(BERmean, "BER")
plotDiagnostic(classErrormean, paste0("Class Error of ", classErrorPlot, " class"))
plotDiagnostic(Accuracymean, "Accuracy")

# ----------------- Calculate optimal ncomp according to each diagnostic ----------------- #
noptimalBER <- findOptimalN(BERmean, "min")
noptimalErrorClass <- findOptimalN(classErrormean, "min")
noptimalAccuracy <- findOptimalN(Accuracymean, "max")
ncompOptimal <- rbind(noptimalBER, noptimalAccuracy, noptimalErrorClass) %>% set_rownames(c("BER", "Accuracy", str_glue("Error {classErrorPlot} class")))

# ----------------- Assignation plot ----------------- #
ncompOpt <- 4
dist <- "max.dist.predictions"

predictions <- getPredictionMatrices(plsdaPerf = plsdaPerf, Y = Y, ncompOpt = ncompOpt, dist = dist, id = Y)
colourPredictions <- pivot_longer(predictions$colours, cols = -c(ID, order))
dataPredictions <- pivot_longer(predictions$predictionSummary, cols = -c(ID, order)) %>% 
  mutate(dataPredictions, ID = factor(ID, levels = unique(ID)))


Ymeans <- calculateMeans(rep = rep, Y = dataPredictions$name %>% as.factor())
dataPredictions <- dataPredictions %>% mutate(.,"mean" = Ymeans) %>% mutate(., "min" = mean - value/2, "max" = mean + value/2) %>%
  mutate(., "colour" = colourPredictions %>% pull(value)) %>% arrange(name)

sizeYaxis = 5
sizeXaxis = 10
sizeLegendTitle = 16
sizeLegendLevels = 14
sizeSegment = 3

predictionPlot(dataPredictions, sizeXaxis, sizeYaxis, sizeLegendTitle, sizeLegendLevels, sizeSegment)




#############################################################################################################
# FUNCTION: partitions calibration and validation sets
doPartition <- function(Y, type = "loo", typePartition = "stratified", folds = 6, rep = 10){
  if (type == "loo") {
    folds <- length(Y)
    rep <- 1
    my.partition <- create_folds(y = Y, k = folds, m_rep = rep, type = typePartition)
  } else{
    if (type == "Mfold") {
      my.partition <- create_folds(y = Y, k = folds, m_rep = rep, type = typePartition)
    }
  }
  return(list("partition" = my.partition, "folds" = folds, "rep" = rep))
}

# FUNCTION: extract assignation from PLS-DA
plsAssignations <- function(X, Y, partition, ncompMax){
  
  # ----------------- Partitions ----------------- #
  Xtrain <- X %>% as_tibble() %>% slice(partition)
  Xtest <- X %>% as_tibble() %>% slice(-partition)
  Ytrain <- Y[partition]
  Ytest <- Y[-partition]
  
  # ----------------- Perform PLS-DA model ----------------- #
  my.plsda <- mixOmics::plsda(Xtrain,Ytrain, ncomp = ncompMax, scale = F)
  predictions <- predict(my.plsda, Xtest)
  
  # ----------------- Obtain predictions from different distances ----------------- #
  max.dist <- predictions$MajorityVote$max.dist %>% as_tibble() %>% dplyr::mutate("Index" = (1:length(Y))[-partition])
  centroids.dist <- predictions$MajorityVote$centroids.dist %>% as_tibble() %>% dplyr::mutate("Index" = (1:length(Y))[-partition])
  mahalanobis.dist <- predictions$MajorityVote$mahalanobis.dist %>% as_tibble() %>% dplyr::mutate("Index" = (1:length(Y))[-partition])
  all.dist <- list("max.dist" = max.dist, "centroids.dist" = centroids.dist, "mahalanobis.dist" = mahalanobis.dist)
  
  return("assignations" = all.dist)
}
# FUNCTION: perform iterations of the cross validation
plsdaCV <- function(X, Y, rep, partition, ncompMax){
    plsAsses <- lapply(partition, function(x){
      plsAssignations(X, Y, partition = x, ncompMax = ncompMax)
    })

  
  fSplit <- rep(1:rep, length(Y)) %>% sort(.)
  max.dist.Assign <- do.call(rbind, purrr::map(plsAsses, 1)) %>% split(., f = fSplit) %>% lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  centroids.dist.Assign <- do.call(rbind, purrr::map(plsAsses, 2)) %>% split(., f = fSplit) %>% lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  mahalanobis.dist.Assign <- do.call(rbind, purrr::map(plsAsses, 3)) %>% split(., f = fSplit) %>% lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  names(max.dist.Assign) <- names(centroids.dist.Assign) <- names(mahalanobis.dist.Assign) <- paste0("rep", 1:length(max.dist.Assign))
  all.dist.Assign <- list("max" = max.dist.Assign, "centroids" = centroids.dist.Assign, "mahalanobis" = mahalanobis.dist.Assign)
  return(all.dist.Assign)
}
# FUNCTION: get errors from the outputs of plsdaCV
getErrors <- function(assignations, Y){
  
  # ----------------- Confusion matrix for each distance ----------------- #
  conf.matrix.max <- alply(assignations$max %>% as.matrix, 2, function(X){table(X,Y)})
  conf.matrix.centroids <- alply(assignations$centroids %>% as.matrix, 2, function(X){table(X,Y)})
  conf.matrix.mahalanobis <- alply(assignations$mahalanobis %>% as.matrix, 2, function(X){table(X,Y)})
  
  # ----------------- Obtain diagnostics for each distance ----------------- #
  BER <- list("max.dist" = lapply(conf.matrix.max, BERF) %>% do.call(cbind,.),
              "centroids.dist" = lapply(conf.matrix.centroids, BERF) %>% do.call(cbind,.),
              "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, BERF) %>% do.call(cbind,.))
  classError <- list("max.dist" = lapply(conf.matrix.max, ClassErrorF) %>% do.call(cbind, .) %>% set_colnames(paste0("Comp", 1:ncol(.))),
                     "centroids.dist" = lapply(conf.matrix.centroids, ClassErrorF) %>% do.call(cbind, .) %>% set_colnames(paste0("Comp", 1:ncol(.))),
                     "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, ClassErrorF) %>% do.call(cbind, .) %>% set_colnames(paste0("Comp", 1:ncol(.))))
  accuracy <- list("max.dist" = lapply(conf.matrix.max, accuracyF) %>% do.call(cbind, .),
                   "centroids.dist" = lapply(conf.matrix.centroids, accuracyF) %>% do.call(cbind, .),
                   "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, accuracyF) %>% do.call(cbind, .))
  
  # ----------------- Redefine variables -> results in a matrix ----------------- #
  BER <- do.call(rbind, BER) %>% set_rownames(., c("max", "centroids", "mahalanobis")) %>%
    t() %>% as.data.frame() %>% dplyr::mutate("Component" = paste0("Comp", 1:nrow(.)), .before = max)
  classError <- classError %>% purrr::map(., function(x) apply(x, 2, unlist)) %>% do.call(rbind, .) %>% as.data.frame() %>%
    dplyr::mutate(., "class" = rep(rownames(.)[1:nlevels(Y)], 3)) %>% as_tibble() %>%
    dplyr::mutate(., "distance" = c(rep("max", nlevels(Y)), rep("centroid", nlevels(Y)), rep("mahalanobis", nlevels(Y)))) %>% arrange(class)
  accuracy <- do.call(rbind, accuracy) %>% set_rownames(., c("max", "centroids", "mahalanobis")) %>%
    t() %>% as.data.frame() %>% dplyr::mutate("Component" = paste0("Comp", 1:nrow(.)), .before = max)
  
  return(list("BER" = BER, "classError" = classError, "accuracy" = accuracy))
}
# FUNCTION: extract accuracy from confusion matrix
accuracyF <- function(conf.matrix){
  cp <- 0
  for (i in 1:nrow(conf.matrix)) {cp <- conf.matrix[i,i] %>% sum(., cp)}
  cp / sum(conf.matrix)
}
# FUNCTION: extract BER from confussion matrix
BERF <- function(conf.matrix){
  BERclass <- list()
  for (i in 1:ncol(conf.matrix)) {
    wrong <- conf.matrix[-i,i] %>% sum
    total <- sum(conf.matrix[,i])
    BERclass[[i]] <- wrong / total
  }
  BER <- do.call(cbind, BERclass) %>% mean()
  return(BER)
}
# FUNCTION: class error from confusion matrix
ClassErrorF <- function(conf.matrix){
  BERclass <- list()
  for (i in 1:ncol(conf.matrix)) {
    wrong <- conf.matrix[-i,i] %>% sum
    total <- sum(conf.matrix[,i])
    BERclass[[i]] <- wrong / total %>% as.numeric()
  }
  BERclass <- BERclass %>% set_names(conf.matrix %>% colnames()) 
  return(BERclass)
}
# FUNCTION: Find optimal number of components
findOptimalN <- function(objectOpt, objective){
  if (objective == "min") {
    opt <- objectOpt[,-1]
  }else{
    opt <- apply(objectOpt[,-1], 2, function(x) 1-x)
  }
  apply(opt, 2, function(opt){
    if (min(opt) > 0.05) {
      ncomp = which(opt == min(opt))[1]
    }else{
      ncomp = which(opt <= 0.05)[1]
    }
    if(length(ncomp) > 1){ncomp = ncomp[1]}
    return(ncomp)
  })
}
# FUNCTION: plot the diagnostics along the components
plotDiagnostic <- function(diagnostic, title = ""){
  diagnosticPlot <- pivot_longer(diagnostic, cols = -Component, names_to = "Distance") %>% as.data.frame()
  gg <- ggplot(data = diagnosticPlot, aes(x = Component, y = value, group = Distance)) +
    geom_point(aes(color=Distance)) +
    geom_line(aes(color=Distance)) + 
    ggtitle(title) +
    theme(title = element_text(size = 10)) +
    theme(plot.title = element_text(hjust = 0.5))
  gg
}
# FUNCTION: extract matrices for prediction plot
getPredictionMatrices <- function(plsdaPerf, Y, ncompOpt, dist, id){
  predictions <- map(plsdaPerf, function(x){map(x, function(x) {
    x <- x %>% as_tibble() %>% dplyr::select(any_of(ncompOpt)) %>% cbind(as_tibble(Y)) %>% dplyr::arrange(., value) %>% dplyr::pull(1) %>% as.character()
  })})
  Y <- sort(Y)
  
  max.dist.predictions = centroids.dist.predictions = mahalanobis.dist.predictions = list()
  for (i in 1:length(Y)) {
    max.dist.predictions[[i]] <- map(predictions$max, i) %>% do.call(cbind, .)
    centroids.dist.predictions[[i]] <- map(predictions$centroids, i) %>% do.call(cbind, .)
    mahalanobis.dist.predictions[[i]] <- map(predictions$mahalanobis, i) %>% do.call(cbind, .)
  }
  all.predictions <- list(
    "max.dist.predictions" = do.call(rbind, max.dist.predictions) %>% set_rownames(Y) %>% as_tibble(),
    "centroids.dist.predictions" = do.call(rbind, centroids.dist.predictions) %>% set_rownames(Y) %>% as_tibble(),
    "mahalanobis.dist.predictions" = do.call(rbind, mahalanobis.dist.predictions) %>% set_rownames(Y) %>% as_tibble()
  )
  
  dist.prediction.selected <- all.predictions %>% extract2(dist)
  predictionSummary <- apply(dist.prediction.selected, 1, function(x){
    sumLevels = c()
    for (i in 1:nlevels(Y)) {
      sumLevels[i] <- sum(x == levels(Y)[i])
    }
    return(sumLevels)
  }) %>% t() %>% set_colnames(levels(Y)) %>% as_tibble() %>% mutate(., "ID" = id, "order" = 1:length(Y))
  
  colours <- predictionSummary %>% cbind(., Y)  
  for (i in 1:nlevels(Y)) {
    green <- which(pull(colours, Y) == levels(Y)[i])
    red <- which(pull(colours, Y) != levels(Y)[i])
    colours[,i][green] = "Correctly classified"
    colours[,i][red] = "Wrongly classified"
  }
  colours <- colours %>% as_tibble() %>% dplyr::select(-Y)
  
  return(list("predictionSummary" = predictionSummary, "colours" = colours, "Y" = Y))
}
# FUNCTION: Calculate position for prediction plot
calculateMeans <- function(rep, Y){
  positionFirst <- rep/2 + 1
  positionRest = c()
  for (i in 1:(length(levels(Y))-1)) {
    positionRest[i] <- rep * i + i/2 + positionFirst
  }
  means <- c(positionFirst, positionRest)
  Ymeans <- Y %>% as.character()
  for (i in 1:length(means)) {
    Ymeans[Ymeans == levels(Y)[i]] <- means[i]
  }
  Ymeans <- Ymeans %>% as.numeric
  return(Ymeans)
}
# FUNCTION: Prediction plot
predictionPlot <- function(dataPredictions, sizeXaxis, sizeYaxis, sizeLegendTitle, sizeLegendLevels, sizeSegment){
  dataPredictions <- dataPredictions %>% mutate(ID = as.character(ID))
  breaksPlot <- dataPredictions$mean %>% as.factor() %>% levels() %>% as.numeric()
  pp <- ggplot(dataPredictions) +
    geom_segment(aes(x = min, y = order, xend = max, yend = order, color = colour), linewidth = sizeSegment) + 
    scale_y_discrete() +
    scale_x_continuous(breaks = breaksPlot, labels = levels(Y)) +
    scale_color_manual(values = c("darkgreen", "darkred")) +
    theme_bw() +
    theme(axis.text.x = element_text(size = sizeXaxis)) +
    theme(axis.text.y = element_text(size = sizeYaxis)) +
    theme(legend.title = element_text(size = sizeLegendTitle)) + 
    theme(legend.text = element_text(size = sizeLegendLevels)) +
    guides(col = guide_legend(title = "Legend")) + 
    xlab("")
  
  cutoffX = 1; cutoffY = 0
  for (i in 1:(nlevels(Y)-1)) {
    cutoffY <- cutoffY + summary(Y)[i]
    if (i > 1) {incr = rep + 0.5} else{incr = rep}
    cutoffX <- cutoffX + incr  
    pp <- pp + geom_hline(yintercept = cutoffY + 0.5, linetype = "dashed")
    pp <- pp + geom_vline(xintercept = cutoffX + 0.25, linetype = "dashed")
  }
  return(pp)
}













