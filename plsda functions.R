library(mixOmics)
my.plsda <- plsda(X,Y,ncomp =6)
perf.plsda <- perf(my.plsda, validation = "loo", progressBar = T)
plot(perf.plsda)
#############################################################################################################
# PLS-DA construction
#############################################################################################################

# ----------------- Load inputs ----------------- #
library(caret); library(tidyverse); library(plyr); library(splitTools); library(foreach); library(magrittr); library(sjmisc)
data <- read_excel("./data.xlsx")
Y <- data$Style %>% as.factor()
X <- data[,-c(1:7)] %>% scale()
ncompMax <- 6 # Max number of components to optimize

type <- "loo" # Mfold / loo

typePartition <- "stratified" # stratified, basic, grouped, blocked

folds = 6
rep = 5

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
ncompOptimal <- rbind(noptimalBER, noptimalErrorClass, noptimalAccuracy) %>% set_rownames(c("BER", str_glue("Error {classErrorPlot} class"), "Accuracy"))

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

