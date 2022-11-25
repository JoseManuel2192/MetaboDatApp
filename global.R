#########################
# Function: Load data
#########################
load_file <- function(name, path){
  ext <- tools::file_ext(name)
  switch (ext,
          xls = readxl::read_xls(paste(path, sep = ""), 1),
          xlsx = readxl::read_xlsx(paste(path, sep = ""), 1),
          validate("Invalid file; Please upload a .xls or .xlsx file")
  )
}

############################
# Grouping by level factors
############################
groupSummary <- function(data, summaryFunction, factors){
  summaryFunction <- enquo(summaryFunction)
  data %>% 
    group_by(., .[,factors]) %>% 
    summarise_each(funs(!!summaryFunction))
}

#########################
# Scatter plot
#########################
# scatterPlot <- function(scatterData, labelSize = 3, pointSize = 3, legendSize = 14, pch, col, titleScatter = "",
#                         Xcomp = 1, Ycomp = 2, Y, labels = "None", segment.size = 0.1, declutterLabels = 10){
#   scatter <- ggplot(scatterData, aes(x = scatterData[,Xcomp], y = scatterData[,Ycomp], label = labels))  +
#     geom_point(aes(color = Y, shape = Y), size = pointSize) +
#     theme_bw() +
#     theme(legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
#     theme(legend.title = element_text(size = legendSize),
#           legend.text = element_text(size = legendSize / 1.3)) +
#     labs(color = "Legend", shape = "Legend") +
#     scale_shape_manual(values = pch) +
#     scale_color_manual(values = col) +
#     ggtitle(titleScatter) + theme(plot.title = element_text(hjust = 0.5)) +
#     xlab(colnames(scatterData)[Xcomp]) +
#     ylab(colnames(scatterData)[Ycomp]) +
#     theme(axis.text.y = element_text(angle = 90)) +
#     geom_hline(yintercept = 0, linetype="dashed") +
#     geom_vline(xintercept = 0, linetype="dashed")
#   # Add labels of the samples
#   if (length(labels) == 1) {
#     scatter
#   }else{
#     scatter <- scatter +
#       geom_text_repel((aes(color = Y)), size = labelSize, show.legend = F, max.overlaps = declutterLabels, segment.size = segment.size,
#                       max.time = 0.5, max.iter = 2000, min.segment.length = 0.1, force = 1, force_pull = 5)
#   }
#
#   return(scatter)
# }

#########################
# Loadings plot
#########################
# loadingsPlot <- function(loadingsData, labelSize = 3, pointSize = 3, legendSize = 14, title = "", segment.size = 0.02,
#                          Xcomp = 1, Ycomp = 2, Y, labels = F, fixAxis = T, radInn = 0.5, declutterLabels = 10, filterVar = F){
#   if (filterVar == T) {
#     radMatrix <- sqrt((loadingsData[,Xcomp])^2 + loadingsData[,Ycomp]^2) %>% as.data.frame()
#     loadingsData <-  filter(loadingsData, radMatrix > radInn)
#   }
#   loadings <- ggplot(loadingsData, aes(x = loadingsData[,Xcomp], y = loadingsData[,Ycomp], label = rownames(loadingsData)))  +
#     geom_point(color = "darkblue", size = pointSize) +
#     theme_bw() +
#     ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
#     xlab(colnames(loadingsData)[Xcomp]) +
#     ylab(colnames(loadingsData)[Ycomp]) +
#     theme(axis.text.y = element_text(angle = 90)) +
#     geom_hline(yintercept = 0, linetype="dashed") +
#     geom_vline(xintercept = 0, linetype="dashed")
#   # Add labels of the variables
#   if (labels == T) {
#     loadings <- loadings +
#       geom_text_repel(size = labelSize, show.legend = F, max.overlaps = declutterLabels, segment.size = segment.size, max.time = 1,
#                       max.iter = 10000, min.segment.length = 0.02, force = 10, force_pull = 10)
#   }
#   # Correlation  plot
#   if (fixAxis == T) {
#     loadings <- loadings + scale_y_continuous(limits=c(-1,1)) + scale_x_continuous(limits=c(-1,1)) +
#       geom_circle(n = 80, aes(x0 = 0, y0 = 0, r = 1, linewidth = 0.3)) +
#       geom_circle(n = 80, aes(x0 = 0, y0 = 0, r = radInn, linewidth = 0.3))
#   }
#
#   return(loadings)
# }

#########################
# Box plot
#########################

# boxplotFunction = function(dataBoxplot, setcolors, rotate = F, size_axis, size_label_axis, height, width, x_label_angle, order, hline, yname = "Variable", violinPlot = F,
#                            dotPlot = "None"){
#   if (order == "increasing") {
#     boxplot <- ggplot(dataBoxplot, aes(x=reorder(x,y), y=y, fill = x))
#   }else{
#     boxplot <- ggplot(dataBoxplot, aes(x=x, y=y, fill = x))
#   }
#   # Violin  or box plot
#   if (violinPlot == T) {
#     boxplot <- boxplot + geom_violin()
#   }else{
#     boxplot <- boxplot + geom_boxplot()
#   }
#   # Include dots
#   if (dotPlot == "Dot") {
#     boxplot <- boxplot + geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1))
#   }
#   if (dotPlot == "Jitter") {
#     boxplot <- boxplot + geom_jitter(shape=16, position=position_jitter(0.2))
#   }
#   boxplot <- boxplot + theme(plot.title = element_text(hjust = 0.35, face = "bold"))
#   # scale_y_continuous(limits=c(-2,12.5))
#   #
#   # if (exists("hline")) {
#   #   boxplot = boxplot + geom_hline(yintercept = hline, colour = "darkred", linetype = "dashed")
#   # }
#   #
#   boxplot <- boxplot + labs(x="", y = yname) +
#     theme(axis.text=element_text(size=size_axis, face="bold", colour = "black"),
#           axis.title=element_text(size=size_label_axis,face="bold"))
#   boxplot <- boxplot  + theme(legend.position="none")  #+ scale_fill_manual(values = setcolors)
#
#   if (rotate == T) {
#     boxplot <- boxplot + coord_flip() + theme(axis.text.y = element_text(angle = x_label_angle, hjust = 1),
#                                               axis.text.x = element_text(angle = 0))
#   } else{
#     boxplot <- boxplot + theme(axis.text.x = element_text(angle = x_label_angle, hjust = 1),
#                                axis.text.y = element_text(angle = 90, hjust = 0.3))
#   }
#   return(boxplot)
# }


#########################
# ANOVA
#########################
# ANOVA <- function(data, factors, doInteractions = T){
# 
#   posResponses <- lapply(data, is.numeric) %>% unlist() %>% which(.)
#   responses <- colnames(data)[posResponses]
#   responsesRenamed <- paste0("Feat_", 1:length(responses)); colnames(data)[posResponses] = responsesRenamed # Changed to avoid naming problems
#   restFactors <- data %>% dplyr::select(-all_of(posResponses)) %>% colnames()
#   posFactors <- restFactors %in% factors %>% which(.)
#   factors <- restFactors[posFactors] # Fix order of factors
#   ini <- length(restFactors) + 1
#   fin <- ncol(data)
# 
#   anovaResults <- list()
#   for(i in ini:fin){
#     variable <- responsesRenamed[i - ini + 1]
# 
#     # Get formula
#     mainEffects <- paste0(factors, "+")[-length(factors)] %>% append(factors[length(factors)])
#     if (doInteractions == F | length(factors) == 1) {
#       .f <- c(variable, "~", mainEffects) %>% paste0(., collapse = " ") %>% as.formula()
#     }else{
#       interactions <- paste0(factors, "*")[-length(factors)] %>% append(factors[length(factors)])
#       .f <- c(variable, "~", mainEffects, "+", interactions) %>% paste0(. ,collapse = " ") %>% as.formula()
#     }
# 
#     # AOV
#     modelAOV <- aov(.f, data = data)
# 
#     # ANOVA
#     anovaResults[[i - ini + 1]] <- rstatix::anova_summary(modelAOV)
#     rmPosHoc <- which(anovaResults[[i - ini + 1]] %>% dplyr::select(p) > 0.05) # Pos-hoc letter to be removed
# 
#     # Main Effects
#     posHocFeat <- list()
#     for (j in 1:length(factors)) {
#       posHocFeat[[j]] <- HSD.test(modelAOV, factors[j], group = T, console = F)$groups %>% mutate(rownames(.)) %>%
#         setNames(., c("Feat", "groups", "names")) %>% arrange(., names) %>% t() %>% as.tibble() %>% slice(2)
#       if (j %in% rmPosHoc) { # Remove letters from pos-hoc when p-value > 0.05
#         posHocFeat[[j]] <- posHocFeat[[j]] %>% mutate_all(funs(str_replace(., ., " ")))
#       }
#     }
# 
#     # Interactions
#     if (doInteractions == T & length(factors) > 1) {
#       posHocFeat[[j+1]] <- HSD.test(modelAOV, factors, group = T, console = F)$groups %>% mutate(rownames(.)) %>%
#         setNames(., c("Feat", "groups", "names")) %>% arrange(., names) %>% t() %>% as.tibble() %>% slice(2)
#       if (c(j+1) %in% rmPosHoc) { # Remove letters from pos-hoc when p-value > 0.05
#         posHocFeat[[j+1]] <- posHocFeat[[j+1]] %>% mutate_all(funs(str_replace(., ., " ")))
#       }
#     }
# 
#     # rbind in a list separated by effect type
#     if (i == ini) {
#       posHoc <- posHocFeat
#     }else{
#       posHoc <- map2(posHoc, posHocFeat, rbind)
#     }
#   }
#   names(anovaResults) <- responsesRenamed
# 
#   # p-values to astherics
#   pvaluesAst <- pvalues <- lapply(anovaResults, function(x) x%>% as_tibble() %>% dplyr::select(p)) %>% sapply(., unlist) %>% t() %>% round(.,3)
#   pvaluesAst[pvalues <= 0.05 & pvalues > 0.01] <- "*"
#   pvaluesAst[pvalues <= 0.01 & pvalues > 0.001] <- "**"
#   pvaluesAst[pvalues <= 0.001] <- "***"
#   pvaluesAst[pvalues > 0.05] <- "ns"
#   if (length(factors) == 1){pvaluesAst <- t(pvaluesAst)}
# 
#   rownames(pvaluesAst) <- responses
# 
#   return(list("posHoc" = posHoc, "pvaluesAst" = pvaluesAst))
# }
# 
# # ####################################
# # # Mean, SD, fit decimals
# # ####################################
# mean_sd <- function(data, factors, foldChange = F){
#   posResponses <- lapply(data, is.numeric) %>% unlist() %>% which(.)
#   restFactors <- data %>% dplyr::select(-all_of(posResponses)) %>% colnames()
#   posFactors <- restFactors %in% factors %>% which(.)
#   factors <- restFactors[posFactors] # Fix order of factors
# 
#   # Main effects: means and SD
#   tableMeans = tableSD = list()
#   for (i in 1:length(posFactors)) {
#     tableMeans[[i]] <- data %>% group_by(data %>% dplyr::select(posFactors[i])) %>% summarise_all(mean) %>% t() %>% as.data.frame() %>%
#       purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
#     tableSD[[i]] <- data %>% group_by(data %>% dplyr::select(posFactors[i])) %>% summarise_all(sd) %>% t() %>% as.data.frame() %>%
#       purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
#   }
# 
#   # Interaction effect (number of factors is > 1): means and SD
#   if (length(posFactors) > 1) {
#     tableMeans[[i+1]] <- data %>% group_by(data %>% dplyr::select(all_of(posFactors))) %>% summarise_all(mean) %>% unite("Interactions", 1:2, remove = T, sep = " x ") %>%
#       t() %>% as.data.frame() %>% purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
#     tableSD[[i+1]] <- data %>% group_by(data %>% dplyr::select(all_of(posFactors))) %>% summarise_all(sd) %>% unite("Interactions", 1:2, remove = T, sep = " x ") %>%
#       t() %>% as.data.frame() %>% purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
#   }
# 
#   # For decimal fit -> extract a vector with the lower mean values of each effect. Extracted here to prevent problems if
#   # the means are changed (eg. when using foldchange).
#   minMean <- tableMeans %>% lapply(., function(x) apply(x, 2, as.numeric) %>% abs() %>% apply(., 1, min)) # Absolute value for negative variables
#   minMean <- do.call(cbind, minMean) %>% apply(., 1, function(x){ # Avoid problems in rows with all the values = 0
#     if (sum(x[x>0]) == 0){0} else{min(x[x>0])}})
#   maxMean <- tableMeans %>% lapply(., function(x) apply(x, 2, as.numeric) %>% apply(., 1, max)) # Extracts max means (for the nex function)
# 
#   # Foldchanges relative to the maximum mean value of each effect
#   if (foldChange == T) {
#     for (i in 1:length(tableSD)) {tableSD[[i]] <- apply(tableSD[[i]], 2, as.numeric) / maxMean[[i]]} # Foldchange of dev
#     tableMeans <- tableMeans %>% lapply(., function(x) apply(x, 2, as.numeric) %>% apply(., 1, function(x) x/max(x)) %>% t())# %>% round(2)) # Foldchange means & round(2)
#   }
# 
#   # Decimal fit (continue)
#   # Assign minMean to a vector from the min non-zero value across all mean effects
#   decimals <- minMean %>% replace(., .<= 0.0001, 5/100000) %>% replace(., .> 0.0001 & .<= 0.001, 4/100000) %>% replace(., .> 0.001 & .<= 0.08, 3/100000) %>%
#     replace(., .> 0.08 & .<= 2, 2/100000) %>% replace(., .> 2 & .<= 15, 1/100000) %>% replace(., .> 15, 0)
#   decimals <- decimals * 100000
#   if (foldChange == T) {decimals <- decimals %>% replace(., is.numeric(.), 2)}
# 
#   return(list("tableMeans" = tableMeans, "tableSD" = tableSD, "decimals" = as.data.frame(decimals)))
# }
# #
# # ############################################
# # # ANOVA format (fitting decimals)
# # ############################################
# formatMeanSD <- function(output_mean, output_SD, decimalsFitted){
#   tableMeans = output_mean
#   tableSD = output_SD
# 
#   # Fitting format
#   tableMeans <- lapply(tableMeans, function(x){
#     for(i in 1:nrow(x)){x[i,] <- x[i,] %>% as.numeric() %>% formatC(., format = "f", digits = decimalsFitted[i,])}
#     return(x)
#   })
#   tableSD <- lapply(tableSD, function(x){
#     for(i in 1:nrow(x)){x[i,] <- x[i,] %>% as.numeric() %>% formatC(., format = "f", digits = decimalsFitted[i,])}
#     return(x)
#   })
# 
#   return(list("tableMeans" = tableMeans, "tableSD" = tableSD))
# }
# 
# ####################################
# # Compile ANOVA table
# ####################################
# compileANOVA <- function(tableMeans, tableSD, posHoc, pvaluesAst, showSD = F){
#   # Building the final table
#   for (i in 1:length(posHoc)) {
#     # Add SD
#     if (showSD == T) {tableMaking <- tableMeans[[i]] %>% paste.matrix(., tableSD[[i]], sep = " ± ")}else
#     {tableMaking <- tableMeans[[i]]}
# 
#     tableMaking <- tableMaking %>% paste.matrix(., posHoc[[i]], sep = "") %>%
#       cbind(., pvaluesAst[,i])
#     colnames(tableMaking) <- c(tableMeans[[i]] %>% colnames(.), "p-value")
#     if (i == 1) {
#       tableANOVA <- tableMaking
#     }else{
#       tableANOVA <- cbind(tableANOVA, tableMaking)
#     }
#     rownames(tableANOVA) <- rownames(pvaluesAst) # Original rownames (since in mean_sd are changed to prevent caption problems)
#   }
#   return(tableANOVA)
# }
# 
# ####################################
# # BarPlot
# ####################################
# barPlot <- function(data, xcomp, ycomp, fill, position, stat, alpha, legendSize = 15, jitter = F, size_Y_title, size_label_axis, x_label_angle,
#                     showSD = F){
#   
#   factors <- colnames(data)[c(xcomp,fill)]
#   mean <- groupSummary(data = data, summaryFunction = mean, factors = factors) %>% data.frame()
#   sd <- groupSummary(data = data, summaryFunction = sd, factors = factors) %>% data.frame()
#   
#   if (length(factors) > 2) {}
#   if (length(factors) == 1) {
#     barPlot <- ggplot(data = mean, aes(x = mean[,1], y =  mean[,ycomp], fill = mean[, 1]))
#   }else{
#     barPlot <- ggplot(data = mean, aes(x = mean[,1], y =  mean[,ycomp], fill = mean[, 2]))
#   }
#   
#   barPlot <- barPlot +
#     geom_bar(stat = stat, position = position_dodge(), alpha = alpha) +
#     theme_bw() +
#     xlab("") + ylab(colnames(mean)[ycomp]) + 
#     ggtitle(title) + 
#     theme(plot.title = element_text(hjust = 0.5)) +
#     theme(legend.title = element_text(size = legendSize), legend.text = element_text(size = legendSize / 1.3)) + 
#     labs(color = "Legend", shape = "Legend") + labs(fill = "Legend") +
#     theme(axis.text=element_text(size=size_Y_title, face="bold", colour = "black"), axis.title=element_text(size=size_label_axis,face="bold")) +
#     theme(axis.text.x = element_text(angle = x_label_angle, hjust = 1), axis.text.y = element_text(angle = 90, hjust = 0.3, size = size_label_axis/1.2))
#   if (jitter == T) {
#     barPlot <- barPlot + geom_jitter(shape=16, position=position_jitter(0.2))
#   }
#   if (showSD == T) {
#     lower <- mean %>% pull(ycomp) - sd %>% pull(ycomp)
#     upper <- mean %>% pull(ycomp) + sd %>% pull(ycomp)
#     barPlot <- barPlot + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position=position_dodge(.9))
#   }
#   barPlot 
# }