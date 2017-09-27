library(stringdist)

data_set <- read.csv(file="/home/xavier/git-repos/free-text-destination-classifier/dataset_002.csv", stringsAsFactors = FALSE)
bic_dictionary <- read.csv(file="/home/xavier/git-repos/free-text-destination-classifier/FI_20170902.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
bic_dictionary$PHYSICAL.ADDRESS <- apply(bic_dictionary[,c("PHYSICAL.ADDRESS.1", "PHYSICAL.ADDRESS.2", "PHYSICAL.ADDRESS.3", "PHYSICAL.ADDRESS.4")], MARGIN=1, FUN=function(x) paste(x[!is.na(x)], collapse = " "))

training_idx <- sample(seq_len(nrow(data_set)), size=floor(0.75 * nrow(data_set)))
training_set <- data_set[training_idx,]
training_set$PHYSICAL.ADDRESS <- training_set$CLEAR
test_set <- data_set[-training_idx,]

add_features <- function(x, d1, d2) {
  result <- x
  result$dist1 <- stringdist(d1$NOISED, d2$PHYSICAL.ADDRESS, method = "lv")
  result$dist2 <- stringdist(d1$NOISED, d2$PHYSICAL.ADDRESS, method = "qgram")
  result$dist3 <- stringdist(d1$NOISED, d2$PHYSICAL.ADDRESS, method = "cosine")
  return(result)
}

#training_set$dist1 <- stringdist(training_set$NOISED, training_set$PHYSICAL.ADDRESS, method = "lv", weight = c(d = 1, i = 1, s = 1, t = 1))
#training_set$dist2 <- stringdist(training_set$NOISED, training_set$PHYSICAL.ADDRESS, method = "qgram")
#training_set$dist3 <- stringdist(training_set$NOISED, training_set$PHYSICAL.ADDRESS, method = "cosine")

training_set <- add_features(training_set, training_set, training_set)

# TODO: normalize data
training_means <- colMeans(training_set[,c("dist1", "dist2", "dist3")])
training_sds <- sapply(training_set[,c("dist1", "dist2", "dist3")], sd)
#training_set[,c("dist1", "dist2", "dist3")] - rep(means, rep.int(nrow(training_set), ncol(training_set[,c("dist1", "dist2", "dist3")])))

normalize <- function(x, columns, means, sds) {
  features <- x[, columns]
  n <- (features - rep(means, rep.int(nrow(x), ncol(features)))) / rep(sds, rep.int(nrow(x), ncol(features)))
  result <- x
  result[columns] <- n
  return(result)
}

normalized_training_set <- normalize(training_set,c("dist1", "dist2", "dist3"), training_means, training_sds)

# build model
fit.lm = lm(MATCH ~ dist1 + dist2 + dist3, data=normalized_training_set)

classify <- function(fit, x, means, sds) {
  result <- matrix(, nrow = nrow(x), ncol = 1)
  for(i in 1:nrow(x)) {
    #input <- matrix(, nrow = nrow(bic_dictionary), ncol = 0)
    input <- bic_dictionary
    
     #input$dist1 <- stringdist(x[i,]$NOISED, bic_dictionary$PHYSICAL.ADDRESS, method = "lv", weight = c(d = 1, i = 1, s = 1, t = 1))
    #input$dist2 <- stringdist(x[i,]$NOISED, bic_dictionary$PHYSICAL.ADDRESS, method = "qgram")
    #input$dist3 <- stringdist(x[i,]$NOISED, bic_dictionary$PHYSICAL.ADDRESS, method = "cosine")
    input <- add_features(input, x[i,], bic_dictionary)
    normalized_input <- normalize(input, c("dist1", "dist2", "dist3"), means, sds)
    
    result_i <- predict(fit, normalized_input)
    result[i,1] <- bic_dictionary[which.min(abs(result_i - 1)), "BIC.CODE"]
  }
  
  return(result)
}

test_set$OUTPUT <- classify(fit.lm, test_set, training_means, training_sds)
score = sum(test_set$OUTPUT == test_set$BIC, na.rm=TRUE)/nrow(test_set)