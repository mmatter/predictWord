
source("ngrams.R")
source("easywriter/prediction.R")

NGR_all <- list()
load("learned_data/learned_news_300000.Rdata")

NGR_all[[1]] <- NGR_model
rm(NGR_model)
load("learned_data/learned_blogs_300000.Rdata")
NGR_all[[2]] <- NGR_model
rm(NGR_model)
load("learned_data/learned_twitter_100000.Rdata")
NGR_all[[3]] <- NGR_model
rm(NGR_model)

ACCS <- list("acc" = NULL, "acc3" = NULL, "acc3part" = NULL)

#set.seed(4945)

niter <- 10
n <- 4 # verifying on n-grams
nsam_test <- 500
ngr <- ngrams(TEST, shift=n-1)

for (j in 1:niter) {
    
    ngrams_test <- ngr[sample(1:length(ngr), nsam_test)]
    names(ngrams_test) <- NULL
    splitted <- do.call(rbind, strsplit(ngrams_test, ' (?=[^ ]+$)', perl=TRUE))
    indices <- which(splitted[, 2] != "<UNK>")
    
    test_in <- splitted[indices, 1]
    names(test_in) <- NULL
    test_out <- splitted[indices, 2]
    names(test_out) <- NULL
   
    # Most likely prediction:
    pred <- sapply(paste0(test_in, " "), FUN=function(text, NGR) { names(prediction(text, NGR, 1)) }, NGR_all)
    ACCS$acc[j] <- sum(pred == test_out)/length(indices)
    rm(pred)
    # 3 most likely predictions:
    pred <- lapply(paste0(test_in, " "), FUN=function(text, NGR) { names(prediction(text, NGR, 3)) }, NGR_all)
    counter <- 0
    for (i in 1:length(indices)) {
        if (test_out[i] %in% pred[[i]]) {
            counter <- counter + 1
        }
    }
    ACCS$acc3[j] <- counter/length(indices)
    rm(pred, test_in, test_out)
    # with first letter known:
    test_in <- paste(splitted[indices, 1], substr(splitted[indices,2], 1, 1))
    names(test_in) <- NULL
    test_out <- splitted[indices, 2]
    names(test_out) <- NULL
    
    pred <- lapply(test_in, FUN=function(text, NGR) { names(prediction(text, NGR, 3)) }, NGR_all)
    counter <- 0
    for (i in 1:length(indices)) {
        if (test_out[i] %in% pred[[i]]) {
            counter <- counter + 1
        }
    }
    ACCS$acc3part[j] <- counter/length(indices)
    
}

save(ACCS, file="accuracy.Rdata")