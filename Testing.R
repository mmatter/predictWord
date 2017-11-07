
source("ngrams.R")
source("predict.R")

load("learned_data/learned_en_US_100000.Rdata")

set.seed(456)

n <- 3 # verifying on n-grams
nsam_test <- 5000
ngr <- ngrams(TEST$news, shift=n-1)
ngrams_test <- ngr[sample(1:length(ngr), nsam_test)]

splitted <- do.call(rbind, strsplit(ngrams_test, ' (?=[^ ]+$)', perl=TRUE))
test_in <- splitted[, 1]
names(test_in) <- NULL
test_out <- splitted[, 2]
names(test_out) <- NULL

pred <- sapply(test_in, FUN=function(text, NGR) { names(predict(text, NGR)$prediction[1]) }, NGR)

accuracy <- sum(pred == test_out)/nsam_test
print(accuracy)