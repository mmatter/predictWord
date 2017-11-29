

phrase <- "the main goal of this job"
history <- strsplit(phrase, " ")[[1]]


pred <- NULL
morder <- 4
while(is.null(pred)) {
  
  if (morder > 1) {
    
    subhistory <- paste0(tail(history, n=morder-1), collapse=" ")
    row <- which(dimnames(NGR_model[[morder]])[[1]] == subhistory)
    if (length(row) > 0) {
      #pred <- names(which.max(NGR_model[[morder]][row,]))
      pred <- head(sort(NGR_model[[morder]][row,], decreasing = TRUE))
      print(paste0("Used model: ", names(NGR_model)[morder]))
    } else {
      morder <- morder - 1 
    }
  } else {
    #pred <- names(which.max(NGR_model$unigrams))
    pred <- head(sort(NGR_model$unigrams, decreasing = TRUE))
  }
}

pred

