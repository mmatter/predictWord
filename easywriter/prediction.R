
# NGR_all <- list()
# load("learned_data/learned_news_10000.Rdata")
# NGR_all[[1]] <- NGR_model
# rm(NGR_model)
# load("learned_data/learned_blogs_300000.Rdata")
# NGR_all[[2]] <- NGR_model
# rm(NGR_model)
# load("learned_data/learned_twitter_100000.Rdata")
# NGR_all[[3]] <- NGR_model
# rm(NGR_model)


# nouts - number of outputs
# phrase <- "i am very happy to in"
# output: nouts top words 

prediction <- function(phrase, NGR_all, nouts=5) {
    
    # splitting phrase:
    splitted_bylast <- strsplit(phrase, " (?=[^ ]+$)", perl=TRUE)[[1]]
    splitted_byall <- strsplit(phrase, " ")[[1]]
    
    if (length(splitted_bylast) == 2) { # the next word is being typed
        beg_nxt <- splitted_bylast[2]
        history <- strsplit(splitted_bylast[1], " ")[[1]]
    } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) > 0) { # phrase ends by a space
        beg_nxt <- ""
        history <- splitted_byall
    } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) == 0) { # the first word is being typed
        beg_nxt <- splitted_bylast[1]
    }
    
    pred_all <- lapply(NGR_all, FUN=function(NGR) { # NGR_all=list(NGR_news, NGR_twitter, NGR_blogs)
        stupid_backoff <- 0.4
        morder_max <- 4
        backoff <- list()
        for (morder in morder_max:2) {
            
            if (beg_nxt != "") {
                retain <- grep(pattern=paste0("^", beg_nxt), dimnames(NGR[[morder]])[[2]]) # if beg_nxt is empty, all words are matched
            } else {
                retain <- !(dimnames(NGR[[morder]])[[2]] %in% c("<UNK>"))
            }
            subhistory <- paste0(tail(history, n=morder-1), collapse=" ")
            row <- which(dimnames(NGR[[morder]])[[1]] == subhistory)
            if (length(row) > 0) {
                backoff[[morder]] <- stupid_backoff^(morder_max-morder)*NGR[[morder]][row, retain]
            } else {
                backoff[[morder]] <- NA
            }
        }
        if (beg_nxt != "") {
            retain <- grep(pattern=paste0("^", beg_nxt), names(NGR[[1]])) # if beg_nxt is empty, all words are matched
        } else {
            retain <- !(names(NGR[[1]]) %in% c("<UNK>"))
        }
        backoff[[1]] <- stupid_backoff^(morder_max-1)*NGR[[1]][retain]
        
        pred <- NULL
        for (morder in morder_max:1) {
            if (!is.na(backoff[[morder]][1])) {
                if (is.null(pred)) {
                    pred <- backoff[[morder]][backoff[[morder]] > 0]
                } else {
                    pred <- c(pred, backoff[[morder]][backoff[[morder]] > 0 & !(names(backoff[[morder]]) %in% names(pred))])
                }
            }
        }
        pred <- head(sort(pred, decreasing = TRUE), n=20)
        
        return(pred)
    })
    
    pred_merged <- unlist(pred_all)
    pred_final <- NULL
    for (word in unique(names(pred_merged))) {
        pred_final[word] <- sum(pred_merged[names(pred_merged) == word])
    }
    pred_final <- head(sort(pred_final, decreasing = TRUE), n=nouts)
    
    return(pred_final)
}