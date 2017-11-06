
# phrase - input phrase
# nouts - number of outputs

# output: nouts top words 

# phrase <- "there is "

predict <- function(phrase, NGR, nouts=3) {
    
    # splitting phrase:
    splitted_bylast <- strsplit(phrase, " (?=[^ ]+$)", perl=TRUE)[[1]]
    splitted_byall <- strsplit(phrase, " ")[[1]]
                                            
    if (length(splitted_bylast) == 2) { # the next word is being typed
        beg_nxt <- splitted_bylast[2]
    } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) > 0) { # phrase ends by a space
        beg_nxt <- NULL
    } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) == 0) { # the first word is being typed
        beg_nxt <- splitted_bylast[1]
    }
    
    if (is.null(beg_nxt)) {
        history <- splitted_byall
        PREDS <- lapply(NGR, FUN=function(s, history, nouts) {
            mapply(FUN=function(P, ord, history, nouts) {
                numord <- 0:3
                names(numord) <- c("prior", "P1", "P2", "P3")
                if (ord == "prior") {
                    pred <- head(P, n=nouts)
                } else {
                    subphrase <- paste0(tail(history, n=numord[ord]), collapse=" ")
                    if (subphrase %in% dimnames(P)[[1]]) {
                        pred <- head(sort(P[subphrase, ], decreasing=TRUE), n=nouts)
                    } else {
                        pred <- NULL
                    }
                }
                return(pred)
            }, s, names(s), MoreArgs = list(history, nouts), SIMPLIFY = FALSE)
        }, history, nouts)
        
        PRED_smerged <- list() # PRED_smerged used for testing/verification only
        pred_final <- NULL
        for (ord in c("P3", "P2", "P1", "prior")) {
            pred_all <- c(PREDS$news[[ord]], PREDS$blogs[[ord]], PREDS$twitter[[ord]])
            if (length(pred_all) > 0) {
                n_occ <- table(names(pred_all))
                avg_prob <- sapply(unique(names(pred_all)), FUN=function(w, pred_all) {
                    mean(pred_all[names(pred_all) == w]) 
                    }, pred_all)
            } else {
                n_occ <- NA
                avg_prob <- NA
            }
            PRED_smerged[[ord]] <- list("freq"=sort(n_occ, decreasing = TRUE), "prob"=sort(avg_prob, decreasing = TRUE))
            if (is.null(pred_final) & !is.na(n_occ)[1]) {
                pred_final <- head(sort(n_occ[sort(names(n_occ))]*avg_prob[sort(names(n_occ))], decreasing = TRUE), n=nouts)
            }
        }

    } else {
        # history <- head(splitted_byall, n=-1)
        # # Looking recursively if the subphrase exist in the n-gram model:
        # pred <- NULL
        # for (i in 3:1) { 
        #     indices <- grep(pattern=paste0("^", beg_nxt), dimnames(NGR[[i]])[[2]]) # if beg_nxt is empty, all words are matched
        #     if (is.null(pred)) {
        #         subphrase <- paste0(tail(history, n=i), collapse=" ")
        #         if (subphrase %in% dimnames(NGR[[i]])[[1]]) {
        #             # transition probabilities from subphrase to a word starting with beg_nxt:
        #             potwords <- dimnames(NGR[[i]])[[2]][indices]
        #             p1 <- NGR[[i]][subphrase, potwords]
        #             # probability of having a word starting with beg_nxt after subphrase:
        #             p2 <- sum(NGR[[i]][subphrase, potwords])
        #             if (p2 > 0) {
        #                 tmp <- p1/p2
        #                 names(tmp) <- potwords
        #                 pred <- head(sort(tmp, decreasing = TRUE), n=nouts)
        #                 print(paste("ngram exists for i =", i))
        #             }
        #         } 
        #     }
        # }
        # 
        # # if the last word of phrase does not exist in dictionary, then suggesting the most likely word starting with beg_nxt from the dictionary
        # if (is.null(pred)) { 
        #     indices2 <- grep(pattern=paste0("^", beg_nxt), names(prior))
        #     p1 <- prior[indices2]
        #     p2 <- sum(prior[indices2])
        #     pred <- head(sort(p1/p2, decreasing = TRUE), n=nouts)
        #     print("ngram doesn't exists")
        # }
        pred_final <- NULL
    }
    
    return(list("test"=PRED_smerged, "prediction"=pred_final))
    
 
}