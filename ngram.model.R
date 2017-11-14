# Build a n-gram model:


ngram.model <- function(n, text) {
    
    NGR_cnts <- list()
    NGR_cnts[["unigrams"]] <- sort(table(text), decreasing = TRUE)
    
    if (n == 1) {
        NGR_model <- NGR_cnts$unigrams/sum(NGR_cnts$unigrams)
    } else {
        
        ngr <- 1:(n-1)
        names(ngr) <- head(c("bigrams", "trigrams", "quadgrams"), n=n-1)
        TMP <- lapply(lapply(ngr, FUN=ngrams, text), function(ngr) {
            counts <- sort(table(ngr), decreasing = TRUE)
            return(counts)
        })
        NGR_cnts <- c(NGR_cnts, TMP)
        
        # Pruning the ngrams sets
        minocc <- list("unigrams"=1, "bigrams"=5, "trigrams"=5, "quadgrams"=5)
        NGR_freq <- mapply(FUN=function(mo, ngr) { ngr[ngr >= mo] }, minocc, NGR_cnts)
        
        NGR_model <- list()
        NGR_model[["unigrams"]] <- NGR_cnts$unigrams/sum(NGR_cnts$unigrams)
        nmodel <- 2:n
        names(nmodel) <- head(c("bigrams", "trigrams", "quadgrams"), n=n-1)
        TMP <- lapply(nmodel, function(m, NGR_freq) {
            
            splitted <- do.call(rbind, strsplit(names(NGR_freq[[m]]), ' (?=[^ ]+$)', perl=TRUE))
            wstart <- unique(splitted[, 1])
            wend <- unique(splitted[, 2])
            # Initializing transition sparse matrix:
            P <- Matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
            # filling it with counts:
            for (i in names(NGR_freq[[m]])) {
                indices <- strsplit(i, split=" (?=[^ ]+$)", perl=TRUE)[[1]]
                P[indices[1], indices[2]] <- NGR_freq[[m]][i]/NGR_freq[[m-1]][indices[1]]
            }
            
            P[, "<UNK>"] <- P[, "<UNK>"] + 1 - rowSums(P) # this is because we have pruned ngrams sets
            
            return(P)
            
        }, NGR_freq)
        
        NGR_model <- c(NGR_model, TMP)
    }
    return(NGR_model)
}