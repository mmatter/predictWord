# Build a n-order transition matrix based on n+1 grams

ngram.model <- function(n, text) {
    
    ngr <- ngrams(text, shift=n)
    counts <- sort(table(ngr), decreasing = TRUE)
    
    # keeping only n-grams appearing more than a very few times:
    minocc_ngr <- 2 #round(0.000001*length(ngr))
    counts_freq <- counts[counts >= minocc_ngr]
    
    splitted <- do.call(rbind, strsplit(names(counts_freq), ' (?=[^ ]+$)', perl=TRUE))
    wstart <- unique(splitted[, 1])
    wend <- unique(splitted[, 2])
    
    # Initializing transition sparse matrix:
    P <- Matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
    # filling it with counts:
    
    for (i in names(counts_freq)) {
        indices <- strsplit(i, split=" (?=[^ ]+$)", perl=TRUE)[[1]]
        P[indices[1], indices[2]] <- counts_freq[i]
    }
    
    # normalizing rows:
    P <- P/rowSums(P)
    
    return(P)
}