# Build a n-order transition matrix based on n+1 grams

ngram.model <- function(n, minocc, text) {
    
    if (n == 1) {
        counts <- sort(table(text), decreasing = TRUE)
        counts_freq <- counts[counts >= minocc]
        P <- counts_freq/sum(counts_freq)
        
    } else {
        
        ngr <- ngrams(text, shift=n-1)
        counts <- sort(table(ngr), decreasing = TRUE)
        
        #minocc <- 2 #round(0.000001*length(ngr))
        counts_freq <- counts[counts >= minocc] # keeping only n-grams appearing more than a very few times
        
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
    }
    return(P)
}