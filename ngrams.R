
ngrams <- function(seq, shift=1) {
    seqshifted <- c(tail(seq, n=-1), rep(NA, 1))
    seqshifted2 <- c(tail(seq, n=-2), rep(NA, 2))
    seqshifted3 <- c(tail(seq, n=-3), rep(NA, 3))
    if (shift == 1) {
        output <- mapply(FUN=function(w1, w2) { paste(w1, w2, sep=" ") },
                         seq, seqshifted)
    } else if (shift == 2) {
        output <- mapply(FUN=function(w1, w2, w3) { paste(w1, w2, w3, sep=" ") },
                         seq, seqshifted, seqshifted2)
    } else if (shift == 3) {
        output <- mapply(FUN=function(w1, w2, w3, w4) { paste(w1, w2, w3, w4, sep=" ") },
                         seq, seqshifted, seqshifted2, seqshifted3)
    } else {
        stop("Not allowed value for argument 'shift' !")
    } 
    return(head(output, n=-shift))
}