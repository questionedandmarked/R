add2 <- function(x,y) {
     x+y
}

above10 <- function(x) {
     return <- x > 10
     x[return]
}

above <- function(x,n) {
     oeafr <- x >= n
     x[oeafr]
}

columnmean <- function(z,removeNA=TRUE) {
     columns <- ncol(z)
     means <- numeric(columns)
     for (i in columns) {
     means <- mean(y[,i],na.rm=removeNA)
     }
     means
}