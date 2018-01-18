corr <- function(directory,threshold=0) {
     df = complete(directory)
     ids = df[df["nobs"] > threshold, ]$id
     corr1 = numeric()
     for (i in ids) {
          newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = ""))
          dff = newRead[complete.cases(newRead), ]
          corr1 = c(corr1, cor(dff$sulfate, dff$nitrate))
     }
     return(corr1)
}