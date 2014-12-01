corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        ## Include the complete.R package
        source("complete.R")
        
        ## Initialize vectors
        nitrate <- numeric()
        sulfate <- numeric()
        correls <- numeric()
        
        ## Count files in directory
        fileList <- list.files(directory)
        fileCount <- NROW(fileList)
        
        ## Get a list of complete cases for all files in the directory
        compCases <- complete("specdata", 1:fileCount)
        ctr <- NROW(compCases)
        
        for (i in 1:ctr) {
                
                if (compCases[i,"nobs"] > threshold){
                        ## Build file name
                        monNum <- formatC(i, width = 3, flag = "0") 
                        datFile <- paste(directory, "/", monNum, ".csv", sep = "")
                        
                        ## Read Data
                        tempDat <- read.csv(datFile)
                        isCompl <- complete.cases(tempDat)
                        dat <- tempDat[isCompl,]
                        nitrate <- dat[,"nitrate"]
                        sulfate <- dat[,"sulfate"]
                        
                        ## Compute the correlation
                        corCoeff <- cor(nitrate, sulfate)
                        
                        ## Append to the return result
                        correls <- c(correls, corCoeff)
                }
        }
        
        ## Return the correlations
        return(correls)
        
}