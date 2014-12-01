complete <- function(directory, idVect = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        ## Initialize 
        id <- numeric()
        nobs <- numeric()
        ##complCases["id"] <- id
        ##complCases["nobs"] <- nobs
                
        ## Loop through the monitors
        for (i in idVect) {
                
                ## Build file name
                monNum <- formatC(i, width = 3, flag = "0") 
                datFile <- paste(directory, "/", monNum, ".csv", sep = "")
                
                ## Read Data
                dat <- read.csv(datFile)
                
                ## Determine number of complete cases
                isCompl <- complete.cases(dat)
                complDat <- dat[isCompl,]
                numComplete <- nrow(complDat)
                
                ## Append Data
                id <- c(id, i)
                nobs <- c(nobs, numComplete)
                
        }
        
        ## build the data frame
        complCases <- data.frame(id,nobs)
        return(complCases)
        
}