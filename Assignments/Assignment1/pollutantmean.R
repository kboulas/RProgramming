pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        ## Initialize 
        pollutantData <- numeric()
        
        ## Loop through the monitors
        for (i in id) {
        
                ## Build file name
                monNum <- formatC(i, width = 3, flag = "0") 
                datFile <- paste(directory, "/", monNum, ".csv", sep = "")
                
                ## Read Data
                dat <- read.csv(datFile)
        
                ## Append Data
                pollutantData <- c(pollutantData, dat[,pollutant])
                
        }
        
        ## Calculate mean and return
        pollutantMean <- mean(pollutantData, na.rm = TRUE)
        return(pollutantMean)
}