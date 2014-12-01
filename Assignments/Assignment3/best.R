## Finds the best hospital in a state for a given outcome
best <- function(state, outcome) {
                ## Read outcome data
        datFile <- "outcome-of-care-measures.csv"
        dat <- read.table(datFile, header = TRUE, colClasses = "character", sep = ",", 
                          check.names = FALSE)
        
        ## Clean up column names (strip extraneous spaces and convert 
        ## to upper for search purposes); convert columns 11,17,23 (30 day mortality data)
        ## to numeric columns
        nameList <- names(dat)
        tNameList <- gsub("  ", " ", nameList)
        tNameList <- toupper(tNameList)
        colnames(dat) <- tNameList
        suppressWarnings(dat[,11] <- as.numeric(dat[,11]))
        suppressWarnings(dat[,17] <- as.numeric(dat[,17]))
        suppressWarnings(dat[,23] <- as.numeric(dat[,23]))
        
        ## Check that state and outcome are valid
        
        ## Validate State 
        foundState <- dat[dat[, 7] == state, ]
        if (nrow(foundState) == 0) {
                stop("invalid state")
        }
        
        ## Validate outcome
        foundOutcome <- grep(outcome, tNameList, ignore.case = TRUE)
        if (length(foundOutcome) == 0) {
                stop("invalid outcome")
        }
        
        ## Subset data to selected state; sort the state data by hospital name
        
        stateDat <- dat[(dat[ , 7] == toupper(state)), ]
        stateDat <- stateDat[with(stateDat, order(stateDat[,2])), ]
                
        ## Return hospital name in that state with lowest 30-day death rate
        ## bestHospital <- min(dat[ochdr], na.rm = TRUE)
        ochdr <- paste("Hospital 30-Day Death (Mortality) Rates from", outcome, sep = " ")
        ucochdr <- toupper(ochdr)
        minForOutcome <- suppressWarnings(min(stateDat[ , ucochdr], 
                             na.rm = TRUE))
        bestDat <- stateDat[which(stateDat[ucochdr] == minForOutcome), ]
        return(bestDat[,2])
        
}
