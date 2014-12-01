rankall <- function(outcome, num = "best") {
        
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
        ##suppressWarnings(dat[,7] <- as.factor(dat[,7]))
        suppressWarnings(dat[,11] <- as.numeric(dat[,11]))
        suppressWarnings(dat[,17] <- as.numeric(dat[,17]))
        suppressWarnings(dat[,23] <- as.numeric(dat[,23]))
        
        ## Get a list of states from the data
        uoStateList <- unique(dat[,7])
        stateList <- sort(uoStateList)
        
        ## Check that outcome is valid
        
        ## Validate outcome
        foundOutcome <- grep(outcome, tNameList, ignore.case = TRUE)
        if (length(foundOutcome) == 0) {
                stop("invalid outcome")
        }    
     
        ## Find the right column name for the outcome
        ochdr <- paste("Hospital 30-Day Death (Mortality) Rates from", outcome, sep = " ")
        ucochdr <- toupper(ochdr)
        
        ## Get the column index for the appropriate outcome
        colidx <- which(colnames(dat) == ucochdr)
        
        ## Iterate over the list of states
        numStates <- length(stateList)
        stateRanks <- data.frame(hospital = character(0), state = character(0), rate = numeric(0), rank = numeric(0))
        colnames(stateRanks) <- c("hospital", "state", "rate", "rank")
        
        for (i in 1:numStates) {
                ## Return hospital name in that state with lowest 30-day death rate
                ## bestHospital <- min(dat[ochdr], na.rm = TRUE)
                
                ## Subset the data for the staten
                state <- stateList[i]
                stateDat <- dat[(dat[ , 7] == toupper(state)), ]
                
                ## Rank the data
                rankDat <- stateDat[order(stateDat[,7], stateDat[,colidx], 
                                     stateDat[,2]),c(2,7,colidx) ]
                
                ## Append the rank order
                numReturned <- nrow(rankDat)
                rankOrderVector <- 1:numReturned
                rankDat <- cbind(rankDat,rankOrderVector)
                
                ## Add column names
                colnames(rankDat) <- c("hospital", "state", "rate", "rank")
                
                ## Return the appropriate ranked hospital
                if (is.numeric(num) && num > numReturned) {
                        rankDat[1, ] <- c("NA", state, "0", "NA")
                        stateRanks <- rbind(stateRanks, rankDat[1, c(1:4)])
                        next
                } else if (num == "best") {
                        idx <- 1
                } else if (num == "worst") {
                        worstVal <- max(rankDat[, 3], na.rm = TRUE)
                        worstDat <- rankDat[which(rankDat["rate"] == worstVal), ]
                        idx <- max(worstDat[,4], na.rm = TRUE)  
                        
                } else {
                        idx <- num
                }
                
                ## Check that the value for the state rank returned isn't NA, then append
                ## to the stateRanks data frame
                if (!is.na(rankDat[idx,3])){
                        stateRanks <- rbind(stateRanks, rankDat[idx, c(1:4)])
                }
        }
     
        return(stateRanks[ ,c(1,2)])
}