rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Subset data to selected state
        stateDat <- dat[(dat[ , 7] == toupper(state)), ]
        
        
        ## Return hospital name in that state with lowest 30-day death rate
        ## bestHospital <- min(dat[ochdr], na.rm = TRUE)
        ochdr <- paste("Hospital 30-Day Death (Mortality) Rates from", outcome, sep = " ")
        ucochdr <- toupper(ochdr)
        
        ## Get the column index for the appropriate outcome
        colidx <- which(colnames(stateDat) == ucochdr)
        
        ## Rank the data
        rankDat <- stateDat[order(stateDat[,colidx], stateDat[,2]),c(2,colidx) ]
        
        ## Append the rank order
        numReturned <- nrow(rankDat)
        rankOrderVector <- 1:numReturned
        rankDat <- cbind(rankDat,rankOrderVector)

        ## Add column names
        colnames(rankDat) <- c("Hospital.Name", "Rate", "Rank")
        
        ## Return the appropriate ranked hospital
        if (is.numeric(num) && num > numReturned) {
                return("NA")
        } else if (num == "best") {
                idx <- 1
        } else if (num == "worst") {
                worstVal <- max(rankDat[, 2], na.rm = TRUE)
                worstDat <- rankDat[which(rankDat["Rate"] == worstVal), ]
                idx <- max(worstDat[,"Rank"], na.rm = TRUE)
        } else {
                idx <- num
        }

        ##rankDat <- stateDat[with(stateDat, order(stateDat[,colidx], stateDat[,2])), ]
        return(rankDat[idx, 1])
        
}