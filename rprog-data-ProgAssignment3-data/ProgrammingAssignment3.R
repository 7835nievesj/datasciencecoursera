#Programming Assignment 3
##R Programming

#1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
nrow(outcome)
names(outcome)

## Histogram of the 30-day death rates from heart attack.(You may get a warning
## about NAs being introduced; that is okay)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

#2
best <- function(state, outcome) {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        validOutcome = c("heart attack","heart failure","pneumonia")
        if (!outcome %in% validOutcome) { stop("invalid outcome")}
        
        validState = unique(data[,7])
        if (!state %in% validState) stop("invalid state")
        
        ## convert outcome name into column name
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        ## Return hospital name in that state with lowest 30-day death rate
        data.state <- data[data$State==state,]
        idx <- which.min(as.double(data.state[,colName]))
        data.state[idx,"Hospital.Name"]
}

#3
##Write a function called __rankhospital__ that takes three arguments: the 
##2-character abbreviated name of a state (state), an outcome (outcome), and the 
##ranking of a hospital in that state for that outcome (num). The function reads 
##the outcome-of-care-measures.csv file and returns a character vector with the 
##name of the hospital that has the ranking specified by the num argument. For 
##example, the callrankhospital("MD", "heart failure", 5)

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        validOutcome = c("heart attack","heart failure","pneumonia")
        if (!outcome %in% validOutcome) { stop("invalid outcome")}
        
        validState = unique(data[,7])
        if (!state %in% validState) stop("invalid state")
        
        ## convert outcome name into column name
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        ## Return hospital name in that state with the given rank 30-day death rate
        data.state <- data[data$State==state,]
        
        # order data by outcome
        sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),
                         data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
        
        #handle num input
        if (num=="best") num = 1
        if (num=='worst') num = nrow(sorted.data.state)
        #will automatically return NA if num > nrow, as well as if it's some other text value
        # if someone passes num < 1, they'll get what's expected
        #if (is.numeric(num) & num > nrwo(sorted.data.state) return(NA)
        
        sorted.data.state[num,"Hospital.Name"]
}

#4  Ranking hospitals in all states

##  Write a function called rankall that takes two arguments: an outcome name 
##  (outcome) and a hospital rank- ing (num). The function reads the 
##  outcome-of-care-measures.csv file and returns a 2-column data frame containing 
##  the hospital in each state that has the ranking specified in num. For example 
##  the function call rankall("heart attack", "best") would return a data frame 
##  containing the names of the hospitals that are the best in their respective states 
##  for 30-day heart attack death rates. The function should return a value for every 
##  state (some may be NA). The first column in the data frame is named hospital, 
##  which contains the hospital name, and the second column is named state, which 
##  contains the 2-character abbreviation for the state name. Hospitals that do not 
##  have data on a particular outcome should be excluded from the set of hospitals 
##  when deciding the rankings.

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",colClasses = "character",
                         na.strings="Not Available")
        
        ## Check that state and outcome are valid
        validOutcome = c("heart attack","heart failure","pneumonia")
        if (!outcome %in% validOutcome) { stop("invalid outcome")}
        
        validState = sort(unique(data[,7]))
        if (!state %in% validState) stop("invalid state")
        
        ## convert outcome name into column name
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        ## For each state, find the hospital of the given rank
        hospital<-character(0)
        
        for (i in seq_along(validState)) {
                ## Return hospital name in that state with the given rank 30-day death rate
                data.state <- data[data$State==validState[i],]
                
                # order data by outcome
                sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),
                                                      data.state[["Hospital.Name"]],
                                                      decreasing=FALSE,na.last=NA), ]
                #handle num input
                this.num = num
                if (this.num=="best") this.num = 1
                if (this.num=='worst') this.num = nrow(sorted.data.state)
                
                hospital[i] <- sorted.data.state[this.num,"Hospital.Name"]
        }
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        data.frame(hospital=hospital,state=validState,row.names=validState)
}


