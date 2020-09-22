best <- function(state, outcome){
    bd <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # Convert in numeric   
    bd[,11] <- as.numeric(bd[,11])
    bd[,17] <- as.numeric(bd[,17])
    bd[,23] <- as.numeric(bd[,23])

    # Get a list of states names
    vec_states<-unique(bd$State)
    
    # if the state var is a valid state name
    if (length(vec_states[vec_states==state])>0){
        newbd <- bd[which(bd$State==state),]
        if (outcome=="heart attack") {
            temp <- newbd[!is.na(newbd$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
            vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),] 
            vari$Hospital.Name
        }
        else if (outcome=="heart failure") {
            temp <- newbd[!is.na(newbd$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
            vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),] 
            vari$Hospital.Name
        }
        else if (outcome=="pneumonia") {
            temp <- newbd[!is.na(newbd$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
            vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),] 
            vari$Hospital.Name
        } 
           
        else {
            stop("invalid outcome")
        }
    }   
    
    else
    {
        stop("invalid state")
    }
}

best("TX", "heart failure")

