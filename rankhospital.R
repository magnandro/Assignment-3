rankhospital <- function(state, outcome,num="best"){
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
            if(num=="best")
            {
                #Get the min
                vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),] 
            }
            else if(num=="worst"){
                #Get the max
                vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==max(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),] 
            }
            else {
                # sort by outcome and name
                temp <- temp[with(temp,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)),]
                if(is.numeric(num))
                {
                    vari <- temp[num,]
                }        
                else
                {
                    stop("invalid num")
                }
            }    
            
            vari$Hospital.Name
        }
        else if (outcome=="heart failure") {
            temp <- newbd[!is.na(newbd$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
            if(num=="best")
            {
                #Get the min
                vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),] 
            }
            else if(num=="worst"){
                #Get the max
                vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==max(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),] 
            }
            else {
                # sort by outcome and name
                temp <- temp[with(temp,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)),]
                if(is.numeric(num))
                {
                    vari <- temp[num,]
                }        
                else
                {
                    stop("invalid num")
                }
            }    
            
            vari$Hospital.Name
        }
        else if (outcome=="pneumonia") {
            temp <- newbd[!is.na(newbd$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
            if(num=="best")
            {
                #Get the min
                vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),] 
            }
            else if(num=="worst"){
                #Get the max
                vari <- temp[temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==max(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),] 
            }
            else {
                # sort by outcome and name
                temp <- temp[with(temp,order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)),]
                if(is.numeric(num))
                {
                    vari <- temp[num,]
                }        
                else
                {
                    stop("invalid num")
                }
            }    
            
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