rankall <- function(outcome,num="best"){
    bd <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # Convert in numeric   
    bd[,11] <- as.numeric(bd[,11])
    bd[,17] <- as.numeric(bd[,17])
    bd[,23] <- as.numeric(bd[,23])
    
    if (outcome=="heart attack") {
        temp <- bd[!is.na(bd$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
        if(num=="best")
        {
            #Get the min
            s<- split(temp,temp$State)
            vari<- lapply(s, function(x) x[x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),c(2,7)])
        }
        else if(num=="worst"){
            #Get the max
            s<- split(temp,temp$State)
            vari<- lapply(s, function(x) x[x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==max(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),c(2,7)])
        }
        else {
            # sort by outcome and name
            if(is.numeric(num))
            {
                temp <- temp[with(temp,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)),]
                s<- split(temp,temp$State)
                vari<- lapply(s, function(x) x[num,c(2,7)])
            }        
            else
            {
                stop("invalid num")
            }
        }    

        vari
    }
    else if (outcome=="heart failure") {
        temp <- bd[!is.na(bd$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
        if(num=="best")
        {
            #Get the min
            s<- split(temp,temp$State)
            vari<- lapply(s, function(x) x[x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),c(2,7)])
        }
        else if(num=="worst"){
            #Get the max
            s<- split(temp,temp$State)
            vari<- lapply(s, function(x) x[x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==max(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),c(2,7)])
        }
        else {
            # sort by outcome and name
            if(is.numeric(num))
            {
                temp <- temp[with(temp,order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)),]
                s<- split(temp,temp$State)
                vari<- lapply(s, function(x) x[num,c(2,7)])
            }        
            else
            {
                stop("invalid num")
            }
        }    
        
        vari
    }
    else if (outcome=="pneumonia") {
        temp <- bd[!is.na(bd$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
        if(num=="best")
        {
            #Get the min
            s<- split(temp,temp$State)
            vari<- lapply(s, function(x) x[x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),c(2,7)])
        }
        else if(num=="worst"){
            #Get the max
            s<- split(temp,temp$State)
            vari<- lapply(s, function(x) x[x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==max(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),c(2,7)])
        }
        else {
            # sort by outcome and name
            if(is.numeric(num))
            {
                temp <- temp[with(temp,order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)),]
                s<- split(temp,temp$State)
                vari<- lapply(s, function(x) x[num,c(2,7)])
            }        
            else
            {
                stop("invalid num")
            }
        }    
        
        vari
    } 
    
    else {
        stop("invalid outcome")
    }
}


as.character(subset(r, state == "NV")$hospital)