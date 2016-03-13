eWrapper.RealTimeBars <-
function (nbars = 1, nsymbols = 1) 
{
    eW <- eWrapper(NULL)
    eW$realtimeBars <- function(curMsg, msg, timestamp, file, 
        ...) {
        id <- as.numeric(msg[2])
        data <- eW$get.Data("data")
        data[[id]][1] <- as.numeric(msg[3])
        data[[id]][2:8] <- as.numeric(msg[4:10])
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    return(eW)
}
