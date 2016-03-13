eWrapper.RealTimeBars.CSV <-
function (n = 1) 
{
    eW <- eWrapper(NULL)
    eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 
        7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", 
        "Low", "Close", "Volume", "WAP", "Count")))), n))
    eW$realtimeBars <- function(curMsg, msg, timestamp, file, 
        ...) {
        id <- as.numeric(msg[2])
        file <- file[[id]]
        data <- eW$get.Data("data")
        attr(data[[id]], "index") <- as.numeric(msg[3])
        nr.data <- NROW(data[[id]])
        cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], 
            msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
        data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    return(eW)
}
