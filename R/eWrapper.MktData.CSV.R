eWrapper.MktData.CSV <-
function (n = 1) 
{
    eW <- eWrapper(NULL)
    eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 
        7), ncol = 7), 0), .Dimnames = list(NULL, c("BidSize", 
        "BidPrice", "AskPrice", "AskSize", "Last", "LastSize", 
        "Volume")))), n))
    eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) {
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- as.numeric(msg[2])
        file <- file[[id]]
        data <- eW$get.Data("data")
        attr(data[[id]], "index") <- as.numeric(Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID) {
            cat(paste(timestamp, msg[5], msg[4], "", "", "", 
                "", "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 1:2] <- msg[5:4]
        }
        else if (tickType == .twsTickType$ASK) {
            cat(paste(timestamp, "", "", msg[4], msg[5], "", 
                "", "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 3:4] <- msg[4:5]
        }
        else if (tickType == .twsTickType$LAST) {
            cat(paste(timestamp, "", "", "", "", msg[4], "", 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 5] <- msg[4]
        }
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    eW$tickSize <- function(curMsg, msg, timestamp, file, ...) {
        data <- eW$get.Data("data")
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- as.numeric(msg[2])
        file <- file[[id]]
        attr(data[[id]], "index") <- as.numeric(Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID_SIZE) {
            cat(paste(timestamp, msg[4], "", "", "", "", "", 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 1] <- msg[4]
        }
        else if (tickType == .twsTickType$ASK_SIZE) {
            cat(paste(timestamp, "", "", "", msg[4], "", "", 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 4] <- msg[4]
        }
        else if (tickType == .twsTickType$LAST_SIZE) {
            cat(paste(timestamp, "", "", "", "", "", msg[4], 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 6] <- msg[4]
        }
        else if (tickType == .twsTickType$VOLUME) {
            cat(paste(timestamp, "", "", "", "", "", "", msg[4], 
                sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 7] <- msg[4]
        }
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    return(eW)
}
