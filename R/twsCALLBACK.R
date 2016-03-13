twsCALLBACK <-
function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{
    if (missing(eWrapper)) 
        eWrapper <- eWrapper()
    con <- twsCon[[1]]
    if (inherits(twsCon, "twsPlayback")) {
        sys.time <- NULL
        while (TRUE) {
            if (!is.null(timestamp)) {
                last.time <- sys.time
                sys.time <- as.POSIXct(strptime(paste(readBin(con, 
                  character(), 2), collapse = " "), timestamp))
                if (!is.null(last.time)) {
                  Sys.sleep((sys.time - last.time) * playback)
                }
                curMsg <- readBin(con, character(), 1)
                if (length(curMsg) < 1) 
                  next
                processMsg(curMsg, con, eWrapper, format(sys.time, 
                  timestamp), file, ...)
            }
            else {
                curMsg <- readBin(con, character(), 1)
                if (length(curMsg) < 1) 
                  next
                processMsg(curMsg, con, eWrapper, timestamp, 
                  file, ...)
                if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) 
                  Sys.sleep(5 * playback)
            }
        }
    }
    else {
        tryCatch(while (isConnected(twsCon)) {
            if (!socketSelect(list(con), FALSE, 0.25)) 
                next
            curMsg <- readBin(con, "character", 1L)
            if (!is.null(timestamp)) {
                processMsg(curMsg, con, eWrapper, format(Sys.time(), 
                  timestamp), file, twsCon, ...)
            }
            else {
                processMsg(curMsg, con, eWrapper, timestamp, 
                  file, twsCon, ...)
            }
        }, error = function(e) {
            close(twsCon)
            stop("IB connection error. Connection closed", call. = FALSE)
        })
    }
}
