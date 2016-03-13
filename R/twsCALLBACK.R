twsCALLBACK <-
function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{
    if (missing(eWrapper)) 
        eWrapper <- eWrapper()
    con <- twsCon[[1]]
    if (inherits(twsCon, "twsPlayback")) {
        stop("No playback support")
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
            print(e)
            stop("IB connection error. Connection closed", call. = FALSE)
        })
    }
}
