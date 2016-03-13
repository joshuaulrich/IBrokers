reqHistory <-
function (conn, Contract, barSize = "1 min", ...) 
{
    if (barSize == "1 min") {
        endDateTime <- Sys.Date() - seq(360, 0, -5)
        duration <- "5 D"
    }
    else if (barSize == "15 mins") {
        endDateTime <- Sys.Date() - seq(360, 0, -10)
        duration <- "10 D"
    }
    reqHistoricalData(conn, Contract, barSize = barSize, duration = duration, 
        endDateTime = endDateTime, ...)
}
