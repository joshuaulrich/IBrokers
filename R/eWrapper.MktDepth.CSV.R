eWrapper.MktDepth.CSV <-
function () 
{
    eW <- eWrapper(NULL)
    eW$updateMktDepth <- function(curMsg, msg, timestamp, file, 
        ...) {
        e_update_mkt_depth_csv(NULL, msg, timestamp, file, ...)
    }
    eW
}
