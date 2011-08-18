eWrapper.MktDepth.CSV <- function() {
  eW <- eWrapper(NULL)
  eW$updateMktDepth <- function(curMsg, msg, timestamp, file, ...) {
    #symbols <- eW$get.Data("symbols")
    e_update_mkt_depth_csv(NULL, msg, timestamp, file, ...)
  }
  eW
}

e_update_mkt_depth_csv <- function (msg, contents, timeStamp, file, ...) 
{
    id <- as.numeric(contents[2])
    file <- file[[id]]
    cat(id,",",
        as.character(timeStamp),",",
        contents[3],",",  # position
        contents[4],",",  # 0=insert, 1=update, 2=delete (operation)
        contents[5],",",  # 1=bid, 2=ask (side)
        contents[6],",",  # price
        contents[7],      # size
        "\n",
        sep="",file=file,append=TRUE)
}

