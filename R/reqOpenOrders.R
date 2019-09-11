.reqOpenOrders <- function(twsconn) {
  if (!is.twsConnection(twsconn)) {
    stop("requires twsConnection object")
  }

  con <- twsconn[[1]]

  VERSION <- "1"

  writeBin(c(.twsOutgoingMSG$REQ_OPEN_ORDERS, VERSION), con)
}

reqOpenOrders <- function(twsconn) {
  .reqOpenOrders(twsconn)

  con <- twsconn[[1]]
  eW <- eWrapper()

  while (TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1L)
    processMsg(curMsg, con, eW)
  }
}

.reqAutoOpenOrders <- function(twsconn, bAutoBind = TRUE) {
  if (!is.twsConnection(twsconn)) {
    stop("requires twsConnection object")
  }

  bAutoBind <- as.character(as.integer(bAutoBind))
  con <- twsconn[[1]]

  VERSION <- "1"

  writeBin(c(.twsOutgoingMSG$REQ_AUTO_OPEN_ORDERS, VERSION, bAutoBind), con)
}

.reqAllOpenOrders <- function(twsconn) {
  if (!is.twsConnection(twsconn)) {
    stop("requires twsConnection object")
  }

  con <- twsconn[[1]]

  VERSION <- "1"

  writeBin(c(.twsOutgoingMSG$REQ_ALL_OPEN_ORDERS, VERSION), con)
}
