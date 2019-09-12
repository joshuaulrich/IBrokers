`reqMktDepth` <-
  function(conn, Contract, tickerId = "1", numRows = "20",
             timeStamp = TRUE, playback = 1,
             file = "", verbose = TRUE,
             eventWrapper = eWrapper(),
             CALLBACK = twsCALLBACK, ...) {
    if (!is.twsConnection(conn)) {
      stop("tws connection object required")
    }

    if (!is.twsPlayback(conn)) {
      # if playback from a file, don't test or require contract
      if (class(Contract) == "twsContract") Contract <- list(Contract)

      for (n in 1:length(Contract)) {
        if (class(Contract[[n]]) != "twsContract") {
          stop("twsContract required")
        }
      }
    }

    con <- conn[[1]]
    if (!isOpen(con)) {
      stop("connection to TWS has been closed")
    }

    cancelMktDepth <- function(con, tickerId) {
      if (inherits(con, "sockconn")) {
        for (i in 1:length(tickerId)) {
          writeBin(.twsOutgoingMSG$CANCEL_MKT_DEPTH, con)
          writeBin("1", con)
          writeBin(tickerId[i], con)
        }
      } else {
        # reset to beginning of file
        seek(con, 0)
      }
    }

    if (!is.character(timeStamp) & timeStamp) {
      timeStamp <- "%Y%m%d %H:%M:%OS"
    } else {
      timeStamp <- NULL
    }

    if (is.null(CALLBACK)) {
      CALLBACK <- twsDEBUG
    }

    VERSION <- "3"

    ticker_id <- as.character(tickerId)

    if (inherits(con, "sockconn")) {
      # write to live TWS Connection
      for (n in 1:length(Contract)) {
        signals <- c(
          .twsOutgoingMSG$REQ_MKT_DEPTH,
          VERSION,
          ticker_id,
          Contract[[n]]$symbol,
          Contract[[n]]$sectype,
          Contract[[n]]$expiry,
          Contract[[n]]$strike,
          Contract[[n]]$right,
          Contract[[n]]$multiplier,
          Contract[[n]]$exch,
          Contract[[n]]$currency,
          Contract[[n]]$local,
          numRows
        )


        for (i in 1:length(signals)) {
          writeBin(signals[i], con)
        }
        ticker_id <- as.character(as.numeric(tickerId) + n)
      }
      msg_expected_length <- NA
    } else {
      msg_expected_length <- as.numeric(readBin(con, character(), 1))
    }

    if (!missing(CALLBACK) && is.na(list(CALLBACK))) {
      if (is.twsPlayback(conn)) {
        seek(conn[[1]], 0)
        stop("CALLBACK=NA is not available for playback")
      }
      return(as.character(as.numeric(tickerId):length(Contract)))
    }
    on.exit(cancelMktDepth(con, as.character(as.numeric(tickerId):length(Contract))))

    CALLBACK(conn,
      eWrapper = eventWrapper, timestamp = timeStamp, file = file,
      playback = playback, ...
    )
  }

`cancelMktDepth` <- function(conn, tickerId) {
  if (!inherits(conn, "twsConnection")) {
    stop("twsConnection object required")
  }

  con <- conn[[1]]

  for (i in 1:length(tickerId)) {
    writeBin(.twsOutgoingMSG$CANCEL_MKT_DEPTH, con)
    writeBin("1", con)
    writeBin(tickerId[i], con)
  }
}
