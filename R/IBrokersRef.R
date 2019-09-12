IBrokersRef <- function() {
  ref <- system.file("doc/IBrokersREFCARD.pdf", package = "IBrokers")
  if (.Platform$OS.type == "windows") {
    shell.exec(ref)
  } else {
    system(paste(shQuote(getOption("pdfviewer")), shQuote(ref)),
      wait = FALSE
    )
  }
}
