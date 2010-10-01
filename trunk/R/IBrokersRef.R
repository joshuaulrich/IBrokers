IBrokersRef <- function() {
  ref <- system.file("doc/IBrokersRef.pdf",package="IBrokers")
  if(.Platform$OS.type == "windows") 
    shell.exec(ref)
  else system(paste(shQuote(getOption("pdfviewer")), shQuote(ref)), 
              wait = FALSE)
}
