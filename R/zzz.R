`.onLoad` <- function(libname,pkgname) {
  packageStartupMessage("IBrokers version 0.9-7.  Implementing API Version 9.64")
  do.call("cat",list("\nIBrokers comes with NO WARRANTY.  Not intended for production use!\n\n"))
  packageStartupMessage("See ?IBrokers for details.")
}
