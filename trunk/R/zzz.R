.IBrokersEnv <- new.env()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("IBrokers version 0.9-10.  Implementing API Version 9.64")
  packageStartupMessage("\nIBrokers comes with NO WARRANTY.  Not intended for production use!\n\n")
  packageStartupMessage("See ?IBrokers for details.")
}

.onLoad <- function(libname,pkgname) {
}

