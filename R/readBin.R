# This is a buffering version of readBin for what=character()
# It was written to workaround an implementation bug in R's
# socket connections, but it can't yet climb that hurdle.
#
# for now it is best to use Unix...

.readBin <-
function(x, what, n) {
  buffer <- try(base::readBin(x, raw(), n=4096), silent=TRUE)
  cur.buffer <- if(exists(".buffer", .GlobalEnv)) {
                  get(".buffer",.GlobalEnv) 
                } else {
                  NULL
                }
  if(!inherits(buffer, "try-error"))
    assign(".buffer",c(cur.buffer,buffer),.GlobalEnv)
  buffer  <- base::readBin(get(".buffer", .GlobalEnv), raw(), 4096)
  max.n <- length(nulbytes <- which(buffer == as.raw(0))) 
  n <- min(n, max.n)
  ret <- base::readBin(buffer, character(), n)
  # truncate buffer
  if(length(buffer) > 0 && length(buffer) >= nulbytes[n])
    assign(".buffer", buffer[-c(1:nulbytes[n])], .GlobalEnv)
  return(ret)
}

