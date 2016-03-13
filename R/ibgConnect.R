ibgConnect <-
function (clientId = 1, host = "localhost", port = 4001, verbose = TRUE, 
    timeout = 5, filename = NULL, blocking = .Platform$OS.type == 
        "windows") 
{
    twsConnect(clientId, host, port, verbose, timeout, filename)
}
