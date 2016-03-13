twsDEBUG <-
function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{
    eWrapper <- eWrapper(debug = TRUE)
    twsCALLBACK(twsCon, eWrapper, timestamp, file, playback, 
        ...)
}
