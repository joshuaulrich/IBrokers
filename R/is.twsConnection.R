is.twsConnection <-
function (x) 
{
    inherits(x, "twsConnection") || inherits(x, "twsconn")
}
