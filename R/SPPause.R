#' SPPause
#'
#' Function which pauses for a keypress.
#' This function is derived from https://diego.assencio.com/?index=86c137b502561d44b8be02f06d80ee16, accessed 16.01.2018.
#' @keywords pause, keypress
#' @export
#' @examples
#' SPPause()

SPPause = function()
{
    if (interactive())
    {
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }
    else
    {
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
}