#' cleanup
#'
#' Function which cleans up the GlobalEnv by removing all objects whose name commences with "t."
#' @param pattern A character vector containing the RegEx pattern of the object names to remove. Make sure to surround names by "^...$" if you want to match the name exactly.
#' @keywords rm
#' @export
#' @examples
#' Instead of writing:
#' rm(list=ls(pattern="^t."))
#' one can write
#' cleanup()

<<<<<<< HEAD
cleanup<-function(pattern="^t."){
=======
cleanup<-function(pattern="^t"){
>>>>>>> master
    objs <- ls(pos = ".GlobalEnv")
    rm(list = objs[grep(pattern, objs)], pos = ".GlobalEnv")
}