#' cleanup
#'
#' Function which cleans up the GlobalEnv by removing all objects whose name commences with "t."
#' @param pattern A character vector containing the RegEx patterns of the object names to remove. Make sure to surround names by "^...$" if you want to match the name exactly.
#' @keywords rm
#' @export
#' @examples
#' Instead of writing:
#' rm(list=ls(pattern="^t[.]"))
#' one can write
#' cleanup()
#'
#' You can also expressly specify objects to clean up:
#' cleanup(c("^temp_var$","^x$"))

cleanup<-function(pattern="^t[.]"){
    for(p in pattern){
        objs <- ls(pos = ".GlobalEnv")
        rm(list = objs[grep(p, objs)], pos = ".GlobalEnv")
    }
}