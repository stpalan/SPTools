#' cleanup
#'
#' Function which cleans up the GlobalEnv by removing all objects whose name commences with "t."
#' @keywords rm
#' @export
#' @examples
#' Instead of writing:
#' rm(list=ls(pattern="^t."))
#' one can write
#' cleanup()

cleanup<-function(){
    objs <- ls(pos = ".GlobalEnv")
    rm(list = objs[grep("^t.", objs)], pos = ".GlobalEnv")
}