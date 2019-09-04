#' SPdhms
#'
#' Function to transform seconds into "days hours:minutes:seconds" format. Taken from https://stackoverflow.com/questions/27312292/convert-seconds-to-days-hoursminutesseconds.
#' @param x A numeric containing the number of seconds to transform.
#' @keywords date, time, transformation, seconds, minutes, hours, days
#' @export
#' @examples
#' SPdhms(48000)
#' "0 13:20:00"

SPdhms=function(t){
    paste(t %/% (60*60*24) 
          ,paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
                 ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
                 ,formatC(t %% 60, width = 2, format = "d", flag = "0")
                 ,sep = ":"
          )
    )
}