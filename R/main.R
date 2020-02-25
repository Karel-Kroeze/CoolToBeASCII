#' Create an ASCII representation of a string
#'
#' Uses http://artii.herokuapp.com/ API, so requires an active internet connection.
#'
#' @param text A string to be ASCII-fied
#' @param font The font to use. See http://artii.herokuapp.com/fonts_list for a comprehensive list of available fonts.
#' @export
#' @example
#' ASCIIfy( "test" )
ASCIIfy <- function( text, font = NULL ){
    url <- paste0( "http://artii.herokuapp.com/make?text=", utils::URLencode( text ) )
    if( !is.null( font ) ) url <- paste0( url, "&font=", utils::URLencode( font ) )
    res <- httr::GET( url )
    if (res$status_code != 200){
        stop(res)
    }
    return( content( res, encoding = "UTF-8" ) )
}

#' Prints a super cool ASCII title in the font of your choice
#'
#' Uses http://artii.herokuapp.com/ API, so requires an active internet connection.
#'
#' @param text A string to be ASCII-fied
#' @param font The font to use. See http://artii.herokuapp.com/fonts_list for a comprehensive list of available fonts.
#' @export
#' @examples
#' CoolToBeASCII("Step 1: Do something")
#' CoolToBeASCII("Step 2: ???", font = "slanted" )
#' CoolToBeASCII("Step 3: Profit", font = "starwars" )
CoolToBeASCII <- function( text, font = NULL ){
    raw <- ASCIIfy( text, font )
    cat( raw )
}
