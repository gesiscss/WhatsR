#' @title Lexical Disperson plots for Keywords in WhatsApp chatlogs
#' @description Visualizes the occurance of specific keywords within the chat
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param keywords A vector of keywords to be displayed, default is c("hello",world")
#' @param return.data Default is FALSE, returns dataframe of plot when TRUE.
#' @param ... Further arguments passed down to code{\link[qdap]{dispersion_plot}}
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom qdap term_match
#' @importFrom qdap dispersion_plot
#' @export
#' @return Lexical Dispersion plots for specified keywords
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_lexical_dispersion(data, keywords = c("smilies","handy"))


######################## lexical dispersion plots for specific words
plot_lexical_dispersion <- function(data,
                                    names = "all",
                                    starttime = anytime("1960-01-01 00:00"),
                                    endtime = Sys.time(),
                                    keywords = c("hello","world"),
                                    return.data = FALSE,
                                    ...) {

  # switch off useless warning message
  defaultW <- getOption("warn")
  options(warn = -1)

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  `keyword` <- NULL

  # transferring keywords to lowercase to make it non case-sensitive
  keywords <- tolower(keywords)

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {

    starttime <- min(data$DateTime)

  } else {starttime <- anytime(starttime, asUTC = TRUE)}

  # setting endtime
  if (difftime(Sys.time(),endtime, units = "min") < 1) {

    endtime <- max(data$DateTime)

  } else {endtime <- anytime(endtime, asUTC = TRUE)}

  # setting names argument
  if (length(names) == 1 && names == "all") {

    # All names in the dataframe except System Messages
    names <- unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  # New Solution

  if (length(unlist(term_match(data$Message, keywords))) == 0) {

    stop("Keyword is not contained in the chat, try another keyword")

  }

  # make plot
  out <- with(data , suppressWarnings(dispersion_plot(Message, keywords ,grouping.var = list(Sender),...)))
  print(out)

  #switching on warnings again
  options(warn = defaultW)

  if (return.data == TRUE) {

    return(data)

  } else {

    return(out)

  }

}
