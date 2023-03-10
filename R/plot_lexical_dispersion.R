#' @title Lexical Disperson plots for Keywords in WhatsApp chatlogs
#' @description Visualizes the occurrence of specific keywords within the chat
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param keywords A vector of keywords to be displayed, default is c("hello",world").
#' @param return.data Default is FALSE, returns data frame of plot when TRUE.
#' @param excludeSM If TRUE, excludes the WhatsApp System Messages from the descriptive statistics. Default is FALSE.
#' @param ... Further arguments passed down to \code{\link[qdap]{dispersion_plot}}.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom qdap term_match
#' @importFrom qdap dispersion_plot
#' @export
#' @return Lexical Dispersion plots for specified keywords
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_lexical_dispersion(data, keywords = c("auch"))
######################## lexical dispersion plots for specific words
plot_lexical_dispersion <- function(data,
                                    names = "all",
                                    starttime = anytime("1960-01-01 00:00"),
                                    endtime = Sys.time(),
                                    keywords = c("hello", "world"),
                                    return.data = FALSE,
                                    excludeSM = FALSE,
                                    ...) {

  # checking for column names of senders
  if (!("Sender" %in% colnames(data))) {
    colnames(data)[colnames(data) == "Anonymous"] <- "Sender"
  }

  # catching bad params
  # start- and endtime are POSIXct
  if (is(starttime, "POSIXct") == F) stop("starttime has to be of class POSIXct.")
  if (is(endtime, "POSIXct") == F) stop("endtime has to be of class POSIXct.")
  # names in data or all names
  if (!("all" %in% names) & any(!names %in% data$Sender)) stop("names has to either be \"all\" or a vector of names to include.")
  # return.data must be bool
  if (!is.logical(return.data)) stop("return.data has to be either TRUE or FALSE.")
  # keywords must be in data
  if (is(keywords, "character") == F) stop("keywords has to be a character and a vector.")
  # excludeSM must be bool
  if (!is.logical(excludeSM)) stop("excludeSM has to be either TRUE or FALSE.")


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
  } else {
    starttime <- anytime(starttime, asUTC = TRUE)
  }

  # setting endtime
  if (difftime(Sys.time(), endtime, units = "min") < 1) {
    endtime <- max(data$DateTime)
  } else {
    endtime <- anytime(endtime, asUTC = TRUE)
  }

  # setting names argument
  if (length(names) == 1 && names == "all") {
    if (excludeSM == TRUE) {
      # All names in the dataframe except System Messages
      names <- unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

      # dropping empty levels
      if (is.factor(names)) {
        names <- droplevels(names)
      }
    } else {
      # including system messages
      names <- unique(data$Sender)
    }
  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ] # THIS IS WHERE THE ERROR OCCURS

  # New Solution

  if (length(unlist(term_match(data$Message, keywords))) == 0) {
    stop("Keyword is not contained in the chat, try another keyword")
  }

  # make plot
  out <- with(data, suppressWarnings(dispersion_plot(Message, keywords, grouping.var = list(Sender), ...)))
  print(out)

  # switching on warnings again
  options(warn = defaultW)

  if (return.data == TRUE) {
    return(data)
  } else {
    return(out)
  }
}
