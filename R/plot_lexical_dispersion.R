#' @title Lexical disperson plots for keywords in WhatsApp chat logs
#' @description Visualizes the occurrence of specific keywords within the chat. Requires the raw message content to be contained in the preprocessed data
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}} using anonimize = FALSE or anonimize = "add".
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with WhatsApp timestamps.
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with WhatsApp timestamps.
#' @param keywords A vector of keywords to be displayed, default is c("hello","world").
#' @param return_data Default is FALSE, returns data frame used for plotting when TRUE.
#' @param exclude_sm If TRUE, excludes the WhatsApp System Messages from the descriptive statistics. Default is FALSE.
#' @param ... Further arguments passed down to \code{\link[qdap]{dispersion_plot}}.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom qdap term_match
#' @importFrom qdap dispersion_plot
#' @importFrom methods is
#' @export
#' @return Lexical Dispersion plots for specified keywords
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_lexical_dispersion(data, keywords = c("auch"))

######################## lexical dispersion plots for specific words
plot_lexical_dispersion <- function(data,
                                    names = "all",
                                    starttime = "1960-01-01 00:00",
                                    endtime = as.character(as.POSIXct(Sys.time(),tz = "UTC")),
                                    keywords = c("hello", "world"),
                                    return_data = FALSE,
                                    exclude_sm = FALSE,
                                    ...) {

  # catching bad params

  # checking data
  if(!is.data.frame(data)){stop("'data' must be a dataframe parsed with parse_chat()")}

  # start- and endtime are convertable to POSIXct
  if (is.character(starttime) == FALSE | is.na(anytime(starttime, asUTC=TRUE,tz="UTC"))) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (is.character(endtime) == FALSE | is.na(anytime(endtime, asUTC=TRUE,tz="UTC"))) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (anytime(starttime, asUTC=TRUE,tz="UTC") >= anytime(endtime, asUTC=TRUE,tz="UTC")) stop("starttime has to be before endtime.")

  # Mesage column must be contained
  if (!is.character(data$Flat)) {stop("'data' must contain a character column named 'Message'")}

  # return_data must be bool
  if (!is.logical(return_data)) stop("return_data has to be either TRUE or FALSE.")

  # keywords must be in data
  if (is(keywords, "character") == F) stop("keywords has to be a character and a vector.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  # switch off useless warning message
  defaultW <- getOption("warn")
  options(warn = -1)

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  `keyword` <- NULL

  # transferring keywords to lowercase to make it non case-sensitive
  keywords <- tolower(keywords)

  # setting starttime
  if (anytime(starttime, asUTC=TRUE,tz="UTC") <= min(anytime(data$DateTime, asUTC=TRUE,tz="UTC"))) {
    starttime <- min(anytime(data$DateTime, asUTC=TRUE,tz="UTC"))
  } else {
    starttime <- anytime(starttime, asUTC=TRUE,tz="UTC")
  }

  # setting endtime
  if (anytime(endtime, asUTC=TRUE,tz="UTC") >= max(anytime(data$DateTime, asUTC=TRUE,tz="UTC"))) {
    endtime <- max(anytime(data$DateTime, asUTC=TRUE,tz="UTC"))
  } else {
    endtime <- anytime(endtime, asUTC=TRUE,tz="UTC")
  }

  # setting names argument
  if (length(names) == 1 && names == "all") {
    if (exclude_sm == TRUE) {
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
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ] # TODO: THIS IS WHERE THE ERROR OCCURS

  # Checking if keywords are contained in flattened message
  if (length(unlist(term_match(data$Flat, keywords))) == 0) {
    stop("Keyword is not contained in the chat, try another keyword")
  }

  # make plot
  out <- with(data, suppressWarnings(dispersion_plot(Flat, keywords, grouping.var = list(Sender), ...)))
  print(out)

  # switching on warnings again
  options(warn = defaultW)

  if (return_data == TRUE) {
    return(data)
  } else {
    return(out)
  }
}
