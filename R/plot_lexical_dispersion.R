#' @title Lexical Disperson plots for Keywords in WhatsApp chatlogs
#' @description Visualizes the occurrence of specific keywords within the chat
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param names_col A column indicated by a string that should be accessed to determine the names. Only needs to be changed when \code{\link[WhatsR]{parse_chat}} used the parameter anon = "add" and the column "Anonymous" should be used. Default is "Sender".
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param keywords A vector of keywords to be displayed, default is c("hello",world").
#' @param return_data Default is FALSE, returns data frame of plot when TRUE.
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
                                    names_col = "Sender",
                                    starttime = anytime("1960-01-01 00:00"),
                                    endtime = Sys.time(),
                                    keywords = c("hello", "world"),
                                    return_data = FALSE,
                                    exclude_sm = FALSE,
                                    ...) {

  # catching bad params
  # start- and endtime are POSIXct
  if (is(starttime, "POSIXct") == F) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (is(endtime, "POSIXct") == F) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (starttime >= endtime) stop("starttime has to be before endtime.")

  # names_col must be in preset options
  if (any(!names_col %in% c("Sender", "Anonymous"))) stop("names_col has to be either Sender or Anonymous.")

  # names in data or all names (Sender or Anonymous)
  if (names_col == "Sender") {
    if (!("all" %in% names) & any(!names %in% data$Sender)) stop("names has to either be \"all\" or a vector of names to include.")}
  else{
    if (!("all" %in% names) & any(!names %in% data$Anonymous)) stop("names has to either be \"all\" or a vector of names to include.")}

  # return_data must be bool
  if (!is.logical(return_data)) stop("return_data has to be either TRUE or FALSE.")

  # keywords must be in data
  if (is(keywords, "character") == F) stop("keywords has to be a character and a vector.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  #if names_col == "Anonymous", rename to Sender and rename Sender to placeholder
  if (names_col == "Anonymous") {
  colnames(data)[colnames(data) == "Sender"] <- "Placeholder"
  colnames(data)[colnames(data) == "Anonymous"] <- "Sender"
  }

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

  # New Solution
  #TODO:

  if (length(unlist(term_match(data$Message, keywords))) == 0) {
    stop("Keyword is not contained in the chat, try another keyword")
  }

  # make plot
  out <- with(data, suppressWarnings(dispersion_plot(Message, keywords, grouping.var = list(Sender), ...)))
  print(out)

  # Rename Sender and Anonymous columns again to what they were initially
  if (names_col == "Anonymous") {
    colnames(data)[colnames(data) == "Sender"] <- "Anonymous"
    colnames(data)[colnames(data) == "Placeholder"] <- "Sender"
  }

  # switching on warnings again
  options(warn = defaultW)

  if (return_data == TRUE) {
    return(data)
  } else {
    return(out)
  }
}
