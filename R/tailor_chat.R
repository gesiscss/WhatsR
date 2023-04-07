#' @title Restricting Chatlogs to certain authors or timeframes.
#' @description Excluding parts of the chat by senders or timestamps
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of names that the output is restricted to. Messages from other authors are excluded.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param exclude_sm If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @importFrom anytime anytime
#' @export
#' @return A dataframe that is restricted to the specified timeframe and authors
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' tailor_chat(data, names = c("Mallory", "Alice"))

# Function to tailor dataframe with respect to time and date
tailor_chat <- function(data,
                        names = "all",
                        starttime = anytime("1960-01-01 00:00"),
                        endtime = Sys.time(),
                        exclude_sm = FALSE) {

  # catching bad params
  # start- and endtime are POSIXct
  if (is(starttime, "POSIXct") == F) stop("starttime has to be of class POSIXct.")
  if (is(endtime, "POSIXct") == F) stop("endtime has to be of class POSIXct.")

  # names in data or all names
  if (!("all" %in% names) & any(!names %in% data$Sender)) stop("names has to either be \"all\" or a vector of names to include.")

  # excludeSM must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {
    starttime <- min(data$DateTime)
  } else {
    starttime <- anytime(starttime)
  }

  # setting endtime
  if (difftime(Sys.time(), endtime, units = "min") < 1) {
    endtime <- max(data$DateTime)
  } else {
    endtime <- anytime(endtime)
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
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ]

  # spitting out dataframe
  return(data)
}
