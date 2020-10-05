#' @title Basic WhatsApp Chatlog Statistics
#' @description Excluding parts of the chat by senders or timestamps
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names If TRUE, excludes the WhatsAppSystemmessages from the descriptive statistics. Default is TRUE.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param excludeSM If TRUE, excludes the WhatsAppSystemmessages from the descriptive statistics. Default is TRUE.
#' @importFrom anytime anytime
#' @export
#' @return A dataframe that is restricted to the specified timeframe and authors
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' tailor_chat(data, names = c("Mallory","Alice"))


# Function to tailor dataframe with respect to time and date
tailor_chat <- function(data,
                        names = "all",
                        starttime = anytime("1960-01-01 00:00"),
                        endtime = Sys.time(),
                        excludeSM = TRUE){

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {

    starttime <- min(data$DateTime)

  } else {starttime <- anytime(starttime)}

  # setting endtime
  if (difftime(Sys.time(),endtime, units = "min") < 1) {

    endtime <- max(data$DateTime)

  } else {endtime <- anytime(endtime)}

  # setting names argument
  if (length(names) == 1 && names == "all") {


    if (excludeSM == TRUE) {

      # All names in the dataframe except System Messages
      names <- unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

    } else {

      # All names
      names <- unique(data$Sender)

    }

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  # spitting out dataframe
  return(data)

}
