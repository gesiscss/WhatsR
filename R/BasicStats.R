#' @title Basic WhatsApp Chatlog Statistics
#' @description Creates a list of basic information about a single WhatsApp chatlog
#' @param data A WhatsApp chatlog that was parsed with WhatsAppParse()
#' @param excludeSM If TRUE, excludes the WhatsAppSystemmessages from the descriptive statistics. Default is TRUE.
#' @export
#' @return A list containing:
#'
#'      1) The number of participants in the chat \cr
#'      2) The Author of the first message \cr
#'      3) The body of the first message \cr
#'      4) The date of the first message and the last message \cr
#'      6) The total duration of the chat
#'
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' BasicStats(data)


###### Basic statistics
# TODO: Add number of links, number of emoji, number of smilies, number of locations
BasicStats <- function(data, excludeSM = TRUE) {

  # creating list object
  Basics <- list(1,2,3,4)
  names(Basics) <- c("Participants","FirstSender","FirstMessage","TimeSpan")

  # Number of unique participants
  Basics$Participants <- length(unique(data$Sender[data$Sender != "WhatsApp System Message"]))

  if (excludeSM == TRUE) {

    # First sender
    Basics$FirstSender <- unique(data$Sender[data$Sender != "WhatsApp System Message"])[1]

    # First message
    Basics$FirstMessage <- data$Message[data$Sender != "WhatsApp System Message"][1]

  }

  if (excludeSM == FALSE) {

    # First Sender
    Basics$FirstSender <- data$Sender[1]

    # First Message
    if (is.na(data$Message[1])) {

      Basics$FirstMessage <- data$SystemMessage[1]

    } else {Basics$FirstMessage <- data$Message[1]}

  }

  # Time span
  Basics$TimeSpan <- list("FirstMessage" = min(data$DateTime), "LastMessage" = max(data$DateTime), "Duration" = difftime(max(data$DateTime),min(data$DateTime)))

  # Return list
  return(Basics)

}
