#' @title Basic WhatsApp Chatlog Statistics
#' @description Creates a list of basic information about a single WhatsApp chatlog
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param excludeSM If TRUE, excludes the WhatsAppSystemmessages from the descriptive statistics. Default is TRUE.
#' @export
#' @return A list containing:
#'
#'      1) The number of messages in the chat \cr
#'      2) The number of tokens in the chat  \cr
#'      3) The number of participants in the chat  \cr
#'      4) The date of the first message\cr
#'      6) The date of the  last message\cr
#'      7) The total duration of the chat \cr
#'      8) The number of system messages in the chat \cr
#'      9) The number of Emoji in the chat \cr
#'      10) The number of Smilies in the chat \cr
#'      11) The number of Links in the chat\cr
#'      12) The number of Media in the chat\cr
#'      12) The number of Locations in the chat\cr
#'
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' summarize_chat(data)


###### Basic statistics

summarize_chat <- function(data, excludeSM = TRUE) {

  # getting existing column names
  vars <- colnames(data)

  if (excludeSM == TRUE) {

    data <- data[data$Sender !="WhatsApp System Message",]

  }

  # creating list object
  Basics <- as.list(rep(NA,12))
  names(Basics) <- c("NumberOfMessages",
                     "NumberOfTokens",
                     "NumberOfParticipants",
                     "StartDate",
                     "EndDate",
                     "TimeSpan",
                     "NumberOfSystemMessages",
                     "NumberOfEmoji",
                     "NumberOfSmilies",
                     "NumberOfLinks",
                     "NumberOfMedia",
                     "NumberOfLocation")

  # NumberOfMessages
  Basics$NumberOfMessages <- dim(data)[1]

  # NumberOfTokens
  if ("TokCount" %in% vars) {Basics$NumberOfTokens <- sum(data$TokCount)}

  # NumberOfParticipants
  if ("Sender" %in% vars) {Basics$NumberOfParticipants <- length(unique(data$Sender))} else{

    if ("Anonymous" %in% vars)  {Basics$NumberOfParticipants <- length(unique(data$Anonymous))}

  }

  # StartingDate
  if ("DateTime" %in% vars) {Basics$StartDate <- min(data$DateTime)}

  # EndingDate
  if ("DateTime" %in% vars) {Basics$EndDate <- max(data$DateTime)}

  # TimeSpan
  if ("DateTime" %in% vars) {Basics$TimeSpan <- difftime(max(data$DateTime),min(data$DateTime))}

  # NumberOfSystemMessages
  if ("SystemMessage" %in% vars) {Basics$NumberOfSystemMessages <- sum(!is.na(data$SystemMessage))}

  #NumberOfEmoji
  if ("Emoji" %in% vars) {Basics$NumberOfEmoji <- sum(!is.na(unlist(data$Emoji)))}

  #NumberOfSmilies
  if ("Smilies" %in% vars) {Basics$NumberOfSmilies <- sum(!is.na(unlist(data$Smilies)))}

  #NumberOfLinks
  if ("URL"%in% vars) {Basics$NumberOfLinks <- sum(!is.na(unlist(data$Links)))}

  #NumberOfMedia
  if ("Media"%in% vars) {Basics$NumberOfMedia <- sum(!is.na(unlist(data$Media)))}

  #NumberOfLocation
  if ("Location"%in% vars) {Basics$NumberOfLocation <- sum(!is.na(unlist(data$Location)))}

  # Return list
  return(Basics)

}
