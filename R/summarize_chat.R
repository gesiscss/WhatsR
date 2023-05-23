#' @title Basic WhatsApp chat log Statistics
#' @description Creates a list of basic information about a single WhatsApp chat log
#' @param data A WhatsApp chat log that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param exclude_sm If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @export
#' @return A list containing: \cr
#'
#'      1) The number of messages in the chat \cr
#'      2) The number of tokens in the chat  \cr
#'      3) The number of participants in the chat  \cr
#'      4) The date of the first message\cr
#'      6) The date of the last message\cr
#'      7) The total duration of the chat \cr
#'      8) The number of system messages in the chat \cr
#'      9) The number of emoji in the chat \cr
#'      10) The number of smilies in the chat \cr
#'      11) The number of links in the chat\cr
#'      12) The number of media in the chat\cr
#'      12) The number of locations in the chat\cr
#'
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' summarize_chat(data)

###### Basic statistics
summarize_chat <- function(data, exclude_sm = FALSE) {

  # catching bad params

  # checking data
  if(!is.data.frame(data)){stop("'data' must be a dataframe parsed with parse_chat()")}

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  # getting existing column names
  vars <- colnames(data)

  # creating list object
  Basics <- as.list(rep(NA, 12))
  names(Basics) <- c(
    "NumberOfMessages",
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
    "NumberOfLocation"
  )

  if ((exclude_sm == TRUE) & "Sender" %in% vars) {
    data <- data[data$Sender != "WhatsApp System Message", ]
  } else {
    if ((exclude_sm == TRUE) & "Anonymous" %in% vars) {
      data <- data[data$Anonymous != "WhatsApp System Message",]
    }
  }

  # NumberOfMessages
  Basics$NumberOfMessages <- dim(data)[1]

  # NumberOfTokens
  if ("TokCount" %in% vars) {
    Basics$NumberOfTokens <- sum(data$TokCount,na.rm = TRUE)
  }

  # NumberOfParticipants
  if ("Sender" %in% vars) {
    Basics$NumberOfParticipants <- length(unique(data$Sender))
  } else {
    if ("Anonymous" %in% vars) {
      Basics$NumberOfParticipants <- length(unique(data$Anonymous))
    }
  }

  # StartingDate
  if ("DateTime" %in% vars) {
    Basics$StartDate <- min(data$DateTime)
  }

  # EndingDate
  if ("DateTime" %in% vars) {
    Basics$EndDate <- max(data$DateTime)
  }

  # TimeSpan
  if ("DateTime" %in% vars) {
    Basics$TimeSpan <- difftime(max(data$DateTime, na.rm = T), min(data$DateTime, na.rm = T))
  }

  # NumberOfEmoji
  if ("Emoji" %in% vars) {
    Basics$NumberOfEmoji <- sum(!is.na(unlist(data$Emoji)))
  }

  # NumberOfSmilies
  if ("Smilies" %in% vars) {
    Basics$NumberOfSmilies <- sum(!is.na(unlist(data$Smilies)))
  }

  # NumberOfSystemMessages (We need to do this before so we can stil pick them up)
  if ("SystemMessage" %in% vars) {
    Basics$NumberOfSystemMessages <- sum(!is.na(data$SystemMessage))
  }

  # NumberOfLinks
  if ("URL" %in% vars) {
    Basics$NumberOfLinks <- sum(!is.na(unlist(data$URL)))
  }

  # NumberOfMedia
  if ("Media" %in% vars) {
    Basics$NumberOfMedia <- sum(!is.na(unlist(data$Media)))
  }

  # NumberOfLocation
  if ("Location" %in% vars) {
    Basics$NumberOfLocation <- sum(!is.na(unlist(data$Location)))
  }

  # Return list
  return(Basics)
}
