#' @title Parsing raw WhatsApp chat logs according to Android text structure
#'
#' @description Creates a data frame from an exported WhatsApp chat log containing one row per message
#' and a column for DateTime when the message was sent, name of the sender and body of the message. Only works as an intermediary function
#' called from within \code{\link[WhatsR]{parse_chat}}
#' @param chatlog WhatsApp chat preprocessed by \code{\link[WhatsR]{parse_chat}}
#' @param newline_indicator character string defining character for newline indicators. Default is a Unicode newline.
#' @param media_indicator character string for detecting media and file attachments.
#' @param media_omitted character string inserted by WhatsApp instead of file names when not exporting media.
#' @param sent_location Regex for detecting auto generated messages for locations shared via chat.
#' @param live_location Regex for detecting auto generated messages for live locations shared via chat.
#' @param datetime_indicator Regex for detecting the DateTime indicator at the beginning of each message.
#' @param newline_replace replacement string for a newline character in parsed message. Default is " start_newline ".
#' @param media_replace replacement string for omitted media files. Default is " media_omitted ".
#' @export
#' @importFrom qdapRegex rm_default
#' @importFrom stringi stri_split_fixed stri_split_regex stri_split stri_extract_all
#' @importFrom lubridate parse_date_time
#' @return A data frame containing the timestamp, name of the sender and message body
#' @examples
#' ParsedChat <- parse_android("29.01.18, 23:33 - Alice: Hi?\n 29.01.18, 23:45 - Bob: Hi\n")

# function to further parse pre-parsed chat logs from android phones
parse_android <- function(chatlog,
                          newline_indicator = "\n",
                          media_omitted = "<media omitted>",
                          media_indicator = "(file attached)",
                          sent_location = paste0(
                            "Location: (?=https:\\/\\/maps\\.google\\.com\\/",
                            "\\?q=\\d\\d.\\d{6}\\,\\d\\.\\d{6})"
                          ),
                          live_location = "^live location shared$",
                          datetime_indicator = paste("(?!^)(?=((\\d{2}\\.\\d{2}\\.\\d{2})|(\\d{1,2}",
                            "\\/\\d{1,2}\\/\\d{2})),\\s\\d{2}\\:\\d{2}((\\s\\-)|(\\s(?i:(am|pm))\\s\\-)))",
                            sep = ""
                          ),
                          newline_replace = " start_newline ",
                          media_replace = " media_omitted ") {

  # Replacing string for omitted media
  chat1 <- gsub(
    pattern = media_omitted,
    chatlog,
    replacement = media_replace,
    perl = TRUE
  )

  # TODO for later:
  # due to partial matching, the first character of the string is being cut off.
  # As a fix for now, we can simply paste them back together. Ultimately,
  # there should be a better RegEx to deal with this.
  chat2 <- unlist(stri_split(chat1, regex = datetime_indicator))

  chat3 <- chat2
  for (i in 1:length(chat2)) {
    if (nchar(chat2[i]) == 1) {
      chat3[i] <- paste(chat2[i], chat2[i + 1], sep = "")
    } else {
      chat3[i] <- chat2[i]
    }
  }

  # deleting the additional lines with the incomplete dates
  chat3[which(nchar(chat2) == 1) + 1] <- NA
  chat4 <- chat3[!is.na(chat3)]

  # deleting trailing linebreaks (simply deletes the last character)
  chat5 <- substr(chat4, 1, nchar(as.character(chat4)) - 1)

  # replacing linebreaks within messages
  chat6 <- rm_default(chat5,
    pattern = newline_indicator,
    replacement = newline_replace
  )

  ### We cut the message to form vectors for datetime, sender and the actual message

  # Splitting
  DateSplit <- stri_split_fixed(
    str = chat6,
    pattern = " - ",
    n = 2
  )

  SenderSplit <- stri_split_fixed(
    str = sapply(DateSplit, "[", 2),
    pattern = ": ",
    n = 2
  )

  # Extract DateTime and encode correctly
  DateTime <- sapply(DateSplit, function(x) {
    x[1]
  })

  # Defining Time orders for lubridate::parse_date_time() function
  timestrings <- c(
    "dmy, HMS",
    "dmy, HM",
    "dmy, IMp",
    "dmy, IMSp",
    "mdy, HMS",
    "mdy, HM",
    "mdy, IMp",
    "mdy, IMSp"
  )

  DateTime <- parse_date_time(DateTime,
    orders = timestrings,
    select_formats = TRUE
  )

  # Extract sender and encode correctly
  Sender <- sapply(SenderSplit, "[", 1)

  # Extract Message
  Message <- sapply(SenderSplit, "[", 2)

  ###### Media

  # removing indicator string for attached files
  Media <- stri_split_regex(str = Message, pattern = media_indicator, n = 2)
  Media[which(sapply(Media, length) == 1)] <- NA
  Media <- sapply(Media, "[", 1)

  # Deleting trailing characters
  Media[!is.na(Media)] <- substr(Media[!is.na(Media)], 1, nchar(Media[!is.na(Media)]) - 2)

  # extract location to column
  Location <- stri_extract_all(Message, regex = paste(c(live_location, sent_location), collapse = "|"))
  Location[sapply(Location, length) == 0] <- NA
  Location <- unlist(Location)

  # binding the columns together
  Result <- cbind.data.frame(DateTime,
    Sender,
    Message,
    Media,
    Location,
    stringsAsFactors = FALSE
  )

  # returning result
  return(Result)
}
