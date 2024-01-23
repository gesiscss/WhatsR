#' @title Parsing raw 'WhatsApp' chat log according to iOs text structure
#'
#' @description Creates a data frame from an exported 'WhatsApp' chat log containing one row per message
#' and a column for DateTime when the message was send, name of the sender and body of the message. Only works as an intermediary function
#' called from within \code{\link[WhatsR]{parse_chat}}
#' @param chatlog 'WhatsApp' chat preprocessed by \code{\link[WhatsR]{parse_chat}}
#' @param newline_indicator Character string defining character for newline indicators. Default is a Unicode newline.
#' @param media_indicator Character string for detecting media and file attachments.
#' @param media_omitted Character string inserted by 'WhatsApp' instead of file names when not exporting media.
#' @param sent_location Regex for detecting auto generated messages for locations shared via chat.
#' @param live_location Regex for detecting auto generated messages for locations shared via chat.
#' @param datetime_indicator Regex for detecting the DateTime indicator at the beginning of each message.
#' @param newline_replace Replacement string for a newline character in parsed message. Default is " start_newline ".
#' @param media_replace Replacement string for omitted media files. Default is " media_omitted ".
#' @param foursquare_loc Regex for detecting sent Locations as FourSquare Links.
#' @export
#' @importFrom qdapRegex rm_default
#' @importFrom stringi stri_split_fixed stri_split_regex stri_extract_all
#' @importFrom lubridate parse_date_time
#' @return A data frame containing the timestamp, name of the sender and message body
#' @examples
#' ParsedChat <- parse_ios("[29.01.18, 23:33:00] Alice: Hello?\\n [29.01.18, 23:45:01] Bob: Hello")

# function to further parse pre-parsed chat logs from iOs phones
parse_ios <- function(chatlog,
                      newline_indicator = "\n",
                      media_omitted = "<media omitted>",
                      media_indicator = "^<attached:\\s(.)*?\\.(.)*?>$",
                      sent_location = paste0(
                        "location: (?=https:\\/\\/maps\\.google\\.com\\/",
                        "\\?q=\\d\\d.\\d{6}\\,\\d\\.\\d{6})"
                      ),
                      live_location = "^live location shared$",
                      datetime_indicator = paste("(?!^)(?=\\[((\\d{2}\\.\\d{2}\\.\\d{2})|",
                        "(\\d{1,2}\\/\\d{1,2}\\/\\d{2})),\\s\\d{1,2}\\:\\d{2}((\\:\\d{2}\\",
                        "s(?i:(pm|am)))|(\\s(?i:(pm|am)))|(\\:\\d{2}\\])|(\\:\\d{2})|(\\s))\\])",
                        sep = ""
                      ),
                      newline_replace = " start_newline ",
                      media_replace = " media_omitted ",
                      foursquare_loc = "^.*: https://foursquare.com/v/.*$") {

  # Deleting string for omitted media "<Media omitted>" and the regex
  # for present media data
  chatA <- gsub(
    pattern = media_omitted,
    chatlog,
    replacement = media_replace,
    perl = TRUE
  )

  # Cutting the textblock into individual messages (cut in front of the datetime)
  chat4 <- strsplit(chatA, datetime_indicator, perl = TRUE)
  chat4 <- unlist(chat4)

  # Replacing newline within messages
  chat5 <- rm_default(chat4,
    pattern = newline_indicator,
    replacement = newline_replace
  )

  ### We cut the message to form vectors for datetime, sender and the actual message

  # Splitting
  DateSplit <- stri_split_fixed(
    str = chat5,
    pattern = "]",
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
  DateTime <- substring(DateTime, 2)

  # Setting orders for time parsing
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

  # parsing datetime
  DateTime <- parse_date_time(DateTime,
    orders = timestrings,
    select_formats = TRUE
  )

  # Extract Sender and encode correctly
  Sender <- sapply(SenderSplit, "[", 1)
  Sender <- trimws(Sender)

  # Extract Message
  Message <- sapply(SenderSplit, "[", 2)

  ###### Media

  # removing indicator string for attached files
  Media <- stri_split_regex(str = Message, pattern = media_indicator, n = 2)
  Media[which(sapply(Media, length) == 1)] <- NA
  Media <- sapply(Media, "[", 2)
  Media <- substr(Media, 1, nchar(Media) - 1)

  # extract location to column
  Location <- stri_extract_all(Message, regex = paste(c(live_location, sent_location), collapse = "|"))
  Location[sapply(Location, length) == 0] <- NA
  Location <- unlist(Location)

  # Add foursquare columns
  Location_fs <- stri_extract_all(Message, regex = foursquare_loc)
  Location[!is.na(Location_fs)] <- unlist(Location_fs)[!is.na(Location_fs)]


  # binding the columns together
  Result <- cbind.data.frame(DateTime,
    Sender,
    Message,
    Media,
    Location,
    stringsAsFactors = FALSE
  )

  # returning parsed Result
  return(Result)
}
