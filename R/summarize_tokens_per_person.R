#' @title Token Distributions for sent messages
#' @description Summarizing the distribution of tokens for sent messages
#' @param data A 'WhatsApp' chat log that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with\code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param exclude_sm If TRUE, excludes the 'WhatsApp' system messages from the descriptive statistics. Default is FALSE.
#' @importFrom anytime anytime
#' @export
#' @return A summary of tokens per message distribution per author
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' summarize_tokens_per_person(data)

############ Function to return a summary for token count messages
summarize_tokens_per_person <- function(data,
                                        names = "all",
                                        starttime = "1960-01-01 00:00",
                                        endtime = "2200-01-01 00:00",
                                        exclude_sm = FALSE) {

  # catching bad params

  # checking data
  if (!is.data.frame(data)) {stop("'data' must be a dataframe parsed with parse_chat()")}

  # start- and endtime are convertable to POSIXct
  if (is.character(starttime) == FALSE | is.na(as.POSIXct(starttime,tz = "UTC"))) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (is.character(endtime) == FALSE | is.na(as.POSIXct(endtime,tz = "UTC"))) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (as.POSIXct(starttime,tz = "UTC") >= as.POSIXct(endtime,tz = "UTC")) stop("starttime has to be before endtime.")

  # names in data or all names
  if (!("all" %in% names) & any(!names %in% data$Sender)) stop("names has to either be \"all\" or a vector of names to include.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("excludeSM has to be either TRUE or FALSE.")

  # setting starttime
  if (as.POSIXct(starttime,tz = "UTC") <= min(data$DateTime)) {
    starttime <- min(data$DateTime)
  } else {
    starttime <- as.POSIXct(starttime,tz = "UTC")
  }

  # setting endtime
  if (as.POSIXct(endtime,tz = "UTC") >= max(data$DateTime)) {
    endtime <- max(data$DateTime)
  } else {
    endtime <- as.POSIXct(endtime,tz = "UTC")
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


  SumFunc <- function(string) {
    # Tokens per Message summary
    TokSum <- summary(data[is.element(data$Sender, string) & data$DateTime >= starttime & data$DateTime <= endtime, ]$TokCount)

    # Basic Stats
    return(list("Timespan" = list("Start" = starttime, "End" = endtime), "TokenStats" = TokSum))
  }

  # apply to all names
  ListOut <- sapply(names, SumFunc, simplify = FALSE, USE.NAMES = TRUE)

  # setting names
  names(ListOut) <- as.character(names)

  # return results
  return(ListOut)
}
