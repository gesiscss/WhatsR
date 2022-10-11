#' @title Token Distributions per Message
#' @description Summarizing the distribution of tokens per message
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param excludeSM If TRUE, excludes the WhatsApp System Messages from the descriptive statistics. Default is FALSE.
#' @importFrom anytime anytime
#' @export
#' @return A summary of tokens per message disitribution per author
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' summarize_tokens_per_person(data)

############ Function to return a summary for token count messages
summarize_tokens_per_person <- function(data,
                                        names = "all",
                                        starttime = anytime("1960-01-01 00:00"),
                                        endtime = Sys.time(),
                                        excludeSM = FALSE) {

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {

    starttime <- min(data$DateTime)

  } else {starttime <- anytime(starttime, asUTC = TRUE)}

  # setting endtime
  if (difftime(Sys.time(),endtime, units = "min") < 1) {

    endtime <- max(data$DateTime)

  } else {endtime <- anytime(endtime, asUTC = TRUE)}

  # setting names argument
  if (length(names) == 1 && names == "all") {

    if (excludeSM == TRUE) {

      # All names in the dataframe except System Messages
      names = unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

      # dropping empty levels
      if (is.factor(names)) {names <- droplevels(names)}

    } else {

      # including system messages
      names = unique(data$Sender)

    }

  }


  SumFunc <- function(string){

    # Tokens per Message summary
    TokSum <- summary(data[is.element(data$Sender,string) & data$DateTime >= starttime & data$DateTime <= endtime,]$TokCount)

    # Basic Stats
    return(list("Timespan" = list("Start" = starttime, "End" = endtime),"TokenStats" = TokSum))

  }

  # apply to all names
  ListOut <- sapply(names,SumFunc, simplify = FALSE, USE.NAMES = TRUE)

  # setting names
  names(ListOut) <- as.character(names)

  # return results
  return(ListOut)

}
