#' @title Lexical Disperson plots for Keywords in WhatsApp chatlogs
#' @description Visualizes the occurance of specific keywords within the chat
#' @param data A WhatsApp chatlog that was parsed with WhatsAppParse()
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param palettes Palette name to be used. Default is "Paired"
#' @param keywords A vector of keywords to be displayed, default is c("hello",world")
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' @return Lexical Disperson plots for specified keywords
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_lexical_dispersion(data, keywords = c("smilies","handy"))


######################## lexical disperson plots for specific words
plot_lexical_dispersion <- function(data,
                                    names = "all",
                                    starttime = anytime("1960-01-01 00:00"),
                                    endtime = Sys.time(),
                                    palettes = "Paired",
                                    keywords = c("hello","world")) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  keyword <- NULL

  # transferring keywords to lowercase to make it non case-sensitive
  keywords <- tolower(keywords)

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

    # All names in the dataframe except System Messages
    names <- unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  # transferring data to list by sender
  listframe <- split(data, data$Sender)

  # collpasing all messages per sender into one big document
  Msgs <- character()

  for (i in 1:length(listframe)){

    Msgs[i] <- paste(listframe[[i]]$Flat, collapse = " ")

  }

  # creating new corpus object
  NewCorp <- corpus(Msgs)
  docvars(NewCorp, "Sender") <- names(listframe)
  docnames(NewCorp) <- names(listframe)

  # transferring corpus to lowercase for comparison
  NewCorp <- tolower(NewCorp)


  ### check if the keywords are contained in the corpus at all

  indicator <- vector()

  for (i in 1:length(keywords)){

    indicator[i] <- length(grep(keywords[i],NewCorp))

  }

  if (sum(indicator) == 0) {

    warning("None of the keywords are contained in the chat at all")

  } else {

    # plotting
    out <- textplot_xray(kwic(NewCorp, pattern = keywords))
    print(out +
            aes(color = keyword) + scale_color_brewer(palette = palettes) +
            labs(title = "Lexical Dispersion Plot",
                 subtitle = paste(starttime, " - ", endtime)))

  }

}
