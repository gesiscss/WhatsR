#' @title Ports-of-Speech tags
#' @description Plots the parts-of-speech tags used per person
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param plot Type of plot to be returned, options include "bar" and "pie". Default is "bar".
#' @param language Language of the chat. The paramter is passed down to download the necessary spaCy modules, see code{\link[https://spacyr.quanteda.io/]}
#' @param return.data If TRUE, returns a dataframe with POS tags in long format. Default is FALSE.
#' @import cleanNLP ggplot2
#' @importFrom anytime anytime
#' @export
#' @return Plots parts-of-speech tags used per person
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_pos(data)

######## overall amount of POs-tags per person
plot_pos <- function(data,
                     names = "all",
                     starttime = anytime("1960-01-01 00:00"),
                     endtime = Sys.time(),
                     plot = "bar",
                     language = "en",
                     return.data = FALSE) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  Var1 <- Freq <-  NULL

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
    names = unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  #### POS tags

  # initiating spaCy language model
  cnlp_init_spacy(model_name = language)

  # annotation
  data$Flat[is.na(data$Flat)] <- ""
  annotations <- cnlp_annotate(data$Flat)

  # converting to long format
  tags <- vector(mode = "list", length = max(annotations$document$doc_id))

  # looping through docs and saving tags in list
  for (i in 1:max(annotations$document$doc_id)) {

    tags[[i]] <- annotations$token$upos[annotations$token$doc_id == i]

  }

  # New Frame for easier plotting
  VisFrame <- cbind.data.frame(data$DateTime,data$Sender,data$Flat,I(tags))

  Names <- vector(mode = "list", length = length(VisFrame$tags))

  for (i in 1:dim(VisFrame)[1]) {

    Names[[i]] <- rep(VisFrame$`data$Sender`[i],length(unlist(VisFrame$tags[i])))

  }

  # unlisting for long format
  LongTags <- unlist(tags)
  LongNames <- unlist(Names)

  # binding the long versions into a dataframe and converting to factor
  bind <- cbind.data.frame(LongTags,LongNames)
  bind$LongTags <- as.factor(bind$LongTags)

  if (return.data == TRUE) {


    output <- bind

  } else {

    # creating plot
    output <- ggplot(bind, aes(LongNames, fill = LongTags)) +
                  geom_bar(stat = 'count', position = 'dodge') +
                  ggtitle("Parts-of-Speech Tags") +
                  xlab("Persons") +
                  ylab("Frequency") +
                  labs(fill = "POS tags")

    # plotting
    print(output)

  }

  # returning plot or data
  return(output)

}


### testing it
data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
plot_pos(data)
test <- plot_pos(data, return.data = TRUE)

data2 <- parse_chat("/home/kohnejn/Desktop/Google Drive/Dissertation/WhatsR-Package/BuildStuff/Testfiles/ActualExports/Hannah2_Android.txt",
                   language = "german",
                   os = "android")

plot_pos(data2, language = "de")
test2 <- plot_pos(data2, return.data = TRUE, language = "de")
