#' @title Visualizing links in WhatsApp chatlogs
#' @description Visualizes the occurance of links in a WhatsApp chatlog
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param use.domains If TRUE, links are shortend to domains. Default is TRUE.
#' @param exclude.long If TRUE, links that are longer than length will be exlcuded. Default is TRUE.
#' @param length Gives the number of maximum characters for exlcude.long. Default is 50.
#' @param min.occur The minimum number of occurances a Link has to have to be included in the Visualization. Default is 1
#' @param return.data If TRUE, returns the subsetted dataframe. Default is FALSE.
#' @param LinkVec A vector of Links that the visualizations will be restricted to.
#' @param plot The type of plot that should be outputted. Options include "heatmap", "cumsum", "bar" and "splitbar"
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom dplyr n
#' @export
#' @return Plots and/or the subsetted dataframe based on author names, datetime and Emoji occurance
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_links(data)

# Visualizing sent Links
plot_links <- function(data,
                       names = "all",
                       starttime = anytime("1960-01-01 00:00"),
                       endtime = Sys.time(),
                       use.domains = TRUE,
                       exclude.long = TRUE,
                       length = 50,
                       min.occur = 1,
                       return.data = FALSE,
                       LinkVec = "all",
                       plot = "bar") {

  # muting useless dplyr message
  options(dplyr.summarise.inform = FALSE)

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  Date <- Sender <- Links <- URL <- day <- hour <- n <- `Number of Links` <- ave <- total <- Var1 <- Freq <-  NULL

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

  # This tells us if at least one link is present (if it's TRUE then theres at least one link)
  LinkPresent <- !sapply(sapply(data$URL, is.na),sum)

  # This tells us how many elements are in each list element (Problem: NAs are counted as length one, we can thus not differentiate between
  # Nas and elements with 1 link)
  NoElements <- lengths(data$URL)

  # We take the New counter and set it to zero where-ever no links are present
  NoElements[LinkPresent == FALSE] <- 0

  # URLS
  UnlistedUrls <- unlist(data$URL)
  NewUrls <- UnlistedUrls[!is.na(UnlistedUrls)]

  # Senders
  NewSender <- list()

  for (i in seq_along(data$Sender)) {

    NewSender[[i]] <- rep(data$Sender[i],NoElements[i])

  }

  NewSender <- unlist(NewSender)

  # New Dates
  NewDates <- list()

  for (i in seq_along(data$DateTime)) {

    NewDates[[i]] <- rep(data$DateTime[i],NoElements[i])

  }

  NewDates <- as.POSIXct(unlist(NewDates),origin = '1970-01-01')

# shorten URLs to domain
  if (use.domains == TRUE) {

    # function for shortening to domains
    shortener <- function(URL){

      # Reduce the links to domain-names
      helper <- lapply(URL,strsplit,"(?<=/)",perl = TRUE)
      helper2 <- rapply(helper,function(x){x <- unlist(x)[1:3]},how = "list")
      helper3 <- rapply(helper2,function(x){x <- paste(x,collapse = "")},how = "list")
      helper4 <- lapply(helper3,unlist)
      helper4[helper4 == "NANANA"] <- NA
      URL <- helper4

      return(unlist(URL))

    }

    # applying shortening function
    NewUrls <- shortener(NewUrls)

  } else {}

  # pasting together
  options(stringsAsFactors = FALSE)
  NewFrame <- cbind.data.frame(NewDates,NewSender,NewUrls)

  # creating time data
  NewFrame$hour <- as.POSIXlt(NewFrame$NewDates)$hour
  NewFrame$year <- as.POSIXlt(NewFrame$NewDates)$year + 1900
  NewFrame$day <- weekdays(as.POSIXlt(NewFrame$NewDates), abbreviate = FALSE)

  # setting correct LinkVec
  if (length(LinkVec) == 1 && LinkVec == "all") {

    LinkVec <- unique(NewUrls)

  }

  # restricting to LinkVec range
  NewFrame <- NewFrame[is.element(NewFrame$NewUrls,LinkVec),]

  if (dim(NewFrame)[1] == 0) {

    # exit
    warning("No Url defined by LinkVec is contained in the chat")
    stop()

  }

  if (plot == "heatmap") {

    # shaping dataframe
    helperframe2 <- NewFrame %>%
      group_by(day, hour) %>%
      summarise("Number of Links" = n())

    # factor ordering
    weekdays <- rev(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

    # transalte to english for better compatibility
    helperframe2$day <- mgsub(helperframe2$day,
                              pattern = c("Sonntag","Samstag","Freitag","Donnerstag","Mittwoch","Dienstag","Montag"),
                              replacement = weekdays)

    helperframe2$day <- as.factor(helperframe2$day)

    if (sum(weekdays %in% levels(helperframe2$day)) == 7) {

      helperframe2$day <- factor(helperframe2$day, levels = weekdays)

    } else {

      helperframe2$day <- factor(helperframe2$day,c(levels(helperframe2$day),weekdays[!weekdays %in% levels(helperframe2$day)]))
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)

    }

    # plotting Heatmap
    out <- ggplot(helperframe2, aes(hour, day)) +
      theme_minimal() +
      geom_tile(aes(fill = `Number of Links`), colour = "black", width = 1) +
      labs(title = "Links by Weekday and Hour",
           subtitle = paste(starttime, " - ", endtime),
           x = "",
           y = "") +
      scale_fill_distiller(palette = "YlGnBu", direction = 1) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.ticks.x = element_blank(),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            panel.grid = element_blank()) +
      coord_equal() +
      scale_x_continuous(breaks = seq(-0.5,23.5,1),
                         limits = c(-0.5,23.5),
                         labels = c("00:00",
                                    "01:00",
                                    "02:00",
                                    "03:00",
                                    "04:00",
                                    "05:00",
                                    "06:00",
                                    "07:00",
                                    "08:00",
                                    "09:00",
                                    "10:00",
                                    "11:00",
                                    "12:00",
                                    "13:00",
                                    "14:00",
                                    "15:00",
                                    "16:00",
                                    "17:00",
                                    "18:00",
                                    "19:00",
                                    "20:00",
                                    "21:00",
                                    "22:00",
                                    "23:00",
                                    "24:00"))

    # print top ten links at bottom of heatmap
    # if (length(LinkVec) == 1 && LinkVec == "all") {
    #
    #   print(out)
    #
    # } else {
    #
    #   if (length(LinkVec) <= 10) {
    #
    #     print(out + labs(caption = paste0(names(sort(table(NewFrame$NewEmoji), decreasing = TRUE)),collapse = "\n ")) + theme(plot.caption = element_text(hjust = 0.5)))
    #
    #   } else {
    #
    #     print(out + labs(caption = paste0(names(sort(table(NewFrame$NewLinks), decreasing = TRUE))[1:10],collapse = "\n ")) + theme(plot.caption = element_text(hjust = 0.5)))
    #
    #   }
    #
    #
    # }

    # printing
    print(out)

    if (return.data == TRUE) {

      # returning
      return(as.data.frame(helperframe2))

    } else {return(out)}

  }


  if (plot == "cumsum") {

    # cumulative number of links per sender
    NewFrame$counter <- rep(1,length(NewFrame$NewUrls))
    NewFrame$total <- ave(NewFrame$counter,NewFrame$NewSender,FUN = cumsum)

    # renaming to fix plot legend
    names(NewFrame)[1:3] <- c("Date","Sender","URL")

    # constructing graph
    out <- ggplot(NewFrame, aes(x = Date, y = total, color = Sender)) +
      theme_minimal() +
      geom_line() +
      labs(title = "Cumulative number of Links sent",
           subtitle = paste(starttime, " - ", endtime))  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
      xlab("Time") +
      ylab("Total Links Sent") +
      geom_point()

    # pinting plot
    print(out)

    if (return.data == TRUE) {

      # returning
      return(NewFrame)

    } else {return(out)}

  }

  if (plot == "bar") {

    # Converting to dataframe to make it usable by ggplot
    #df <- as.data.frame(sort(table(NewFrame$NewUrls),decreasing = TRUE))





    Links <- names(sort(table(NewFrame$NewUrls),decreasing = TRUE))
    Freq <- sort(table(NewFrame$NewUrls),decreasing = TRUE)
    names(Freq) <- NULL
    df <- cbind.data.frame(Links,Freq)

    # removing some bullshit variable that comes out of nowhere
    if ("Var1" %in% colnames(df)){

      df <- df[,-c(2)]

    }




    # renaming to fix legend title
    #names(df) <- c("Links","Freq")

    # excluding links that are to long
    if (exclude.long == TRUE) {

      df <- df[sapply(as.character(df$Links),nchar) <= length,]

    }

    # Visualizig the distribution of domains (domains sent more than x)
    out <- ggplot(df[df$Freq >= min.occur,],aes(x = Links,y = Freq, fill = Links)) +
            theme_minimal() +
            geom_bar(stat = "identity") +
            labs(title = "Distribution of Domains of sent Links",
                 subtitle = paste(starttime, " - ", endtime),
                 x = "Domains",
                 y = "Frequency") +
            theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                  legend.position = "none")

    # printing
    print(out)

    # return data
    if (return.data == TRUE) {

      # returning
      return(df)

    } else {return(out)}

  }

  if ( plot == "splitbar") {

    ## Summarize per Sender who often each domain was sent
    SumFrame <-  group_by(NewFrame, NewSender, NewUrls) %>% summarise(n = n())
    SumFrame <- SumFrame[SumFrame$n >= min.occur,]

    # renaming to fix legend title
    names(SumFrame) <- c("Sender","URL","n")

    # building graph object
    out <-   ggplot(SumFrame, aes(x = Sender, y = n,fill = URL)) +
      theme_minimal() +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Links sent per Person and Domain",
           subtitle = paste(starttime, " - ", endtime),
           x = "Sender",
           y = "Frequency")

    # only printing legend if we have unique websites or less
    if (length(unique(SumFrame$URL)) <= 20) {

      print(out)

    } else {

      warning("Legend was dropped because it contained too many links")
      print(out + theme(legend.position = "none"))

    }

    # return data
    if (return.data == TRUE) {

      # returning
      return(SumFrame)

    } else {return(out)}

  }

  # unmute dplyr
  options(dplyr.summarise.inform = TRUE)

}
