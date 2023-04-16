#' @title Plotting emoji distributions in WhatsApp chat logs
#' @description Plots four different types of graphs for the emoji contained in a parsed WhatsApp chat log. Returns dataframe used for plotting if desired.
#' @param data A WhatsApp chat log that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param min_occur Minimum number of occurrences for emoji to be included in the plots. Default is 1.
#' @param return_data If TRUE, returns the subsetted data frame used for plotting. Default is FALSE.
#' @param emoji_vec A vector of emoji that the visualizations and data will be restricted to.
#' @param plot The type of plot that should be returned. Options are "heatmap", "cumsum", "bar" and "splitbar".
#' @param emoji_size Determines the size of the emoji displayed on top of the bars for "bar" and "splitbar", default is 10.
#' @param font_family Character string for indicating font family used to plot_emoji. Fonts might need to be installed manually, see \code{\link[extrafont]{font_import}}.
#' @param exclude_sm If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2 ragg
#' @importFrom anytime anytime
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom mgsub mgsub
#' @importFrom methods is
#' @importFrom utils read.csv
#' @export
#' @return Plots and/or the subset data frame based on author names, datetime and emoji occurrence
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_emoji(data,font_family="Times", exclude_sm = TRUE) #font_family = "Noto Color Emoji" on Linux

# Visualizing sent emoji
plot_emoji <- function(data,
                       names = "all",
                       starttime = "1960-01-01 00:00",
                       endtime = as.character(Sys.time()),
                       min_occur = 1,
                       return_data = FALSE,
                       emoji_vec = "all",
                       plot = "bar",
                       emoji_size = 10,
                       font_family = "Noto Color Emoji",
                       exclude_sm = FALSE) {


  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  Date <- Sender <- day <- hour <- `Number of Emoji` <- ave <- total <- Var1 <- Freq <- n <- emoji <- Emoji <- Glyph <-  NULL

  # catching bad params
  # start- and endtime are convertable to POSIXct
  if (is.character(starttime) == FALSE | is.na(anytime(starttime))) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (is.character(endtime) == FALSE | is.na(anytime(endtime))) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (anytime(starttime) >= anytime(endtime)) stop("starttime has to be before endtime.")

  # min_occur needs to be 1 or bigger
  if (min_occur < 1) stop("Please provide a min_occur of >= 1.")

  # return_data must be bool
  if (!is.logical(return_data)) stop("return_data has to be either TRUE or FALSE.")

  # emoji_vec must be in data
  if (!("all" %in% emoji_vec) & any(!emoji_vec %in% data$EmojiDescriptions)) stop("emoji_vec has to either be \"all\" or a vector of emojis to include.")

  # plot must be one of the the preset options
  if (any(!plot %in% c("heatmap", "cumsum", "bar", "splitbar"))) stop("The plot type has to be heatmap, cumsum, bar or splitbar.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  # switching off dplyr warning
  options(dplyr.summarise.inform = FALSE)

  # importing Emoji dictionary
  Dictionary <- read.csv(system.file("EmojiDictionary.csv", package = "WhatsR"))

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {
    starttime <- min(anytime(data$DateTime, asUTC = TRUE))
  } else {
    starttime <- anytime(starttime, asUTC = TRUE)
  }

  # setting endtime
  if (difftime(Sys.time(), endtime, units = "min") < 1) {
    endtime <- max(anytime(data$DateTime, asUTC = TRUE))
  } else {
    endtime <- anytime(endtime, asUTC = TRUE)
  }

  # setting names argument
  if (length(names) == 1 && names == "all") {

    if (exclude_sm == TRUE) {

      # All names in the dataframe except System Messages
      names = unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

      # dropping empty levels
      if (is.factor(names)) {names <- droplevels(names)}


    } else {

      # including system messages
      names = unique(data$Sender)

    }

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  # This tells us if at least one emoji is present (if TRUE then there's at least one emoji)
  EmojiPresent <- !sapply(sapply(data$Emoji, is.na),sum)

  # This tells us how many elements are in each list element (includes NA as well)
  NoElements <- lengths(data$Emoji)

  # We take the new counter and set it to zero where-ever no emoji are present
  NoElements[EmojiPresent == FALSE] <- 0

  # Emoji
  UnlistedEmojiDescriptions <- unlist(data$EmojiDescriptions)
  NewEmoji <- UnlistedEmojiDescriptions[!is.na(UnlistedEmojiDescriptions)]

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

  # pasting together
  options(stringsAsFactors = FALSE)
  NewFrame <- cbind.data.frame(NewDates,NewSender,NewEmoji)

  # creating time data
  NewFrame$hour <- as.POSIXlt(NewFrame$NewDates)$hour
  NewFrame$year <- as.POSIXlt(NewFrame$NewDates)$year + 1900
  NewFrame$day <- weekdays(as.POSIXlt(NewFrame$NewDates), abbreviate = FALSE)

  # setting correct emoji_vec
  if (length(emoji_vec) == 1 && emoji_vec == "all") {

    emoji_vec <- unique(NewEmoji)

  }

  # restricting to emoji_vec range
  NewFrame <- NewFrame[is.element(NewFrame$NewEmoji,emoji_vec),]

  if (dim(NewFrame)[1] == 0) {

    # exit
    warning("No Emoji defined by 'emoji_vec' is contained in the chat")
    stop()

  }


  if (plot == "heatmap") {

    # shaping dataframe
    helperframe2 <- NewFrame %>%
      group_by(day, hour) %>%
      summarise("Number of Emoji" = n())

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
      geom_tile(aes(fill = `Number of Emoji`), colour = "black") +
      labs(title = "Emoji by Weekday and Hour",
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

    if (return_data == TRUE) {

      # returning
      return(as.data.frame(helperframe2))

    } else(return(out))

  }


  if (plot == "cumsum") {

    # cumulative number of links per sender
    NewFrame$counter <- rep(1,length(NewFrame$NewEmoji))
    NewFrame$total <- ave(NewFrame$counter,NewFrame$NewSender,FUN = cumsum)

    # renaming to fix plot legend
    names(NewFrame)[1:3] <- c("Date","Sender","Emoji")

    # constructing graph
    out <- ggplot(NewFrame, aes(x = Date, y = total, color = Sender)) +
      theme_minimal() +
      geom_line() +
      labs(title = "Cumulative number of Emoji sent",
           subtitle = paste(starttime, " - ", endtime))  +
      theme(axis.text.x = element_text(angle = 90))  +
      xlab("Time") +
      ylab("Emoji Sent") +
      geom_point()

    # pinting plot
    print(out)

    if (return_data == TRUE) {

      # returning
      return(as.data.frame(NewFrame))

    } else(return(out))

  }

  if (plot == "bar") {

    # Converting to dataframe to make it usable by ggplot
    Emoji <- names(sort(table(NewFrame$NewEmoji),decreasing = TRUE))
    Freq <- sort(table(NewFrame$NewEmoji),decreasing = TRUE)
    names(Freq) <- NULL
    df <- cbind.data.frame(Emoji = Emoji, Freq)

    # removing some random variable that comes out of nowhere
    if ("Var1" %in% colnames(df)) {

      df <- df[,-c(2)]

    }

    # restricting data frame to min_occur
    df <- df[df$Freq >= min_occur,]

    # matching emoji contained in the data with row number of dictionary
    indicator <- NULL

    for (i in df$Emoji) {

      indicator[i] <- which(Dictionary$Desc == gsub("Emoji_","",i))

    }

    # retranslating emoji to description
    df$Glyph <- sapply(gsub("Emoji_","",df$Emoji), function(x){Dictionary[x == Dictionary$Desc,]$R.native})

    # Visualizig the distribution of emoji and putting the emoji on top of the bars
    out <- ggplot(df,aes(x = factor(Emoji,levels = Emoji[order(Freq, decreasing = TRUE)]),y = Freq, fill = Emoji, label = Dictionary$HTML[indicator])) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(title = "Distribution of sent Emoji",
           subtitle = paste(starttime, " - ", endtime),
           x = "Emojis",
           y = "Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position = "none") +
      geom_label(aes(label = Glyph),
                 family = font_family,
                 label.size = NA,
                 fill = alpha(c("white"),0),
                 size = emoji_size)

    # printing
    print(out)

    # return data
    if (return_data == TRUE) {

      # returning
      return(as.data.frame(df))

    } else{return(out)}

  }

  if ( plot == "splitbar") {

    ## Summarize per Sender how often each domain was sent
    SumFrame <-  group_by(NewFrame, NewSender, NewEmoji) %>% summarise(n = n())
    SumFrame <- SumFrame[SumFrame$n >= min_occur,]

    # renaming to fix plot legend
    names(SumFrame)[1:2] <- c("Sender","Emoji")

    # Adding HTML to SumFrame
    indicator <- NULL

    for (i in seq_along(SumFrame$Emoji)) {

      indicator[i] <- which(Dictionary$Desc == gsub("Emoji_","",SumFrame$Emoji[i]))

    }

    # TODO: When we have an unequal amount of bars per person, the bar width for one person can get mangled up
    # retranslating emoji to description
    SumFrame$Glyph <- sapply(gsub("Emoji_","",SumFrame$Emoji), function(x){Dictionary[x == Dictionary$Desc,]$R.native})

    # building graph object
    out <-   ggplot(SumFrame, aes(x = Sender, y = n,fill = Emoji, label = Glyph)) +
      theme_minimal() +
      geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single")) +
      labs(title = "Emoji sent per Person",
           subtitle = paste(starttime, " - ", endtime),
           x = "Sender",
           y = "Frequency") +
      theme(legend.title = element_text("Emoji")) +
      geom_label(aes(label = Glyph, fill = Emoji),
                 family = font_family,
                 label.size = NA,
                 fill = alpha(c("white"),0),
                 size = emoji_size,
                 position = position_dodge2(width = 0.9, preserve = "single"))


    #switching warnings back on
    options(dplyr.summarise.inform = TRUE)

    # return data
    if (return_data == TRUE) {

      # returning
      return(as.data.frame(SumFrame))

    } else{return(out)}

  }

}
