#' @title Visualizing links in 'WhatsApp' chat logs
#' @description Visualizes the occurrence of links in a 'WhatsApp' chatlog
#' @param data A 'WhatsApp' chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param use_domains If TRUE, links are shortened to domains. This includes the inputs in link_vec. Default is TRUE.
#' @param exclude_long Either NA or a numeric value. If numeric value is provided, removes all links/domains longer than x characters. Default is 50.
#' @param min_occur The minimum number of occurrences a link has to have to be included in the visualization. Default is 1.
#' @param return_data If TRUE, returns the subset data frame. Default is FALSE.
#' @param link_vec A vector of links that the visualizations will be restricted to.
#' @param plot The type of plot that should be returned Options are "heatmap", "cumsum", "bar" and "splitbar".
#' @param exclude_sm If TRUE, excludes the 'WhatsApp' system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom dplyr n
#' @importFrom methods is
#' @export
#' @return Plots and/or the subset data frame based on author names, datetime and emoji occurrence
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_links(data)

# Visualizing sent links or domains
plot_links <- function(data,
                       names = "all",
                       starttime = "1960-01-01 00:00",
                       endtime = "2200-01-01 00:00",
                       use_domains = TRUE,
                       exclude_long = 50,
                       min_occur = 1,
                       return_data = FALSE,
                       link_vec = "all",
                       plot = "bar",
                       exclude_sm = FALSE) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  Date <- Sender <- Links <- URL <- day <- hour <- n <- `Number of Links` <- ave <- total <- Var1 <- Freq <- NULL

  # catching bad params

  # checking data
  if (!is.data.frame(data)) {stop("'data' must be a dataframe parsed with parse_chat()")}

  # start- and endtime are convertable to POSIXct
  if (is.character(starttime) == FALSE | is.na(as.POSIXct(starttime,tz = "UTC"))) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (is.character(endtime) == FALSE | is.na(as.POSIXct(endtime,tz = "UTC"))) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (as.POSIXct(starttime,tz = "UTC") >= as.POSIXct(endtime,tz = "UTC")) stop("starttime has to be before endtime.")

  # min_occur must be >= 1
  if (min_occur < 1) stop("Please provide a min_occur of >= 1.")

  # return_data must be bool
  if (!is.logical(return_data)) stop("return_data has to be either TRUE or FALSE.")

  # plot must be one of the the preset options
  if (any(!plot %in% c("heatmap", "cumsum", "bar", "splitbar"))) stop("The plot type has to be heatmap, cumsum, bar or splitbar.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  # use_domains must be bool
  if (!is.logical(use_domains)) stop("use_domains has to be either TRUE or FALSE.")


  # function for shortening links to domains
  shortener <- function(URL) {
    # Reduce the links to domain-names
    helper <- lapply(URL, strsplit, "(?<=/)", perl = TRUE)
    helper2 <- rapply(helper, function(x) {
      x <- unlist(x)[1:3]
    }, how = "list")
    helper3 <- rapply(helper2, function(x) {
      x <- paste(x, collapse = "")
    }, how = "list")
    helper4 <- lapply(helper3, unlist)
    helper4[helper4 == "NANANA"] <- NA
    URL <- helper4

    return(unlist(URL))
  }

  # applying shortening function to link_vec
  if (use_domains == TRUE & (link_vec != "all")) {link_vec <- shortener(link_vec)}

  # exclude_long must be bool
  if (!c(is.na(exclude_long) | is.numeric(exclude_long))) stop("exclude_long has to be either NA or a numeric value")

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

  # limiting data to time and namescope
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ]

  # This tells us if at least one link is present (if it's TRUE then theres at least one link)
  LinkPresent <- !sapply(sapply(data$URL, is.na), sum)

  # This tells us how many elements are in each list element (Problem: NAs are counted as length one, we can thus not differentiate between
  # Nas and elements with 1 link) -> See next line
  NoElements <- lengths(data$URL)

  # We take the New counter and set it to zero where-ever no links are present
  NoElements[LinkPresent == FALSE] <- 0

  # URLS
  UnlistedUrls <- unlist(data$URL)
  NewUrls <- UnlistedUrls[!is.na(UnlistedUrls)]

  # Senders
  NewSender <- list()

  for (i in seq_along(data$Sender)) {
    NewSender[[i]] <- rep(data$Sender[i], NoElements[i])
  }

  NewSender <- unlist(NewSender)

  # New Dates
  NewDates <- list()

  for (i in seq_along(data$DateTime)) {
    NewDates[[i]] <- rep(data$DateTime[i], NoElements[i])
  }

  NewDates <- as.POSIXct(unlist(NewDates), origin = "1970-01-01",tz = "UTC")

  # shorten URLs to domain
  if (use_domains == TRUE) {

    # applying shortening function
    NewUrls <- shortener(NewUrls)

  } else {}

  # pasting together
  NewFrame <- cbind.data.frame(NewDates, NewSender, NewUrls)

  # creating time data
  NewFrame$hour <- as.POSIXlt(NewFrame$NewDates,tz = "UTC")$hour
  NewFrame$year <- as.POSIXlt(NewFrame$NewDates,tz = "UTC")$year + 1900
  NewFrame$day <- weekdays(as.POSIXlt(NewFrame$NewDates,tz = "UTC"), abbreviate = FALSE)

  # setting correct link_vec
  if (length(link_vec) == 1 && link_vec == "all") {
    link_vec <- unique(NewUrls)
  }

  # restricting to link_vec range
  NewFrame <- NewFrame[is.element(NewFrame$NewUrls, link_vec), ]

  if (dim(NewFrame)[1] == 0) {

    # exit
    warning("No Url defined by link_vec is contained in the chat")
    stop()
  }

  if (plot == "heatmap") {
    # shaping dataframe
    helperframe2 <- NewFrame %>%
      group_by(day, hour) %>%
      summarise("Number of Links" = n())

    # factor ordering
    weekdays <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    # transalte to english for better compatibility
    helperframe2$day <- mgsub(helperframe2$day,
      pattern = c("Sonntag", "Samstag", "Freitag", "Donnerstag", "Mittwoch", "Dienstag", "Montag"),
      replacement = weekdays
    )

    helperframe2$day <- as.factor(helperframe2$day)

    if (sum(weekdays %in% levels(helperframe2$day)) == 7) {
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)
    } else {
      helperframe2$day <- factor(helperframe2$day, c(levels(helperframe2$day), weekdays[!weekdays %in% levels(helperframe2$day)]))
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)
    }

    # plotting Heatmap
    out <- ggplot(helperframe2, aes(hour, day)) +
      theme_minimal() +
      geom_tile(aes(fill = `Number of Links`), colour = "black", width = 1) +
      labs(
        title = "Links by Weekday and Hour",
        subtitle = paste(starttime, " - ", endtime),
        x = "",
        y = ""
      ) +
      scale_fill_distiller(palette = "YlGnBu", direction = 1) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()
      ) +
      coord_equal() +
      scale_x_continuous(
        breaks = seq(-0.5, 23.5, 1),
        limits = c(-0.5, 23.5),
        labels = c(
          "00:00",
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
          "24:00"
        )
      )

    # printing
    print(out)

    if (return_data == TRUE) {
      # returning
      return(as.data.frame(helperframe2))
    } else {
      return(out)
    }
  }


  if (plot == "cumsum") {
    # cumulative number of links per sender
    NewFrame$counter <- rep(1, length(NewFrame$NewUrls))
    NewFrame$total <- ave(NewFrame$counter, NewFrame$NewSender, FUN = cumsum)

    # renaming to fix plot legend
    names(NewFrame)[1:3] <- c("Date", "Sender", "URL")

    # constructing graph
    out <- ggplot(NewFrame, aes(x = Date, y = total, color = Sender)) +
      theme_minimal() +
      geom_line() +
      labs(
        title = "Cumulative number of Links sent",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Time") +
      ylab("Total Links Sent") +
      geom_point()

    # printing plot
    print(out)

    if (return_data == TRUE) {
      # returning
      return(as.data.frame(NewFrame))
    } else {
      return(out)
    }
  }

  if (plot == "bar") {

    # creating dataframe for plotting
    Links <- names(sort(table(NewFrame$NewUrls), decreasing = TRUE))
    Freq <- sort(table(NewFrame$NewUrls), decreasing = TRUE)
    names(Freq) <- NULL
    df <- cbind.data.frame(Links, Freq)

    # removing a random variable that comes out of nowhere
    if ("Var1" %in% colnames(df)) {
      df <- df[, -c(2)]
    }


    # excluding links that are too long
    if (!is.na(exclude_long)) {
      df <- df[sapply(as.character(df$Links), nchar) <= exclude_long, ]
    }

    # Visualizing the distribution of domains (domains sent more than x)
    out <- ggplot(df[df$Freq >= min_occur, ], aes(x = Links, y = Freq, fill = Links)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Distribution of Domains of sent Links",
        subtitle = paste(starttime, " - ", endtime),
        x = "Domains",
        y = "Frequency"
      ) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
        legend.position = "none"
      )

    # printing
    print(out)

    # return data
    if (return_data == TRUE) {
      # returning
      return(as.data.frame(df))
    } else {
      return(out)
    }
  }

  if (plot == "splitbar") {
    ## Summarize per Sender who often each domain was sent
    SumFrame <- group_by(NewFrame, NewSender, NewUrls) %>% summarise(n = n())
    SumFrame <- SumFrame[SumFrame$n >= min_occur, ]

    # renaming to fix legend title
    names(SumFrame) <- c("Sender", "URL", "n")

    # building graph object
    out <- ggplot(SumFrame, aes(x = Sender, y = n, fill = URL)) +
      theme_minimal() +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(
        title = "Links sent per Person and Domain",
        subtitle = paste(starttime, " - ", endtime),
        x = "Sender",
        y = "Frequency"
      )

    # only printing legend if we have 20 unique websites or less
    if (length(unique(SumFrame$URL)) <= 20) {
      print(out)
    } else {
      warning("Legend was dropped because it contained too many links")
      print(out + theme(legend.position = "none"))
    }

    # return data
    if (return_data == TRUE) {
      # returning
      return(as.data.frame(SumFrame))
    } else {
      return(out)
    }
  }

}
