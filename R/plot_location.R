#' @title Plotting Locations sent in WhatsApp chatlogs on maps
#' @description Plots the location data that is sent in the WhatsApp Cchatlog on an autoscaled map
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param mapzoom Value for zoom into the map passed down to get_map. default value is 5. Higher zoom will auto-download more mapfiles which can take a while.
#' @param return.data If TRUE, returns a dataframe of LatLon coordinates extracted from the chat for more elaborate plotting. Default is FALSE.
#' @param add.jitter IF TRUE, adds some random jitter to geolocations to obscure exact locations
#' @param jitter.val Amount of random jitter to add to the geolocations to hide exact locations. Default value is 0.01
#' @param jitter.seed Add
#' @param mapleeway Adds additional space to the map so that points do not sit exactly at the border of the plot. Default value is 5
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom ggmap get_map
#' @importFrom ggmap ggmap
#' @importFrom stats runif
#' @importFrom stringi stri_extract_all
#' @export
#' @return Plots for geolocation and/or a dataframe of LatLon coordinates
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_location(data,mapzoom = 10)

#### Location
# TODO: Find a way to get a good auto-zoom level
plot_location <- function(data,
                          names = "all",
                          starttime = anytime("1960-01-01 00:00"),
                          endtime = Sys.time(),
                          mapzoom = 5,
                          return.data = FALSE,
                          add.jitter = FALSE,
                          jitter.val = 0.01,
                          jitter.seed = 123,
                          mapleeway = 0.1) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  Lon <- Lat <- Sender <-  NULL

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {

    starttime <- min(data$DateTime)

  } else {starttime <- anytime(starttime,asUTC = TRUE)}

  # setting endtime
  if (difftime(Sys.time(),endtime, units = "min") < 1) {

    endtime <- max(data$DateTime)

  } else {endtime <- anytime(endtime,asUTC = TRUE)}

  # setting names argument
  if (length(names) == 1 && names == "all") {

    # All names in the dataframe except System Messages
    names <- unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  # extracting locations with geocoordinates
  Places <- unlist(stri_extract_all(data$Location, regex = "(<?)https.*"))
  Places <- Places[!is.na(Places)]

  # extracting latitude and longitude
  LatLong <- unlist(stri_extract_all(Places, regex = "(?<=q=).*$"))
  LatLong <- strsplit(LatLong,",")
  LatLong <- cbind.data.frame(Lat = sapply(LatLong, `[[`, 1), Lon = sapply(LatLong, `[[`, 2))
  LatLong[,1] <- as.numeric(as.character(LatLong[,1]))
  LatLong[,2] <- as.numeric(as.character(LatLong[,2]))

  if (add.jitter == TRUE) {

    # Add some jitter to the data for anonmization purposes
    Coord_no <- dim(LatLong)[1] * dim(LatLong)[2]
    set.seed(jitter.seed)
    jitter <- runif(Coord_no,-jitter.val,jitter.val)
    LatLong <- LatLong + jitter

  }

  # creating LatLong dataframe
  Metainfo <- data[grepl(pattern = "(<?)https.*",x = data$Location,perl = TRUE),c("DateTime","Sender")]
  LatLong <- cbind.data.frame(Metainfo,LatLong)

  # round locations and add some leeway
  location = c(floor(min(LatLong[,4]))-mapleeway,
               floor(min(LatLong[,3]))-mapleeway,
               ceiling(max(LatLong[,4]))+mapleeway,
               ceiling(max(LatLong[,3]))+mapleeway)

  # Fetch the map
  map <- get_map(location = location, source = "google", zoom = mapzoom)

  # Add the points layer
  map <- ggmap(map) +
         geom_point(data = LatLong, aes(x = Lon, y = Lat, fill = Sender), color = "black", size = 2, pch = 21) +
         labs(title = "Locations in Conversation",
              subtitle = paste(starttime, " - ", endtime),
              x = "Longitude",
              y = "Latitude")

  # plot
  plot(map)

  # returning LatLon data if desired
  if (return.data == TRUE){

    return(LatLong)

  } else{

    return(map)

  }


}
