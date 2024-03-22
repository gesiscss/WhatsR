#' @title Plotting locations sent in 'WhatsApp' chat logs on maps
#' @description Plots the location data that is sent in the 'WhatsApp' chatlog on an auto-scaled map. Requires unanonymized 'Location' column in data
#' @param data A 'WhatsApp' chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}with anonymize= FALSE or anonymize = "add".
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param mapzoom Value for zoom into the map passed down to \code{\link[ggmap]{get_map}}. Default value is 5. Higher zoom will auto-download more map files which can take a while.
#' @param return_data If TRUE, returns a data frame of LatLon coordinates extracted from the chat for more elaborate plotting. Default is FALSE.
#' @param jitter_value Amount of random jitter to add to the geolocations to hide exact locations. Default value is 0.01. Can be NA for exact locations.
#' @param jitter_seed Seed for adding random jitter to coordinates. Passed to \code{\link[base]{set.seed}}
#' @param exclude_sm If TRUE, excludes the 'WhatsApp' system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet setView
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet addLegend
#' @importFrom grDevices rainbow
#' @importFrom stats runif
#' @importFrom stringi stri_extract_all
#' @importFrom methods is
#' @export
#' @return Plots for geolocation and/or a data frame of latitude and longitude coordinates
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_locations(data)
#'
### Plotting locations conained in WhatsApp chat logs on maps
plot_locations <- function(data,
                           names = "all",
                           starttime = "1960-01-01 00:00",
                           endtime = "2200-01-01 00:00",
                           mapzoom = 5,
                           return_data = FALSE,
                           jitter_value = NA,
                           jitter_seed = 12345,
                           exclude_sm = FALSE
                           ) {

   # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
   cond <- Lon <- Lat <- Sender <- NULL

   # checking data
   if (!is.data.frame(data)) {stop("'data' must be a dataframe parsed with parse_chat()")}

   # start- and endtime are convertable to POSIXct
   if (is.character(starttime) == FALSE | is.na(as.POSIXct(starttime,tz = "UTC"))) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
   if (is.character(endtime) == FALSE | is.na(as.POSIXct(endtime,tz = "UTC"))) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
   if (as.POSIXct(starttime,tz = "UTC") >= as.POSIXct(endtime,tz = "UTC")) stop("starttime has to be before endtime.")

   # jitter_value checks
   if (!(is.numeric(jitter_value) | is.na(jitter_value))) {stop("jitter_value must be either NA for exact location or a numeric value > 0")}
   if (!is.numeric(jitter_seed)) {stop("jitter_seed must be a numeric value")}

   # return_data must be bool
   if (!is.logical(return_data)) stop("return_data has to be either TRUE or FALSE.")

   # exclude_sm must be bool
   if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

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

   # extracting locations with geo-coordinates
   Places <- unlist(stri_extract_all(data$Location, regex = "(<?)https://maps.google.com.*"))
   Places <- Places[!is.na(Places)]

   # breaking out of function if no locations are present
   if (length(Places) == 0) {
     warning("No locations in the format www.maps.google.com/q=Latitude,Longitude contained in the chat", immediate. = TRUE)
     return(NA)
   }

   # extracting latitude and longitude
   LatLong <- unlist(stri_extract_all(Places, regex = "(?<=q=).*$"))
   LatLong <- strsplit(LatLong, ",")
   LatLong <- cbind.data.frame(Lat = sapply(LatLong, `[[`, 1), Lon = sapply(LatLong, `[[`, 2))
   LatLong[, 1] <- as.numeric(as.character(LatLong[, 1]))
   LatLong[, 2] <- as.numeric(as.character(LatLong[, 2]))

   # adding jitter to conceal exact locations if desired
   if (!is.na(jitter_value)) {
     # Add some jitter to the data
     Coord_no <- dim(LatLong)[1] * dim(LatLong)[2]
     set.seed(jitter_seed)
     jitter <- runif(Coord_no, -jitter_value, jitter_value)
     LatLong <- LatLong + jitter
   }

   # creating LatLong dataframe
   Metainfo <- data[grepl(pattern = "(<?)https://maps.google.com.*", x = data$Location, perl = TRUE), c("DateTime", "Sender")]
   LatLong <- cbind.data.frame(Metainfo, LatLong)

   # round locations and add some leeway
   #location <- c(
   #   floor(min(LatLong[, 4])) - map_leeway,
   #   floor(min(LatLong[, 3])) - map_leeway,
   #  ceiling(max(LatLong[, 4])) + map_leeway,
   #  ceiling(max(LatLong[, 3])) + map_leeway
   # )

   # Create a color palette
   unique_senders <- unique(LatLong$Sender)
   colors <- rainbow(length(unique_senders))

   # Assign colors to groups by creating a factor with levels in the order of appearance
   LatLong$color <- colors[match(LatLong$Sender, unique_senders)]

   # plotting
   map <- leaflet(LatLong) %>%
           addTiles() %>%
           setView(LatLong$Lon[1],LatLong$Lat[1],zoom = mapzoom) %>%
           addCircleMarkers(~Lon,
                            ~Lat,
                            color = ~color,
                            popup = ~DateTime,
                            radius = 6,
                            fillOpacity = 0.9) %>%
           addLegend("bottomleft",
                     colors = colors,
                     labels = unique_senders,
                     title = "Sender")

     # returning LatLon data if desired
     if (return_data == TRUE) {
       return(LatLong)
     } else {
       return(map)
     }

 }
