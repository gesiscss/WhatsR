% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_locations.R
\name{plot_locations}
\alias{plot_locations}
\title{Plotting locations sent in 'WhatsApp' chat logs on maps}
\usage{
plot_locations(
  data,
  names = "all",
  starttime = "1960-01-01 00:00",
  endtime = "2200-01-01 00:00",
  mapzoom = 5,
  return_data = FALSE,
  jitter_value = NA,
  jitter_seed = 12345,
  exclude_sm = FALSE
)
}
\arguments{
\item{data}{A 'WhatsApp' chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}with anonymize= FALSE or anonymize = "add".}

\item{names}{A vector of author names that the plots will be restricted to.}

\item{starttime}{Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.}

\item{endtime}{Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.}

\item{mapzoom}{Value for zoom into the map passed down to \code{\link[ggmap]{get_map}}. Default value is 5. Higher zoom will auto-download more map files which can take a while.}

\item{return_data}{If TRUE, returns a data frame of LatLon coordinates extracted from the chat for more elaborate plotting. Default is FALSE.}

\item{jitter_value}{Amount of random jitter to add to the geolocations to hide exact locations. Default value is 0.01. Can be NA for exact locations.}

\item{jitter_seed}{Seed for adding random jitter to coordinates. Passed to \code{\link[base]{set.seed}}}

\item{exclude_sm}{If TRUE, excludes the 'WhatsApp' system messages from the descriptive statistics. Default is FALSE.}
}
\value{
Plots for geolocation and/or a data frame of latitude and longitude coordinates
}
\description{
Plots the location data that is sent in the 'WhatsApp' chatlog on an auto-scaled map. Requires unanonymized 'Location' column in data
}
\examples{
data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
plot_locations(data)

}
