#' @title Visualize the Network of replies in Whatsapp chatlogs
#' @description Plots a network for replies between authors in chatlogs
#' @param data A WhatsApp chatlog that was parsed with WhatsAppParse()
#' @param names A vector of author names that the visulaization will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param return.data If TRUE, returns a dataframe of LatLon coordinates extracted from the chat for more elaborate plotting. Default is FALSE.
#' @param save If FALSE, does not save the network visualization in a file. Any other value saves the visualization as an html file.
#' @param filename filename for the saved network visualization, default is "whatsapp_network"
#' @importFrom anytime anytime
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table rleid
#' @importFrom data.table :=
#' @importFrom networkDynamic networkDynamic
#' @importFrom network set.vertex.attribute
#' @importFrom network set.edge.attribute
#' @importFrom network get.vertex.attribute
#' @importFrom network get.edge.attribute
#' @importFrom ndtv compute.animation
#' @importFrom ndtv render.d3movie
#' @importFrom utils read.csv
#' @export
#' @return html file with network visualization of authors in WhatsApp chatlog
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' WaNetworkAnimation(data)

### visualizing Distribution of reply times only between multiple senders and recipients
# TODO: There is a naming error, WAnetwork is not properly defined
WaNetworkAnimation <- function(data,
                               names = "all",
                               starttime = anytime("1960-01-01 00:00"),
                               endtime = Sys.time(),
                               return.data = FALSE,
                               save = "html",
                               filename = "whatsapp_network") {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  `.` <- WAnetwork <- `%v%<-` <- `%e%<-` <- NULL

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

  # We need to exclude the WhatsApp system messages
  Tempframe <- data[data$Sender != "WhatsApp System Message",]

  # finding start and end postions of streaks
  streaks <- data.table(Tempframe$Sender)[ , .(start = .I[1], end = .I[.N]), by = rleid(Tempframe$Sender)][, rleid := NULL][]

  # we find start and endtimes of streaks
  Starttime <- data$DateTime[streaks$start]
  Endtime <- data$DateTime[streaks$end]

  # creating new vector for time difference between sessions
  SessionDiff <- vector()

  # computing time differences for sessions
  Sessionlength <- difftime(Endtime,Starttime, units = "mins")

  # What we need now is a vector of sender names in the right order but without repetitions
  # and to put int into a new dataframe
  Sessionframe <- cbind.data.frame(Sender = rle(as.character(Tempframe$Sender))$values,
                                   Starttime = Starttime,
                                   Endtime = Endtime,
                                   MessageAmount = rle(as.character(Tempframe$Sender))$lengths,
                                   Duration = Sessionlength)

  # creating new vector for computing the timediff since the last message of another person
  Replytime <- vector()

  for (i in 1:length(Sessionframe$Starttime)) {

    if (i == 1) {

      Replytime[i] <- NA

    }

    if (i != 1) {

      Replytime[i] <- difftime(Sessionframe$Starttime[i],Sessionframe$Endtime[i - 1], units = "mins")

    }

  }

  # putting it into the new dataframe:
  Sessionframe <- cbind.data.frame(Sessionframe,
                                   Replytime)

  # We need to reverse the order first so we can interpret edges from A -> B as a response of A to B
  # We take the Sessionframe as a basis because it excludes the WhatsApp System Messages already and
  # accounts for sessions
  Graphframe <- Sessionframe[seq(dim(Sessionframe)[1],1),]

  # Putting relevant info together and bundling it into a dataframe
  OP <- as.character(Graphframe$Sender[seq(2,(length(Graphframe$Sender)),1)])
  Replyer <- as.character(Graphframe$Sender[seq(0,length(Graphframe$Sender) - 1,1)])
  TimeStamp <- Graphframe$Endtime[seq(2,(length(Graphframe$Sender)),1)]
  Replytime <- Graphframe$Replytime[seq(2,(length(Graphframe$Sender)),1)]
  MessageAmount <- Graphframe$MessageAmount[seq(2,(length(Graphframe$Sender)),1)]

  # pasting everything together
  NetFrame <- cbind.data.frame(Replyer,
                               OP,
                               TimeStamp,
                               Replytime,
                               MessageAmount,
                               stringsAsFactors = FALSE)

  # order Netframe chronologically based on TimeStamp
  NetFrame <- NetFrame[nrow(NetFrame):1,]

  # Creating static data
  StaticEdges <- NetFrame[,c(1,2)]
  StaticVertexAttributes <- data.frame(name = unique(NetFrame$OP), stringsAsFactors = FALSE)

  # Creating dynamic Data
  DynamicEdges <- StaticEdges

  # We add the onset and terminus to the Dynamic Edges (when each edge should be drawn for the first time and when it should fade out)
  onset <- as.numeric(1:length(DynamicEdges$OP))
  terminus <- as.numeric(length(DynamicEdges$OP) + 1)

  # right order
  DynamicEdges <- data.frame(onset,terminus, tail = StaticEdges$Replyer, head = StaticEdges$OP, stringsAsFactors = TRUE)
  DynamicEdges$tail <- as.numeric(DynamicEdges$tail)
  DynamicEdges$head <- as.numeric(DynamicEdges$head)

  # To create usefull information in the network like edge weights etc.
  Interactions <- NetFrame[,c(1,2)]
  PastedInteractions <- rep(NA,nrow(Interactions))

  for (i in 1:nrow(Interactions)) {

    PastedInteractions[i] <- paste(Interactions[i,1],Interactions[i,2], collapse = "_")

  }

  # Initializing counter
  IncreasingCounter <- rep(NA,nrow(Interactions))

  # computing counter
  for (i in unique(PastedInteractions)) {

    IncreasingCounter[which(PastedInteractions == i)] <- 1:length(which(PastedInteractions == i))

  }

  # factorized interactions
  FacInt <- as.numeric(as.factor(PastedInteractions))

  # Adding the Vars to the Frame as Edgeweights
  DynamicEdges <- cbind.data.frame(DynamicEdges,FacInt,IncreasingCounter,stringsAsFactors = FALSE)

  # We add onset and Terminus to the Dynamic Vertex attributes (when each vertex hould be drawn first and when it should fade out)
  DynamicNodes <- StaticVertexAttributes
  onset <- as.numeric(1)
  terminus <- as.numeric(length(DynamicEdges$head) + 1)

  # right order
  DynamicNodes <- data.frame(onset,terminus, vertex.id = as.numeric(1:nrow(StaticVertexAttributes)),stringsAsFactors = FALSE)

  # Make the temporal network (combining everything)
  dynamicWA <- networkDynamic(
    WAnetwork,
    edge.spells = DynamicEdges,
    vertex.spells = DynamicNodes
  )

  # fixing the levelling of the node attributes
  # dynamicWA %v% "name" <- levels(as.factor(StaticVertexAttributes$name))
  set.vertex.attribute(dynamicWA,"name",levels(as.factor(StaticVertexAttributes$name)))

  # setting edge weights
  # dynamicWA %e% "amount" <- DynamicEdges$IncreasingCounter
  set.edge.attribute(dynamicWA,"amount",DynamicEdges$IncreasingCounter)

  # Calculate how to plot an animated version of the dynamic network
    compute.animation(
    dynamicWA,
    animation.mode = "kamadakawai",
    slice.par = list(
      start = 0,
      end = nrow(DynamicEdges) + 1,
      interval = 1,
      aggregate.dur = 1,
      rule = "any"
    )
  )

  # Render the animation and open it in a web brower
    render.d3movie(
    dynamicWA,
    displaylabels = FALSE,
    main = "Visualization of WhatsApp Network Dynamic",
    usearrows = TRUE,

    # This slice function makes the vertex labels work
    vertex.tooltip = function(slice) {
      paste(
        "<b>Name:</b>", (get.vertex.attribute(slice,"name"))
      )
    },

    # This slice function makes the edge labels work
    edge.tooltip = function(slice) {
      paste(
        "<b>Message Amount:</b>", (get.edge.attribute(slice,"amount"))
      )
    },

    # This makes edges thicker the more exchanges happen
    edge.lwd = DynamicEdges$IncreasingCounter,

    # saving the object
    if (save != FALSE){file = "WANetworkAnimation.html"})

  # returning the Network object(s)
  if (return.data == TRUE) {

    # putting together output list
    out <- list(NetworkDataFrame = NetFrame, DynamicNodeList = DynamicNodes, DynamicEdgeList = DynamicEdges, DynamicNetwork = dynamicWA)

    # returning list
    return(out)
  }

}
