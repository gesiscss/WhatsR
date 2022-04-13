#' @title Fetching Emoji Dictionary from Emojipedia.org
#'
#' @description Fetches a dictionary of emojis from www.emojipedia.org, assuming that the websites structure does not change.
#' Can be used to update Emoji dictionary contained in this package. The dictionary is ordered according to the length
#' the emojis' byte representation (longer ones first) to prevent partial matching of shorter strings when iterating
#' trough the dataframe from top to bottom.
#' @param pages A character vector containing the URLs of the emoji categories you want to fetch
#' @param skinpages A character vector containing the URLs of the skintone modifier categories you want to fetch
#' @param RegularXpath Xpath of the html table containing the emoji information
#' @param SkinXpath Xpath of the html table containing the skintone modifier information
#' @param ExceptionXpath Xpath of the html table containing the skintone modifier information for Fitzpatrick 1-2
#' @export
#' @importFrom rvest html_nodes
#' @importFrom XML xmlTreeParse xmlToList
#' @importFrom xml2 read_html
#' @importFrom pryr bytes
#' @return A dataframe containing:
#'      1) The native representation of all Emoji in R \cr
#'      2) A textual description of what the Emoji is displaying \cr
#'      3) The body of the message. Linebreaks and Emojis are replaced with textual indicators \cr
#'      4) Original Order of HTML table that the Emojis were fetched from
#'
#' @examples
#' EmojiDic <- download_emoji()

# Function to fetch and update the Emoji dictionary from www.emojipedia.org
download_emoji <- function(pages =         c("https://emojipedia.org/people/",
                                             "https://emojipedia.org/nature/",
                                             "https://emojipedia.org/food-drink/",
                                             "https://emojipedia.org/activity/",
                                             "https://emojipedia.org/travel-places/",
                                             "https://emojipedia.org/objects/",
                                             "https://emojipedia.org/symbols/",
                                             "https://emojipedia.org/flags/"),
                           skinpages =     c("https://emojipedia.org/emoji-modifier-fitzpatrick-type-1-2/",
                                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-3/",
                                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-4/",
                                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-5/",
                                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-6/"),
                           RegularXpath =    "/html/body/div[3]/div[1]/ul", # this keeps changing occasionally, mostly the index of the first div changes between 2 and 3
                           SkinXpath =       "/html/body/div[3]/div[1]/article/section[1]/ul",# this keeps changing occasionally, mostly the index of the first div changes between 2 and 3
                           ExceptionXpath =  "/html/body/div[3]/div[1]/article/section[1]/ul[2]"){ # this keeps changing occasionally, mostly the index of the first div changes between 2 and 3


  # function to scrape and parse XML tables
  scraper <- function(url,UseXpath, ExceptionXpaths = ExceptionXpath){

    # exception handling
    if(url == "https://emojipedia.org/emoji-modifier-fitzpatrick-type-1-2/"){UseXpath <- ExceptionXpaths}

    #### importing data
    Emojis <- read_html(url)
    EmojiList<-html_nodes(Emojis, xpath = UseXpath)[[1]]
    EmojiNode <- xmlTreeParse(EmojiList)
    EmojiList <- xmlToList(EmojiNode)

    # Deleting unnecessary attributes
    EmojiList$.attrs <- NULL

    #### Parsing Data

    Emojis <- EmojiList[seq(1,length(EmojiList),3)]
    Emojis <- sapply(Emojis, `[[`, 1)

    EmojiNames <- EmojiList[seq(2,length(EmojiList),3)]
    EmojiNames <- unlist(EmojiNames)

    # adding byte column
    Bytes <- bytes(Emojis)

    DF <- data.frame(Emojis,EmojiNames,Bytes)
    return(DF)

  }

  # Initiating list objects
  Emojis <- list()
  SkinEmojis <- list()

  # Scraping Emojis
  RegularEmojis <- lapply(pages,scraper, UseXpath = RegularXpath)

  # Scraping Skin-tone Emojis
  OtherEmojis <- lapply(skinpages,scraper, UseXpath = SkinXpath)

  # Pasting lists together
  Emojis <- c(RegularEmojis,OtherEmojis)

  # collapsing list of lists into dataframe
  EmojiDF <- do.call(rbind, Emojis)

  # remove duplicates
  EmojiDF <- EmojiDF[duplicated(EmojiDF) == FALSE,]

  # fixing rownames
  rownames(EmojiDF) <- 1:dim(EmojiDF)[1]

  # fixing descriptions
  EmojiDF$EmojiNames <- gsub(" ","_",EmojiDF$EmojiNames)

  # fixing column names
  colnames(EmojiDF) <- c("R.native","Desc", "Bytestring")

  # ordering from longest to shortest (prevents partial matching of shorter strings further down the line)
  EmojiDF <- EmojiDF[rev(order(nchar(as.character(EmojiDF$R.native)))),]

  # saving original order
  EmojiDF$OriginalOrder <- rownames(EmojiDF)

  # fixing rownames
  rownames(EmojiDF) <- 1:dim(EmojiDF)[1]

  # return dictionary
  return(EmojiDF)
}
