#' @title Fetching Emoji Dictionary from Emojipedia.org
#'
#' @description Fetches a dictionary of emojis from www.emojipedia.org, assuming that the website is available and its structure does not change.
#' Can be used to update the emoji dictionary contained in this package. The dictionary is ordered according to the length of
#' the emojis' byte representation (longer ones first) to prevent partial matching of shorter strings when iterating
#' through the data frame from top to bottom.
#' @param pages A character vector containing the URLs of the emoji categories you want to fetch, e.g. 'https://emojipedia.org/people/'
#' @param skinpages A character vector containing the URLs of the skintone modifier categories you want to fetch, e.g. 'https://emojipedia.org/emoji-modifier-fitzpatrick-type-3/'
#' @param regular_xpath Xpath of the html table containing the emoji information, e.g. '/html/body/div[5]/div[1]/ul'
#' @param skinpages_xpath Xpath of the html table containing the skintone modifier information, e.g. '/html/body/div[5]/div[1]/article/section[1]/ul'
#' @param exception_xpath Xpath of the html table containing the skintone modifier information for Fitzpatrick 1-2, e.g. '/html/body/div[5]/div[1]/article/section[1]/ul[2]'
#' @export
#' @importFrom rvest html_nodes html_text
#' @importFrom XML xmlTreeParse xmlToList
#' @importFrom xml2 read_html
#' @return A data frame containing:
#'      1) The native representation of all emoji in R \cr
#'      2) A textual description of what the emoji is displaying \cr
#'      3) Original order of the HTML table that the emojis were fetched from
#'
#' @examples
#'Emoji_dictionary <- download_emoji()

# Function to fetch and update the emoji dictionary from www.emojipedia.org
download_emoji <- function(pages = c(
                             "https://emojipedia.org/people/",
                             "https://emojipedia.org/nature/",
                             "https://emojipedia.org/food-drink/",
                             "https://emojipedia.org/activity/",
                             "https://emojipedia.org/travel-places/",
                             "https://emojipedia.org/objects/",
                             "https://emojipedia.org/symbols/",
                             "https://emojipedia.org/flags/"
                           ),
                           skinpages = c(
                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-1-2/",
                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-3/",
                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-4/",
                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-5/",
                             "https://emojipedia.org/emoji-modifier-fitzpatrick-type-6/"
                           ),
                           regular_xpath = "/html/body/div[5]/div[1]/ul", # this keeps changing occasionally, mostly the index of the first div changes between 4 and 5
                           skinpages_xpath = "/html/body/div[5]/div[1]/article/section[1]/ul", # this keeps changing occasionally, mostly the index of the first div changes between 4 and 5
                           exception_xpath = "/html/body/div[5]/div[1]/article/section[1]/ul[2]") { # this keeps changing occasionally, mostly the index of the first div changes between 4 and 5


  # defining function to scrape and parse XML tables
  scraper <- function(url, UseXpath, exception_xpaths = exception_xpath) {
    # exception handling
    if (url == "https://emojipedia.org/emoji-modifier-fitzpatrick-type-1-2/") {
      UseXpath <- exception_xpaths
    }

    #### importing data
    Emoji <- read_html(url)
    EmojiList <- html_nodes(Emoji, xpath = UseXpath)[[1]]

    # forcing it to text
    EmojiText <- html_text(EmojiList)

    # removing inconsistent whitespaces
    EmojiText <- gsub("\n ", "\n", EmojiText)
    EmojiText <- strsplit(EmojiText, "\\n", fixed = FALSE)
    EmojiText <- unlist(EmojiText)

    # removing empty strings and html leftovers
    EmojiText <- EmojiText[EmojiText != ""]
    EmojiText <- EmojiText[nchar(EmojiText) < 100]
    EmojiText <- EmojiText[EmojiText != "          "]
    EmojiText <- EmojiText[EmojiText != "         "]
    EmojiText <- strsplit(EmojiText, " ")

    # Extracting emoji
    Emojis <- sapply(EmojiText, `[[`, 1)

    # Extracting description
    EmojiNames <- NULL
    for (i in 1:length(EmojiText)) {
      EmojiNames[i] <- paste(EmojiText[[i]][2:length(EmojiText[[i]])], collapse = " ")
    }

    # creating dataframe
    DF <- data.frame(Emojis, EmojiNames)

    # returning data frame
    return(DF)
  }

  # Scraping Notone Emojis
  NotoneEmojis <- tryCatch(
    expr = {
      lapply(pages, scraper, UseXpath = regular_xpath)
    },
    error = function(e) {
      message("There has been an error fetching the emojis. Please check if all the pages you want to scrape are accessible and if the XPath is up to date.")
      message("This is the original error message:")
      message(e)
      return(NA)
    },
    warning = function(w) {
      message(w)
    }
  )

  # Scraping skin-tone emoji
  SkintoneEmojis <- tryCatch(
    expr = {
      lapply(skinpages, scraper, UseXpath = skinpages_xpath)
    },
    error = function(e) {
      message("There has been an error fetching the skintone modifiers. Please check if all the pages you want to scrape are accessible and if the XPath is up to date.")
      message("This is the original error message:")
      message(e)
      return(NA)
    },
    warning = function(w) {
      message(w)
    }
  )

  # Pasting lists together
  Emojis <- c(NotoneEmojis, SkintoneEmojis)

  # collapsing list of lists into dataframe
  EmojiDF <- do.call(rbind, Emojis)

  # removing duplicates
  EmojiDF <- EmojiDF[duplicated(EmojiDF) == FALSE, ]

  # fixing rownames
  rownames(EmojiDF) <- 1:dim(EmojiDF)[1]

  # fixing descriptions
  EmojiDF$EmojiNames <- gsub(" ", "_", EmojiDF$EmojiNames)

  # fixing column names
  colnames(EmojiDF) <- c("R.native", "Desc")

  # ordering from longest to shortest (prevents partial matching of shorter strings further down the line)
  EmojiDF <- EmojiDF[rev(order(nchar(as.character(EmojiDF$R.native)))), ]

  # saving original order
  EmojiDF$OriginalOrder <- rownames(EmojiDF)

  # fixing rownames
  rownames(EmojiDF) <- 1:dim(EmojiDF)[1]

  # return dictionary
  return(EmojiDF)
}
