#' @title Regular Expression for WhatsApp Timestamps
#'
#' @description
#' A unified regular expression pattern used to detect timestamp prefixes in WhatsApp chat exports from both Android and iOS.
#' Supports multiple date and time formats, optional square brackets, and various separators.
#'
#' @details The expression is designed to match:
#' \itemize{
#'   \item Dates in formats like \code{dd.mm.yy}, \code{dd/mm/yyyy}, or \code{yyyy-mm-ddTHH:MM:SS}
#'   \item Optional square brackets and leading/trailing whitespace
#'   \item Times like \code{HH:MM} or \code{HH:MM:SS}, with optional AM/PM
#'   \item Optional separators such as \code{-} or \code{–}
#' }
#'
#' This regular expression is used as a core component for detecting message boundaries in WhatsApp exports.
#'
#' @format A character string containing a regular expression.
#' @keywords internal
"timestamp_regex"


# Unified “timestamp” regex for Android & iOS exports
timestamp_regex <- paste0(
  "^\\s*\\[?",                                                    # optional “[”
  "(?:\\d{1,2}[./-]\\d{1,2}[./-]\\d{2,4}",                         #  29.01.18 or 1/29/18
  "|\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}",                 #  ISO 2018-01-29T23:33:00
  "|[[:alpha:]]+\\s+\\d{1,2},?\\s*\\d{4})",                      #  January 29, 2018
  ",?\\s*",                                                       # optional comma+space
  "\\d{1,2}:\\d{2}(?::\\d{2})?",                                   # 12:24 or 12:24:03
  "(?:\\s*[APap][Mm])?",                                           # optional AM/PM
  "\\]?",                                                         # optional “]”
  "\\s*(?:[-–]\\s)?"                                               # optional “ - ” or “ – ”
)

#' @title Detect New Messages and Parse WhatsApp Logs (Base Version)
#'
#' @description
#' \code{detect_new_message()} identifies new message lines in WhatsApp exports based on timestamp patterns.
#' \code{parse_base()} uses these detections to split and parse an exported WhatsApp chat file into a tidy data frame.
#'
#' @param lines A character vector of lines from the exported WhatsApp .txt file (used by \code{detect_new_message()}).
#' @param path A character string indicating the file path to an exported WhatsApp .txt file (used by \code{parse_base()}).
#'
#' @return
#' \describe{
#'   \item{\code{detect_new_message()}}{A logical vector indicating whether each line starts a new message.}
#'   \item{\code{parse_base()}}{A tibble with one row per message and columns \code{Timestamp}, \code{Sender}, and \code{Message}.}
#' }
#'
#' @details
#' This minimal parser supports both Android and iOS exports. Messages are split based on timestamp detection.
#' System messages (e.g., group join/leave) are labeled as \code{"WhatsApp System Message"}.
#'
#' Unicode control characters (e.g., \code{U+FEFF}, \code{U+200E}, \code{U+200F}) are removed before timestamp matching.
#'
#' @examples
#' \dontrun{
#' # Minimal working example:
#' path <- system.file("englishandroid24h.txt", package = "WhatsR")
#' parsed <- parse_base(path)
#' head(parsed)
#' }
#'
#' @importFrom lubridate parse_date_time
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate rowwise ungroup select
#' @importFrom stringr str_replace_all str_detect str_remove str_extract str_trim
#' @importFrom tibble tibble
#' @export
detect_new_message
#' @export
parse_base


# Detect new-message boundaries by testing if the line starts with a real timestamp
detect_new_message <- function(lines) {
  clean <- str_replace_all(lines, "[\uFEFF\u200E\u200F]", "")   # strip BOM + LTR / RTL marks  ← NEW
  str_detect(clean, timestamp_regex)
}


# Main parser
parse_base <- function(path) {
  # 3a) Read & pre‐clean
  lines <- readLines(path, encoding = "UTF-8")
  lines <- str_replace_all(lines, "\uFEFF", "")
  lines <- str_replace_all(lines, '^"|"$', "")
  if (any(str_detect(lines, "\\\\n"))) {
    lines <- unlist(str_split(lines, "\\\\n\\s*"))
  }
  lines <- str_trim(lines)

  # 3b) Chunk by boundary
  is_new <- detect_new_message(lines) # old, working version
  groups <- cumsum(is_new)
  chunks <- split(lines, groups)

  # 3c) Build tidy tibble
  tibble(raw = chunks) %>%
    mutate(
      full   = map_chr(raw, ~ paste(.x, collapse = "\n")),
      raw_ts = str_extract(full, timestamp_regex) %>%
        str_remove_all("^\\s*\\[|\\]|\\s*[-–]\\s*$") %>%
        str_trim(),
      body   = str_trim(str_remove(full, timestamp_regex))
    ) %>%
    rowwise() %>%
    mutate(
      Sender = if (str_detect(body, "^[^:]+:")) {
        str_trim(str_remove(str_extract(body, "^[^:]+:"), ":"))
      } else {
        "WhatsApp System Message"
      },
      Message = if (Sender == "WhatsApp System Message") {
        body
      } else {
        str_trim(str_remove(body, "^[^:]+:\\s*"))
      },
      Timestamp = parse_date_time(
        raw_ts,
        orders = c(
          "dmy HM", "dmy HMS", "mdy HM", "mdy HMS",
          "dmy HM p", "dmy HMS p", "mdy HM p", "mdy HMS p",
          "Ymd HMS", "BdY HM p", "BdY HMS"
        ),
        locale = "en_US.utf8"
      )
    ) %>%
    ungroup() %>%
    select(Timestamp, Sender, Message)
}
