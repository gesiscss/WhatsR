# WhatsR 1.0.6

- Updated parser to handle new chat log export document structure across all supported OS and languages
- Updated RegExes for detecting new or changed system messages
- Added new system message detection RegExes for `MetaAI` chat bot inclusion in chats
- Updated Regex for detecting timestamps, especially German AM/PM translations
- Added and updated some minor, group-specific system messages in `languages.csv`
- Accounted for multiple versions of "this chat is encrypted" start message in `languages.csv`

# WhatsR 1.0.5

- Fixed some issues with `plot_network()` for recipient names including space characters
- Switched to `leaflet` for `plot_locations()` for better interactivity

# WhatsR 1.0.4

- Updated Regexes, tests, and testing files to new WhatsApp document structure
- Renamed `plot_location()` to `plot_locations()`

# WhatsR 1.0.2

- Temporarily removed `plot_locations()` functions until updated `ggmap` version becomes available on CRAN
- Added `stringi::stri_enc_tonative()` in tests and examples to avoid character encoding issues
- Fixed a bug in all heatmaps where width could be displayed incorrectly by adding `width = n` parameter
- Corrected spelling in all functions and test files (e.g., "anonymize")
- Converted `download_emoji()` function to rely on unicode websites instead of Emojipedia
- Added code to remove NAs before passing objects to `ggplot2` to remove warnings
- Ordering dataframes by `TimeOrder` column for plotting functions
- Fixed issue in `plot_network()` by using `dplyr::desc()` instead of `stats::dist()`

# WhatsR 1.0.1

- First CRAN release

# WhatsR 1.0.0

- Initial release of WhatsR
- Included citation
- Removed local timezone dependency and switched to UTC conversions
