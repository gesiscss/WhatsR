## Test environments
* local Linux, R 4.5.1
* Github runner: Ubuntu, Windows, macOS (optional)
* Checked with `R CMD check --as-cran`

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Comments
- Removed LICENSE file and mention of + LICENSE file in Description as per request
- Fixed incorrect date in DESCRIPTION file leading to NOTE in auto-check
- This is a maintenance update that:
  - Improves parsing of changed WhatsApp export formats
  - Updates regex coverage for system messages
