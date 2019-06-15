js_lines = readLines("internal_data/get_clicked_bar.js")
SCRIPT_TEMPLATE = paste(trimws(js_lines), collapse = " ")

usethis::use_data(
  SCRIPT_TEMPLATE,
  internal = TRUE,
  overwrite = TRUE
)
