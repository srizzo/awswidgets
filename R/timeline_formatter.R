#' @export
timeline_formatter <- function(start, duration, label, error_message, highlight) {
  mapply(function(start, duration, label, error_message, highlight) {
    htmltools::doRenderTags(
      htmltools::div(class = "timeline-widget",
                     style = formattable::style(display = "grid", `grid-template-columns` = "fit-content(100px) fit-content(1500px)", `grid-column-gap` = "5px"),
                     list(
                       htmltools::span(class = "timeline-marker",
                                       style = formattable::style(`margin-left` = paste0(round(start * 3) + 10, "px"),
                                                                  `min-width` = '1px',
                                                                  width = paste0(round(duration * 3), "px"),
                                                                  `background-color` = formattable::csscolor("#FA614B66")),
                                       htmltools::HTML("&nbsp;")),
                       htmltools::span(class = "timeline-label",
                                       style = formattable::style(`margin-left` = "5px",
                                                                  `font-weight` = ifelse(highlight, "bold", NA),
                                                                  `color` = ifelse(!is.na(error_message), "red", NA)),
                                       label))))
  }, start, duration, label, error_message, highlight, SIMPLIFY = FALSE)
}
