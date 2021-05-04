color_square <- function(row_id, color) {
  htmltools::span(style = formattable::style(display = "inline-block", width = "30px", `background-color` = color),
                  title = row_id,
                  onclick = paste0("alert('", row_id, "')"),
                  htmltools::span(
                    style = formattable::style(visibility = "hidden"),
                    row_id
                  ))
}

color_squares <- function(.x, row_color, row_id, row_parent_id) {
  if (length(row_parent_id) == 0) {
    return(list())
  }

  if (is.na(row_parent_id)) {
    return(list(color_square(row_id, row_color)))
  }

  parent <- .x[row_parent_id,]

  if (is.na(parent$.widget.color)) {
    return(list(color_square(row_id, row_color)))
  } else {
    return(c(color_squares(.x, parent$.widget.color, parent$.prop.id, parent$.prop.parent_id), list(color_square(row_id, row_color))))
  }
}

#' @export
visual_ancestors_formatter <- function(.x, .widget.color, .prop.id, .prop.parent_id) {
  mapply(function(.widget.color, .prop.id, .prop.parent_id) htmltools::doRenderTags(
    htmltools::div(style = formattable::style(`white-space` = "nowrap", width = "0%"),
                   color_squares(.x, .widget.color, .prop.id, .prop.parent_id)
    )
  ), .widget.color, .prop.id, .prop.parent_id, SIMPLIFY = FALSE)
}
