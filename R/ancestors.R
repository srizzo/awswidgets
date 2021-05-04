#' @export
ancestors <- function(.x, .prop.id, .prop.parent_id) {
  .x <- magrittr::set_rownames(.x, .x$.prop.id)
  .x %>%
  {
    mapply(function(.prop.id, .prop.parent_id)
             get_ancestors(.x, .prop.id, .prop.parent_id),
           .x$.prop.id, .x$.prop.parent_id, SIMPLIFY = FALSE)
  }
}

get_ancestors <- function(.x, row_id, row_parent_id) {
  if (length(row_parent_id) == 0) {
    return(list())
  }

  if (is.na(row_parent_id)) {
    return(list(row_id))
  }

  stopifnot(".prop.id not set as rownames" = tibble::has_rownames(.x))

  parent <- .x[row_parent_id,]

  if (is.na(parent$.prop.id)) {
    return(list(row_id))
  } else {
    return(c(get_ancestors(.x, parent$.prop.id, parent$.prop.parent_id), list(row_id)))
  }
}
