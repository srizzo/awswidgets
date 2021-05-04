#' @export
xray_widget <- function(xray_dataset) {
  xray_dataset %>%
    dplyr::filter(.widget.excluded == F) %>%
    identity -> filtered

  xray_dataset %>%
    magrittr::set_rownames(.$.prop.id) %>%
    identity -> indexed

  filtered %>%
    formattable::formattable(list(
      .widget.start_time = x ~ format(as.POSIXlt(x, origin = "1970-01-01"), '%Y-%m-%d  %H:%M:%OS'),
      .widget.label = ~timeline_formatter(.widget.start, .widget.duration, .widget.label, .widget.error, .widget.highlight),
      .widget.ancestors = ~visual_ancestors_formatter(indexed, .widget.color, .prop.id, .prop.parent_id)
    )) %>%
    formattable::as.datatable(filter = "top",
                              extensions = c("FixedHeader", "Buttons", "Select", "SearchPanes"),
                              options = list(
                                scroller = T,
                                deferRender = T,
                                searchPanes = list(
                                  viewTotal = T
                                ),
                                dom = "Bfrtip",
                                buttons = "searchPanes",
                                pageLength = 1000,
                                columnDefs = list(
                                  list(
                                    targets = as.integer(which(!sapply(., class) %in% c("factor", "logical"))),
                                    searchPanes = list(show = F)
                                  )
                                )
                              ),
                              selection = 'none'
    )
}
