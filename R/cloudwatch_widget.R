#' @export

cloudwatch_widget <- function(cloudwatch_dataset) {
  cloudwatch_dataset %>%
    formattable::formattable(list(
      .widget.label = ~timeline_formatter(.widget.start, .widget.duration, .widget.label, .widget.error, .widget.highlight),
      .widget.stack_trace = ~mapply(function(f) {
        ifelse(!is.na(f),
               htmltools::doRenderTags(pre(
                 f
               )),
               ""
        )
      }, .widget.stack_trace, SIMPLIFY = FALSE)
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
                                pageLength = 200,
                                columnDefs = list(
                                  list(
                                    targets = as.integer(which(!sapply(., class) %in% c("factor", "logical"))),
                                    searchPanes = list(show = F)
                                  ),
                                  list(
                                    targets = as.integer(which(colnames(.) %in% "_highlighted")),
                                    searchPanes = list(preSelect = "TRUE")
                                  )
                                )
                              ),
                              selection = 'none'
    )
}
