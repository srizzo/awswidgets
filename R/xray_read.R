augment_subsegments <- function(.x, segment) {
  .x %>%
    jsontools::modify_in_maybe("subsegments",
                               function(subsegments)
                                 subsegments %>%
                                   purrr::map(function(subsegment)
                                                subsegment %>%
                                                  purrr::list_modify(
                                                    SEGMENT_ID = segment$id,
                                                    PARENT_ID = .x$id) %>%
                                                  augment_subsegments(., segment)
                                   )
    )
}

# TODO
# if ($LimitExceeded) {
#   stop("xray limit exceeded")
# }

#' @export
xray_read <- function(files) {
  files %>%
    purrr::map(~readr::read_file(.x)) %>%
    purrr::map(~jsonlite::parse_json(.x)) %>%
    purrr::map("Traces") %>%
    purrr::reduce(c) %>%
    purrr::map("Segments") %>%
    purrr::reduce(c) %>%
    purrr::map("Document") %>%
    purrr::map(jsonlite::parse_json) %>%
    purrr::map(function(segment) segment %>% augment_subsegments(., segment)) %>%
    identity -> segments_aggs

  segments_aggs %>%
    purrr::map(function(segment) {
      segment %>%
        jsontools::recursive_pluck("subsegments") %>%
        purrr::reduce(c, .init = list())
    }) %>%
    purrr::reduce(c, .init = list()) %>%
    purrr::map(jsontools::except, "subsegments") %>%
    tidyjson::as_tbl_json() %>%
    jsontools::flatten_json() %>%
    dplyr::select(-document.id) %>%
    dplyr::rename_with(~paste0('Subsegment.', .x)) %>%
    union_all(tibble(
      Subsegment.http.request.url = character(),
      Subsegment.http.request.method = character(),
      Subsegment.http.response.status = integer()
    )) %>%
    identity -> subsegments

  segments_aggs %>%
    purrr::map(function(segment)
                 segment %>%
                   jsontools::except("subsegments") %>%
                   jsontools::modify_in_maybe(c("aws", "ecs"), ~NULL)
    ) %>%
    tidyjson::as_tbl_json() %>%
    jsontools::flatten_json() %>%
    dplyr::select(-document.id) %>%
    dplyr::rename_with(~paste0('Segment.', .x)) %>%
    identity -> segments

  segments %>%
    dplyr::union_all(segments %>% dplyr::inner_join(subsegments, by = c(Segment.id = "Subsegment.SEGMENT_ID"))) %>%
    unique %>%
    dplyr::mutate(
      .prop.parent_id = coalesce(Subsegment.PARENT_ID, Segment.parent_id),
      .prop.duration = round(as.POSIXlt(Subsegment.end_time, origin = "1970-01-01") - as.POSIXlt(Subsegment.start_time, origin = "1970-01-01"), 6),
      .prop.is_segment = is.na(Subsegment.id),
      .prop.id = ifelse(.prop.is_segment, Segment.id, Subsegment.id),
      .prop.start_time = ifelse(.prop.is_segment, Segment.start_time, Subsegment.start_time),
      .prop.end_time = ifelse(.prop.is_segment, Segment.end_time, Subsegment.end_time),
      .prop.name = paste(Segment.name, Subsegment.name, sep = " | "),
      .prop.request.url = ifelse(.prop.is_segment, Segment.http.request.url, Subsegment.http.request.url),
      .prop.request.method = ifelse(.prop.is_segment, Segment.http.request.method, Subsegment.http.request.method),
      .widget.start_time = as.POSIXlt(.prop.start_time, origin = "1970-01-01"),
      .widget.end_time = as.POSIXlt(.prop.end_time, origin = "1970-01-01"),
      .widget.start = scales::rescale(.widget.start_time, from = c(min(.widget.start_time, na.rm = T), max(.widget.end_time, na.rm = T)), to = c(1, 100)),
      .widget.end = scales::rescale(.widget.end_time, from = c(min(.widget.start_time, na.rm = T), max(.widget.end_time, na.rm = T)), to = c(1, 100)),
      .widget.duration = .widget.end - .widget.start,
      .widget.label = paste(Segment.name, Subsegment.name),
      .widget.excluded = F,
      .widget.highlight = .prop.is_segment,
      .widget.error = NA_character_,
      .widget.color = NA_character_
    ) %>%
    mutate(
      .widget.ancestors = ancestors(.)
    ) %>%
    identity -> results

  color_domain <- as.factor(unique(c(results$.prop.parent_id, results$.prop.id)))
  segment_color <- scales::col_factor(palette = randomcoloR::distinctColorPalette(length(color_domain)),
                                      domain = color_domain, alpha = TRUE)

  results %>%
    mutate(
      Segment.id = factor(Segment.id, levels = color_domain),
      Subsegment.id = factor(Subsegment.id, levels = color_domain),
      .widget.color = segment_color(.prop.id)
    ) %>%
    relocate(.widget.start_time, .widget.ancestors, .widget.label, Segment.name, Subsegment.name, .before = everything()) %>%
    arrange(.widget.start_time, .widget.end_time, Segment.name) %>%
    identity()
}

# dplyr::union_all(xray_default_cols) %>%
# xray_default_cols <- tidyr::tibble()
