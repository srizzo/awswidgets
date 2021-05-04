#' @export
cloudwatch_read <- function(files) {
  files %>%
    purrr::map(~tidyjson::read_json(.)) %>%
    purrr::reduce(dplyr::union_all) %>%
    tidyjson::enter_object(results) %>%
    tidyjson::gather_array("result.index") %>%
    select(-document.id, -result.index) %>%
    unique %>%
    dplyr::mutate(.prop.id = as.character(dplyr::row_number())) %>%
    jsontools::deep_map(
      list("..JSON", "[]", 2), .f = function(attribute)
        list(field = attribute$field, value =
          if (grepl( "^\\{", attribute$value))
            jsonlite::parse_json(attribute$value)
          else
            list(message = attribute$value)
        )
    ) %>%
    tidyjson::gather_array("field.id") %>%
    tidyjson::gather_object("field.name") %>%
    dplyr::mutate(field.value = ..JSON) %>%
    as_tibble %>%
    dplyr::select(.prop.id, field.id, field.name, field.value) %>%
    tidyr::pivot_wider(id_cols = c(.prop.id, field.id),
                       names_from = field.name,
                       values_from = field.value,
                       names_prefix = "field.") %>%
    tidyr::pivot_wider(id_cols = .prop.id, names_from = field.field, values_from = field.value) %>%
    tidyr::unnest_wider(`@message`) %>%
    dplyr::mutate(
      .widget.timestamp = strptime(`@timestamp`, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
      .widget.label = message,

      .widget.start_time = lubridate::with_tz(.widget.timestamp, Sys.timezone()),
      .widget.end_time = .widget.start_time,

      .widget.start = scales::rescale(.widget.start_time, from = c(min(.widget.start_time, na.rm = T), max(.widget.end_time, na.rm = T)), to = c(1, 100)),
      .widget.end = scales::rescale(.widget.end_time, from = c(min(.widget.start_time, na.rm = T), max(.widget.end_time, na.rm = T)), to = c(1, 100)),

      .widget.duration = .widget.end - .widget.start,

      .widget.excluded = F,
      .widget.highlight = F,
      .widget.error = NA_character_,
      .widget.color = NA_character_
    ) %>%
    relocate(.widget.timestamp, .widget.label, .before = everything()) %>%
    arrange(.widget.timestamp)
}
