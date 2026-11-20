# Eric Heinze
# MEES 702 Homework: NOAA API Data Retrieval and Visualization Function

fetch_recent_noaa <- function(station = "8577330",
                              product = c("water_temperature", "air_temperature", "wind"),
                              days_back = 30,
                              start_date = NULL,
                              end_date = NULL,
                              plot = TRUE,
                              plot_backend = c("ggplot2", "base"), ...) {
  required_packages <- c("cli", "httr2", "readr", "dplyr", "ggplot2")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) {
    stop("Required package(s) missing: ", paste(missing_packages, collapse = ", "), ". Please install them before using fetch_recent_noaa().")
  }
  if (!is.character(station) || length(station) != 1L || is.na(station) || !nzchar(station)) {
    cli::cli_abort(c("x" = "{.arg station} must be a single non-empty character string",
                     "i" = "You supplied an object of class {.cls {class(station)}}"),
                   call. = FALSE)
  }
  if (!is.numeric(days_back) || length(days_back) != 1L || is.na(days_back) || !is.finite(days_back)) {
    cli::cli_abort(c("x" = "{.arg days_back} must be a finite numeric scalar.",
                     "i" = "You supplied an object of class {.cls {class(days_back)}}"),
                   call. = FALSE)
  }
  if (!is.null(start_date) && (!is.character(start_date) || length(start_date) != 1L)) {
    cli::cli_abort("{.arg start_date} must be NULL or a character scalar in YYYY-MM-DD format", call. = FALSE)
  }
  if (!is.null(end_date) && (!is.character(end_date) || length(end_date) != 1L)) {
    cli::cli_abort("{.arg end_date} must be NULL or a character scalar in YYYY-MM-DD format", call. = FALSE)
  }
  if (!is.null(start_date)) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date)) {
      stop("start_date must be in YYYY-MM-DD format")
    }
    start_date <- as.Date(start_date, format = "%Y-%m-%d")
    if (is.na(start_date)) stop("start_date must be a valid date")
  }
  if (is.null(end_date)) {
    end_date <- Sys.Date()
  } else {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)) {
      stop("end_date must be in YYYY-MM-DD format")
    }
    end_date <- as.Date(end_date, format = "%Y-%m-%d")
    if (is.na(end_date)) stop("end_date must be a valid date")
  }
  if (is.null(start_date)) {
    start_date <- end_date - days_back
  }
  if (start_date > end_date) {
    cli::cli_abort("{.arg start_date} must be less than or equal to {.arg end_date}", call. = FALSE)
  }
  if (as.numeric(end_date - start_date, units = "days") > 31) {
    cli::cli_abort(c("x" = "The requested date span cannot exceed 31 days",
                     "i" = "The maximum NOAA API request is 31 days"), 
                   call. = FALSE)
  }
  if (!is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    cli::cli_abort(c("x" = "{.arg plot} must be a single non-missing logical value (TRUE or FALSE).",
                     "i" = "You supplied incorrect parameters for the plot"),
                   call. = FALSE)
  }
  product <- match.arg(product)
  plot_backend <- match.arg(plot_backend)
  
  validate_api_response <- function(data) {
    is_error <- is.list(data) &&
      any(c("error", "errors") %in% names(data)) &&
      any(vapply(data[intersect(c("error", "errors"), names(data))],
                 function(x) !is.null(x) && length(x) > 0L,
                 logical(1)))
    is_empty <- is.null(data) || length(data) == 0L ||
      (is.data.frame(data) && nrow(data) == 0L)
    
    if (is_error) stop("NOAA API returned an error payload.", call. = FALSE)
    if (is_empty) stop("NOAA API returned an empty dataset.", call. = FALSE)
    data
  }
  
  begin_date <- format(start_date, "%Y%m%d")
  api_end_date <- format(end_date, "%Y%m%d")
  req <- httr2::request("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter") |>
    httr2::req_url_query(
      begin_date = begin_date,
      end_date = api_end_date,
      station = station,
      product = product,
      units = "metric",
      time_zone = "gmt",
      application = "UMCES_MEES702",
      format = "csv"
    )
  
  resp <- httr2::req_perform(req)
  data <- resp |>
    httr2::resp_body_string() |>
    readr::read_csv(show_col_types = FALSE)
  data <- validate_api_response(data)
  normalized_names <- tolower(gsub("[^[:alnum:]]", "", sub("^\\ufeff", "", names(data))))
  datetime_col <- which(grepl("^date.*time", normalized_names))[1]
  date_col <- which(grepl("^date", normalized_names) & !grepl("^date.*time", normalized_names))[1]
  time_col <- which(grepl("^time", normalized_names))[1]
  if (is.na(datetime_col) && (is.na(date_col) || is.na(time_col))) {
    cli::cli_abort("NOAA response is missing date or time columns.", call. = FALSE)
  }
  
  value_names <- switch(
    product,
    water_temperature = "watertemperature",
    air_temperature = "airtemperature",
    # NOAA's wind CSV labels wind speed as either "Wind" or "Speed".
    wind = c("wind", "speed", "windspeed")
  )
  value_col <- which(normalized_names %in% value_names)[1]
  if (is.na(value_col)) {
    cli::cli_abort("NOAA response is missing the requested measurement column.", call. = FALSE)
  }
  
  datetime_text <- if (!is.na(datetime_col)) {
    as.character(data[[datetime_col]])
  } else {
    paste(as.character(data[[date_col]]), as.character(data[[time_col]]))
  }
  datetime <- as.POSIXct(datetime_text, format = "%Y-%m-%d %H:%M", tz = "GMT")
  date_only <- is.na(datetime) & grepl("^\\d{4}-\\d{2}-\\d{2}$", datetime_text)
  datetime[date_only] <- as.POSIXct(datetime_text[date_only], format = "%Y-%m-%d", tz = "GMT")
  value <- readr::parse_number(as.character(data[[value_col]]), na = c("", "NA", "MM"))
  df <- dplyr::tibble(datetime = datetime, value = value)
  
  if (plot) {
    product_label <- switch(
      product,
      water_temperature = "Water temperature",
      air_temperature = "Air temperature",
      wind = "Wind speed"
    )
    units <- if (product == "wind") "m/s" else "°C"
    date_range <- paste(format(start_date, "%Y-%m-%d"),
                        format(end_date, "%Y-%m-%d"), sep = " to ")
    
    title <- paste(product_label, "at NOAA Station", station)
    x_label <- "Date and time (GMT)"
    y_label <- paste0(product_label, " (", units, ")")
    
    if (plot_backend == "ggplot2") {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = datetime, y = value)) +
        ggplot2::geom_line(...) +
        ggplot2::labs(
          title = title,
          subtitle = paste("Date range:", date_range),
          x = x_label,
          y = y_label
        ) +
        ggplot2::theme_minimal()
      print(p)
    } else {
      plot_args <- c(
        list(x = df$datetime, y = df$value, type = "l", main = title,
             xlab = x_label, ylab = y_label),
        list(...)
      )
      do.call(graphics::plot, plot_args)
    }
  }
  
  df
}
