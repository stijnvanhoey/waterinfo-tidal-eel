
library(ggplot2)
library(ggpubr)
library(ggridges)


#' Plot tidal data from waterinfo together with eel tracking data
#'
#' @param tidal_data data.frame as derived from waterinfo using the
#' wateRinfo package and enriched with a `distance_to_refence_station` variable
#' @param eels data.frame as derived from the etn network and enriched 
#' with a `distance_to_refence_station` variable
#' @param tide_periods data.frame, containing for each tidal period (provide any kind
#' of id) from low tide to high tide a `start` and `end` period. Make sure df contains 
#' a start and end column
#' @param date_breaks character, cfr. ggplot2 date_breaks options, 
#' e.g. 7 days, 10 hours,...
#'
#' @return
#' @export
#'
#' @examples
plot_tidal_tracks <- function(tidal_data, eels, 
                              tide_periods, date_breaks = "7 days") {
    
    date_plot_sea <- tidal_data %>% 
        select(Timestamp) %>% 
        pull() %>% median()

    ggplot() +
        geom_rect(data = tide_periods, 
                  aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), 
                  alpha = 0.05) +
        geom_segment(data = tidal_data, aes(x = Timestamp, xend = lag(Timestamp),
                     y = distance_to_refence_station, yend = distance_to_refence_station,
                     color = Value), size = 5) + 
        geom_line(data = eels, aes(x = Arrival, 
                                          y = distance_to_refence_station),
                  alpha = 0.9, color = "#545454", linetype = "dashed", size = 0.4) +
        geom_segment(data = eels, aes(x = Arrival, xend = Departure, 
                     y = distance_to_refence_station, yend = distance_to_refence_station), 
                     size = 3) +
        scale_x_datetime(date_labels = "%Y-%m-%d",
                         date_breaks = date_breaks) +
        scale_color_distiller("Water height\n(m TAW)", type = "seq", 
                              palette = "Blues", direction = 1) +
        theme_minimal() +
        xlab("") + ylab("") +
        scale_y_continuous("Municipalities along the Scheldt Estuary", 
                           breaks = station_municipality$distance_to_refence_station,
                           labels = station_municipality$municipality) +
        annotate("text", x = as_datetime(date_plot_sea),
                 y = max(eels$distance_to_refence_station) * 0.22, 
                 colour = "#2b8cbe", label = "Sea") +
        annotate("segment", x = as_datetime(date_plot_sea), xend = as_datetime(date_plot_sea),
                 y = max(eels$distance_to_refence_station) * 0.2, 
                 yend = max(eels$distance_to_refence_station) * 0.1, 
                 arrow = arrow(length = unit(0.5, "cm")), color = "#2b8cbe")                           
    }


#' Plot tidal data from waterinfo and add background for tidal periods
#'
#' @param tidal_data data.frame as derived from waterinfo using the
#' wateRinfo package and enriched with a `distance_to_refence_station` variable
#' @param tide_periods data.frame, containing for each tidal period (provide any kind
#' of id) from low tide to high tide a `start` and `end` period. Make sure df contains 
#' a start and end column
#' @param date_breaks character, cfr. ggplot2 date_breaks options, 
#' e.g. 7 days, 10 hours,...
#'
#' @return
#' @export
#'
#' @examples
plot_tide_with_background <- function(tidal_data, 
                                      tide_periods, 
                                      date_breaks = "7 days") {
    ggplot() +
        geom_rect(data = tide_periods, 
                  aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), 
                  alpha = 0.05) +
        geom_line(data = tidal_data, 
                  aes(x = Timestamp, y = Value, color = Value)) + 
        scale_x_datetime(date_labels = "%Y-%m-%d",
                         date_breaks = date_breaks) +
        scale_color_distiller("Water height\n(m TAW)", type = "seq", 
                              palette = "Blues", direction = 1) +
        theme_minimal() +
        ylab("Water level (m TAW)")                                                        
    }                                              


#' Pure showcase example of the ggridges package
#' not created for reuse...
ggridges_showcase <- function(eels, tidal_data, station_municipality) {
    start_moment <- "2016-11-14"
    end_moment <- "2016-11-18"

    # focus on Uitbergen 
    max_zoom <- station_municipality %>% 
        filter(municipality == "Uitbergen") %>% 
        select(distance_to_refence_station) %>% pull()
    min_zoom <- station_municipality %>% 
        filter(municipality == "Temse") %>% 
        select(distance_to_refence_station) %>% pull()

    eels %>% filter(Departure >= start_moment, Arrival <= end_moment,
                    Transmitter == "A69-1601-52644") %>%
        filter(distance_to_refence_station > min_zoom, 
               distance_to_refence_station < max_zoom) -> eels_zoom
    tidal_data %>% filter(Timestamp >= start_moment, Timestamp <= end_moment) %>%
        filter(distance_to_refence_station > min_zoom, 
               distance_to_refence_station < max_zoom) -> tidal_data_zoom
    station_municipality %>% 
        filter(distance_to_refence_station > min_zoom, 
               distance_to_refence_station < max_zoom) -> station_municipality_zoom

    ggplot() +
        geom_ridgeline(data = tidal_data_zoom, 
                       aes(x = Timestamp, y = distance_to_refence_station, 
                           height = Value, group = distance_to_refence_station),
                       scale = 700, fill = "#9ecae1", color = "#2b8cbe") +
        geom_line(data = eels_zoom, aes(x = Arrival, y = distance_to_refence_station),
                  alpha = 0.75, color = "grey", size = 0.5) +
        geom_segment(data = eels_zoom, aes(x = Arrival, xend = Departure, 
                     y = distance_to_refence_station, yend = distance_to_refence_station), 
                     size = 10) +
        xlab("") + ylab("") +
        scale_y_continuous("", 
                           breaks = station_municipality_zoom$distance_to_refence_station,
                           labels = station_municipality_zoom$municipality) +
        theme_minimal() +
        coord_fixed(ratio = 8) +
        theme(legend.position="none")
    
}