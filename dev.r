


available_features()

ggplot(landuse_sf_data) +
  geom_sf(data = landuse_sf_data, aes(color=fclass))

landuse_sf_data %>% str()

stations_data %>% str()
trips_data %>% str()

stations_data %>% dim()
trips_data %>% dim()



## trip counts by maximillian priem
summary_trips_start <- trips_data_latlon %>%
  group_by(start_rental_zone_hal_id) %>%
  dplyr::summarise(trips = n()) %>%
  ungroup()

summary_trips_end <- trips_data_latlon %>%
  group_by(end_rental_zone_hal_id) %>%
  dplyr::summarise(trips = n()) %>%
  ungroup()

summary_trips_start_zeit <- trips_data_latlon %>%
  mutate(zeit = as.factor(ifelse(
    as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) < 500 &
      as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) >
      2300,
    "Nacht",
    ifelse(
      as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) < 1100,
      "Morgen",
      ifelse(
        as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) < 1430,
        "Mittag",
        ifelse(as.numeric(
          str_replace_all(trips_data_latlon$hourmin_from, ":", "")
        ) < 1800, "Nachmittag", "Abends")
      )
    )
  ))) %>%
  group_by(start_rental_zone_hal_id, zeit) %>%
  dplyr::summarise(trips_data_latlon = n()) %>%
  ungroup()

summary_trips_end_zeit <- trips_data_latlon %>%
  mutate(zeit = as.factor(ifelse(
    as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) < 500 &
      as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) >
      2300,
    "Nacht",
    ifelse(
      as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) < 1100,
      "Morgen",
      ifelse(
        as.numeric(str_replace_all(trips_data_latlon$hourmin_from, ":", "")) < 1430,
        "Mittag",
        ifelse(as.numeric(
          str_replace_all(trips_data_latlon$hourmin_from, ":", "")
        ) < 1800, "Nachmittag", "Abends")
      )
    )
  ))) %>%
  group_by(end_rental_zone_hal_id, zeit) %>%
  dplyr::summarise(trips = n()) %>%
  ungroup()
https://app.slack.com/client/T05092DNR/C2X7037K7

## visualize counts via leaflet
# 
# m <- leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>%
#   addCircles(data = filter(geom_starts_zeit,zeit == "Morgen"), lng = ~lon, lat = ~lat, weight = 1, group = "Origins - Morning",
#              radius = ~sqrt(trips) * 10, popup = geom_starts_zeit$name,
#              highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                  bringToFront = TRUE)) %>%
#   addCircles(data = filter(geom_starts_zeit,zeit == "Mittag"), lng = ~lon, lat = ~lat, weight = 1, group = "Origins - Noon",
#              radius = ~sqrt(trips) * 10, popup = geom_starts_zeit$name,
#              highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                  bringToFront = TRUE)) %>%
#   addCircles(data = filter(geom_starts_zeit,zeit == "Nachmittag"), lng = ~lon, lat = ~lat, weight = 1, group = "Origins - Afternoon",
#              radius = ~sqrt(trips) * 10, popup = geom_starts_zeit$name,
#              highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                  bringToFront = TRUE)) %>%
#   addCircles(data = filter(geom_starts_zeit,zeit == "Abends"), lng = ~lon, lat = ~lat, weight = 1, group = "Origins - Evening",
#              radius = ~sqrt(trips) * 10, popup = geom_starts_zeit$name,
#              highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                  bringToFront = TRUE))  %>%
#   addLayersControl(
#     baseGroups = c("Origins - Morning", "Origins - Noon","Origins - Afternoon", "Origins - Evening"),
#     options = layersControlOptions(collapsed = TRUE))

library(leaflet)
library(leaflet.extras)

# leaflet(t) %>% 
#   addProviderTiles(providers$OpenStreetMap) %>%
#   addMarkers(lng = ~,
#              lat = ~lat,
  




trips_stats <- trips_data_latlon %>% 
  group_by(start_rental_zone_hal_id, roundtrip) %>% 
  summarise(count_round_trips = n(), 
            minutes_mean = mean(timediff_mins),
            minutes_sd = sd(timediff_mins)) %>% 
  mutate(freq_round_trips = count_round_trips / sum(count_round_trips)) %>% 
  ungroup() %>% 
  # arrange(desc(roundtrip), start_rental_zone_hal_id, count_round_trips)
  arrange(desc(roundtrip), desc(freq_round_trips))
trips_stats

table(trips_data_latlon$start_rental_zone_hal_id == trips_data_latlon$end_rental_zone_hal_id)

# histogram on trip length
trips_data_latlon %>% 
  mutate(timediff_custom = difftime(datetime_to, datetime_from, units = "hours")) %>% 
  ggplot(aes(timediff_custom)) + 
  geom_histogram(bins = 200) 





