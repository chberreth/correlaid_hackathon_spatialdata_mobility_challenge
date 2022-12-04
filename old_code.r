

```{r eval=FALSE, include=FALSE}
p1 <- as.matrix(trips_lonlat[,c("start_lon", "start_lat")])
p2 <- as.matrix(trips_lonlat[,c("end_lon", "end_lat")])

gcIntermediate(p1, p2,  
               n=100, 
               addStartEnd=TRUE,
               sp=TRUE) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines(weight=5)
```

```{r eval=FALSE, include=FALSE}
library(sp)
library(maptools)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  # Function of  Kyle Walker, see https://stackoverflow.com/questions/32275213/how-do-i-connect-two-coordinates-with-a-line-using-leaflet-in-r
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}


z <- gather(trips_lonlat, measure, val, -route_id) %>% group_by(route_id) %>%
  do(data.frame(lat=c(.[["val"]][.[["measure"]]=="start_lat"],
                      .[["val"]][.[["measure"]]=="end_lat"]),
                long = c(.[["val"]][.[["measure"]]=="start_lon"],
                         .[["val"]][.[["measure"]]=="end_lon"]))) %>% 
  as.data.frame() %>% 
  mutate(lat = as.numeric(lat), 
         long = as.numeric(long))

y <- points_to_line(z, "long", "lat", "route_id")

map1 = leaflet(trips_lonlat) %>% 
  addTiles() %>% 
  addCircles(~start_lon, ~start_lat, popup = ~start_name, weight = 1) %>%
  addPolylines(data = y, weight = 5)

map1
```

  
toc: yes
toc_float: yes
number_sections: yes
df_print: paged
theme: united
highlight: tango




