library(R.matlab)
library(ggplot2)
library(leaflet)

#"elkapwq", "elkvmwq", "delslwq", "wellmwq",

names = c(
  "acebbwq","acefcwq","acemcwq","acespwq","apacpwq","apadbwq","apaebwq","apaeswq",
  "cbvcbwq","cbvgiwq","cbvtcwq","delblwq","delslwq","elkapwq","elknmwq","elksmwq","elkvmwq",
  "grbgbwq","grblrwq","grborwq","grbsqwq","gtmfmwq","gtmpcwq","gtmpiwq","gtmsswq",
  "hudskwq","hudtswq","jacb6wq","jacb9wq","jacbawq","jacnewq","job09wq","job10wq","job19wq","job20wq",
  "narncwq","narpcwq","nartbwq","nartswq",
  "niwcbwq","niwdcwq","niwolwq","niwtawq","nocecwq","nocrcwq","noczbwq","owcbrwq", "owcolwq",
  "owcwmwq","pdbbpwq","pdbbywq",
  "rkbfbwq","rkbfuwq","rkblhwq","rkbmbwq","saphdwq","sapldwq","soschwq",
  "sosvawq","soswiwq","welhtwq","welinwq","wellmwq",
  "wkbfrwq","wkbwbwq","wqbmhwq","wqbmpwq")

# Read the CSV file into a data frame
stations = read.csv('sampling_stations.csv')

stations$Station.Code <- gsub(" ", "", stations$Station.Code)
stations$Station.Name <- trimws(stations$Station.Name)


names.pos = rep(NA, length(names))

for (i in 1:length(names)){
  names.pos[i] = which(stations$Station.Code == names[i])
}

comp.sta = stations$Station.Code[names.pos]
comp.lat = stations$Latitude[names.pos]
comp.lon = stations$Longitude[names.pos]
sta_name = stations$Station.Name[names.pos]

# EST < 85.8 UTC -5:00

# 85.8 < CST < 105.6 UTC -6:00

# 105.6 < MST < 114.4 UTC -7:00

# 114.4 < PST < 150 UTC -8:00

# HST > 150 UTC -10:00

time.comp <- ifelse(comp.lon < 85.8, 'UTC -5:00',
                    ifelse(comp.lon < 105.6, 'UTC -6:00',
                           ifelse(comp.lon < 114.4, 'UTC -7:00',
                                  ifelse(comp.lon < 150, 'UTC -8:00',
                                         ifelse(comp.lat > 30, 'UTC -9:00', 'UTC -10:00')))))

# Create a new data frame with selected columns and the time.comp column
sta_latlon = data.frame(comp.sta, comp.lat, comp.lon, time.comp)





# Define a function to filter chunk files for a given station
get_chunk_files <- function(station_name) {
  pattern <- paste0(station_name, "_coef_chunk_\\d+\\.mat")
  grep(pattern, all_files, value = TRUE)
}


mean.rate = rep(NA, length(names))

mean.slr = rep(NA, length(names))

for (i in 1:length(names)) {
  # Example usage for station 'acebbwq'
  station_name <- names[i]
  
  cat("Processing:", station_name, '\n')
  
  chunk_files <- get_chunk_files(station_name)
  
  amps <- rep(NA, length(chunk_files))
  
  for (j in 1:length(chunk_files)) {
    data <- readMat(chunk_files[j])
    amp <- data$coef[[2]]
    amp = amp*2
    amps[j] <- amp[1]
  }
  
  
  rates = rep(NA, length(amps)-1)
  
  for (k in 1:length(rates)){
    rates[k] = (amps[k+1]-amps[k])*1000
  }
  
  mean.rate[i]= mean(rates)
  
  slrs <- rep(NA, length(chunk_files)-1)
  
  for (q in 1:length(chunk_files)-1) {
    data <- readMat(chunk_files[q+1])
    slr <- data$coef[[7]]
    slr = slr * 365000
    slrs[q] <- slr
  }
  
  mean.slr[i] = mean(slrs)
  
}



lat = comp.lat
long = comp.lon *(-1)


df = data.frame(comp.sta, lat, long, time.comp, mean.rate, mean.slr, sta_name)


# Define the color palette
pal <- colorNumeric(palette = "viridis", domain = df$mean.rate)
min_radius <- 5
max_radius <- 15
df$radius <- min_radius + (df$mean.slr - min(df$mean.slr, na.rm = TRUE)) * (max_radius - min_radius) / (max(df$mean.slr, na.rm = TRUE) - min(df$mean.slr, na.rm = TRUE))

# Create separate palettes for negative and positive values
pal_neg <- colorNumeric(palette = c("red", "orangered", "orange", "gold", "yellow"),
                        domain = c(-10, 0), na.color = "transparent")
pal_pos <- colorNumeric(palette = c("lightgreen",'limegreen', "green", "darkgreen",'blue', "darkblue"),
                        domain = c(0, 50), na.color = "transparent")

# Combine the palettes into one function
combined_palette <- function(x) {
  ifelse(x < 0, pal_neg(x), pal_pos(x))
}

# Define the radius scaling
min_radius <- 5
max_radius <- 15
df$radius <- min_radius + (df$mean.slr - min(df$mean.slr, na.rm = TRUE)) * 
  (max_radius - min_radius) / (max(df$mean.slr, na.rm = TRUE) - 
                                 min(df$mean.slr, na.rm = TRUE))

# Define specific breakpoints for the legend
breaks <- c(-10, -5, -2.5, 0, 2.5, 5, 10, 20, 30, 40, 50)

# Define colors for each breakpoint
colors <- c(
  pal_neg(-10), pal_neg(-5), pal_neg(-2.5), pal_neg(0),
  pal_pos(2.5), pal_pos(5), pal_pos(10), pal_pos(20),
  pal_pos(30), pal_pos(40), pal_pos(50)
)

# Create the leaflet map
m <- leaflet(data = df) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    popup = paste(
      "<b>Station Name:</b> ", df$sta_name, "<br/>",
      "<b>Rate of Tidal Range Change (mm/yr):</b> ", df$mean.rate, "<br/>",
      "<b>Rate of Sea Level Rise (mm/yr):</b> ", df$mean.slr
    ),
    color = ~combined_palette(mean.rate),
    fillColor = ~combined_palette(mean.rate),
    fillOpacity = 0.7,
    radius = ~radius
  ) %>%
  addLegend(
    position = "bottomright",
    colors = colors,
    labels = breaks,
    title = "Rate of Tidal Range Change (mm/yr)",
    opacity = 1
  ) %>%
  addControl(html = 
               "<div style='line-height:1.5;'>
                  Size of point reflects Rate of Sea Level Rise (mm/yr)
                </div>", 
             position = "bottomright")

# Display the map
m