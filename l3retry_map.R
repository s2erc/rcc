require(ggmap)
require(animation)
source('l3retry_locations.Rda')

l3retry_locations$lat <- as.numeric(levels(l3retry_locations$lat))[l3retry_locations$lat]
l3retry_locations$lng <- as.numeric(levels(l3retry_locations$lng))[l3retry_locations$lng]

bb <- make_bbox(lon = c(-125.906947,-65.524346), lat = c(25.554788, 49.737556))
mymap <- get_map(location = bb, zoom = 4, source = 'stamen', maptype = 'toner')
plotmap <- ggmap(mymap, extent = 'device')

overlay <- stat_density2d(
  aes(x = lng, y = lat, fill = ..level.., alpha = ..level..),
  size = 2, bins = 12, data = l3retry_locations,
  geom = "polygon"
)

# HEATMAP
plotmap + overlay + 
  scale_fill_gradientn(colors = rev(heat.colors(256)), labels = c('low', 'medium', 'high'),
                                        breaks = c(10,40,60)) + theme(legend.position = 'left') +
  guides(alpha = FALSE)

# SCATTERPLOT
plotmap + geom_point(data = l3retry_locations, aes(x = lng, y = lat), color = 'red', size = 0.7, alpha = 0.7)

# COUNTOUR MAP (NOT WORKING)
l3retry_locations$z <- with(l3retry_locations, sqrt(l3retry_locations$lat^2 + l3retry_locations$lng^2))
plotmap + geom_contour(data = l3retry_locations, aes(z = z))

# ANIMATION
l3retry_locations$hour <- floor_date(as_datetime(l3retry_locations$answer_dt), '8 hours')
time <- sort(unique(l3retry_locations$hour))
plotfunct <- function(x) {
  df <- subset(l3retry_locations, hour == x)
  p <- plotmap + geom_point(data = df, aes(x = lng, y = lat), color = 'red', size = 0.5)
}

