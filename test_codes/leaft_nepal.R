# install.packages("geojsonio")

# states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")


#
# library(sp)
library(dplyr)
library(nepalmap)
library(geojsonio)

library(leaflet)

pop_density <- readRDS("pop_density.rds")

nmap <- nepalmap::district_df %>%
  left_join(pop_density, by = c("id", "district_id"))


# df1 <- sp::merge(nepalmap::district_df, pop_density, by = c("id", "district_id"))

# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
# bins = c("< 5K", "5 - 10K", "20 - 50K", "50 - 80K", "80 - 1L", "> 1L")

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)

pal <- colorBin("YlOrRd", domain = nmap$pop_density, bins = bins)
# values = setNames(c("#D18D72", "#BB785D", "#A56248","#904D32", "#7A371D","#642208"),
#                   c("< 5K", "5 - 10K", "20 - 50K", "50 - 80K", "80 - 1L", "> 1L"),
#                   breaks = c("< 5K", "5 - 10K", "20 - 50K", "50 - 80K", "80 - 1L", "> 1L"),

labels <- sprintf(
  "<strong>District:</strong> %s<br/>
   <strong>Population:</strong> %s <br/>
  <strong>Density:</strong> %g people / km<sup>2</sup>
  ",
  nmap$district, format(nmap$total_pop, scientific = FALSE), nmap$pop_density
) |> lapply(htmltools::HTML)


# library(leaflet)
options(scipen = 999)

map_chart <- function(data) {
  leaflet(data,
    options = leafletOptions(
      attributionControl = FALSE
    )
  ) %>%
    # setView(84, 28.1, 6) %>%
    setView(84.1240, 28.3949, 6) %>%
    addProviderTiles("CartoDB.Positron",
      options = providerTileOptions(opacity = 0)
    ) %>%
    # addTiles(none) %>%
    addPolygons(
      fillColor = ~ pal(pop_density),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#777",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal, values = ~pop_density, opacity = 0.7, title = NULL,
      position = "bottomleft"
    )
}

map_chart(nmap)

all_province <- readRDS("prelim-province.rds")

# map_chart(all_province)

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)

pal <- colorBin("YlOrRd", domain = all_province$pop_density, bins = bins)

labels <- sprintf(
  "<strong>Province:</strong> %s<br/>
   <strong>Population:</strong> %s <br/>
  <strong>Density:</strong> %g people / km<sup>2</sup> <br/>
   <strong>Avg. Family size:</strong> %g <br/>
     <strong>Annual Growth Rate:</strong> %g",
  all_province$region_en, 
  format(all_province$total_pop, scientific = FALSE), 
  all_province$pop_density, 
  all_province$avg_family_size, 
  all_province$annual_pop_growth
) |> lapply(htmltools::HTML)



leaflet(all_province) %>%
  setView(84, 28.1, 7) %>%
  addProviderTiles("CartoDB.Positron",
    options = providerTileOptions(opacity = 0)
  ) %>%
  # addTiles(none) %>%
  addPolygons(
    fillColor = ~ pal(pop_density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#777",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~pop_density, opacity = 0.7, title = NULL,
    position = "bottomleft"
  )

map_chart(all_province)


map_nepal <- leaflet(nmap) %>%
  setView(84, 28.1, 7) %>%
  addProviderTiles("CartoDB.Positron",
    options = providerTileOptions(opacity = 0)
  ) %>%
  # addTiles(none) %>%
  addPolygons(
    fillColor = ~ pal(pop_density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#777",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~pop_density, opacity = 0.7, title = NULL,
    position = "bottomleft"
  )
