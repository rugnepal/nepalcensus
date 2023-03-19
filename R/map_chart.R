
library(dplyr)
library(nepalmap)
library(geojsonio)
library(leaflet)


options(scipen = 999)

map_chart <- function(data, labels) {
  leaflet(data,
    options = leafletOptions(
      attributionControl = FALSE
    )
  ) %>%
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

all_province <- readRDS("data/prelim-province.rds")

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

map_chart(all_province, labels)
