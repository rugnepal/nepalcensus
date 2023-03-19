library(highcharter)
library(RColorBrewer)


final_region <- readRDS("data/final_region.rds")


cols2 <- c("#1E88E5","#FFC107","#D81B60")
sex_region <- final_region %>%
  filter(
    category_en %in% c("Male", "Female"),
    primary_cat == "Province"
  ) %>%
  hchart("column", hcaes(
    x = region_en,
    y = number,
    group = category_en
  )) %>%
  hc_colors(cols2[1:2]) %>%
  
  hc_exporting(enabled = TRUE)

cols3 <- brewer.pal(n = 2, name = "Dark2")

household_province <- final_region %>%
  filter(
    category_en %in% c("Household Number", "Preliminary Population 2078"),
    primary_cat == "Province"
  ) %>%
  hchart("column", hcaes(
    x = region_en,
    y = number,
    group = category_en
  )) %>%
  hc_colors(cols3) %>%
  hc_exporting(enabled = TRUE)


piecols <- c("#ff6361","#6aaa96","#0088cc","#ffa600","#bc5090")


pie_gender <- final_region %>% 
  filter(   primary_cat == "Country", 
            category_en %in% c("Male", "Female")
            
  ) %>% 
  hchart("pie",hcaes(name=category_en,y=number),name = "Population Distribution:") %>%
  hc_colors(piecols) %>% hc_exporting(enabled = TRUE)%>%
  hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")

