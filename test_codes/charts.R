


create_gen_chart <- function(data) {

  # main_df <- read_csv(df_address)
  gender_df <- main_df %>% gather(key = "gender", value = "population", "male", "female")


  data %>%
    gender_df() <- arrange(gender_df, population)
  cols <- brewer.pal(n = 2, name = "Set1")
  hchart(gender_df, "column", hcaes(x = state, y = population, group = gender)) %>%
    hc_colors(cols) %>%
    hc_exporting(enabled = TRUE)
}

# premlim_pop %>%

library(highcharter)

library(RColorBrewer)

cols <- brewer.pal(n = 2, name = "Set1")



prelim_district <- readRDS("prelim-pop.rds")

prelim_district %>%
  filter(category_en %in% c("Male", "Female")) %>%
  hchart("column", hcaes(x = district, y = number, group = category_en)) %>%
  hc_colors(cols) %>%
  hc_exporting(enabled = TRUE)





final_region <- readRDS("final_region.rds")


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


family_size_rate <- final_region %>% 
  filter(
    category_en %in% c("Average Family size", "Annual Population Growth Rate (%)"),
    primary_cat == "Province"
  ) %>%
  hchart("column", hcaes(
    x = region_en,
    y = number,
    group = category_en
  )) %>%
  hc_colors(cols3) %>%
  hc_exporting(enabled = TRUE)




avg_family_size <- final_region %>% 
  filter(
    category_en %in% c("Average Family size"),
    primary_cat == "Province"
  ) %>%
  hchart("column", hcaes(
    x = region_en,
    y = number,
    group = category_en
  )) %>%
  hc_colors(cols3) %>%
  hc_exporting(enabled = TRUE)
  








# "Average Family size", 

piecols <- c("#ff6361","#6aaa96","#0088cc","#ffa600","#bc5090")

final_region %>% 
  filter(
    category_en %in% c("Annual Population Growth Rate (%)"),
    primary_cat == "Province"
  ) %>%
  hchart("pie",hcaes(name=region_en,y=number),name = "Population Distribution:") %>%
  hc_colors(piecols) %>% hc_exporting(enabled = TRUE)%>%
  # hc_subtitle(text="Population distribution of Nepal on Basis of adminstrative region") %>% 
  hc_tooltip(pointFormat = "<b>Value:</b> {point.y}%")


pie_gender <- final_region %>% 
  filter(   primary_cat == "Country", 
            category_en %in% c("Male", "Female")
            
            ) %>% 
hchart("pie",hcaes(name=category_en,y=number),name = "Population Distribution:") %>%
  hc_colors(cols) %>% hc_exporting(enabled = TRUE)%>%
  # hc_subtitle(text="Population distribution of Nepal on Basis of adminstrative region") %>% 
  hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")



# prelim_district %>%
#   filter(category_en %in% c("Male", "Female")) %>%
#   hchart("column", hcaes(x = district, y = number, group = category_en)) %>%
#   hc_colors(cols) %>%
#   hc_exporting(enabled = TRUE)

# 
# hchart(main_df,"pie",hcaes(name=type,y=percent),name = "Population Distribution:") %>%
#   hc_colors(cols) %>% hc_exporting(enabled = TRUE)%>%
#   hc_subtitle(text="Population distribution of Nepal on Basis of adminstrative region")
# 
# 
# 
# hchart(main_df,"pie",hcaes(name=type,y=percent),name = "Population Distribution:") %>%
#   hc_colors(cols) %>% hc_exporting(enabled = TRUE)%>%
#   hc_subtitle(text="Population distribution of Nepal on Basis of adminstrative region")
# 
# 


