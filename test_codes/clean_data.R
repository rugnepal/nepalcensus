library(readxl)
library(tidyverse)
# library(tidyr)
library(janitor)

options(scipen = 999)


prelim <- read_excel("prelim-census-2021.xls") %>%
  fill(names(.), .direction = "up") %>%
  row_to_names(2) %>%
  discard(~ all(is.na(.))) %>%
  slice(c(-1, -3, -7)) %>%
  pivot_longer(
    cols = 2:10,
    names_to = "category",
    values_to = "number"
  ) %>%
  mutate(number = as.numeric(number)) %>%
  select("region" = 1, everything())


cat <- prelim %>%
  distinct(category) %>%
  pull()

reg <- prelim %>%
  distinct(region) %>%
  pull()

final_region <- prelim %>%
  mutate(
    category_en = case_match(
      category,
      cat[1] ~ "Census House Number",
      cat[2] ~ "Household Number",
      cat[3] ~ "Preliminary Population 2078",
      cat[4] ~ "Male",
      cat[5] ~ "Female",
      cat[6] ~ "Sex ratio",
      cat[7] ~ "Average Family size",
      cat[8] ~ "Annual Population Growth Rate (%)",
      cat[9] ~ "Population Density (per sq. km)"
    ),
    region_en = case_match(
      region,
      reg[1] ~ "Nepal",
      reg[2] ~ "Himalayan",
      reg[3] ~ "Hilly",
      reg[4] ~ "Terai",
      reg[5] ~ "Koshi",
      reg[6] ~ "Madhesh",
      reg[7] ~ "Bagmati",
      reg[8] ~ "Gandaki",
      reg[9] ~ "Lumbini",
      reg[10] ~ "Karnali",
      reg[11] ~ "Sudurpaschim"
    ),
    primary_cat = case_match(
      region_en,
      c("Nepal") ~ "Country",
      c("Himalayan", "Hilly", "Terai") ~ "Region",
      c(
        "Koshi", "Madhesh", "Bagmati", "Gandaki", "Lumbini",
        "Karnali", "Sudurpaschim"
      ) ~ "Province"
    ), 
    
    id = case_match(region_en,
      "Koshi" ~ "1",
      "Madhesh" ~ "2",
      "Bagmati" ~ "3",
      "Gandaki" ~ "4",
      "Lumbini" ~ "5",
      "Karnali" ~ "6",
      "Sudurpaschim" ~ "7"
    )
  )

saveRDS(final_region, "final_region.rds")

# final_province <- filter(final_region, primary_cat == "Province") 

final_province <- left_join(
filter(final_region, primary_cat == "Province" & category_en == "Preliminary Population 2078") %>% 
  rename(total_pop = number), 

filter(final_region, primary_cat == "Province" & category_en == "Population Density (per sq. km)") %>% 
  select(id, pop_density = number)
) %>% 
  left_join(filter(final_region, primary_cat == "Province" & category_en == "Average Family size") %>% 
              select(id, avg_family_size = number)) %>% 
  left_join(filter(final_region, primary_cat == "Province" & category_en == "Annual Population Growth Rate (%)") %>% 
              select(id, annual_pop_growth = number))


all_province <- nepalmap::province_df %>% 
  left_join(final_province, by = "id") %>% 
  left_join(nepalmap::province_val, by = c("id", "capital", "name"))

saveRDS(all_province, "prelim-province.rds")

final %>%
  filter(
    prim_cat == "Region",
    category_en == "Preliminary Population"
  ) %>%
  mutate(region_en = fct_reorder(region_en, pop)) %>%
  ggplot(aes(x = region_en, y = pop)) +
  geom_col()

# library(extrafont)
# library(ragg)
# 
# loadfonts(device = "win", quiet = TRUE)
# 
# font_import(pattern = "Preeti")


final %>%
  filter(
    prim_cat == "Province",
    category_en == "Preliminary Population"
  ) %>%
  mutate(region_en = fct_reorder(region_en, pop)) %>%
  ggplot(aes(x = region_en, y = pop)) +
  geom_col() +
  coord_flip() +
  bbplot::bbc_style() +
  labs(
    title = "2021 Preliminiary Census Population by Province"
  )
# theme(axis.text.y = element_text(family = "Preeti", size = 16))



prelim_district <- read_excel("prelim-census-2021.xls", sheet = 2) %>%
  fill(names(.), .direction = "up") %>%
  row_to_names(1) %>%
  discard(~ all(is.na(.))) %>%
  select("region" = 2, everything(), -1) %>%
  slice(c(-1, -3)) %>%
  pivot_longer(
    cols = 2:11,
    names_to = "category",
    values_to = "number"
  ) %>%
  filter(row_number() > 10) %>%
  rename(district_n = region) %>%
  mutate(
    district_n = case_match(
      district_n,
      "बैतड़ी" ~ "बैतडी",
      "सोलुखुम्बु" ~ "सोलुखुम्वु",
      "ओखलढुङ्गा" ~ "ओखलढुङगा",
      "रौतहट" ~ "रौटहट",
      "बारा" ~ "बारा",
      "काठमाडौं" ~ "काठमाण्डौं",
      "स्याङ्गजा" ~ "स्याङ्जा",
      "बाँके" ~ "बाँके",
      "बाजुरा" ~ "बाजुरा",
      "बझाङ" ~ "बझाङ",
      "रुकुम-पूर्व" ~ "रुकुम (पूर्वी भाग)",
      "प्युठान" ~ "प्यूठान",
      "नवलपरासी (बर्दघाट सुस्ता  पश्चिम)" ~ "नवलपरासी (बर्दघाट सुस्ता पश्चिम)",
      "रुपन्देही" ~ "रूपन्देही",
      "दाङ्ग" ~ "दाङ",
      "बाँके" ~ "बाँके",
      "कालीकोट" ~ "कालिकोट",
      "रुकुम - पश्चिम" ~ "रुकुम (पश्चिम भाग)",
      "बाजुरा" ~ "बाजुरा",
      "बझाङ" ~ "बझाङ",
      .default = district_n
    ), 
    number = as.numeric(number)

    # district_n = trimws(district_n)
  )
# %>%
# mutate(pop = as.numeric(pop))

cat <- prelim_district %>%
  distinct(category) %>%
  pull()

prelim_district <- prelim_district %>%
  mutate(
    category_en = case_match(
      category,
      cat[1] ~ "Total Population (2068)",
      cat[2] ~ "Preliminary Population 2078",
      cat[3] ~ "Family Number",
      cat[4] ~ "Total Population",
      cat[5] ~ "Male",
      cat[6] ~ "Female",
      cat[7] ~ "Sex Ratio",
      cat[8] ~ "Average Family Size",
      cat[9] ~ "Population Density (per sq. km)",
      cat[10] ~ "Annual Population Growth Rate (%)"
    )
  )


pop_density <- left_join(
filter(prelim_district, category_en == 'Total Population') %>% 
  select(district_n, total_pop = number),
filter(prelim_district, category_en == 'Population Density (per sq. km)') %>% 
  select(district_n, pop_density = number), 
by = c("district_n") 
) 



map_dist <- nepalmap::district_val %>%
  mutate(district_n = trimws(district_n))



tpop_density <- pop_density %>%
  left_join(map_dist, by = "district_n") %>%
  filter(is.na(province)) %>%
  distinct(district_n)


pop_density <- pop_density %>%
  left_join(map_dist, by = "district_n") %>%
  # filter(category == "प्रारम्भिक जनसंख्या २०७८") %>%
  mutate(
    province = case_match(province, 
                          "Province 1" ~ "Koshi", 
                          "Province 2" ~ "Madhesh", 
                          "Province 5" ~ "Lumbini",
                          .default = province
    ),
    pop_cat = case_when(
      total_pop < 5000 ~ "< 5K",
      between(total_pop, 5000, 20000) ~ "5 - 10K",
      between(total_pop, 20000, 50000) ~ "20 - 50K",
      between(total_pop, 50000, 80000) ~ "50 - 80K",
      between(total_pop, 80000, 100000) ~ "80 - 1L",
      total_pop > 100000 ~ "> 1L"
    )
  ) %>% 
  arrange(id)


saveRDS(pop_density, "pop_density.rds")


library(nepalmap)

province_map()

# nepalmap::district_val %>% View()


prelim_district


map_dist <- nepalmap::district_val %>%
  mutate(district_n = trimws(district_n))



comb <- prelim_district %>%
  left_join(map_dist, by = "district_n") %>%
  filter(is.na(province)) %>%
  distinct(district_n)


tcomb <- prelim_district %>%
  left_join(map_dist, by = "district_n") %>%
  # filter(category == "प्रारम्भिक जनसंख्या २०७८") %>%
  mutate(
    # pop = as.numeric(pop),
    province = case_match(province, 
                          "Province 1" ~ "Koshi", 
                          "Province 2" ~ "Madhesh", 
                          "Province 5" ~ "Lumbini",
                          .default = province
                          ),
    pop_cat = case_when(
      number < 5000 ~ "< 5K",
      between(number, 5000, 20000) ~ "5 - 10K",
      between(number, 20000, 50000) ~ "20 - 50K",
      between(number, 50000, 80000) ~ "50 - 80K",
      between(number, 80000, 100000) ~ "80 - 1L",
      number > 100000 ~ "> 1L"
    )
  ) %>% 
  arrange(id)


saveRDS(tcomb, "prelim-pop.rds")



scale_fill_new <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#D18D72", "#BB785D", "#A56248","#904D32", "#7A371D","#642208"),
                      c("< 5K", "5 - 10K", "20 - 50K", "50 - 80K", "80 - 1L", "> 1L"),
    breaks = c("< 5K", "5 - 10K", "20 - 50K", "50 - 80K", "80 - 1L", "> 1L"),
    ...
  ))
}


d <- district_map(
  data = tcomb,
  fill = tcomb$pop_cat,
  label = "district",
  color = "white",
  x = "lat",
  y = "long",
  size = 2,
  legend = "right"
) 

d2 <- ggplot2::ggplot(nepalmap::district_df) +
  ggplot2::geom_sf(ggplot2::aes(fill = tcomb$pop_cat)) +
  ggplot2::geom_text(ggplot2::aes_string(
    x = "lat", y = "long",
    label = "district"
  ), data = tcomb, size = 2.9, color = "white") +
  ggplot2::theme_void() +
  ggplot2::scale_fill_manual(
    values = c("< 5K" = "#D18D72", "5 - 10K" = "#BB785D", "20 - 50K" = "#A56248", "50 - 80K" = "#904D32", "80 - 1L" = "#7A371D", "> 1L" = "#642208"),
    breaks = c("< 5K", "5 - 10K", "20 - 50K", "50 - 80K", "80 - 1L", "> 1L")
  ) +
  # bbplot::bbc_style() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    axis.line = element_blank()
  ) +
  labs(fill = "Population")












# clean_df <- read_excel("prelim-census-2021.xls") %>%
#   filter(!is.na(...3)) |>
#   select(-1,  "पुरुष"=6, "महिला"= 7)
#


# set_names(
#   c("region", "census_house_number", "house_family_number", "preliminary_pop", "male", "female", "sex_ratio", "avg_family_size", "annual_rise_per",
#     "pop_density_sq_km")
# ) |>
# pivot_longer(2:10, names_to = "cols", values_to = "val")


#
# prelim |>
#   rename()
#   filter(row_number() == 4) |>
#   pivot_longer(
#     cols = 2:11,
#     names_to = "country",
#     values_to = "pop"
#   ) |>
#   View()
#
#
