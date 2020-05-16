library(tidyverse)
library(stringr)
library(janitor)
library(maps)
library(ggdark)
library(ggthemes)





volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

volcano <- janitor::remove_empty(volcano, which = c("rows"))



eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

eruptions <- janitor::remove_empty(eruptions, which = c("rows"))



events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

events <- janitor::remove_empty(events, which = c("rows"))



tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')

tree_rings <- janitor::remove_empty(tree_rings, which = c("rows"))




sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

sulfur <- janitor::remove_empty(sulfur, which = c("rows"))


# Top 10 Volcano Types & Counts
# Bar Chart

p_bar1 <- volcano %>% count(primary_volcano_type) %>% 
  mutate(rown = row_number(desc(n))) %>%
  filter(rown <= 10) %>% ggplot() + 
  geom_bar(mapping = aes(x = reorder(primary_volcano_type, n), y = n, fill = primary_volcano_type), stat = "identity", show.legend = FALSE, width = 1) + 
  theme(aspect.ratio = 1) + labs(x = "Volcano Type", y = "No of Volcanoes", title = "Volcanoes by Type")

p_bar1 + coord_flip() + theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"))

#Top 10 Volcnao Types by Counts
#But this time it is by Pie Chart

p_bar2 <- volcano %>% count(primary_volcano_type) %>% 
  mutate(rown = row_number(desc(n))) %>%
  filter(rown <= 10) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = primary_volcano_type, y = n, fill = primary_volcano_type, color = primary_volcano_type, alpha = 1), stat = "identity", show.legend = FALSE, width = 1) + 
  theme(aspect.ratio = 1) + labs(x = NULL, y = NULL, title = "Volcano Types")


p_bar2 + coord_polar() + theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"))


#Top 10 Countries with Volcanoes
#Again a Pie Chart

p_bar3 <- volcano %>% count(country) %>% mutate(rown = row_number(desc(n))) %>% 
  filter(rown <= 10) %>% ggplot() + 
  geom_bar(mapping = aes(x = country, y = n, color = country, fill = country, alpha = 1), stat = "identity", show.legend = FALSE, width = 1) + 
  theme(aspect.ratio = 1) + labs(x = NULL, y = NULL, title = "Volcanoes by Country")

p_bar3 + coord_polar()  + theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"))



#World Map of Volcanic Eruptions by VEI and Duration of eruption


tbl_eruptions <- eruptions %>% filter(
  !is.na(vei)) %>% unite(start_date, start_day, start_month, start_year, sep = "/") %>%
  unite(end_date, end_day, end_month, end_year, sep = "/") %>%
  transmute(
  volcano_number = volcano_number,
  volcano_name = volcano_name, 
  eruption_number = eruption_number, 
  eruption_category = eruption_category, 
  Vei = as.factor(vei),
  start_date = as.Date(start_date, format = "%d/%m/%Y"), 
  end_date = as.Date(end_date, format = "%d/%m/%Y"), 
  Duration = as.numeric(end_date - start_date), 
  longitude = longitude, 
  latitude = latitude
)  %>% filter(!is.na(Duration))




world <- map_data("world")

ggplot() + 
  geom_map(data = world, 
           map = world, 
           mapping = aes(long, lat, map_id = region), 
           color = "black", 
           fill = "lightgray", 
           size = 0.1) + 
  geom_point(data = tbl_eruptions, 
             mapping = aes(longitude, latitude, color = Vei, size = Duration), 
             show.legend = TRUE) + 
  theme_void() +
  labs(title = "Volcanic Eruptions") + 
  guides(color = guide_legend(title = "VEI"), 
         size = guide_legend(title = "Eruption Duration"))


  












