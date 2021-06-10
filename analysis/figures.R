library('ctmm')      # using the github version (0.6.1)
library('ctmmweb')   # for shiny plots
library('dplyr')     # for data wrangling
library('lubridate') # makes working with dates smoother
library('purrr')     # for functional mapping
library('move')      # for movement data
library('ggplot2')   # for plotting
library('ggmap')     # for maps
# library('magrittr')
theme_set(theme_bw())
N <- 74 # number of tapirs

tapirs <- readRDS('models/tapirs-akdes.rds')

sf::st_read('C:/Users/stefa/Dropbox/Tapir_HR/Map_Info/1- ATLANTIC FOREST AKDE95-ESA.kmz')

# https://methodsblog.com/2021/03/22/mapping-animal-movement-in-r-the-science-and-the-art/
# https://github.com/pratikunterwegs/elemove/blob/master/R/04_plot_code.R

raw <-
  map_dfr(1:N,
          function(i)
            mutate(suppressWarnings(as_tibble(tapirs$data[[i]])),
                   region = tapirs$region[i],
                   region = case_when(region == 'atlantica' ~ 'Mata Atlantica',
                                      region == 'pantanal' ~ 'Pantanal',
                                      region == 'cerrado' ~ 'Cerrado'),
                   name = tapirs$name.short[i]) %>%
            dplyr::select(timestamp, longitude, latitude, t, x, y, region,
                          name)) %>%
  mutate(day = date(timestamp))

# country-level map with median location
brazil <-
  get_map(c(left = -82, bottom = -25, right = -30, top = 7), zoom = 5)
ggmap(brazil) +
  geom_point(aes(longitude, latitude, color = region), alpha = 0.5,
             group_by(raw, name, region) %>%
               summarize(longitude = median(longitude),
                         latitude = median(latitude))) +
  scale_color_brewer('Region', type = 'qual', palette = 6) +
  theme(legend.position = 'top')

# region-level map with median location
basemap <-
  get_map(c(left = -56, bottom = -23, right = -52, top = -19), zoom = 8)

ggmap(basemap) +
  geom_point(aes(longitude, latitude, color = region), alpha = 0.6,
             group_by(raw, name, region) %>%
               summarize(longitude = median(longitude),
                         latitude = median(latitude))) +
  scale_color_brewer('Region', type = 'qual', palette = 6, direction = 1)

# Mata Atlantica
get_map(c(left = -52.5, bottom = -22.7, right = -52.1, top = -22.35),
          zoom = 12) %>%
  ggmap() +
  geom_point(aes(longitude, latitude, color = day),
             alpha = 0.3, filter(raw, region == 'Mata Atlantica')) +
  geom_path(aes(longitude, latitude, group = name, color = day),
            alpha = 0.3, filter(raw, region == 'Mata Atlantica')) +
  scale_color_viridis_c('Time', option = 'A')

# zoomed to a few individuals
get_map(c(left = -52.43, bottom = -22.57, right = -52.3, top = -22.5),
        zoom = 12) %>%
  ggmap() +
  facet_wrap(~ name) +
  geom_point(aes(longitude, latitude), show.legend = FALSE, alpha = 0.2,
             filter(raw, region == 'Mata Atlantica', longitude < -52.3,
                    latitude < -22.5)) +
  geom_path(aes(longitude, latitude), alpha = 0.2,
            filter(raw, region == 'Mata Atlantica',
                   longitude < -52.3, latitude < -22.5))

# plot home ranges
range_map(tapirs$akde, hr_levels = 0.95, hr_color_vec = rep('red', N))
