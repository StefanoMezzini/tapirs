library('sf')           # to work with spData maps
library('sp')           # to import spatial layers
library('ctmm')         # using the github version (0.6.1)
library('raster')       # to import human footprint index raster
library('dplyr')        # for data wrangling
library('tidyr')        # for data wrangling (want to mask tidyr::extract())
library('purrr')        # for functional mapping
library('ggplot2')      # for plotting
library('cowplot')      # for plot grids
library('tabularaster') # to convert rasters to tibbles easily
theme_set(theme_bw())
N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE')

# single column of all relevant habitat type proportions
tapirs <- readRDS('models/tapirs-land-use.rds') %>%
  mutate(region.lab = if_else(region.lab == 'Mata Atlantica',
                              'Atlantic forest', as.character(region.lab)) %>%
           factor(levels = c('Atlantic forest', 'Pantanal', 'Cerrado'))) %>%
  pivot_longer(c(forest, floodplain, pasture, crop, dirt, savannah, water,
                 urban, plantation), names_to = 'Habitat type',
               values_to = 'Proportion') %>%
  mutate(`Habitat type` = stringr::str_to_title(`Habitat type`))

# rasters
find.areas <- function(r) {
  
  file.name <-
    case_when(r == 'atlantica' ~ '1- ATLANTIC FOREST/MA_Habitat.tif',
              r == 'pantanal' ~ '2- PANTANAL/PANTANAL_USO_SOLO_03_2017.tif',
              r == 'cerrado' ~ '3- CERRADO/CERRADO_USO_SOLO_09_2016.tif')
  
  # K <- if_else(grepl('atlantica', r), 30^2, 10^2) / 1000^2
  
  tot <-
    raster(paste0('data/spatial-layers/', file.name)) %>%
    as_tibble() %>%
    filter(!is.na(cellvalue))
  
  if(r == 'atlantica') {
    tot <-
      mutate(tot,
             type = case_when(cellvalue == 2 ~ '?',
                              TRUE ~ 'forest'))
  } else if(r == 'cerrado'){
    tot <- mutate(tot,
                  type =
                    case_when(cellvalue == 1 ~ 'urban',
                              cellvalue == 2 ~ 'savannah',
                              cellvalue == 3 ~ 'floodplain',
                              cellvalue == 4 ~ 'savannah',
                              cellvalue == 5 ~ 'savannah',
                              cellvalue == 6 ~ 'crop',
                              cellvalue == 7 ~ 'crop',
                              cellvalue == 8 ~ 'dirt',
                              cellvalue == 9 ~ 'plantation',
                              cellvalue == 10 ~ 'forest',
                              cellvalue == 11 ~ 'forest',
                              cellvalue == 12 ~ 'water',
                              cellvalue == 13 ~ 'crop',
                              cellvalue == 14 ~ 'plantation'))
  } else {
    tot <-
      mutate(tot,
             type = case_when(cellvalue == 1 ~ 'pasture',
                              cellvalue == 2 ~ 'floodplain',
                              cellvalue == 3 ~ 'forest',
                              cellvalue == 4 ~ 'floodplain',
                              cellvalue == 5 ~ 'forest',
                              cellvalue == 6 ~ 'pasture',
                              cellvalue == 7 ~ 'headquarters'))
  }
  
  tot <- tot %>%
    group_by(type) %>%
    summarize(area = n()) %>%
    mutate(area = area / sum(area),
           type = stringr::str_to_title(type),
           Region = case_when(r == 'atlantica' ~ 'Mata Atlantica',
                              r == 'pantanal' ~ 'Pantanal',
                              r == 'cerrado' ~ 'Cerrado'))
}

rasters <- bind_rows(find.areas('atlantica'),
                     find.areas('pantanal'),
                     find.areas('cerrado'))

# boxplots of entire habitat type
p1 <-
  mutate(rasters,
         Region = factor(Region,
                         levels = c('Mata Atlantica', 'Pantanal', 'Cerrado')),
         type = case_when(type == 'Forest' ~ 'Forest',
                          type == 'Dirt' ~ 'Exposed soil',
                          type == 'Savannah' ~ 'Savannah',
                          type == 'Floodplain' ~ 'Floodplain',
                          type == 'Water' ~ 'Water',
                          TRUE ~ 'Other') %>%
           factor(levels = c('Forest', 'Savannah', 'Floodplain', 'Exposed soil',
                             'Water', 'Other'))) %>%
  ggplot() +
  geom_bar(aes(x = Region, y = area, fill = type), stat = 'identity',
           position = position_stack(reverse = TRUE)) +
  ylab('Proportion') +
  theme(legend.position = 'top') +
  scale_fill_manual('Habitat type',
                    values = c('forestgreen', 'goldenrod', 'aquamarine2',
                               'brown4', 'cornflowerblue', 'grey75'))

# boxplots of land use type (forest, water, dirt)
p2 <-
  ggplot(tapirs) +
  geom_boxplot(aes(`Habitat type`, Proportion, fill = region.lab)) +
  scale_fill_manual('Region', values = pal) +
  theme(legend.position = 'top')

plot_grid(p1, p2, labels = c('a)', 'b)'))
ggsave('figures/habitat-types.png', width = 6.86, height = 4, dpi = 300,
       scale = 1.5)
