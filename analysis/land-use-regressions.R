library('tidyr')      # for data wrangling (want to mask tidyr::extract())
library('sf')         # to work with spData maps
library('sp')         # to import spatial layers
library('ctmm')       # using the github version (0.6.1)
library('raster')     # to import human footprint index raster
library('dplyr')      # for data wrangling
library('purrr')      # for functional mapping
library('ggplot2')    # for plotting
library('cowplot')    # for plot grids
library('mgcv')       # for fitting GAMs
theme_set(theme_bw())
N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

sa <- filter(spData::world, continent == 'South America') # south america layer

# land use rasters
lu.atl <- raster('data/spatial-layers/1- ATLANTIC FOREST/MA_Habitat.tif')
lu.pan <-raster('data/spatial-layers/2- PANTANAL/PANTANAL_USO_SOLO_03_2017.tif')
lu.cer <- raster('data/spatial-layers/3- CERRADO/CERRADO_USO_SOLO_09_2016.tif')

# extract land use raster and land use types
tapirs <- readRDS('models/tapirs-land-use.rds') # tapir data

if(FALSE) { # re-crop rasters
  tapirs <- readRDS('models/tapirs-final.rds') # tapir data
  
  tapirs <-
    mutate(tapirs,
           region = factor(region), # need factors for GAMs
           region.lab = factor(region.lab,
                               levels = c('Mata Atlantica','Pantanal','Cerrado')),
           lu.raster =
             map(1:N,
                 function(i) {
                   r <- tapirs$region[i]
                   
                   rast <-
                     case_when(r == 'atlantica' ~ 'lu.atl',
                               r == 'pantanal' ~ 'lu.pan',
                               r == 'cerrado' ~ 'lu.cer') %>%
                     get() %>%
                     extract(as.sf(tapirs$akde[[i]]))
                   
                   cat(i, '\n', sep = '')
                   # count unique values in estimate layer
                   tot <-
                     table(rast[[2]]) %>%
                     as.data.frame()
                   
                   if(r == 'atlantica') {
                     tot <-
                       mutate(tot,
                              type = case_when(Var1 == 2 ~ '?',
                                               TRUE ~ 'forest'))
                   } else if(r == 'cerrado'){
                     tot <- mutate(tot,
                                   type =
                                     case_when(Var1 == 1 ~ 'urban',
                                               Var1 == 2 ~ 'savannah',
                                               Var1 == 3 ~ 'floodplain',
                                               Var1 == 4 ~ 'savannah',
                                               Var1 == 5 ~ 'savannah',
                                               Var1 == 6 ~ 'crop',
                                               Var1 == 7 ~ 'crop',
                                               Var1 == 8 ~ 'dirt',
                                               Var1 == 9 ~ 'plantation',
                                               Var1 == 10 ~ 'forest',
                                               Var1 == 11 ~ 'forest',
                                               Var1 == 12 ~ 'water',
                                               Var1 == 13 ~ 'crop',
                                               Var1 == 14 ~ 'plantation'))
                   } else {
                     tot <-
                       mutate(tot,
                              type = case_when(Var1 == 1 ~ 'pasture',
                                               Var1 == 2 ~ 'floodplain',
                                               Var1 == 3 ~ 'forest',
                                               Var1 == 4 ~ 'floodplain',
                                               Var1 == 5 ~ 'forest',
                                               Var1 == 6 ~ 'pasture',
                                               Var1 == 7 ~ 'headquarters'))
                   }
                   
                   tot <-
                     tot %>%
                     group_by(type) %>%
                     summarize(value = sum(Freq)) %>%
                     pivot_wider(names_from = type, values_from = value)
                 }))
  unique(warnings())
  
  tapirs <-
    full_join(tapirs,
              with(tapirs,
                   bind_cols(name = name,
                             mutate(bind_rows(lu.raster),
                                    total = rowSums(bind_rows(lu.raster),
                                                    na.rm = TRUE)))) %>%
                pivot_longer(-c('total', 'name'), names_to = 'type') %>%
                mutate(value = value / total) %>%
                pivot_wider(names_from = 'type', values_from = 'value'),
              by = 'name') %>%
    pivot_longer(`?`:plantation, names_to = 'lu', values_to = 'value') %>%
    mutate(value = if_else(is.na(value), 0, value)) %>%
    pivot_wider(names_from = lu, values_from = value)
  
  #saveRDS(tapirs, 'models/tapirs-land-use.rds')
}

# land use on hr size ####
# groups with less than 5 unique values
select(tapirs, `?`:plantation) %>%
  pivot_longer(-c()) %>%
  filter(value > 0) %>%
  group_by(name) %>%
  summarize(unique = length(unique(value)),
            min = min(value),
            max = max(value)) %>%
  arrange(desc(unique)) %>%
  mutate(`>5` = unique > 5,
         `>10` = unique > 10)

# modelling ####
# Gamma GAM regression on home range estimate
m.hr <- gam(area.est ~ s(dirt, k = 5),
            # s(floodplain, k = 10) +
            # s(pasture, k = 10) +
            # s(forest, k = 10) +
            # s(crop, k = 5) +
            # s(savannah, k = 10) +
            # s(water, k = 5),
            family = Gamma('log'),
            data = tapirs,
            method = 'REML')
summary(m.hr)

# Gamma GAM regression on home range estimate
m.speed <- gam(speed.est ~
                 s(floodplain, k = 5) +
                 s(dirt, k = 5) +
                 s(pasture, k = 5) +
                 s(savannah, k = 5) +
                 s(forest, k = 5) +
                 s(headquarters, k = 5) +
                 s(crop, k = 5) +
                 s(water, k = 5),
               family = Gamma('log'),
               data = tapirs,
               method = 'REML')
m.speed0 <- gam(speed.est ~ 1,
               family = Gamma('log'),
               data = tapirs,
               method = 'REML')
AIC(m.speed, m.speed0)

# initial plots
gratia::draw(m.hr, residuals = TRUE)
gratia::draw(m.speed, residuals = TRUE)

# predictions ####
pred <- tibble(dirt = seq(0, 0.22, length.out = 100))
pred <- bind_cols(pred,
                  predict(m.hr, newdata = pred, se.fit = TRUE)) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

# regression plot
p <-
  ggplot() +
  geom_ribbon(aes(dirt, ymin = lwr, ymax = upr), pred, alpha = 0.2) +
  geom_line(aes(dirt, est), pred) +
  geom_point(aes(dirt, area.est, color = region.lab), tapirs, alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Proportion of exposed dirt in the habitat',
       y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top'); p

ggsave('figures/lu-regression.png', height = 1.5, width = 3.23, scale = 2)
