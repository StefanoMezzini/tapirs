library('tidyr')      # for data wrangling (want to mask tidyr::extract())
# library('sf')         # to work with spData maps
# library('sp')         # to import spatial layers
library('ctmm')       # using the github version (0.6.1)
# library('raster')     # to import human footprint index raster
library('dplyr')      # for data wrangling
library('purrr')      # for functional mapping
# library('ggplot2')    # for plotting
# library('cowplot')    # for plot grids
# library('mgcv')       # for fitting GAMs
# library('gratia')     # for GAM plots
# theme_set(theme_bw())
N <- 74 # number of tapirs

tapirs <-
  readRDS('models/tapirs-land-use.rds') %>% # tapir data
  mutate(tstart = map(data,
                      function(x)
                        as.POSIXct(x$timestamp) %>%
                        min(na.rm = TRUE) %>% as.character) %>%
           unlist() %>% as.POSIXct(),
         tend = map(data,
                    function(x)
                      as.POSIXct(x$timestamp) %>%
                      max(na.rm = TRUE) %>% as.character) %>%
           unlist() %>% as.POSIXct(),
         days = tend - tstart,
         median.long = map_dbl(data, function(x)
           median(x$longitude)),
         median.lat = map_dbl(data, function(x)
           median(x$latitude)),
         color = if_else(color == '#009900', '#66CCEE', color),
         # parameter units follow the units in figures/meta.png
         tau.position.est = tau.position.est / 60 / 60 / 24, # seconds to days
         tau.position.low = tau.position.low / 60 / 60 / 24,
         tau.position.high = tau.position.high / 60 / 60 / 24,
         tau.velocity.est = tau.velocity.est / 60 / 60, # seconds to hours
         tau.velocity.low = tau.velocity.low / 60 / 60,
         tau.velocity.high = tau.velocity.high / 60 / 60,
         cell.area = if_else(region == 'Mata Atlantica', 30^2, 10^2)) %>%
  # est, low, and high are likely for tau_v or speed (possibly not in SI)
  select(-c(region, `?`, data, svf, est, low, high, lu.raster, headquarters,
            theta0, model, akde)) %>%
  rename(region = region.lab,
         uere.calibtration = calib,
         total.cells = total)
colnames(tapirs) %>% cat(sep = ', ')

# ra-arrange columns
tapirs %>%
  select(region, name, name.short, method, sex, age, adult, tstart, tend, days,
         median.long, median.lat, uere.calibtration, color, speed.est,
         speed.high, speed.low, tau.position.est, tau.position.high,
         tau.position.low, tau.velocity.est, tau.velocity.high,
         tau.velocity.low, area.low, area.est, area.high, x.df, y.df, cell.area,
         total.cells, forest, floodplain, pasture, crop, dirt, savannah, water,
         urban, plantation) %>%
  write.csv('data/summary-dataset.csv', row.names = FALSE)
