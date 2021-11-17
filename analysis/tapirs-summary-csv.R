library('sf')        # to import ml-HFI raster
library('sp')        # to import ml-HFI raster
library('ncdf4')     # to import ml-HFI raster
library('raster')    # to import ml-HFI raster
library('lubridate') # makes working with dates smoother
library('ctmm')      # for continuous time movement models
library('dplyr')     # for data wrangling (transmute(), %>%, ...)
library('tidyr')     # for data wrangling (unnest())
library('purrr')     # for functional programming (e.g. map_*())

# machine-learning human footprint index raster
hfi.raster <- raster('data/hfi-layers/ml_hfi_v1_2019.nc')

tapirs <-
  readRDS('models/tapirs-land-use.rds') %>%
  transmute(
    name, # tapir ID
    sex,
    age, # Adult, sub-adult, juvenile
    calibrated = calib, # was location instrument calibrated? TRUE/FALSE
    median.longitude = map_dbl(data, \(x) median(x$longitude)),
    median.latitude = map_dbl(data, \(x) median(x$latitude)),
    longitude.df = x.df, # home range degrees of freedom 
    latitude.df = y.df,
    adult, # Yes/No
    start = map_chr(data, \(x) as.character(min(x$timestamp))) %>% as_date,
    end = map_chr(data, \(x) as.character(max(x$timestamp))) %>% as_date,
    duration.days = as.numeric(end - start), # sampling duration
    method, # sampling method (e.g. VHF or GPS)
    biome = if_else(region.lab == 'Mata Atlantica', # region label for plotting
                    'Atlantic forest', as.character(region.lab)),
    area.est, area.low, area.high, # HR size estimate and 95% CI
    speed.est, speed.low, speed.high, # average speed estimate and 95% CI
    # parameter units follow the units in figures/meta.png
    tau.position.est = tau.position.est / 60 / 60 / 24, # seconds to days
    tau.position.low = tau.position.low / 60 / 60 / 24,
    tau.position.high = tau.position.high / 60 / 60 / 24,
    tau.velocity.est = tau.velocity.est / 60 / 60, # seconds to hours
    tau.velocity.low = tau.velocity.low / 60 / 60,
    tau.velocity.high = tau.velocity.high / 60 / 60,
    hfi.mean = map_dbl(1:74, # average human footprint index
                       function(i)
                         # take HR estimate and extract its mean HFI
                         raster::extract(hfi.raster, as.sf(akde[[i]]))[[2]] %>%
                         suppressWarnings() %>% # warns that projection changed
                         mean(na.rm = TRUE)),
    # keep all relevant habitat type columns
    total.cells = total,
    cell.area = if_else(biome == 'Atlantic forest', 30^2, 10^2),
    total.area = total.cells * cell.area,
    forest,
    floodplain,
    pasture,
    crop,
    dirt,
    savannah,
    water,
    urban,
    plantation,
    other = `?`)

tapirs

# save the tibble as a csv
readr::write_csv(tapirs, 'data/tapirs-summary.csv')
