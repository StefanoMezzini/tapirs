library('ctmm') # using the github version (0.6.1)
library('dplyr')
library('purrr') # for vectorization

# for remote parallel computation (linux)
NCORES <- 15
Sys.setenv(MC_CORES = NCORES) # set max number of cores to NCORES
Sys.getenv('MC_CORES')        # should return NCORES

# for local parallel computation (windows)
library('furrr') # for parallel computation
NCORES <- 4 # number of cores, not number of logical processors!
plan(multisession, workers = NCORES)

# import data and convert to `telemetry` format, then plot by position and time
atlantica <- read.csv('data/1_ATLANTICFOREST_11.csv') %>%
  filter(!is.na(individual.local.identifier)) %>% # remove empty rows
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S')

pantanal <- read.csv('data/2_PANTANAL_ERRORDATASET_FINAL.csv') %>%
  arrange(individual.local.identifier, timestamp) %>% # to avoid warning
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S')

cerrado <- read.csv('data/3_CERRADO_ERRORDATASET_FINAL.csv') %>%
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S')

if(FALSE) {
  tapirs <- 
    bind_rows(tibble(region = 'atlantica', name = names(atlantica)),
              tibble(region = 'pantanal', name = names(pantanal)),
              tibble(region = 'cerrado', name = names(cerrado))) %>%
    # no map_***() available for telemetry, variogram, and ctmm objects
    mutate(data = map2(region, name,
                       function(x, y) get(x)[[y]]),
           svf = map(data, variogram),
           theta0 = map(svf,
                        function(x) variogram.fit(x, interactive = FALSE)))
  
  tapirs$fitted.mods <- rep(list('no models'), nrow(tapirs))
  
  # 5.6 hours on 15 cores
  for(i in 1:nrow(tapirs)) {
    if(i %% 10 == 0) cat(i, '\n')
    tapirs$fitted.mods[i] <-
      list(ctmm.select(data = tapirs$data[[i]],
                       CTMM = tapirs$theta0[[i]],
                       verbose = TRUE, # to return all models
                       control = list(method = 'pNewton', cores = NCORES)))
    tapirs <- mutate(tapirs,
                     best.model = map2(.x = data, .y = fitted.mods,
                                       function(x, y)
                                         ctmm.select(x, y, cores = NCORES)))
  }
  #saveRDS(tapirs, file = 'models/tapirs.rds')
  
  # Five warnings:
  # In ctmm.fit(data, GUESS, trace = trace2, ...) :
  # pREML failure: indefinite ML Hessian or divergent REML gradient.
  
} else {
  tapirs <- readRDS('models/tapirs.rds')
}

save.plots <- function(r, n) {
  tapir <- filter(tapirs, region == r, name == n)
  fun <- tapir$svf # semi-variance function
  models <- tapir$fitted.mods[[1]]
  
  pdf(file = paste0('models/diagnostics/', r, '-', n, '.pdf'), width = 16,
      height = 9) # save to pdf
  layout(t(1:2)) # plot both diagnostic plots in one page
  diagn <- outlie(tapir$data[[1]]) # data plot with diagnostics
  plot(diagn) # speed vs distance diagnostic
  
  # plot all models at 3 different zooms
  NMODELS <- length(models)
  NCELLS <- ceiling(NMODELS/2) * 2
  layout(matrix(1:NCELLS, ncol = NCELLS / 2))
  for(i in 1:NMODELS) plot(fun, CTMM = models[[i]], col.CTMM = 'red',
                           fraction = 2e-4, main = names(models)[i])
  for(i in 1:NMODELS) plot(fun, CTMM = models[[i]], col.CTMM = 'red',
                           fraction = 0.01, main = names(models)[i])
  for(i in 1:NMODELS) plot(fun, CTMM = models[[i]], col.CTMM = 'red',
                           fraction = 0.5, main = names(models)[i])
  dev.off()
  cat(n, 'complete.\n')
}

# open plots with SumatraPDF, use Ctrl+Shift+Right to move to next file
with(tapirs, future_map2(region, name, save.plots,
                         .options = furrr_options(seed = NULL)))
dev.off() # make sure all pdf devices are closed
layout(1)

# estimate speed and home range
tapirs <-
  mutate(tapirs,
         speed.est = future_map(best.model, function(x) speed(x),
                                .options = furrr_options(seed = NULL)),
         akde = future_map2(data, best.model,
                            function (x, y) akde(x, CTMM = y),
                            .options = furrr_options(seed = NULL)))
unique(warnings()) # almost half have fractal movement

plot.estimates <- function(ROW) {
  AKDE <- tapirs[ROW, 'akde'][[1]]
  DATA <- tapirs[ROW, 'data'][[1]]
  
  pdf(paste0('figures/akde/', tapirs[ROW, 'region'], '-', tapirs[ROW, 'name'],
             '.pdf'), width = 12, height = 6.75)
  plot(AKDE) # 95% quantile of home range distribution with 95% CIs
  plot(DATA, add = TRUE) # add datapoints
  dev.off()
}

# AKDE plots with data open plots with SumatraPDF, use Ctrl+Shift+Right to move to next file
with(tapirs, future_map(1:nrow(tapirs), plot.estimates,
                         .options = furrr_options(seed = NULL)))
dev.off() # make sure all pdf devices are closed
layout(1)
