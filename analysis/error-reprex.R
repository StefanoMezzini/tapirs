library('ctmm')
library('dplyr')

pantanal <- read.csv('data/2_PANTANAL_ERRORDATASET_FINAL.csv') %>%
  arrange(individual.local.identifier, timestamp) %>% # to avoid warning
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S', mark.rm = TRUE)

pantanal.calib <- read.csv('data/CALIBRATION_Pantanal.csv') %>% as.telemetry()
pantanal.uere <- uere.fit(pantanal.calib)
uere(pantanal) <- pantanal.uere

DATA <- pantanal[['PA_03_BANDAID']] # fits
DATA <- pantanal[['PA_19_SERGIAO']] # fails

svf <- variogram(DATA)
theta0 <- ctmm.guess(data = DATA, CTMM = ctmm(error = TRUE),
                     variogram = svf, interactive = FALSE)

model <- ctmm.select(data = DATA, CTMM = theta0)

# in tapir-models.R:
# tapirs[c(1:22, 25, 26), ] (and likely more) fit
# tapirs[c(23, 24, 74), ] (and likely more) fail

library('readxl') # for read_xlsx()
library('purrr') # for map()
N <- 74 # number of tapirs

atlantica <- read.csv('data/1_ATLANTICFOREST_11.csv') %>% # degraded
  as.telemetry(timeformat = '%Y-%m-%d %H:%M', mark.rm = TRUE)

cerrado <- read.csv('data/3_CERRADO_ERRORDATASET_FINAL.csv') %>% # reference
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S', mark.rm = TRUE)

cerrado.calib <- read.csv('data/CALIBRATION_Cerrado.csv') %>% as.telemetry()
cerrado.uere <- uere.fit(pantanal.calib)
uere(cerrado) <- cerrado.uere

traits <-
  bind_rows(read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'ATLANTIC FOREST (11)',
                      range = 'A3:D13',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'atlantica'),
            read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'PANTANAL (46)',
                      range = 'A3:D48',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'pantanal'),
            read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'CERRADO (19)',
                      range = 'A3:D21',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'cerrado')) %>%
  mutate(name.short = map_chr(name,
                              function(x) substr(x,
                                                 gregexpr(' ', x)[[1]][1],
                                                 100)),
         name.short = gsub(' ', '', name.short),
         name.short = gsub('-', '', name.short))

# two individuals not in Cerrado dataset
filter(traits, name.short %in% c('SILVIO', 'SOFIA'))

# fit models ####
tapirs <-
  bind_rows(tibble(region = 'atlantica', name = names(atlantica)),
            tibble(region = 'pantanal', name = names(pantanal)),
            tibble(region = 'cerrado', name = names(cerrado))) %>%
  mutate(name.short = map_chr(name,
                              function(x) substr(x,
                                                 gregexpr('_', x)[[1]][2],
                                                 100)),
         name.short = gsub('_', '', name.short)) %>%
  left_join(select(traits, -region, -name), by = 'name.short') %>%
  mutate(data = map2(region, name,
                     function(x, y) get(x)[[y]]))
classes <-
  map(1:N, function(i) {
    x <- unique(tapirs$data[[i]]$class)
    if(is.null(x)) return('') else return(as.character(x))
  })

# models fail if "Succeeded [HDOP] [vertical] [speed]" %in% DATA$class:
unique(unlist(classes)) # all classes
unique(unlist(classes[c(1:22, 25:29)])) # classes of models that fit
unique(unlist(classes[c(23:24, 74)])) # classes of models that fail to fit
