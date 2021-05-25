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
