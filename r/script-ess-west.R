#### Required packages ####

# # IMPORTANT:
# # Next line shoud be run once to install the latest development version from GitHub, enabling the use of HMC with dcar_leroux
# remotes::install_github("nimble-dev/nimble", subdir = "packages/nimble")

# install.packages("pacman", dep = TRUE)

pacman::p_load(sf, spdep, readxl, abind, ggplot2, RColorBrewer, patchwork, 
               nimble, nimbleHMC, MCMCvis, scales, install = FALSE)

# # Loading functions for calling Nimble
# source("https://raw.githubusercontent.com/MigueBeneito/pNimble/refs/heads/main/RutinasNimble.0.2.R")
# load.leroux()

#### European Social Survey (ESS) data loading ####

# ------------------------------------------------------------- #
# Data source: European Social Survey (ESS)
#
# To replicate the analysis, please download the dataset from:
# https://ess.sikt.no/en/datafile/242aaa39-3bbb-40f5-98bf-bfb1ce53d8ef
#
# Access to the ESS data requires a free user registration.
# Once downloaded, extract the .zip file and place the extracted
# CSV file in the "data" folder before running this script.
# ------------------------------------------------------------- #

# ESS data
ESSData <- read.csv(file.path("data", "ESS11e04_1.csv"))

# Western Europe data
country <- "west"
ESSData <- ESSData[ESSData$cntry == "AT" | ESSData$cntry == "BE" |
                     ESSData$cntry == "CH" | ESSData$cntry == "DE" |
                     ESSData$cntry == "ES" | ESSData$cntry == "FR" |
                     ESSData$cntry == "IT" | ESSData$cntry == "NL" |
                     ESSData$cntry == "PT", ]

ESSData <- ESSData[ESSData$region != "ES63" & ESSData$region != "ES64" & ESSData$region != "ES70" & 
                     !grepl("^FRY|^FRZ", ESSData$region) & ESSData$region != "FRM0" & 
                     ESSData$region != "PT20" & ESSData$region != "PT30", ]

# Definition of the categorical covariates
# Age groups: 1 = (14, 34]; 2 = (34, 54]; 3 = (54, 100]
ESSData$agecut <- cut(ESSData$agea, c(14, 34, 54, 100))
levels(ESSData$agecut) <- c("15-34", "35-54", "55...")
# Education groups: 1 = (-1, 225]; 2 = (225, 500]; 3 = (500, 1000]
ESSData$educut <- cut(ESSData$edulvlb,c(-1, 225, 500, 1000))
levels(ESSData$educut) <- c("edulow", "edumid", "eduhigh")

# ESS cleaned
ESS <- ESSData[, c("health", "region", "gndr", "agecut", "educut", 
                   "dweight", "pspwght", "anweight", "cntry", 
                   "domain", "psu", "stratum", "prob")]

# Removing NA rows for age and education groups for simplicity
ESS <- ESS[!is.na(ESS$agecut), ]
ESS <- ESS[!is.na(ESS$educut), ]
str(ESS)

# Transforming gender and region into factors
ESS$gndr <- factor(ESS$gndr, labels = c("Man", "Woman"))

# Western Europe
ESS$region <- factor(ESS$region, 
                     levels = c("AT11", "AT12", "AT13", "AT21", "AT22", "AT31", "AT32",
                                "AT33", "AT34", "BE10", "BE21", "BE22", "BE23", "BE24", 
                                "BE25", "BE31", "BE32", "BE33", "BE34", "BE35", "CH01", 
                                "CH02", "CH03", "CH04", "CH05", "CH06", "CH07", "DE1", 
                                "DE2", "DE3", "DE4", "DE5", "DE6", "DE7", "DE8", "DE9", 
                                "DEA", "DEB", "DEC", "DED", "DEE", "DEF", "DEG", "ES11", 
                                "ES12", "ES13", "ES21", "ES22", "ES23", "ES24", "ES30",
                                "ES41", "ES42", "ES43", "ES51", "ES52", "ES53", "ES61", 
                                "ES62", "FR10", "FRB0", "FRC1", "FRC2", "FRD1", "FRD2", 
                                "FRE1", "FRE2", "FRF1", "FRF2", "FRF3", "FRG0", "FRH0", 
                                "FRI1", "FRI2", "FRI3", "FRJ1", "FRJ2", "FRK1", "FRK2", 
                                "FRL0", "ITC", "ITF", "ITG", "ITH", "ITI", "NL11", "NL12", 
                                "NL13", "NL21", "NL22", "NL23", "NL31", "NL32", "NL33", 
                                "NL34", "NL41", "NL42", "PT11", "PT15", "PT16", "PT17", 
                                "PT18"))

# # Design weight
# weight <- ESS$dweight/mean(ESS$dweight)
# # Post-stratification weight including design weight
# weight <- ESS$pspwght/mean(ESS$pspwght)
# Analysis weight
weight <- ESS$anweight/mean(ESS$anweight)

# Gender of each respondent
gnd <- as.numeric(ESS$gndr)
# Age group of each respondent
age <- as.numeric(ESS$agecut)
# Education group of each respondent
edu <- as.numeric(ESS$educut)
# NUTS of each respondent
nuts <- as.numeric(ESS$region)

#### health - Subjective general health ####

# How is your health in general? Would you say it is...
# 1 = Very good, 2 = Good, 3 = Fair, 4 = Bad, 5 = Very bad,
# 7 = Refusal, 8 = Don't know, 9 = No answer
table(ESS$health)
y <- as.numeric(ESS$health)
y[y > 5] <- NA
variable <- "health"
y_labels <- c("Very good", "Good", "Fair", "Bad", "Very bad")

# survey: data frame containing the variables collected for each respondent (ESS)
# - nuts: small-area (NUTS1) from 1 to number of areas
# - gnd: 1 = Man, 2 = Woman
# - age: 1 = (14, 34]; 2 = (34, 54]; 3 = (54, 100]
# - edu: 1 = (-1, 225]; 2 = (225, 500]; 3 = (500, 1000]
# - weight: design/post-stratification/analysis weight
survey <- data.frame("y" = y, "nuts" = nuts, "gnd" = gnd, 
                     "age" = age, "edu" = edu, "weight" = weight)

#### Spatial neighbourhood structure ####

# Cartography of Europe
# Source: Eurostat GISCO - NUTS 2021 regions
# https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/
cartography <- st_read(file.path("data", "NUTS_RG_03M_2021_4326.shp"))
cartography <- cartography[order(cartography$NUTS_ID), ]

# Western Europe
cartography <- cartography[(cartography$LEVL_CODE == 2 &
                              cartography$CNTR_CODE %in% c("AT", "BE", "CH", "ES", 
                                                           "FR", "NL", "PT")) |
                             (cartography$LEVL_CODE == 1 & 
                                cartography$CNTR_CODE %in% c("DE", "IT")), ]
cartography <- cartography[cartography$NUTS_ID != "ES63" &
                             cartography$NUTS_ID != "ES64" &
                             cartography$NUTS_ID != "ES70" &
                             !grepl("^FRY|^FRZ", cartography$NUTS_ID) &
                             cartography$NUTS_ID != "FRM0" &
                             cartography$NUTS_ID != "PT20" &
                             cartography$NUTS_ID != "PT30", ]
cartography <- cartography[order(cartography$NUTS_ID), ]

# Neighbourhood structure by contiguity
Neigh <- poly2nb(cartography)

# Manually add adjacency between Cataluña (ES51), Comunitat Valenciana (ES52)
# and Illes Balears (ES53) to avoid treating Illes Balears as isolated.
Neigh[[which(cartography$NUTS_ID == "ES53")]] <- unique(c(Neigh[[which(cartography$NUTS_ID == "ES53")]], 
                                                          which(cartography$NUTS_ID == "ES51")))
Neigh[[which(cartography$NUTS_ID == "ES53")]] <- unique(c(Neigh[[which(cartography$NUTS_ID == "ES53")]], 
                                                          which(cartography$NUTS_ID == "ES52")))

Neigh[[which(cartography$NUTS_ID == "ES51")]] <- unique(c(Neigh[[which(cartography$NUTS_ID == "ES51")]],
                                                          which(cartography$NUTS_ID == "ES53")))
Neigh[[which(cartography$NUTS_ID == "ES52")]] <- unique(c(Neigh[[which(cartography$NUTS_ID == "ES52")]],
                                                          which(cartography$NUTS_ID == "ES53")))

# Adjacency matrix
W <- nb2mat(Neigh, style = "B", zero.policy = TRUE)
# D - W matrix 
Q <- diag(apply(W, 1, sum)) - W
# Number of areas
NNUTS <- nrow(W)
# Number of neighbours of each area
nadj <- apply(W, 1, sum)
# Neighbours of each area
map <- unlist(apply(W, 1, function(x) which(x != 0)))
# Sum of all the neighbour numbers of all areas
nadj.tot <- length(map)
# Cumulative sums of the number of neighbours of each area
index <- c(0, cumsum(nadj))
# All the neighbourhoods j ~ i where i < j
from.to <- cbind(rep(1:NNUTS, times = nadj), map); colnames(from.to) <- c("from", "to")
from.to <- from.to[which(from.to[, 1] < from.to[, 2]), ]
NDist <- nrow(from.to)
# Eigenvalues of D - W
Lambda <- eigen(Q)$values

plot_nuts_neighbours <- function(cartography, nuts_id, nuts_col = "red",
                                 neigh_col = "pink", base_col = "grey90",
                                 border_col = "black") {
  
  i <- which(cartography$NUTS_ID == nuts_id)
  fill_col <- rep(base_col, nrow(cartography))
  fill_col[i] <- nuts_col
  fill_col[Neigh[[i]]] <- neigh_col
  
  plot(st_geometry(cartography), col = fill_col, border = border_col, 
       main = paste("NUTS:", nuts_id))
}

# Plot selected regions and their neighbouring areas
plot_nuts_neighbours(cartography, nuts_id = "FRB0")
plot_nuts_neighbours(cartography, nuts_id = "ES52")

cartography$NAME_LATN[cartography$NUTS_ID == "AT13"] <- "Vienna"
cartography$NAME_LATN[cartography$NUTS_ID == "BE10"] <- "Brussels-Capital Region"
cartography$NAME_LATN[cartography$NUTS_ID == "CH02"] <- "Espace Mittelland"
cartography$NAME_LATN[cartography$NUTS_ID == "DE3"]  <- "Berlin"
cartography$NAME_LATN[cartography$NUTS_ID == "ES30"] <- "Community of Madrid"
cartography$NAME_LATN[cartography$NUTS_ID == "FR10"] <- "Ile-de-France"
cartography$NAME_LATN[cartography$NUTS_ID == "ITI"]  <- "Central Italy"
cartography$NAME_LATN[cartography$NUTS_ID == "NL32"] <- "North Holland"
cartography$NAME_LATN[cartography$NUTS_ID == "PT17"] <- "Lisbon Metropolitan Area"

#### Descriptive spatial analysis ####

# Gender selection
Gender <- 1
Gender_Cat <- c("Man", "Woman")[Gender]
gender <- tolower(Gender_Cat)

ordinal_survey <- survey[survey$gnd == Gender, ]
y <- as.numeric(ordinal_survey$y)
y[y > 5] <- NA
table(y)/sum(table(y))
nuts_all <- factor(ordinal_survey$nuts, levels = 1:NNUTS)
table(nuts_all)
cartography$y_mean_ordinal <- as.numeric(by(y, nuts_all, mean, na.rm = TRUE))

bernoulli1_survey <- survey[survey$gnd == Gender, ]
y <- as.numeric(bernoulli1_survey$y)
y[y > 5] <- NA
y[y == 1 | y == 2] <- 0
y[y == 3 | y == 4 | y == 5] <- 1
table(y)/sum(table(y))
nuts_all <- factor(bernoulli1_survey$nuts, levels = 1:NNUTS)
table(nuts_all)
cartography$y_mean_bernoulli1 <- as.numeric(by(y, nuts_all, mean, na.rm = TRUE))

bernoulli2_survey <- survey[survey$gnd == Gender, ]
y <- as.numeric(bernoulli2_survey$y)
y[y > 5] <- NA
y[y == 1 | y == 2 | y == 3] <- 0
y[y == 4 | y == 5] <- 1
table(y)/sum(table(y))
nuts_all <- factor(bernoulli2_survey$nuts, levels = 1:NNUTS)
table(nuts_all)
cartography$y_mean_bernoulli2 <- as.numeric(by(y, nuts_all, mean, na.rm = TRUE))

# Ordinal
limit <- c(min(cartography$y_mean_ordinal, na.rm = TRUE),
           max(cartography$y_mean_ordinal, na.rm = TRUE))
p_y_mean_ordinal <- ggplot(cartography) + 
  geom_sf(aes(fill = y_mean_ordinal), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Purples"), 
                       limits = c(limit[1], limit[2]),
                       name = NULL) + ggtitle("Ordinal") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

# Bernoulli (O1)
limit <- c(min(cartography$y_mean_bernoulli1, na.rm = TRUE),
           max(cartography$y_mean_bernoulli1, na.rm = TRUE))
p_y_mean_bernoulli1 <- ggplot(cartography) + 
  geom_sf(aes(fill = y_mean_bernoulli1), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Oranges"), 
                       limits = c(limit[1], limit[2]),
                       name = NULL) + ggtitle("Bernoulli (D+)") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

# Bernoulli (O2)
limit <- c(min(cartography$y_mean_bernoulli2, na.rm = TRUE),
           max(cartography$y_mean_bernoulli2, na.rm = TRUE))
p_y_mean_bernoulli2 <- ggplot(cartography) + 
  geom_sf(aes(fill = y_mean_bernoulli2), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Oranges"), 
                       limits = c(limit[1], limit[2]),
                       name = NULL) + ggtitle("Bernoulli (D-)") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

(p_y_mean_ordinal + p_y_mean_bernoulli1 + p_y_mean_bernoulli2)

ggsave(file.path("figures", paste0("descriptive_", Gender_Cat, ".png")), 
       device = "png", width = 8, height = 6, dpi = 600)

#### Data preparation and gender filtering ####

# Gender selection
Gender <- 1
survey <- survey[survey$gnd == Gender, ]

# Number of respondents
NResp <- nrow(survey)
# Vector of zeros for the zero-trick
zero <- rep(0, NResp)

# NUTS of each respondent
nuts <- survey$nuts
# Age group of each respondent
age <- survey$age
# Education group of each respondent
edu <- survey$edu
# Sampling weight
weight <- survey$weight

# Response variable
y <- survey$y
# Number of levels
NCats <- length(table(y))
# Vector of ones for Dirichlet prior
ones <- rep(1, NCats)

# Number of levels of each (categorical) covariate
NAge <- length(table(survey$age))
NEdu <- length(table(survey$edu))

#### Population processing ####

# # The population data were obtained from Eurostat:
# # https://ec.europa.eu/eurostat/databrowser/view/cens_21cobe_r2/default/table?lang=en&category=cens.cens_21.cens_21dc
# 
# process_population <- function(country_name, country_code) {
# 
#   NGnd <- 2
#   NNuts <- sum(startsWith(cartography$NUTS_ID, country_code))
# 
#   excels <- array(dim = c(NNuts, NGnd, 10, 9))
#   dimnames(excels) <- list(levels(ESS$region)[startsWith(levels(ESS$region), country_code)],
#                            levels(ESS$gndr),
#                            c("15-29", "30-34", "35-39", "40-44", "45-49",
#                              "50-54", "55-59", "60-64", "65-84", "85..."),
#                            c("ED0", "ED1", "ED2", "ED3", "ED4", "ED5", "ED6", "ED7", "ED8"))
# 
#   for (Gender in 1:NGnd) {
#     for (AgeGroup in 1:dim(excels)[3]) {
#       df_aux <- read_excel(file.path("data", paste0("population-", country_name, "-eu.xlsx")),
#                            sheet = paste0(interaction(dimnames(excels)[[2]][Gender],
#                                                       dimnames(excels)[[3]][AgeGroup])))
#       excels[, Gender, AgeGroup, ] <- as.matrix(df_aux[, -1])
#     }
#   }
# 
#   population_aux <- array(dim = c(NNuts, NGnd, 10, NEdu))
#   dimnames(population_aux) <- list(dimnames(excels)[[1]],
#                                    dimnames(excels)[[2]],
#                                    dimnames(excels)[[3]],
#                                    c("edulow", "edumid", "eduhigh"))
# 
#   for (NUTS in 1:NNuts) {
#     for (Gender in 1:NGnd) {
#       for (AgeGroup in 1:dim(population_aux)[3]) {
#         population_aux[NUTS, Gender, AgeGroup, 1] <- sum(excels[NUTS, Gender, AgeGroup, 1:3], na.rm = TRUE)
#         population_aux[NUTS, Gender, AgeGroup, 2] <- sum(excels[NUTS, Gender, AgeGroup, 4:5], na.rm = TRUE)
#         population_aux[NUTS, Gender, AgeGroup, 3] <- sum(excels[NUTS, Gender, AgeGroup, 6:9], na.rm = TRUE)
#       }
#     }
#   }
# 
#   population <- array(dim = c(NNuts, NGnd, NAge, NEdu))
#   dimnames(population) <- list(dimnames(population_aux)[[1]],
#                                dimnames(population_aux)[[2]],
#                                c("15-34", "35-54", "55..."),
#                                dimnames(population_aux)[[4]])
# 
#   for (NUTS in 1:NNuts) {
#     for (Gender in 1:NGnd) {
#       for (EduGroup in 1:NEdu) {
#         population[NUTS, Gender, 1, EduGroup] <- sum(population_aux[NUTS, Gender, 1:2, EduGroup], na.rm = TRUE)
#         population[NUTS, Gender, 2, EduGroup] <- sum(population_aux[NUTS, Gender, 3:6, EduGroup], na.rm = TRUE)
#         population[NUTS, Gender, 3, EduGroup] <- sum(population_aux[NUTS, Gender, 7:10, EduGroup], na.rm = TRUE)
#       }
#     }
#   }
# 
#   saveRDS(population, file = file.path("data", paste0("population-", country_name, "-eu.rds")))
# }
# 
# countries <- data.frame(name = c("austria", "belgium", "switzerland", "germany", "spain",
#                                  "france", "italy", "netherlands", "portugal"),
#                         code = c("AT", "BE", "CH", "DE", "ES", "FR", "IT", "NL", "PT"))
# 
# for (Country in seq_len(nrow(countries))) {
#   process_population(countries$name[Country], countries$code[Country])
# }

#### Population loading ####

# population_eu: four-dimensional array containing population counts by 
# nuts, gender, age and education group.
# The population data were obtained from Eurostat:
# https://ec.europa.eu/eurostat/databrowser/view/cens_21cobe_r2/default/table?lang=en&category=cens.cens_21.cens_21dc

countries <- c("austria", "belgium", "switzerland", "germany", "spain",
               "france", "italy", "netherlands", "portugal")

population_list <- lapply(countries,
                          function(country) {
                            readRDS(file.path("data", paste0("population-", country, "-eu.rds")))})
population_eu <- do.call(abind, c(population_list, along = 1))

# Checking
match(dimnames(population_eu)[[1]], cartography$NUTS_ID) - 1:NNUTS

# Gender selection
population_eu <- population_eu[, Gender, , ]

#### Ordinal model ####

### Model code ###

modelCode <- nimbleCode(
  {
    # Likelihood
    for (Resp in 1:NResp) {
      y[Resp] ~ dcat(prlevels[Resp, 1:NCats])
      
      # Definition of the probabilities of each category as a function of the
      # cumulative probabilities
      prlevels[Resp, 1] <- p.gamma[Resp, 1]
      for (Cat in 2:(NCats-1)) {
        prlevels[Resp, Cat] <- p.gamma[Resp, Cat] - p.gamma[Resp, Cat-1]
      }
      prlevels[Resp, NCats] <- 1 - p.gamma[Resp, NCats-1]
      
      # Linear predictor
      for (Cat in 1:(NCats-1)) {
        logit(p.gamma[Resp, Cat]) <- kappa[Cat] - 
          beta_age[age[Resp]] - beta_edu[edu[Resp]] - 
          sd.theta * theta[nuts[Resp]]
      }
    }
    
    # Prior distributions
    
    # kappa[1:(NCats-1)] cut points
    # Monotonic transformation
    for (Cat in 1:(NCats-1)) {
      kappa[Cat] <- logit(sum(delta[1:Cat]))
    }
    # delta[1:NCats] Dirichlet prior
    delta[1:NCats] ~ ddirch(ones[1:NCats])
    
    # beta_age[1:NAge] age group fixed effect (corner constraint)
    beta_age[1] <- 0
    for (AgeGroup in 2:NAge) {
      beta_age[AgeGroup] ~ dflat()
    }
    
    # beta_edu[1:NEdu] education group fixed effect (corner constraint)
    beta_edu[1] <- 0
    for (EduGroup in 2:NEdu) {
      beta_edu[EduGroup] ~ dflat()
    }
    
    # theta[1:NNUTS] spatial random effect
    # LCAR distribution
    theta[1:NNUTS] ~ dcar_leroux(rho = rho,
                                 sd.theta = 1,
                                 Lambda = Lambda[1:NNUTS],
                                 from.to = from.to[1:NDist, 1:2])
    
    # Hyperparameters of the spatial random effect
    rho ~ dunif(0, 1)
    sd.theta ~ dhalfflat()
    
    # Stochastic restrictions in order to avoid confounding problems
    # Required vectors
    for (Resp in 1:NResp) {
      theta.Resp[Resp] <- theta[nuts[Resp]]
    }
    
    # Weighted constraint
    # Zero-mean constraint for theta.Resp[1:NResp]
    zero.theta.resp ~ dnorm(mean.thetas.resp, 10000)
    mean.thetas.resp <- mean(theta.Resp[1:NResp])
    
  }
)

### Data to be loaded ###

modelData <- list(y = y,
                  zero.theta.resp = 0)

modelConstants <- list(age = age, edu = edu, nuts = nuts,
                       NResp = NResp, NCats = NCats, NAge = NAge,
                       NEdu = NEdu, ones = ones, NNUTS = NNUTS,
                       NDist = NDist, Lambda = Lambda, from.to = from.to)

### Parameters to be saved ###

modelParameters <- c("kappa", "beta_age", "beta_edu",
                     "theta", "sd.theta", "rho")

### Loading functions for calling Nimble ###

source("https://raw.githubusercontent.com/MigueBeneito/pNimble/refs/heads/main/RutinasNimble.0.2.R")
load.leroux()

# Inital values
modelInits <- local({
  constants <- modelConstants
  function() {
    library(extraDistr)
    NAge <- constants$NAge
    NEdu <- constants$NEdu; NNUTS <- constants$NNUTS
    NCats <- constants$NCats; ones <- constants$ones
    list(delta = as.numeric(rdirichlet(1, ones)),
         beta_age = c(NA, rnorm(NAge - 1)),
         beta_edu = c(NA, rnorm(NEdu - 1)),
         rho = runif(1), sd.theta = runif(1),
         theta = rnorm(NNUTS, sd = 0.1))
  }
})

# # Number of chains to run in parallel
# nchains <- 5
# # pNimble call
# salnimble <- pNimble(code = modelCode, data = modelData, constants = modelConstants,
#                      inits = modelInits, nchains = nchains, seeds = 1:nchains,
#                      niter = 2000, nburnin = 1000, thin = 5,
#                      summary = TRUE, WAIC = TRUE, monitors = modelParameters,
#                      # ntfyAccount = "MigueBeneito",
#                      HMC = TRUE, parallel = TRUE)
# 
# saveRDS(salnimble, file = file.path("results", paste0("ordinal-leroux-hmc-waic-", gender, ".rds")))

#### Bernoulli (D+) model ####

# Option 1 (D+): 0 = First two categories; 1 = Last three categories.
y[y > 5] <- NA
y[y == 1 | y == 2] <- 0
y[y == 3 | y == 4 | y == 5] <- 1

### Model code ###

modelCode <- nimbleCode(
  {
    # Likelihood
    for (Resp in 1:NResp) {
      y[Resp] ~ dbern(prsuccess[Resp])
      
      # Linear predictor  
      logit(prsuccess[Resp]) <- beta_0 + 
        beta_age[age[Resp]] + beta_edu[edu[Resp]] + 
        sd.theta * theta[nuts[Resp]]
    }
    
    # Prior distributions
    
    # beta_0 intercept
    beta_0 ~ dflat()
    
    # beta_age[1:NAge] age group fixed effect (corner constraint)
    beta_age[1] <- 0
    for (AgeGroup in 2:NAge) {
      beta_age[AgeGroup] ~ dflat()
    }
    
    # beta_edu[1:NEdu] education group fixed effect (corner constraint)
    beta_edu[1] <- 0
    for (EduGroup in 2:NEdu) {
      beta_edu[EduGroup] ~ dflat()
    }
    
    # theta[1:NNUTS] spatial random effect
    # LCAR distribution
    theta[1:NNUTS] ~ dcar_leroux(rho = rho,
                                 sd.theta = 1,
                                 Lambda = Lambda[1:NNUTS],
                                 from.to = from.to[1:NDist, 1:2])
    
    # Hyperparameters of the spatial random effect
    rho ~ dunif(0, 1)
    sd.theta ~ dhalfflat()
    
    # Stochastic restrictions in order to avoid confounding problems
    # Required vectors
    for (Resp in 1:NResp) {
      theta.Resp[Resp] <- theta[nuts[Resp]]
    }
    
    # Weighted constraint
    # Zero-mean constraint for theta.Resp[1:NResp]
    zero.theta.resp ~ dnorm(mean.thetas.resp, 10000)
    mean.thetas.resp <- mean(theta.Resp[1:NResp])
    
  }
)

### Data to be loaded ###

modelData <- list(y = y,
                  zero.theta.resp = 0)

modelConstants <- list(age = age, edu = edu, nuts = nuts,
                       NResp = NResp, NAge = NAge,
                       NEdu = NEdu, NNUTS = NNUTS,
                       NDist = NDist, Lambda = Lambda, from.to = from.to)

### Parameters to be saved ###

modelParameters <- c("beta_0", "beta_age", "beta_edu",
                     "theta", "sd.theta", "rho")

### Loading functions for calling Nimble ###

source("https://raw.githubusercontent.com/MigueBeneito/pNimble/refs/heads/main/RutinasNimble.0.2.R")
load.leroux()

# Inital values
modelInits <- local({
  constants <- modelConstants
  function() {
    NAge <- constants$NAge
    NEdu <- constants$NEdu; NNUTS <- constants$NNUTS
    list(beta_0 = rnorm(1),
         beta_age = c(NA, rnorm(NAge - 1)),
         beta_edu = c(NA, rnorm(NEdu - 1)),
         rho = runif(1), sd.theta = runif(1),
         theta = rnorm(NNUTS, sd = 0.1))
  }
})

# # Number of chains to run in parallel
# nchains <- 5
# # pNimble call
# salnimble <- pNimble(code = modelCode, data = modelData, constants = modelConstants, 
#                      inits = modelInits, nchains = nchains, seeds = 1:nchains, 
#                      niter = 2000, nburnin = 1000, thin = 5, 
#                      summary = TRUE, WAIC = TRUE, monitors = modelParameters, 
#                      # ntfyAccount = "MigueBeneito", 
#                      HMC = TRUE, parallel = TRUE)
# 
# saveRDS(salnimble, file = file.path("results", paste0("bernoulli-leroux-hmc-waic-", gender, "-o1.rds")))

#### Bernoulli (D-) model ####

# Option 2 (D-): 0 = First three categories; 1 = Last two categories.
y[y > 5] <- NA
y[y == 1 | y == 2 | y == 3] <- 0
y[y == 4 | y == 5] <- 1

### Model code ###

modelCode <- nimbleCode(
  {
    # Likelihood
    for (Resp in 1:NResp) {
      y[Resp] ~ dbern(prsuccess[Resp])
      
      # Linear predictor  
      logit(prsuccess[Resp]) <- beta_0 + 
        beta_age[age[Resp]] + beta_edu[edu[Resp]] + 
        sd.theta * theta[nuts[Resp]]
    }
    
    # Prior distributions
    
    # beta_0 intercept
    beta_0 ~ dflat()
    
    # beta_age[1:NAge] age group fixed effect (corner constraint)
    beta_age[1] <- 0
    for (AgeGroup in 2:NAge) {
      beta_age[AgeGroup] ~ dflat()
    }
    
    # beta_edu[1:NEdu] education group fixed effect (corner constraint)
    beta_edu[1] <- 0
    for (EduGroup in 2:NEdu) {
      beta_edu[EduGroup] ~ dflat()
    }
    
    # theta[1:NNUTS] spatial random effect
    # LCAR distribution
    theta[1:NNUTS] ~ dcar_leroux(rho = rho,
                                 sd.theta = 1,
                                 Lambda = Lambda[1:NNUTS],
                                 from.to = from.to[1:NDist, 1:2])
    
    # Hyperparameters of the spatial random effect
    rho ~ dunif(0, 1)
    sd.theta ~ dhalfflat()
    
    # Stochastic restrictions in order to avoid confounding problems
    # Required vectors
    for (Resp in 1:NResp) {
      theta.Resp[Resp] <- theta[nuts[Resp]]
    }
    
    # Weighted constraint
    # Zero-mean constraint for theta.Resp[1:NResp]
    zero.theta.resp ~ dnorm(mean.thetas.resp, 10000)
    mean.thetas.resp <- mean(theta.Resp[1:NResp])
    
  }
)

### Data to be loaded ###

modelData <- list(y = y,
                  zero.theta.resp = 0)

modelConstants <- list(age = age, edu = edu, nuts = nuts,
                       NResp = NResp, NAge = NAge,
                       NEdu = NEdu, NNUTS = NNUTS,
                       NDist = NDist, Lambda = Lambda, from.to = from.to)

### Parameters to be saved ###

modelParameters <- c("beta_0", "beta_age", "beta_edu",
                     "theta", "sd.theta", "rho")

### Loading functions for calling Nimble ###

source("https://raw.githubusercontent.com/MigueBeneito/pNimble/refs/heads/main/RutinasNimble.0.2.R")
load.leroux()

# Inital values
modelInits <- local({
  constants <- modelConstants
  function() {
    NAge <- constants$NAge
    NEdu <- constants$NEdu; NNUTS <- constants$NNUTS
    list(beta_0 = rnorm(1),
         beta_age = c(NA, rnorm(NAge - 1)),
         beta_edu = c(NA, rnorm(NEdu - 1)),
         rho = runif(1), sd.theta = runif(1),
         theta = rnorm(NNUTS, sd = 0.1))
  }
})

# # Number of chains to run in parallel
# nchains <- 5
# # pNimble call
# salnimble <- pNimble(code = modelCode, data = modelData, constants = modelConstants, 
#                      inits = modelInits, nchains = nchains, seeds = 1:nchains, 
#                      niter = 2000, nburnin = 1000, thin = 5, 
#                      summary = TRUE, WAIC = TRUE, monitors = modelParameters, 
#                      # ntfyAccount = "MigueBeneito", 
#                      HMC = TRUE, parallel = TRUE)
# 
# saveRDS(salnimble, file = file.path("results", paste0("bernoulli-leroux-hmc-waic-", gender, "-o2.rds")))

#### Loading posterior samples ####

# Ordinal regression
ordinal_results <- readRDS(file = file.path("results", paste0("ordinal-leroux-hmc-waic-", gender, ".rds")))
# Logistic regression (Option 1): 0 = First two categories; 1 = Last three categories.
bernoulli_results1 <- readRDS(file = file.path("results", paste0("bernoulli-leroux-hmc-waic-", gender, "-o1.rds")))
# Logistic regression (Option 2): 0 = First three categories; 1 = Last two categories.
bernoulli_results2 <- readRDS(file = file.path("results", paste0("bernoulli-leroux-hmc-waic-", gender, "-o2.rds")))

#### Convert NIMBLE output to WinBUGS-style format ####

nchains <- 5
nsims <- nchains * nrow(ordinal_results$samples[[1]])

# NimToWin: 
# - transforms NIMBLE output to WinBUGS sims.list output format

NimToWin <- function(salnimble) {
  
  kappa <- matrix(nrow = nsims, ncol = NCats - 1)
  beta_age <- matrix(nrow = nsims, ncol = NAge)
  beta_edu <- matrix(nrow = nsims, ncol = NEdu)
  theta <- matrix(nrow = nsims, ncol = NNUTS)
  sd.theta <- numeric(length = nsims)
  rho <- numeric(length = nsims)
  
  for (Cat in 1:(NCats - 1)) {
    kappa[, Cat] <- c(salnimble[[1]][,  paste0("kappa[", Cat, "]")],
                      salnimble[[2]][,  paste0("kappa[", Cat, "]")], 
                      salnimble[[3]][,  paste0("kappa[", Cat, "]")],
                      salnimble[[4]][,  paste0("kappa[", Cat, "]")], 
                      salnimble[[5]][,  paste0("kappa[", Cat, "]")])
  }
  
  for (AgeGroup in 1:NAge) {
    beta_age[, AgeGroup] <- c(salnimble[[1]][, paste0("beta_age[", AgeGroup, "]")], 
                              salnimble[[2]][, paste0("beta_age[", AgeGroup, "]")], 
                              salnimble[[3]][, paste0("beta_age[", AgeGroup, "]")],
                              salnimble[[4]][, paste0("beta_age[", AgeGroup, "]")],
                              salnimble[[5]][, paste0("beta_age[", AgeGroup, "]")])
  }
  
  for (EduGroup in 1:NEdu) {
    beta_edu[, EduGroup] <- c(salnimble[[1]][, paste0("beta_edu[", EduGroup, "]")], 
                              salnimble[[2]][, paste0("beta_edu[", EduGroup, "]")], 
                              salnimble[[3]][, paste0("beta_edu[", EduGroup, "]")],
                              salnimble[[4]][, paste0("beta_edu[", EduGroup, "]")],
                              salnimble[[5]][, paste0("beta_edu[", EduGroup, "]")])
  }
  
  for (NUTS in 1:NNUTS) {
    theta[, NUTS] <- c(salnimble[[1]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[2]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[3]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[4]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[5]][, paste0("theta[", NUTS, "]")])
  }
  
  sd.theta <- c(salnimble[[1]][, "sd.theta"], salnimble[[2]][, "sd.theta"], 
                salnimble[[3]][, "sd.theta"], salnimble[[4]][, "sd.theta"], 
                salnimble[[5]][, "sd.theta"])
  
  rho <- c(salnimble[[1]][, "rho"], salnimble[[2]][, "rho"], 
           salnimble[[3]][, "rho"], salnimble[[4]][, "rho"], 
           salnimble[[5]][, "rho"])
  
  summary <- MCMCsummary(object = salnimble, round = 4)
  sims.list <- list("kappa" = kappa, "beta_age" = beta_age, "beta_edu" = beta_edu,
                    "theta" = theta, "sd.theta" = sd.theta, "rho" = rho)
  
  salwinbugs <- list("summary" = summary, "sims.list" = sims.list,
                     "nchains" = nchains, "nsims" = nsims)
  
  return(salwinbugs)
}

ordinal_salwinbugs <- NimToWin(salnimble = ordinal_results$samples)

NimToWin <- function(salnimble) {
  
  beta_0 <- numeric(length = nsims)
  beta_age <- matrix(nrow = nsims, ncol = NAge)
  beta_edu <- matrix(nrow = nsims, ncol = NEdu)
  theta <- matrix(nrow = nsims, ncol = NNUTS)
  sd.theta <- numeric(length = nsims)
  rho <- numeric(length = nsims)
  
  beta_0 <- c(salnimble[[1]][, "beta_0"], salnimble[[2]][, "beta_0"], 
              salnimble[[3]][, "beta_0"], salnimble[[4]][, "beta_0"], 
              salnimble[[5]][, "beta_0"])
  
  for (AgeGroup in 1:NAge) {
    beta_age[, AgeGroup] <- c(salnimble[[1]][, paste0("beta_age[", AgeGroup, "]")], 
                              salnimble[[2]][, paste0("beta_age[", AgeGroup, "]")], 
                              salnimble[[3]][, paste0("beta_age[", AgeGroup, "]")],
                              salnimble[[4]][, paste0("beta_age[", AgeGroup, "]")],
                              salnimble[[5]][, paste0("beta_age[", AgeGroup, "]")])
  }
  
  for (EduGroup in 1:NEdu) {
    beta_edu[, EduGroup] <- c(salnimble[[1]][, paste0("beta_edu[", EduGroup, "]")], 
                              salnimble[[2]][, paste0("beta_edu[", EduGroup, "]")], 
                              salnimble[[3]][, paste0("beta_edu[", EduGroup, "]")],
                              salnimble[[4]][, paste0("beta_edu[", EduGroup, "]")],
                              salnimble[[5]][, paste0("beta_edu[", EduGroup, "]")])
  }
  
  for (NUTS in 1:NNUTS) {
    theta[, NUTS] <- c(salnimble[[1]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[2]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[3]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[4]][, paste0("theta[", NUTS, "]")], 
                       salnimble[[5]][, paste0("theta[", NUTS, "]")])
  }
  
  sd.theta <- c(salnimble[[1]][, "sd.theta"], salnimble[[2]][, "sd.theta"], 
                salnimble[[3]][, "sd.theta"], salnimble[[4]][, "sd.theta"], 
                salnimble[[5]][, "sd.theta"])
  
  rho <- c(salnimble[[1]][, "rho"], salnimble[[2]][, "rho"], 
           salnimble[[3]][, "rho"], salnimble[[4]][, "rho"], 
           salnimble[[5]][, "rho"])
  
  summary <- MCMCsummary(object = salnimble, round = 4)
  sims.list <- list("beta_0" = beta_0, "beta_age" = beta_age, "beta_edu" = beta_edu,
                    "theta" = theta, "sd.theta" = sd.theta, "rho" = rho)
  
  salwinbugs <- list("summary" = summary, "sims.list" = sims.list,
                     "nchains" = nchains, "nsims" = nsims)
  
  return(salwinbugs)
}

bernoulli1_salwinbugs <- NimToWin(salnimble = bernoulli_results1$samples)
bernoulli2_salwinbugs <- NimToWin(salnimble = bernoulli_results2$samples)

#### Covariate effects ####

df_plot_ord <- data.frame("param" = rownames(ordinal_results$summary)[1:6], 
                          "mean" = ordinal_results$summary[1:6, 1],
                          "lower" = ordinal_results$summary[1:6, 3],
                          "upper" = ordinal_results$summary[1:6, 5],
                          "model" = "Ordinal")
df_plot_bern1 <- data.frame("param" = rownames(bernoulli_results1$summary)[2:7], 
                            "mean" = bernoulli_results1$summary[2:7, 1],
                            "lower" = bernoulli_results1$summary[2:7, 3],
                            "upper" = bernoulli_results1$summary[2:7, 5],
                            "model" = "Bernoulli (D+)")
df_plot_bern2 <- data.frame("param" = rownames(bernoulli_results2$summary)[2:7], 
                            "mean" = bernoulli_results2$summary[2:7, 1],
                            "lower" = bernoulli_results2$summary[2:7, 3],
                            "upper" = bernoulli_results2$summary[2:7, 5],
                            "model" = "Bernoulli (D-)")
df_plot <- rbind(df_plot_ord, df_plot_bern1, df_plot_bern2)
df_plot$param <- factor(df_plot$param, levels = df_plot$param[1:6])
df_plot$model <- factor(df_plot$model, levels = c("Ordinal", "Bernoulli (D+)", "Bernoulli (D-)"))

p_fixed <- ggplot(df_plot, aes(x = param, y = mean, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.5), width = 0.25, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Ordinal" = "tomato", 
                                "Bernoulli (D+)" = "steelblue", 
                                "Bernoulli (D-)" = "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.45, "cm")) +
  labs(x = NULL, y = "Posterior mean and 95% CI", color = "")

p_fixed

ggsave(file.path("figures", paste0("fixed_", Gender_Cat, ".png")), 
       plot = p_fixed,
       device = "png", width = 8, height = 6, dpi = 600)

#### Spatial effect ####

# Posterior mean of theta's
cartography$thetamean_ordinal <- ordinal_results$summary[startsWith(rownames(ordinal_results$summary), "theta"), 1]
cartography$thetamean_bernoulli1 <- bernoulli_results1$summary[startsWith(rownames(bernoulli_results1$summary), "theta"), 1]
cartography$thetamean_bernoulli2 <- bernoulli_results2$summary[startsWith(rownames(bernoulli_results2$summary), "theta"), 1]

# 95% credible intervals of theta's
cartography$thetalow_ordinal <- ordinal_results$summary[startsWith(rownames(ordinal_results$summary), "theta"), 3]
cartography$thetaup_ordinal <- ordinal_results$summary[startsWith(rownames(ordinal_results$summary), "theta"), 5]
cartography$thetalow_bernoulli1 <- bernoulli_results1$summary[startsWith(rownames(bernoulli_results1$summary), "theta"), 3]
cartography$thetaup_bernoulli1 <- bernoulli_results1$summary[startsWith(rownames(bernoulli_results1$summary), "theta"), 5]
cartography$thetalow_bernoulli2 <- bernoulli_results2$summary[startsWith(rownames(bernoulli_results2$summary), "theta"), 3]
cartography$thetaup_bernoulli2 <- bernoulli_results2$summary[startsWith(rownames(bernoulli_results2$summary), "theta"), 5]

# Posterior sd of theta's
cartography$thetasd_ordinal <- ordinal_results$summary[startsWith(rownames(ordinal_results$summary), "theta"), 2]
cartography$thetasd_bernoulli1 <- bernoulli_results1$summary[startsWith(rownames(bernoulli_results1$summary), "theta"), 2]
cartography$thetasd_bernoulli2 <- bernoulli_results2$summary[startsWith(rownames(bernoulli_results2$summary), "theta"), 2]

# Checking when theta is greater than zero
ordinal_stepsim <- 1 * (ordinal_salwinbugs$sims.list$theta > 0)
bernoulli1_stepsim <- 1 * (bernoulli1_salwinbugs$sims.list$theta > 0)
bernoulli2_stepsim <- 1 * (bernoulli2_salwinbugs$sims.list$theta > 0)

# Posterior probabilities of theta's
cartography$probmean_ordinal <- apply(ordinal_stepsim, 2, mean)
cartography$probmean_bernoulli1 <- apply(bernoulli1_stepsim, 2, mean)
cartography$probmean_bernoulli2 <- apply(bernoulli2_stepsim, 2, mean)

# 95% credible intervals of posterior probabilities
cartography$problow_ordinal <- apply(ordinal_stepsim, 2, quantile, probs = 0.025)
cartography$probup_ordinal <- apply(ordinal_stepsim, 2, quantile, probs = 0.975)
cartography$problow_bernoulli1 <- apply(bernoulli1_stepsim, 2, quantile, probs = 0.025)
cartography$probup_bernoulli1 <- apply(bernoulli1_stepsim, 2, quantile, probs = 0.975)
cartography$problow_bernoulli2 <- apply(bernoulli2_stepsim, 2, quantile, probs = 0.025)
cartography$probup_bernoulli2 <- apply(bernoulli2_stepsim, 2, quantile, probs = 0.975)

# Borders for mean and sd rows: theta interval does not include zero
cartography$border_mean_ordinal <- NA
cartography$border_mean_ordinal[cartography$thetalow_ordinal > 0] <- "#543005"
cartography$border_mean_ordinal[cartography$thetaup_ordinal < 0] <- "#1B7837"

cartography$border_mean_bernoulli1 <- NA
cartography$border_mean_bernoulli1[cartography$thetalow_bernoulli1 > 0] <- "#543005"
cartography$border_mean_bernoulli1[cartography$thetaup_bernoulli1 < 0] <- "#1B7837"

cartography$border_mean_bernoulli2 <- NA
cartography$border_mean_bernoulli2[cartography$thetalow_bernoulli2 > 0] <- "#543005"
cartography$border_mean_bernoulli2[cartography$thetaup_bernoulli2 < 0] <- "#1B7837"

cartography$border_sd_ordinal <- NA
cartography$border_sd_ordinal[cartography$thetalow_ordinal > 0 | cartography$thetaup_ordinal < 0] <- "#08306B"

cartography$border_sd_bernoulli1 <- NA
cartography$border_sd_bernoulli1[cartography$thetalow_bernoulli1 > 0 | cartography$thetaup_bernoulli1 < 0] <- "#08306B"

cartography$border_sd_bernoulli2 <- NA
cartography$border_sd_bernoulli2[cartography$thetalow_bernoulli2 > 0 | cartography$thetaup_bernoulli2 < 0] <- "#08306B"

# Borders for significance row: probability interval does not include 0.5
cartography$border_prob_ordinal <- NA
cartography$border_prob_ordinal[cartography$problow_ordinal > 0.5] <- "#67000D"
cartography$border_prob_ordinal[cartography$probup_ordinal < 0.5] <- "#00441B"

cartography$border_prob_bernoulli1 <- NA
cartography$border_prob_bernoulli1[cartography$problow_bernoulli1 > 0.5] <- "#67000D"
cartography$border_prob_bernoulli1[cartography$probup_bernoulli1 < 0.5] <- "#00441B"

cartography$border_prob_bernoulli2 <- NA
cartography$border_prob_bernoulli2[cartography$problow_bernoulli2 > 0.5] <- "#67000D"
cartography$border_prob_bernoulli2[cartography$probup_bernoulli2 < 0.5] <- "#00441B"

# Mean
limit <- max(abs(c(cartography$thetamean_ordinal, 
                   cartography$thetamean_bernoulli1,
                   cartography$thetamean_bernoulli2)), na.rm = TRUE)

p_thetamean_ordinal <- ggplot(cartography) + 
  geom_sf(aes(fill = thetamean_ordinal, color = border_mean_ordinal), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "BrBG")[9:1], 
                       limits = c(-limit, limit),     
                       values = rescale(c(-limit, 0, limit)),
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

p_thetamean_bernoulli1 <- ggplot(cartography) + 
  geom_sf(aes(fill = thetamean_bernoulli1, color = border_mean_bernoulli1), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "BrBG")[9:1], 
                       limits = c(-limit, limit),     
                       values = rescale(c(-limit, 0, limit)),
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

p_thetamean_bernoulli2 <- ggplot(cartography) + 
  geom_sf(aes(fill = thetamean_bernoulli2, color = border_mean_bernoulli2), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "BrBG")[9:1], 
                       limits = c(-limit, limit),     
                       values = rescale(c(-limit, 0, limit)),
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

# Sd
limit <- c(min(c(cartography$thetasd_ordinal, 
                 cartography$thetasd_bernoulli1,
                 cartography$thetasd_bernoulli2), na.rm = TRUE),
           max(c(cartography$thetasd_ordinal, 
                 cartography$thetasd_bernoulli1,
                 cartography$thetasd_bernoulli2), na.rm = TRUE))

p_thetasd_ordinal <- ggplot(cartography) + 
  geom_sf(aes(fill = thetasd_ordinal, color = border_sd_ordinal), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), 
                       limits = c(limit[1], limit[2]), 
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

p_thetasd_bernoulli1 <- ggplot(cartography) + 
  geom_sf(aes(fill = thetasd_bernoulli1, color = border_sd_bernoulli1), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), 
                       limits = c(limit[1], limit[2]), 
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

p_thetasd_bernoulli2 <- ggplot(cartography) + 
  geom_sf(aes(fill = thetasd_bernoulli2, color = border_sd_bernoulli2), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), 
                       limits = c(limit[1], limit[2]), 
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

# Significance
p_probmean_ordinal <- ggplot(cartography) +
  geom_sf(aes(fill = probmean_ordinal, color = border_prob_ordinal), linewidth = 0.45) +
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlGn")[9:1],
                       limits = c(0, 1),
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

p_probmean_bernoulli1 <- ggplot(cartography) +
  geom_sf(aes(fill = probmean_bernoulli1, color = border_prob_bernoulli1), linewidth = 0.45) +
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlGn")[9:1],
                       limits = c(0, 1),
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

p_probmean_bernoulli2 <- ggplot(cartography) +
  geom_sf(aes(fill = probmean_bernoulli2, color = border_prob_bernoulli2), linewidth = 0.45) +
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlGn")[9:1],
                       limits = c(0, 1),
                       name = NULL) +
  scale_color_identity(na.value = NA) +
  theme_void()

p_thetamean_ordinal <- p_thetamean_ordinal + ggtitle("Ordinal")
p_thetamean_bernoulli1 <- p_thetamean_bernoulli1 + ggtitle("Bernoulli (D+)")
p_thetamean_bernoulli2 <- p_thetamean_bernoulli2 + ggtitle("Bernoulli (D-)")

p_thetamean_ordinal <- p_thetamean_ordinal + labs(tag = "Mean")
p_thetasd_ordinal <- p_thetasd_ordinal + labs(tag = "Sd")
p_probmean_ordinal <- p_probmean_ordinal + labs(tag = "P(> 0)")

tema_mapas <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                    plot.tag = element_text(face = "bold", size = 13, angle = 90),
                    plot.tag.position = c(-0.08, 0.5),
                    plot.margin = margin(5.5, 5.5, 5.5, 20))

final_plot <- wrap_plots(p_thetamean_ordinal + tema_mapas,
                         p_thetamean_bernoulli1 + tema_mapas,
                         p_thetamean_bernoulli2 + tema_mapas,
                         p_thetasd_ordinal + tema_mapas,
                         p_thetasd_bernoulli1 + tema_mapas,
                         p_thetasd_bernoulli2 + tema_mapas,
                         p_probmean_ordinal + tema_mapas,
                         p_probmean_bernoulli1 + tema_mapas,
                         p_probmean_bernoulli2 + tema_mapas, ncol = 3)

final_plot

ggsave(file.path("figures", paste0("spatialEffect_", Gender_Cat, ".png")), 
       device = "png", width = 8, height = 6, dpi = 600)

#### Poststratified estimates ####

# poststratify: 
# - (1) computes the nsims simulated probabilities for each age, education and nuts
# - (2) post-stratifies these probabilities for each nuts and category

poststratify <- function(salwinbugs) {
  
  p.gamma <- array(dim = c(nsims, NNUTS, NAge, NEdu, NCats - 1))
  prlevels <- array(dim = c(nsims, NNUTS, NAge, NEdu, NCats))
  prlevels_post <- array(dim = c(nsims, NNUTS, NCats))
  population <- population_eu
  
  # Probabilities for each age, education and nuts
  for (sim in 1:nsims) {
    for (AgeGroup in 1:NAge) {
      for (EduGroup in 1:NEdu) {
        for (NUTS in 1:NNUTS) {
          for (Cat in 1:(NCats - 1)) {
            p.gamma[sim, NUTS, AgeGroup, EduGroup, Cat] <- 
              ilogit(salwinbugs$sims.list$kappa[sim, Cat] - 
                       salwinbugs$sims.list$beta_age[sim, AgeGroup] - 
                       salwinbugs$sims.list$beta_edu[sim, EduGroup] - 
                       salwinbugs$sims.list$sd.theta[sim] * salwinbugs$sims.list$theta[sim, NUTS])
          }
          
          prlevels[sim, NUTS, AgeGroup, EduGroup, 1] <- p.gamma[sim, NUTS, AgeGroup, EduGroup, 1]
          prlevels[sim, NUTS, AgeGroup, EduGroup, NCats] <- 1 - p.gamma[sim, NUTS, AgeGroup, EduGroup, NCats - 1]
          
          for (Cat in 2:(NCats - 1)) {
            prlevels[sim, NUTS, AgeGroup, EduGroup, Cat] <- 
              p.gamma[sim, NUTS, AgeGroup, EduGroup, Cat] - p.gamma[sim, NUTS, AgeGroup, EduGroup, Cat-1]
          }
        }
      }
    }
    
    # Post-stratification
    for (NUTS in 1:NNUTS) {
      for (Cat in 1:NCats) {
        prlevels_post[sim, NUTS, Cat] <- sum(population[NUTS, , ])^(-1) * sum(prlevels[sim, NUTS, , , Cat] * population[NUTS, , ])
      }
    }
    if (sim %in% c(1, seq(nsims/nchains, nsims, nsims/nchains))) {
      cat(sim, "of", nsims, "simulations", "\n")
    } else {}
  }
  
  return(prlevels_post)
}

ordinal_post <- poststratify(salwinbugs = ordinal_salwinbugs)

poststratify <- function(salwinbugs) {
  
  prsuccess <- array(dim = c(nsims, NNUTS, NAge, NEdu))
  prsuccess_post <- matrix(nrow = nsims, ncol = NNUTS)
  population <- population_eu
  
  # Probabilities for each age, education and nuts
  for (sim in 1:nsims) {
    for (AgeGroup in 1:NAge) {
      for (EduGroup in 1:NEdu) {
        for (NUTS in 1:NNUTS) {
          prsuccess[sim, NUTS, AgeGroup, EduGroup] <- 
            ilogit(salwinbugs$sims.list$beta_0[sim] + 
                     salwinbugs$sims.list$beta_age[sim, AgeGroup] + 
                     salwinbugs$sims.list$beta_edu[sim, EduGroup] + 
                     salwinbugs$sims.list$sd.theta[sim] * salwinbugs$sims.list$theta[sim, NUTS])
        }
      }
    }
    
    # Post-stratification
    for (NUTS in 1:NNUTS) {
      prsuccess_post[sim, NUTS] <- sum(population[NUTS, , ])^(-1) * sum(prsuccess[sim, NUTS, , ] * population[NUTS, , ])
    }
    if (sim %in% c(1, seq(nsims/nchains, nsims, nsims/nchains))) {
      cat(sim, "of", nsims, "simulations", "\n")
    } else {}
  }
  
  return(prsuccess_post)
}

bernoulli1_post <- poststratify(salwinbugs = bernoulli1_salwinbugs)
bernoulli2_post <- poststratify(salwinbugs = bernoulli2_salwinbugs)

# Post-stratified posterior means and sd's of the percentages for each nuts and category

### Ordinal model ###

# Mean
cartography$percentage_mean1 <- apply(ordinal_post, 2:3, mean)[, 1] * 100
p_percentage_mean1 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean1), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"), name = NULL) +
  theme_void()
cartography$percentage_mean2 <- apply(ordinal_post, 2:3, mean)[, 2] * 100
p_percentage_mean2 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean2), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"), name = NULL) +
  theme_void()
cartography$percentage_mean3 <- apply(ordinal_post, 2:3, mean)[, 3] * 100
p_percentage_mean3 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean3), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"), name = NULL) +
  theme_void()
cartography$percentage_mean4 <- apply(ordinal_post, 2:3, mean)[, 4] * 100
p_percentage_mean4 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean4), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"), name = NULL) +
  theme_void()
cartography$percentage_mean5 <- apply(ordinal_post, 2:3, mean)[, 5] * 100
p_percentage_mean5 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean5), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"), name = NULL) +
  theme_void()

# Sd
cartography$percentage_sd1 <- apply(ordinal_post, 2:3, sd)[, 1] * 100
p_percentage_sd1 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd1), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), name = NULL) +
  theme_void()
cartography$percentage_sd2 <- apply(ordinal_post, 2:3, sd)[, 2] * 100
p_percentage_sd2 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd2), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), name = NULL) +
  theme_void()
cartography$percentage_sd3 <- apply(ordinal_post, 2:3, sd)[, 3] * 100
p_percentage_sd3 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd3), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), name = NULL) +
  theme_void()
cartography$percentage_sd4 <- apply(ordinal_post, 2:3, sd)[, 4] * 100
p_percentage_sd4 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd4), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), name = NULL) +
  theme_void()
cartography$percentage_sd5 <- apply(ordinal_post, 2:3, sd)[, 5] * 100
p_percentage_sd5 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd5), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), name = NULL) +
  theme_void()

# CV
cartography$percentage_CV1 <- 100 * cartography$percentage_sd1/cartography$percentage_mean1
p_percentage_CV1 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV1), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"), name = NULL) + 
  theme_void()
cartography$percentage_CV2 <- 100 * cartography$percentage_sd2/cartography$percentage_mean2
p_percentage_CV2 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV2), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"), name = NULL) + 
  theme_void()
cartography$percentage_CV3 <- 100 * cartography$percentage_sd3/cartography$percentage_mean3
p_percentage_CV3 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV3), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"), name = NULL) + 
  theme_void()
cartography$percentage_CV4 <- 100 * cartography$percentage_sd4/cartography$percentage_mean4
p_percentage_CV4 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV4), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"), name = NULL) + 
  theme_void()
cartography$percentage_CV5 <- 100 * cartography$percentage_sd5/cartography$percentage_mean5
p_percentage_CV5 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV5), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"), name = NULL) + 
  theme_void()

# Probability above European mean
eu_mean1 <- apply(ordinal_post[, , 1], 1, mean)
eu_mean2 <- apply(ordinal_post[, , 2], 1, mean)
eu_mean3 <- apply(ordinal_post[, , 3], 1, mean)
eu_mean4 <- apply(ordinal_post[, , 4], 1, mean)
eu_mean5 <- apply(ordinal_post[, , 5], 1, mean)

eu_mean_post1 <- mean(eu_mean1) * 100
eu_mean_post2 <- mean(eu_mean2) * 100
eu_mean_post3 <- mean(eu_mean3) * 100
eu_mean_post4 <- mean(eu_mean4) * 100
eu_mean_post5 <- mean(eu_mean5) * 100

cartography$percentage_prob_above1 <- apply(ordinal_post[, , 1] > eu_mean1, 2, mean)
cartography$percentage_prob_above2 <- apply(ordinal_post[, , 2] > eu_mean2, 2, mean)
cartography$percentage_prob_above3 <- apply(ordinal_post[, , 3] > eu_mean3, 2, mean)
cartography$percentage_prob_above4 <- apply(ordinal_post[, , 4] > eu_mean4, 2, mean)
cartography$percentage_prob_above5 <- apply(ordinal_post[, , 5] > eu_mean5, 2, mean)

cartography$percentage_low1 <- apply(ordinal_post[, , 1] * 100, 2, quantile, probs = 0.025)
cartography$percentage_low2 <- apply(ordinal_post[, , 2] * 100, 2, quantile, probs = 0.025)
cartography$percentage_low3 <- apply(ordinal_post[, , 3] * 100, 2, quantile, probs = 0.025)
cartography$percentage_low4 <- apply(ordinal_post[, , 4] * 100, 2, quantile, probs = 0.025)
cartography$percentage_low5 <- apply(ordinal_post[, , 5] * 100, 2, quantile, probs = 0.025)

cartography$percentage_up1 <- apply(ordinal_post[, , 1] * 100, 2, quantile, probs = 0.975)
cartography$percentage_up2 <- apply(ordinal_post[, , 2] * 100, 2, quantile, probs = 0.975)
cartography$percentage_up3 <- apply(ordinal_post[, , 3] * 100, 2, quantile, probs = 0.975)
cartography$percentage_up4 <- apply(ordinal_post[, , 4] * 100, 2, quantile, probs = 0.975)
cartography$percentage_up5 <- apply(ordinal_post[, , 5] * 100, 2, quantile, probs = 0.975)

cartography$border_prob_above1 <- NA
cartography$border_prob_above1[cartography$percentage_low1 > eu_mean_post1] <- "#67000D"
cartography$border_prob_above1[cartography$percentage_up1 < eu_mean_post1] <- "#08306B"

cartography$border_prob_above2 <- NA
cartography$border_prob_above2[cartography$percentage_low2 > eu_mean_post2] <- "#67000D"
cartography$border_prob_above2[cartography$percentage_up2 < eu_mean_post2] <- "#08306B"

cartography$border_prob_above3 <- NA
cartography$border_prob_above3[cartography$percentage_low3 > eu_mean_post3] <- "#67000D"
cartography$border_prob_above3[cartography$percentage_up3 < eu_mean_post3] <- "#08306B"

cartography$border_prob_above4 <- NA
cartography$border_prob_above4[cartography$percentage_low4 > eu_mean_post4] <- "#67000D"
cartography$border_prob_above4[cartography$percentage_up4 < eu_mean_post4] <- "#08306B"

cartography$border_prob_above5 <- NA
cartography$border_prob_above5[cartography$percentage_low5 > eu_mean_post5] <- "#67000D"
cartography$border_prob_above5[cartography$percentage_up5 < eu_mean_post5] <- "#08306B"

p_percentage_prob_above1 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_prob_above1, color = border_prob_above1), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_percentage_prob_above2 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_prob_above2, color = border_prob_above2), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_percentage_prob_above3 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_prob_above3, color = border_prob_above3), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_percentage_prob_above4 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_prob_above4, color = border_prob_above4), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_percentage_prob_above5 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_prob_above5, color = border_prob_above5), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_percentage_mean1 <- p_percentage_mean1 + ggtitle(y_labels[1])
p_percentage_mean2 <- p_percentage_mean2 + ggtitle(y_labels[2])
p_percentage_mean3 <- p_percentage_mean3 + ggtitle(y_labels[3])
p_percentage_mean4 <- p_percentage_mean4 + ggtitle(y_labels[4])
p_percentage_mean5 <- p_percentage_mean5 + ggtitle(y_labels[5])

p_percentage_mean1 <- p_percentage_mean1 + labs(tag = "Mean")
p_percentage_sd1   <- p_percentage_sd1 + labs(tag = "Sd")
p_percentage_CV1   <- p_percentage_CV1 + labs(tag = "CV (%)")
p_percentage_prob_above1 <- p_percentage_prob_above1 + labs(tag = "P(> EU mean)")

tema_mapas <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                    plot.tag = element_text(face = "bold", size = 12, angle = 90),
                    plot.tag.position = c(-0.08, 0.5),
                    plot.margin = margin(5.5, 5.5, 5.5, 20))

final_plot <- wrap_plots(p_percentage_mean1 + tema_mapas, 
                         p_percentage_mean2 + tema_mapas,
                         p_percentage_mean3 + tema_mapas, 
                         p_percentage_mean4 + tema_mapas,
                         p_percentage_mean5 + tema_mapas,
                         p_percentage_sd1 + tema_mapas, 
                         p_percentage_sd2 + tema_mapas,
                         p_percentage_sd3 + tema_mapas, 
                         p_percentage_sd4 + tema_mapas,
                         p_percentage_sd5 + tema_mapas,
                         p_percentage_CV1 + tema_mapas, 
                         p_percentage_CV2 + tema_mapas,
                         p_percentage_CV3 + tema_mapas, 
                         p_percentage_CV4 + tema_mapas, 
                         p_percentage_CV5 + tema_mapas,
                         p_percentage_prob_above1 + tema_mapas,
                         p_percentage_prob_above2 + tema_mapas,
                         p_percentage_prob_above3 + tema_mapas,
                         p_percentage_prob_above4 + tema_mapas,
                         p_percentage_prob_above5 + tema_mapas, ncol = 5)
final_plot

ggsave(file.path("figures", paste0("prevalenceOrdinal_", Gender_Cat, ".png")), 
       device = "png", width = 15, height = 10, dpi = 600)

### Bernoulli models: D+ and D- ###

tema_mapas_bern <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                         plot.tag = element_text(face = "bold", size = 12, angle = 90),
                         plot.tag.position = c(-0.08, 0.5),
                         plot.margin = margin(5.5, 5.5, 5.5, 20))

### Bernoulli (D+) ###

cartography$percentage_mean_Dplus <- apply(bernoulli1_post, 2, mean) * 100
p_percentage_mean_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean_Dplus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"), name = NULL) + 
  ggtitle("Mean") + theme_void()

cartography$percentage_sd_Dplus <- apply(bernoulli1_post, 2, sd) * 100
p_percentage_sd_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd_Dplus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), name = NULL) + 
  ggtitle("Sd") + theme_void()

cartography$percentage_CV_Dplus <- 100 * cartography$percentage_sd_Dplus / cartography$percentage_mean_Dplus
p_percentage_CV_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV_Dplus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"), name = NULL) + 
  ggtitle("CV (%)") + theme_void()

eu_mean_Dplus <- apply(bernoulli1_post, 1, mean)
eu_mean_post_Dplus <- mean(eu_mean_Dplus) * 100

cartography$percentage_prob_above_Dplus <- apply(bernoulli1_post > eu_mean_Dplus, 2, mean)
cartography$percentage_low_Dplus <- apply(bernoulli1_post * 100, 2, quantile, probs = 0.025)
cartography$percentage_up_Dplus <- apply(bernoulli1_post * 100, 2, quantile, probs = 0.975)

cartography$border_prob_above_Dplus <- NA
cartography$border_prob_above_Dplus[cartography$percentage_low_Dplus > eu_mean_post_Dplus] <- "#67000D"
cartography$border_prob_above_Dplus[cartography$percentage_up_Dplus < eu_mean_post_Dplus] <- "#08306B"

p_percentage_prob_above_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_prob_above_Dplus, color = border_prob_above_Dplus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + ggtitle("P(> EU mean)") + theme_void()

p_percentage_mean_Dplus <- p_percentage_mean_Dplus + labs(tag = "Bernoulli (D+)")

### Bernoulli (D-) ###

cartography$percentage_mean_Dminus <- apply(bernoulli2_post, 2, mean) * 100
p_percentage_mean_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean_Dminus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"), name = NULL) + 
  theme_void()

cartography$percentage_sd_Dminus <- apply(bernoulli2_post, 2, sd) * 100
p_percentage_sd_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd_Dminus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), name = NULL) + 
  theme_void()

cartography$percentage_CV_Dminus <- 100 * cartography$percentage_sd_Dminus / cartography$percentage_mean_Dminus
p_percentage_CV_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV_Dminus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"), name = NULL) + 
  theme_void()

eu_mean_Dminus <- apply(bernoulli2_post, 1, mean)
eu_mean_post_Dminus <- mean(eu_mean_Dminus) * 100

cartography$percentage_prob_above_Dminus <- apply(bernoulli2_post > eu_mean_Dminus, 2, mean)
cartography$percentage_low_Dminus <- apply(bernoulli2_post * 100, 2, quantile, probs = 0.025)
cartography$percentage_up_Dminus <- apply(bernoulli2_post * 100, 2, quantile, probs = 0.975)

cartography$border_prob_above_Dminus <- NA
cartography$border_prob_above_Dminus[cartography$percentage_low_Dminus > eu_mean_post_Dminus] <- "#67000D"
cartography$border_prob_above_Dminus[cartography$percentage_up_Dminus < eu_mean_post_Dminus] <- "#08306B"

p_percentage_prob_above_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_prob_above_Dminus, color = border_prob_above_Dminus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_percentage_mean_Dminus <- p_percentage_mean_Dminus + labs(tag = "Bernoulli (D-)")

final_plot_bernoulli <- wrap_plots(p_percentage_mean_Dplus + tema_mapas_bern,
                                   p_percentage_sd_Dplus + tema_mapas_bern,
                                   p_percentage_CV_Dplus + tema_mapas_bern,
                                   p_percentage_prob_above_Dplus + tema_mapas_bern,
                                   p_percentage_mean_Dminus + tema_mapas_bern,
                                   p_percentage_sd_Dminus + tema_mapas_bern,
                                   p_percentage_CV_Dminus + tema_mapas_bern,
                                   p_percentage_prob_above_Dminus + tema_mapas_bern,
                                   ncol = 4)

final_plot_bernoulli

ggsave(file.path("figures", paste0("prevalenceBernoulli_", Gender_Cat, ".png")),
       plot = final_plot_bernoulli, device = "png", width = 14, height = 7, dpi = 600)

#### Comparison: Bernoulli and aggregated ordinal models ####

# Ordinal aggregations
ordinal_Dplus_post <- ordinal_post[, , 3] + ordinal_post[, , 4] + ordinal_post[, , 5]
ordinal_Dminus_post <- ordinal_post[, , 4] + ordinal_post[, , 5]

# Mean
cartography$percentage_mean_bernoulli1 <- apply(bernoulli1_post, 2, mean) * 100
cartography$percentage_mean_ordinal_Dplus <- apply(ordinal_Dplus_post, 2, mean) * 100
cartography$percentage_mean_bernoulli2 <- apply(bernoulli2_post, 2, mean) * 100
cartography$percentage_mean_ordinal_Dminus <- apply(ordinal_Dminus_post, 2, mean) * 100

# Sd
cartography$percentage_sd_bernoulli1 <- apply(bernoulli1_post, 2, sd) * 100
cartography$percentage_sd_ordinal_Dplus <- apply(ordinal_Dplus_post, 2, sd) * 100
cartography$percentage_sd_bernoulli2 <- apply(bernoulli2_post, 2, sd) * 100
cartography$percentage_sd_ordinal_Dminus <- apply(ordinal_Dminus_post, 2, sd) * 100

# CV
cartography$percentage_CV_bernoulli1 <- 100 * cartography$percentage_sd_bernoulli1 / cartography$percentage_mean_bernoulli1
cartography$percentage_CV_ordinal_Dplus <- 100 * cartography$percentage_sd_ordinal_Dplus / cartography$percentage_mean_ordinal_Dplus
cartography$percentage_CV_bernoulli2 <- 100 * cartography$percentage_sd_bernoulli2 / cartography$percentage_mean_bernoulli2
cartography$percentage_CV_ordinal_Dminus <- 100 * cartography$percentage_sd_ordinal_Dminus / cartography$percentage_mean_ordinal_Dminus

# Common scales within each comparison
limit_mean_Dplus <- range(c(cartography$percentage_mean_bernoulli1,
                            cartography$percentage_mean_ordinal_Dplus), na.rm = TRUE)
limit_mean_Dminus <- range(c(cartography$percentage_mean_bernoulli2,
                             cartography$percentage_mean_ordinal_Dminus), na.rm = TRUE)

limit_sd_Dplus <- range(c(cartography$percentage_sd_bernoulli1,
                          cartography$percentage_sd_ordinal_Dplus), na.rm = TRUE)
limit_sd_Dminus <- range(c(cartography$percentage_sd_bernoulli2,
                           cartography$percentage_sd_ordinal_Dminus), na.rm = TRUE)

limit_CV_Dplus <- range(c(cartography$percentage_CV_bernoulli1,
                          cartography$percentage_CV_ordinal_Dplus), na.rm = TRUE)
limit_CV_Dminus <- range(c(cartography$percentage_CV_bernoulli2,
                           cartography$percentage_CV_ordinal_Dminus), na.rm = TRUE)

# Mean maps
p_percentage_mean_bernoulli1 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean_bernoulli1), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"),
                       limits = limit_mean_Dplus,
                       name = NULL) + ggtitle("Bernoulli (D+)") + theme_void()

p_percentage_mean_ordinal_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean_ordinal_Dplus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"),
                       limits = limit_mean_Dplus,
                       name = NULL) + ggtitle("Ordinal (D+)") + theme_void()

p_percentage_mean_bernoulli2 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean_bernoulli2), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"),
                       limits = limit_mean_Dminus,
                       name = NULL) + ggtitle("Bernoulli (D-)") + theme_void()

p_percentage_mean_ordinal_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_mean_ordinal_Dminus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrBr"),
                       limits = limit_mean_Dminus,
                       name = NULL) + ggtitle("Ordinal (D-)") + theme_void()

# Sd maps
p_percentage_sd_bernoulli1 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd_bernoulli1), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       limits = limit_sd_Dplus,
                       name = NULL) + theme_void()

p_percentage_sd_ordinal_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd_ordinal_Dplus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       limits = limit_sd_Dplus,
                       name = NULL) + theme_void()

p_percentage_sd_bernoulli2 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd_bernoulli2), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       limits = limit_sd_Dminus,
                       name = NULL) + theme_void()

p_percentage_sd_ordinal_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_sd_ordinal_Dminus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                       limits = limit_sd_Dminus,
                       name = NULL) + theme_void()

# CV maps
p_percentage_CV_bernoulli1 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV_bernoulli1), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"),
                       limits = limit_CV_Dplus,
                       name = NULL) +  theme_void()

p_percentage_CV_ordinal_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV_ordinal_Dplus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"),
                       limits = limit_CV_Dplus,
                       name = NULL) + theme_void()

p_percentage_CV_bernoulli2 <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV_bernoulli2), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"),
                       limits = limit_CV_Dminus,
                       name = NULL) + theme_void()

p_percentage_CV_ordinal_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = percentage_CV_ordinal_Dminus), color = "grey30", linewidth = 0.1) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Greens"),
                       limits = limit_CV_Dminus,
                       name = NULL) + theme_void()

# Probability above European mean

eu_mean_bernoulli1 <- apply(bernoulli1_post, 1, mean)
eu_mean_ordinal_Dplus <- apply(ordinal_Dplus_post, 1, mean)
eu_mean_bernoulli2 <- apply(bernoulli2_post, 1, mean)
eu_mean_ordinal_Dminus <- apply(ordinal_Dminus_post, 1, mean)

eu_mean_post_bernoulli1 <- mean(eu_mean_bernoulli1) * 100
eu_mean_post_ordinal_Dplus <- mean(eu_mean_ordinal_Dplus) * 100
eu_mean_post_bernoulli2 <- mean(eu_mean_bernoulli2) * 100
eu_mean_post_ordinal_Dminus <- mean(eu_mean_ordinal_Dminus) * 100

cartography$prob_above_bernoulli1 <- apply(bernoulli1_post > eu_mean_bernoulli1, 2, mean)
cartography$prob_above_ordinal_Dplus <- apply(ordinal_Dplus_post > eu_mean_ordinal_Dplus, 2, mean)
cartography$prob_above_bernoulli2 <- apply(bernoulli2_post > eu_mean_bernoulli2, 2, mean)
cartography$prob_above_ordinal_Dminus <- apply(ordinal_Dminus_post > eu_mean_ordinal_Dminus, 2, mean)

cartography$low_bernoulli1 <- apply(bernoulli1_post * 100, 2, quantile, probs = 0.025)
cartography$up_bernoulli1 <- apply(bernoulli1_post * 100, 2, quantile, probs = 0.975)

cartography$low_ordinal_Dplus <- apply(ordinal_Dplus_post * 100, 2, quantile, probs = 0.025)
cartography$up_ordinal_Dplus <- apply(ordinal_Dplus_post * 100, 2, quantile, probs = 0.975)

cartography$low_bernoulli2 <- apply(bernoulli2_post * 100, 2, quantile, probs = 0.025)
cartography$up_bernoulli2 <- apply(bernoulli2_post * 100, 2, quantile, probs = 0.975)

cartography$low_ordinal_Dminus <- apply(ordinal_Dminus_post * 100, 2, quantile, probs = 0.025)
cartography$up_ordinal_Dminus <- apply(ordinal_Dminus_post * 100, 2, quantile, probs = 0.975)

cartography$border_above_bernoulli1 <- NA
cartography$border_above_bernoulli1[cartography$low_bernoulli1 > eu_mean_post_bernoulli1] <- "#67000D"
cartography$border_above_bernoulli1[cartography$up_bernoulli1 < eu_mean_post_bernoulli1] <- "#08306B"

cartography$border_above_ordinal_Dplus <- NA
cartography$border_above_ordinal_Dplus[cartography$low_ordinal_Dplus > eu_mean_post_ordinal_Dplus] <- "#67000D"
cartography$border_above_ordinal_Dplus[cartography$up_ordinal_Dplus < eu_mean_post_ordinal_Dplus] <- "#08306B"

cartography$border_above_bernoulli2 <- NA
cartography$border_above_bernoulli2[cartography$low_bernoulli2 > eu_mean_post_bernoulli2] <- "#67000D"
cartography$border_above_bernoulli2[cartography$up_bernoulli2 < eu_mean_post_bernoulli2] <- "#08306B"

cartography$border_above_ordinal_Dminus <- NA
cartography$border_above_ordinal_Dminus[cartography$low_ordinal_Dminus > eu_mean_post_ordinal_Dminus] <- "#67000D"
cartography$border_above_ordinal_Dminus[cartography$up_ordinal_Dminus < eu_mean_post_ordinal_Dminus] <- "#08306B"

p_prob_above_bernoulli1 <- ggplot(cartography) + 
  geom_sf(aes(fill = prob_above_bernoulli1, color = border_above_bernoulli1), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_prob_above_ordinal_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = prob_above_ordinal_Dplus, color = border_above_ordinal_Dplus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

p_prob_above_bernoulli2 <- ggplot(cartography) + 
  geom_sf(aes(fill = prob_above_bernoulli2, color = border_above_bernoulli2), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) + 
  scale_color_identity(na.value = NA) + theme_void()

p_prob_above_ordinal_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = prob_above_ordinal_Dminus, color = border_above_ordinal_Dminus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

# Row labels
p_percentage_mean_bernoulli1 <- p_percentage_mean_bernoulli1 + labs(tag = "Mean")
p_percentage_sd_bernoulli1 <- p_percentage_sd_bernoulli1 + labs(tag = "Sd")
p_percentage_CV_bernoulli1 <- p_percentage_CV_bernoulli1 + labs(tag = "CV (%)")
p_prob_above_bernoulli1 <- p_prob_above_bernoulli1 + labs(tag = "P(> EU mean)")

# Theme
tema_mapas <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                    plot.tag = element_text(face = "bold", size = 12, angle = 90),
                    plot.tag.position = c(-0.08, 0.5), plot.margin = margin(5.5, 5.5, 5.5, 20))

final_plot <- wrap_plots(p_percentage_mean_bernoulli1 + tema_mapas,
                         p_percentage_mean_ordinal_Dplus + tema_mapas,
                         p_percentage_mean_bernoulli2 + tema_mapas,
                         p_percentage_mean_ordinal_Dminus + tema_mapas,
                         p_percentage_sd_bernoulli1 + tema_mapas,
                         p_percentage_sd_ordinal_Dplus + tema_mapas,
                         p_percentage_sd_bernoulli2 + tema_mapas,
                         p_percentage_sd_ordinal_Dminus + tema_mapas,
                         p_percentage_CV_bernoulli1 + tema_mapas,
                         p_percentage_CV_ordinal_Dplus + tema_mapas,
                         p_percentage_CV_bernoulli2 + tema_mapas,
                         p_percentage_CV_ordinal_Dminus + tema_mapas,
                         p_prob_above_bernoulli1 + tema_mapas,
                         p_prob_above_ordinal_Dplus + tema_mapas,
                         p_prob_above_bernoulli2 + tema_mapas,
                         p_prob_above_ordinal_Dminus + tema_mapas, ncol = 4)

final_plot

ggsave(file.path("figures", paste0("comparison_", Gender_Cat, ".png")), 
       device = "png", width = 15, height = 10, dpi = 600)

#### Differences: Bernoulli and aggregated ordinal models ####

ordinal_Dplus_post <- ordinal_post[, , 3] + ordinal_post[, , 4] + ordinal_post[, , 5]
ordinal_Dminus_post <- ordinal_post[, , 4] + ordinal_post[, , 5]

diff_Dplus_post <- ordinal_Dplus_post - bernoulli1_post
diff_Dminus_post <- ordinal_Dminus_post - bernoulli2_post

# Mean
cartography$diffmean_Dplus <- apply(diff_Dplus_post, 2, mean) * 100
cartography$diffmean_Dminus <- apply(diff_Dminus_post, 2, mean) * 100

# Sd
cartography$diffsd_Dplus <- apply(diff_Dplus_post, 2, sd) * 100
cartography$diffsd_Dminus <- apply(diff_Dminus_post, 2, sd) * 100

# 95% credible intervals for the differences
cartography$difflow_Dplus <- apply(diff_Dplus_post * 100, 2, quantile, probs = 0.025)
cartography$diffup_Dplus  <- apply(diff_Dplus_post * 100, 2, quantile, probs = 0.975)

cartography$difflow_Dminus <- apply(diff_Dminus_post * 100, 2, quantile, probs = 0.025)
cartography$diffup_Dminus  <- apply(diff_Dminus_post * 100, 2, quantile, probs = 0.975)

# Probability that the difference is greater than zero
Dplus_stepsim <- 1 * (diff_Dplus_post > 0)
Dminus_stepsim <- 1 * (diff_Dminus_post > 0)

cartography$prob_diff_Dplus_great0 <- apply(Dplus_stepsim, 2, mean)
cartography$prob_diff_Dminus_great0 <- apply(Dminus_stepsim, 2, mean)

cartography$problow_Dplus <- apply(Dplus_stepsim, 2, quantile, probs = 0.025)
cartography$probup_Dplus  <- apply(Dplus_stepsim, 2, quantile, probs = 0.975)

cartography$problow_Dminus <- apply(Dminus_stepsim, 2, quantile, probs = 0.025)
cartography$probup_Dminus  <- apply(Dminus_stepsim, 2, quantile, probs = 0.975)

# Borders
cartography$border_mean_Dplus <- NA
cartography$border_mean_Dplus[cartography$difflow_Dplus > 0] <- "#67000D"
cartography$border_mean_Dplus[cartography$diffup_Dplus < 0] <- "#08306B"

cartography$border_mean_Dminus <- NA
cartography$border_mean_Dminus[cartography$difflow_Dminus > 0] <- "#67000D"
cartography$border_mean_Dminus[cartography$diffup_Dminus < 0] <- "#08306B"

cartography$border_sd_Dplus <- NA
cartography$border_sd_Dplus[cartography$difflow_Dplus > 0 | cartography$diffup_Dplus < 0] <- "#08306B"

cartography$border_sd_Dminus <- NA
cartography$border_sd_Dminus[cartography$difflow_Dminus > 0 | cartography$diffup_Dminus < 0] <- "#08306B"

cartography$border_prob_Dplus <- NA
cartography$border_prob_Dplus[cartography$problow_Dplus > 0.5] <- "#67000D"
cartography$border_prob_Dplus[cartography$probup_Dplus < 0.5] <- "#08306B"

cartography$border_prob_Dminus <- NA
cartography$border_prob_Dminus[cartography$problow_Dminus > 0.5] <- "#67000D"
cartography$border_prob_Dminus[cartography$probup_Dminus < 0.5] <- "#08306B"

# Common scales
limit_diffmean <- max(abs(c(cartography$diffmean_Dplus,
                            cartography$diffmean_Dminus)), na.rm = TRUE)

limit_diffsd <- range(c(cartography$diffsd_Dplus,
                        cartography$diffsd_Dminus), na.rm = TRUE)

# Mean maps
p_diffmean_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = diffmean_Dplus, color = border_mean_Dplus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(-limit_diffmean, limit_diffmean),
                       values = rescale(c(-limit_diffmean, 0, limit_diffmean)),
                       name = NULL) + 
  scale_color_identity(na.value = NA) + ggtitle("Mean") + theme_void()

p_diffmean_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = diffmean_Dminus, color = border_mean_Dminus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(-limit_diffmean, limit_diffmean),
                       values = rescale(c(-limit_diffmean, 0, limit_diffmean)),
                       name = NULL) + 
  scale_color_identity(na.value = NA) + theme_void()

# Sd maps
p_diffsd_Dplus <- ggplot(cartography) + 
  geom_sf(aes(fill = diffsd_Dplus, color = border_sd_Dplus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), limits = limit_diffsd, name = NULL) +
  scale_color_identity(na.value = NA) + ggtitle("Sd") + theme_void()

p_diffsd_Dminus <- ggplot(cartography) + 
  geom_sf(aes(fill = diffsd_Dminus, color = border_sd_Dminus), linewidth = 0.45) + 
  scale_fill_gradientn(colours = brewer.pal(9, "Blues"), limits = limit_diffsd, name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

# Probability maps
p_prob_Dplus <- ggplot(cartography) +
  geom_sf(aes(fill = prob_diff_Dplus_great0, color = border_prob_Dplus), linewidth = 0.45) +
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + ggtitle("P(> 0)") + theme_void()

p_prob_Dminus <- ggplot(cartography) +
  geom_sf(aes(fill = prob_diff_Dminus_great0, color = border_prob_Dminus), linewidth = 0.45) +
  scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1],
                       limits = c(0, 1), values = rescale(c(0, 0.5, 1)), name = NULL) +
  scale_color_identity(na.value = NA) + theme_void()

# Row labels
p_diffmean_Dplus <- p_diffmean_Dplus + labs(tag = "Ordinal (D+) - Bernoulli (D+)")
p_diffmean_Dminus <- p_diffmean_Dminus + labs(tag = "Ordinal (D-) - Bernoulli (D-)")

# Theme
tema_mapas_diff <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                         plot.tag = element_text(face = "bold", size = 12, angle = 90),
                         plot.tag.position = c(-0.08, 0.5),
                         plot.margin = margin(5.5, 5.5, 5.5, 20))

# Final plot
final_plot_diff <- wrap_plots(p_diffmean_Dplus + tema_mapas_diff,
                              p_diffsd_Dplus + tema_mapas_diff,
                              p_prob_Dplus + tema_mapas_diff,
                              p_diffmean_Dminus + tema_mapas_diff,
                              p_diffsd_Dminus + tema_mapas_diff,
                              p_prob_Dminus + tema_mapas_diff, ncol = 3)

final_plot_diff

ggsave(file.path("figures", paste0("differences_", Gender_Cat, ".png")),
       plot = final_plot_diff, device = "png", width = 11, height = 7, dpi = 600)

#### Ordinal profiles in selected regions ####

capital_nuts <- c("AT13", "BE10", "CH02", "DE3", "ES30", 
                  "FR10", "ITI", "NL32", "PT17")

selected_ids <- match(capital_nuts, cartography$NUTS_ID)

df_capitals <- data.frame()
for (capital in selected_ids) {
  
  ordinal_area <- ordinal_post[, capital, ] * 100
  bernoulli1_area <- apply(bernoulli1_post, 2, mean)[capital] * 100
  bernoulli2_area <- apply(bernoulli2_post, 2, mean)[capital] * 100
  
  df_capitals <- rbind(df_capitals,data.frame(
    category = c("Very good", "Good", "Fair", "Bad", "Very bad"),
    mean = apply(ordinal_area, 2, mean),
    lower = apply(ordinal_area, 2, quantile, probs = 0.025),
    upper = apply(ordinal_area, 2, quantile, probs = 0.975),
    area = paste0(cartography$NUTS_ID[capital],
                  " (D+ = ", round(bernoulli1_area, 2), "%",
                  ", D- = ", round(bernoulli2_area, 2), "%)")))
}

df_capitals$category <- factor(df_capitals$category,
                               levels = c("Very good", "Good", "Fair", "Bad", "Very bad"))

p_capitals <- ggplot(df_capitals, aes(x = category, y = mean, color = area)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6), width = 0.25, linewidth = 1) +
  theme_minimal() + labs(x = NULL, y = "Poststratified percentage (posterior mean and 95% PI)", color = NULL) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "bottom", legend.text = element_text(size = 10.25),
        legend.key.size = unit(0.6, "cm"))

p_capitals

ggsave(file.path("figures", paste0("ordinal_profiles_", Gender_Cat, ".png")),
       plot = p_capitals, device = "png", width = 14, height = 9, dpi = 600)

#### Convergence assessment ####

### Ordinal model ###

ordinal_results$summary

MCMCsummary(object = ordinal_results$samples, params = "rho",
            # exact = TRUE,
            # ISB = FALSE,
            round = 4)

MCMCtrace(object = ordinal_results$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "rho")

test <- ordinal_results$samples

which((MCMCsummary(object = test, params = "kappa", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "kappa", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "beta_age", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_age", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "beta_edu", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_edu", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "theta", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "theta", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "sd.theta", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "sd.theta", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "rho", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "rho", round = 4)[, 7] < 400))

### Bernoulli (D+) model ###

bernoulli_results1$summary

MCMCsummary(object = bernoulli_results1$samples, params = "rho",
            # exact = TRUE,
            # ISB = FALSE,
            round = 4)

MCMCtrace(object = bernoulli_results1$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "beta_0")

test <- bernoulli_results1$samples

which((MCMCsummary(object = test, params = "beta_0", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_0", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "beta_age", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_age", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "beta_edu", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_edu", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "theta", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "theta", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "sd.theta", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "sd.theta", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "rho", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "rho", round = 4)[, 7] < 400))

### Bernoulli (D-) model ###

bernoulli_results2$summary

MCMCsummary(object = bernoulli_results2$samples, params = "rho",
            # exact = TRUE,
            # ISB = FALSE,
            round = 4)

MCMCtrace(object = bernoulli_results2$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "beta_0")

test <- bernoulli_results2$samples

which((MCMCsummary(object = test, params = "beta_0", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_0", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "beta_age", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_age", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "beta_edu", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "beta_edu", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "theta", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "theta", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "sd.theta", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "sd.theta", round = 4)[, 7] < 400))
which((MCMCsummary(object = test, params = "rho", round = 4)[, 6]) > 1.02 | (MCMCsummary(object = test, params = "rho", round = 4)[, 7] < 400))
