# Causal Inference Methods with Aerosol Optical Depth Data
# Author: Nick Wibert
# Last Modified: 01/20/22


# Libraries ---------------------------------------------------------------

library(ncdf4)
library(RColorBrewer)
library(raster)
library(sp)
library(Synth) # synthetic control


# Synthetic control -------------------------------------------------------

# Idea: Say there is a pollution policy instated in a given area.
# Synthetic control uses a weighted average of multiple cases
# from other areas which did not receive this policy to create a
# "synthetic control" case of the area which DID. In essence, it allows
# us to create an artificial version of this area that represents
# what would have happened if the policy were NOT instituted
# (hence the name, synthetic *control*) which we can then compare with
# what was actually observed.

# Let's practice using synthetic control with organic matter (OM) alone.
# We will randomly sample some points to have a "treated" group
# and a control group, and designate some year where an imaginary
# intervention took place.


data_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/Annual"

type <- "BC"
years <- 2000:2016

r <- stack()

for (year in years)
{
  filename <- paste("GWRwSPEC_", type, '_NA_',
                    year, '01_', year, '12.nc', sep="")
  
  r <- stack(r, raster(file.path(data_dir, type, filename), varname = type))
  print(filename)
}

names(r) <- years



# take a random sample of only 50 points, which we will
# split into treatment and control groups
r.sample <- as.data.frame(sampleRandom(r, size = 50,
                                       xy=TRUE, na.rm=TRUE))

# add indices
r.sample <- cbind(id = 1:nrow(r.sample), r.sample)

# reshape dataframe so "Year" and "OM" are single columns
sample <- reshape(r.sample,
        direction = "long",
        idvar = "id",
        v.names = type,
        varying = list(names(r.sample)[4:20]),
        timevar = "Year",
        times = 2000:2016)


# use Synth library to prep data and then perform synthetic control method

# Scenario: we have data from 2000 to 2016. Let's say that
# there was some kind of policy introduced in 2008 that affects
# our treatment group, which we will designate to be a single point.
dataprep.out <- dataprep(sample,
                         predictors = c("x", "y"),
                         time.predictors.prior = 2000:2007,
                         dependent = type,
                         unit.variable = "id",
                         time.variable = "Year",
                         treatment.identifier = 50,
                         controls.identifier = 1:49,
                         time.optimize.ssr = 2000:2007,
                         time.plot = 2000:2016)

# the dataprep.out object has four attributes:
#   X1: treated predictor data before treatment
#   X0: control predictor data before treatment
#   Z1: treated response before treatment
#   Z0: control response before treatment

# the 'synth()' function takes in these four matrices, or a single
# dataprep object

# (Broyden-Fletcher-Goldfarb-Shanno method for optimization)
synth.out <- synth(dataprep.out, method = "BFGS")


# calculate difference between true point and synthetic control point
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot
                               %*% synth.out$solution.w)
gaps

# default function for summary tables
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)

# tab.pred: table comparing pre-treatment predictor values
#           for treated unit, synthetic control, and all units in sample
#
# tab.v: contains table of V-weights and resp. variable names
# tab.w: contains table of W-weights and resp. variable names
# tab.loss: contains table of V-loss and W-loss
names(synth.tables)

synth.tables$tab.pred
synth.tables$tab.v
synth.tables$tab.w
synth.tables$tab.loss


# plot the changes before and after the treatment 
path.plot(synth.res=synth.out, dataprep.res = dataprep.out, 
          tr.intake = 2008,
          Ylab = paste(type, "in ug/m^3") , Xlab = "Year",
          Legend = c("Treated point", 
                                    "Synthetic treated point"),
          Legend.position = "topright")

# plot difference (gaps)
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          tr.intake = 2008,
          Ylab = paste(type, "in ug/m^3"),
          Xlab = "Year", Ylim = c(-1.5,1.5), Main = NA)


# Bayesian structural time series (BSTS) ----------------------------------
library(bsts)

data_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/Monthly/netcdf"

type <- "OM"
years <- 2000:2008
months <- c(paste0(0, 1:9), 10:12)
names <- c()

r <- stack()

# Load monthly data. 17 years * 12 months = 204 layers in a single raster
# (204 files for a given component)
for (year in years)
{
  for (month in months)
  {
    names <- c(names, paste(year, "-", month, "-01", sep=""))
    # filename <- paste("GWRwSPEC_", type, '_NA_',
    #                   year, month, '_', year, month, '.nc', sep="")
    # 
    # r <- stack(r, raster(file.path(data_dir, type, year, filename),
    #                      varname = type))
    # print(filename)
  }
}

names(r) <- names


# take a random sample of only 10 points
r.sample <- as.data.frame(sampleRandom(r, size = 10,
                                       xy=TRUE, na.rm=TRUE))

# dim(r.sample) = (10, 206)
# 10 points, each with (x,y) and 204 files


# single point to run time series on
point <- r.sample[1,]


# add indices
r.sample <- cbind(id = 1:nrow(r.sample), r.sample)
pointt <- cbind(id = 1:nrow(point), point)

# reshape dataframe so "Year" and "OM" are single columns
pt <-  reshape(point,
                        direction = "long",
                        idvar = "id",
                        v.names = type,
                        varying = list(names(point)[-(1:3)]),
                        timevar = "Year_Month",
                        times = names)

sample <-  reshape(r.sample[-1,],
                  direction = "long",
                  idvar = "id",
                  v.names = type,
                  varying = list(names(r.sample)[-(1:3)]),
                  timevar = "Year_Month",
                  times = names)


model_components <- list()

# possible trend components
?AddAr
?AddAutoAr
?AddLocalLevel
?AddLocalLinearTrend
?AddStudentLocalLinearTrend
?AddGeneralizedLocalLinearTrend

# let's start with a local linear trend because I don't really know
# what's going on with Bayesian stats...
model_components <- AddLocalLinearTrend(model_components,
                                        y = pt$BC)

# possible seasonal components
?AddTrig
?AddSeasonal
?AddMonthlyAnnualCycle
?AddRandomWalkHoliday
?AddRegressionHoliday
?AddHierarchicalRegressionHoliday

# monthly seasonal component
model_components <- AddSeasonal(model_components, y = pt$BC,
                                nseasons = 12)

# component to match actual seasons (spring, summer, fall, winter)
# model_components <- AddSeasonal(model_components, y = sample$BC,
#                                 nseasons = 4)


# Fitting a model
fit.bs <- bsts(pt$BC, model_components, niter = 1000)

burnin <- 500

tibble(
  date = as.Date(sample$Year_Month),
  trend = colMeans(fit.bs$state.contributions[-(1:burnin),"trend",]),
  seasonality = colMeans(fit.bs$state.contributions[-(1:burnin),"seasonal.12.1",])) %>%
  gather("component", "value", trend, seasonality) %>%
  ggplot(aes(x = date, y= value)) + 
  geom_line() + theme_bw() + 
  theme(legend.title = element_blank()) + ylab("") + xlab("") +
  facet_grid(component ~ ., scales="free") + guides(colour=FALSE) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


# predictions
pred.bs <- predict(fit.bs, newdata = pt,
                   burn = burnin, horizon = 100, quantiles = c(0.05,0.95))
plot(pred.bs, ylim = c(-3,3))


fit2 <- bsts(BC ~ , state.specification = model_components, 
             data = sample, niter = 1000)
