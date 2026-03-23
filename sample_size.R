library(gsDesign)

# correct version
placebo_hazard <- 0.75/100/6   # monthly control hazard
ss <- nSurv(
  lambdaC = placebo_hazard,
  hr = 0.4,
  hr0 = 0.7,
  eta = 0.02/12,
  T = 24, # study duration 24 months
  R = 3, # accrual duration 3 months
  alpha = 0.05,
  beta = 0.20,
  sided = 2
)
ss

# conversion from cumulative incidence to hazard
#ss0 <- nSurvival(
#  lambda1 = -log(1 - 0.0075) / 0.5,   # placebo annual hazard
#  lambda2 = -log(1 - 0.0030) / 0.5,   # vaccine annual hazard
#  eta = -log(0.98),               # annual dropout hazard
#  Ts = 2,
#  Tr = 3/12,
#  sided = 2,
#  alpha = 0.05,
#  beta = 0.10
#)
#ss0

