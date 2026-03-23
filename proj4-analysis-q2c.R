library(readxl)
library(survival)
library(survminer)
# read data
setwd("~/Desktop/CU/BIS9185/BISTP9185-Proj4")
dat <- read_excel("Q2c.xlsx")

# total number of infections
sum(dat$Infection)
# Delayed-entry Kaplan-Meier fit
fit <- survfit(Surv(time = EnrollmentTime,
                    time2 = LastFUTime,
                    event = Infection) ~ 1,
               data = dat)

# 1) KM plot
ggsurvplot(
  fit,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Days since second shot",
  ylab = "Probability of remaining infection-free",
  title = "Kaplan–Meier Curve",
  surv.scale = "percent"
)


# 2) Probability of infection within 12 months
# number of infections at day 365
nrow(dat[dat$Infection==1 & dat$LastFUTime<=365,])

# Summary at day 365
s365 <- summary(fit, times = 365, extend = TRUE)

surv_365 <- s365$surv
lower_surv_365 <- s365$lower
upper_surv_365 <- s365$upper

# Probability of infection by day 365
risk_365 <- 1 - surv_365
lower_risk_365 <- 1 - upper_surv_365
upper_risk_365 <- 1 - lower_surv_365

cat("12-month infection probability:\n")
cat(sprintf("%.4f (95%% CI %.4f to %.4f)\n\n",
            risk_365, lower_risk_365, upper_risk_365))






# 3) Median time to infection
p <- ggsurvplot(
  fit,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Days since second shot",
  ylab = "Probability of remaining infection-free",
  title = "Kaplan–Meier Curve",
  surv.scale = "percent"
)

p$plot <- p$plot + ggplot2::geom_hline(yintercept = 0.5, linetype = 2)

p

# 4) Mean time to infection
p <- ggsurvplot(
  fit,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Days since second shot",
  ylab = "Probability of remaining infection-free",
  title = "Kaplan–Meier Curve",
  surv.scale = "percent"
)

p$plot <- p$plot +
  geom_area(
    data = p$plot$data,
    aes(x = time, y = surv),
    stat = "identity",
    alpha = 0.2
  )

p


summary_fit <- summary(fit, rmean = 365)
summary_fit$table


