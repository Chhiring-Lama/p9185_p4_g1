library(readxl)
library(survival)
library(survminer)
# read data
setwd("~/Desktop/CU/BIS9185/BISTP9185-Proj4")
dat <- read_excel("Q2c.xlsx",  col_types = c("numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric",  # needed to fix infection_time loading as TRUE/FALSE
                                             "numeric"))

# total number of infections
sum(dat$Infection)
# Delayed-entry Kaplan-Meier fit
dat <- dat %>%
  mutate(
    # Raw event/censoring time before applying 365-day cutoff
    raw_time  = LastFUTime,
    
    # Censor at 365 days (12 months after 2nd shot)
    time_to_event = ifelse(dat$Infection == 1, dat$InfectionTime, dat$LastFUTime),
    event         = ifelse(Infection == 1 & LastFUTime <= 365, 1, 0)
  )


fit <- survfit(Surv(time_to_event, Infection) ~ 1, data = dat) 


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
nrow(dat[dat$Infection==1 & dat$time_to_event <= 365,])

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

#parametric model estimation
fit_gengamma <- flexsurvreg(
  Surv(time_to_event, Infection) ~ 1,
  data = dat,
  dist = "gengamma"
)
med_gengamma <- summary(fit_gengamma, type = "quantile", quantiles = 0.5)[[1]]
#probably divergent 
#mean_gengamma <- summary(fit_gengamma, type = "mean")[[1]]



tgrid <- seq(0, 365, by = 1)
gengamma_surv <- summary(
  fit_gengamma,
  type = "survival",
  t = tgrid
)[[1]]

gengamma_df <- data.frame(
  time = gengamma_surv$time,
  surv = gengamma_surv$est
)

# Overlay generalized gamma on KM plot
p$plot <- p$plot +
  geom_line(
    data = gengamma_df,
    aes(x = time, y = surv),
    linetype = 2,
    linewidth = 1
  ) +
  annotate("text", x = 300, y = 0.2, label = "Dashed: Gen. gamma")

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
