set.seed(1234)
n <- 250 # number of subjects
K <- 8 # number of measurements per subject
t_max <- 15 # maximum follow-up time

# we constuct a data frame with the design: 
# everyone has a baseline measurment, and then measurements at random follow-up times
DF <- data.frame(id = rep(seq_len(n), each = K),
                 time = c(replicate(n, c(0, sort(runif(K - 1, 0, t_max))))),
                 cat = rep(gl(4, n/4, labels = c("none", "mild", "moderate", "severe")), each = K), 
                 sex = rep(gl(2, n/2, labels = c("female", "male")), each = K))

# design matrices for the fixed and random effects
X <- model.matrix(~ cat + cat:time + sex + time, data = DF)
Z <- model.matrix(~ time, data = DF)

betas <- c(-2.13, 0.60, 2.84, 4.54, -1.26, 0.24, 0.05, 0.24, 0.467) # fixed effects coefficients
D11 <- 0.48 # variance of random intercepts
D22 <- 0.1 # variance of random slopes

# we simulate random effects
b <- cbind(rnorm(n, sd = sqrt(D11)), rnorm(n, sd = sqrt(D22)))
# linear predictor
eta_y <- drop(X %*% betas + rowSums(Z * b[DF$id, ]))
# we simulate binary longitudinal data
DF$y <- rbinom(n * K, 1, plogis(eta_y))

newdat <- expand.grid('time' = seq(min(DF$time), max(DF$time), 0.01), 
                      'cat' = levels(DF$cat), 
                      'sex' = levels(DF$sex))

fit <- mixed_model(fixed = y ~ time*cat, 
                   random = ~ time | id, 
                   data = DF, 
                   family = binomial)
expit <- function (x) exp(x) / (1 + exp(x))
preds <- GLMMadaptive::effectPlotData(fit, newdat, marginal = TRUE)

labs <- c("Female", "Male")
names(labs) <- c("Female", "Male")

labeller(preds$sex = labs)
preds$sex <- factor(preds$sex, levels = c("female", "male"), labels = c("Female", "Male"))

lab <- function (x) {
  paste0(x)
}

ggplot(preds) + 
  geom_line(aes(x = time, y = expit(pred), color = cat), size = 1) + 
  geom_ribbon(aes(x = time, ymin = expit(low), ymax = expit(upp), fill = cat), alpha = 0.4) + 
  facet_grid(~ sex) + 
  ylab("Marginal Probabilities") + 
  xlab("Follow-up Time") + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1)) + 
  scale_color_manual(values = c("none" = "#454545", 
                                "mild" = "#3C5DC9", 
                                "moderate" = "#C234A8", 
                                "severe" = "#C21033")) + 
  scale_fill_manual(values = c("none" = "#454545", 
                                "mild" = "#3C5DC9", 
                                "moderate" = "#C234A8", 
                                "severe" = "#C21033")) + 
  theme(panel.spacing = unit(0.1, "lines"), panel.grid = element_blank(), 
        panel.background = element_rect(fill = "#EBEBEB"), 
        legend.title = element_blank(), 
        strip.background = element_rect(fill ="#545454"), 
        strip.text = element_text(face = 'bold', size = 12, color = '#FFFFFF'), 
        axis.title.x = element_text(face = 'bold', size = 14, color = '#343434'), 
        axis.title.y = element_text(face = 'bold', size = 14, color = '#343434'), 
        legend.position = 'top')
ggsave(filename = 'D:\\gpapageorgiou\\Github_Repos\\CE08\\Images_URLs\\cover.png')
        