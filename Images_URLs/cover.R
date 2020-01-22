library(JMbayes)
library(splines)

effectPlotData <- function (object, newdata, orig_data, ...) {
  if (inherits(object, "MixMod")) {
    return(GLMMadaptive::effectPlotData(object, newdata, ...))
  }
  form <- formula(object)
  namesVars <- all.vars(form)
  betas <- if (!inherits(object, "lme")) coef(object) else fixef(object)
  V <- if (inherits(object, "geeglm")) object$geese$vbeta else vcov(object)
  orig_data <- orig_data[complete.cases(orig_data[namesVars]), ]
  Terms <- delete.response(terms(form))
  mfX <- model.frame(Terms, data = orig_data)
  Terms_new <- attr(mfX, "terms")
  mfX_new <- model.frame(Terms_new, newdata, xlev = .getXlevels(Terms, mfX))
  X <- model.matrix(Terms_new, mfX_new)
  pred <- c(X %*% betas)
  ses <- sqrt(diag(X %*% V %*% t(X)))
  newdata$pred <- pred
  newdata$low <- pred - 1.96 * ses
  newdata$upp <- pred + 1.96 * ses
  newdata
}

lmeFit.pbc <- lme(log(serBilir) ~ ns(year, 2)*sex,
                  random = list(id = pdDiag(form = ~ ns(year, 2))), data = pbc2)

newdat <- expand.grid('year' = seq(min(pbc2$year), max(pbc2$year), 0.1), 
                     'sex' = levels(pbc2$sex))

plotdat <- effectPlotData(lmeFit.pbc, newdat, pbc2)

plotdat$color <- ifelse(plotdat$sex == 'female', "#343434", "#FF003A")

pbc2_first <- pbc2[pbc2$year < 3.8, ]
pbc2_first$color <- ifelse(pbc2_first$sex == 'female', "#343434", "#FF003A")
pbc2_second <- pbc2[pbc2$year > 2 & pbc2$year < 6, ]
max(pbc2_first$year)
pbc2_second$color <- ifelse(pbc2_second$sex == 'female', "#343434", "#FF003A")
plotdat_third <- plotdat[plotdat$year > 5 & plotdat$year < 10, ]

ggplot() + 
  geom_point(aes(x = pbc2_first$year, y = log(pbc2_first$serBilir), alpha = (1 - pbc2_first$year) / max(pbc2_first$year)), 
             size = 2, color = '#343434', show.legend = FALSE) + 
  geom_line(aes(x = pbc2_second$year, y = log(pbc2_second$serBilir), group = pbc2_second$id, 
                alpha = 1 - abs(pbc2_second$year - mean(pbc2_second$year) / sd(pbc2_second$year))), 
            color = pbc2_second$color, size = 1, show.legend = FALSE) + 
  geom_line(aes(x = plotdat_third$year, y = plotdat_third$pred, group = plotdat_third$sex, 
                alpha = ((plotdat_third$year) / max(plotdat_third$year))), 
            color = plotdat_third$color, size = 2, show.legend = FALSE) + 
  geom_ribbon(aes(x = plotdat_third$year, ymin = plotdat_third$low, ymax = plotdat_third$upp, group = plotdat_third$sex), 
              fill = plotdat_third$color, alpha = 0.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = '#EBEBEB'), 
        axis.ticks = element_blank(), axis.text = element_blank())


#prothro$pro <- log(prothro$pro)

lmeFit.proth <- lme(pro ~ ns(time, knots = c(1, 3, 5))*treat,
                  random = list(id = pdDiag(form = ~ ns(time, knots = c(1, 3, 5)))), data = prothro)

newdat.proth <- expand.grid('time' = seq(min(prothro$time), max(prothro$time), 0.01), 
                      'treat' = levels(prothro$treat))

plotdat.proth <- effectPlotData(lmeFit.proth, newdat.proth, prothro)

plotdat.proth$color <- ifelse(plotdat.proth$treat == 'placebo', "#22306A", "#AD0F25")
prothro_first <- prothro[prothro$time < 2, ]
prothro_first$color <- ifelse(prothro_first$treat == 'placebo', "#22306A", "#AD0F25")
prothro_second <- prothro[prothro$time > 0.75 & prothro$time < 3, ]
prothro_second$color <- ifelse(prothro_second$treat == 'placebo', "#22306A", "#AD0F25")
plotdat.proth_third <- plotdat.proth[plotdat.proth$time > 2.75 & plotdat.proth$time < 6, ]

alpha_scale <- function(x, pow = 3) {
  tmp <- 1 - (abs((x - mean(x)) / 
             sd(x)) / 
         max(x))
  tmp^pow
}

ggplot() + 
  geom_point(aes(x = prothro_first$time, y = prothro_first$pro, alpha = (1 - abs(prothro_first$time / max(prothro_first$time)))^(2)), 
             size = 4, color = prothro_first$color, show.legend = FALSE, shape = 19) + 
  geom_line(aes(x = prothro_second$time, y = prothro_second$pro, group = prothro_second$id, 
                alpha = alpha_scale(prothro_second$time)), 
            color = prothro_second$color, size = 1, show.legend = FALSE) + 
  geom_line(aes(x = plotdat.proth_third$time, y = plotdat.proth_third$pred, group = plotdat.proth_third$treat, 
                alpha = alpha_scale(plotdat.proth_third$time)), 
            color = plotdat.proth_third$color, size = 1, show.legend = FALSE) + 
  geom_ribbon(aes(x = plotdat.proth_third$time, ymin = plotdat.proth_third$low, ymax = plotdat.proth_third$upp, group = plotdat.proth_third$treat), 
              fill = plotdat.proth_third$color, alpha = 0.2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = '#F3F3F3'), 
        axis.ticks = element_blank(), axis.text = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())

?ggsave
ggsave(filename = 'D:\\gpapageorgiou\\Github_Repos\\CE08\\Images_URLs\\cover.png')
