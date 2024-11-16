yyy <- results.natopen.GRUK[['2-sided']]

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

yyy <- yyy %>%
  rowwise() %>%
  mutate(fpi = min(Light1,Light2,Nitrogen1,Nitrogen2,Soil_disturbance1,Soil_disturbance2,na.rm=T)) %>%
  mutate(fpi = na_if(fpi,Inf)) %>%
  mutate(ec = mean(c(fpi,aliens,erosion,shrub), na.rm=T))


yyy %>%
  rowwise() %>%
  mod = betareg(c(fpi,aliens,erosion,shrub)~1) %>%
  mutate(ec2 = summary(mod)$coefficients$mean[1])

yyy %>%
  rowwise() %>%
  predict(betareg(as.vector(c(fpi,aliens,erosion,shrub)~1)))[1]

expit(summary(betareg(c(0.6,0.9,0.3)~1))$coefficients$mean[1])

predict(betareg(c(0.6,0.9,0.3)~1))
