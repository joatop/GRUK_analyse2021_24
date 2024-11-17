yyy <- results.natopen.GRUK[['2-sided']]

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

yyy <- yyy %>%
  rowwise() %>%
  mutate(fpi = min(Light1,Light2,Nitrogen1,Nitrogen2,Soil_disturbance1,Soil_disturbance2,na.rm=T)) %>%
  mutate(fpi = na_if(fpi,Inf)) %>%
  mutate(ec = mean(c(fpi,aliens,erosion,shrub), na.rm=T))


yyy$agg.ec <- NA
for (i in 1:nrow(yyy) ) {
  
  dat <- as.vector(unlist(yyy[i,c("fpi","aliens","erosion","shrub")]))
  
  yyy[i,"agg.ec"] <- expit(summary(betareg(dat~1))$coefficients$mu[1])
  
  
}


betareg(as.vector(unlist(dat))~1)


library(statmod)

agg <- yyy %>%
  rowwise() %>%
  mutate(model = map(data, ~betareg(c(fpi,aliens,erosion,shrub)~1, data = .x))) %>%
  mutate(tidy = map(model, tidy),
         glance = map(model, glance),
         augment = map(model, augment),
         intercept = tidy %>% map_dbl(~ filter(.x, term == "(Intercept)") %>% pull(estimate)) ) %>%
  dplyr::select(ID, intercept)



# from ØT fjell
# calculate linear regressions for each pixel
# calculate linear regressions for each pixel
ndviTrends <- ndviTS  %>% 
  group_by(ID) %>% 
  nest()%>% 
  mutate(model = map(data, ~lm(ndvi ~ year, data = .x))) %>%
  mutate(tidy = map(model, tidy),
         glance = map(model, glance),
         augment = map(model, augment),
         rsq = glance %>% map_dbl('r.squared'),
         pvalue = glance %>% map_dbl('p.value'),
         intercept = tidy %>% map_dbl(~ filter(.x, term == "(Intercept)") %>% pull(estimate)),
         slope = tidy %>% map_dbl(~ filter(.x, term == "year") %>% pull(estimate))) %>%
  dplyr::select(ID, intercept, slope, rsq, pvalue)





yyy %>%
  rowwise() %>%
  mod = betareg(c(fpi,aliens,erosion,shrub)~1) %>%
  mutate(ec2 = summary(mod)$coefficients$mean[1])

yyy %>%
  rowwise() %>%
  predict(betareg(as.vector(c(fpi,aliens,erosion,shrub)~1)))[1]

expit(summary(betareg(c(0.6,0.9,0.3)~1))$coefficients$mean[1])

predict(betareg(c(0.6,0.9,0.3)~1))
