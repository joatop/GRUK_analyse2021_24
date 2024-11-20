yyy <- results.natopen.GRUK[['2-sided']]

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

yyy <- yyy %>%
  rowwise() %>%
  mutate(fpi = min(Light1,Light2,Nitrogen1,Nitrogen2,Soil_disturbance1,Soil_disturbance2,na.rm=T)) %>%
  mutate(fpi = na_if(fpi,Inf)) %>%
  mutate(ec.mean_agg = mean(c(fpi,aliens,erosion,shrub), na.rm=T))


yyy$ec.beta_agg <- NA
for (i in 1:nrow(yyy) ) {
  
  dat <- as.vector(unlist(yyy[i,c("fpi","aliens","erosion","shrub")]))
  dat[!is.na(dat) & dat==0] <- rnorm( length(dat[!is.na(dat) & dat==0]) ,0.0001,0.000001)
  dat[!is.na(dat) & dat==1] <- rnorm( length(dat[!is.na(dat) & dat==1]) ,0.9999,0.000001)
  yyy[i,"ec.beta_agg"] <- expit(summary(betareg(dat~1))$coefficients$mean[1])

}





names(res.natopen.GRUK_wide)


res.natopen.GRUK_wide %>%
  ggplot(aes(x=factor(Tilstand,levels=c("Dårlig","Moderat","God")),y=ec.beta_agg, fill=Tilstand)) +
  geom_hline(yintercept=0.6, linetype="dashed") + 
  geom_violin(color=NA)+
  geom_point(size=1, shape=16, color="black", position = position_jitter(seed = 1, width = 0.05)) + 
  xlab("Tilstandsevaluering Mdir instruks") + 
  ylab("Aggregert tilstandsindex") + 
  theme(legend.position="none")
