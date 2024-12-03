## aggregating the circle ec-indices for polygons
ec_polygon <- res.natopen.GRUK_wide %>%
  group_by(PolygonID) %>%
  group_modify(.f = ~ broom::tidy(betareg(ec.vs ~ 1, data = .x)))

# did not work to add this
#    %>%
    ## expit of the intercept coefficient to get the mean for the polygon
#  mutate(mu = expit(estimate) %>%
  ## clean up
#  filter(term != "(phi)") %>%
#  select(PolygonID, mu)
  
## calculating an overall ec.index for GRUK
res.natopen.GRUK_wide <- res.natopen.GRUK_wide %>%
  mutate(ec.vs2 = ifelse(ec.vs < 0.001, 0.001, ec.vs) ) %>%
  mutate(ec.vs2 = ifelse(ec.vs2 > 0.999, 0.999, ec.vs2) )

ec.total.vs <- glmmTMB(ec.vs2 ~ 1 +(1|LokalitetID/PolygonID), family=beta_family(), data=res.natopen.GRUK_wide)
summary(ec.total.vs)
expit(summary(ec.total.vs)$coefficients$cond[1])

ec.omr.vs <- glmmTMB(ec.vs2 ~ 0 + omr +(1|LokalitetID/PolygonID), family=beta_family(), data=res.natopen.GRUK_wide)
summary(ec.omr.vs)
expit(summary(ec.omr.vs)$coefficients$cond[,1])

# with mean circle aggregation for comparison
ec.total.mu <- glmmTMB(ec.beta_agg ~ 1 +(1|LokalitetID/PolygonID), family=beta_family(), data=res.natopen.GRUK_wide)
summary(ec.total.mu)
expit(summary(ec.total.mu)$coefficients$cond[1])
  

  

#### plotting ec.vs for each GRUK-region (indre, midtre og ytre)
# creating spatial objects for the bounding boxes
bokses <- data.frame(lon=c(241100,241100,270300,270300,
                           225000,225000,270300,270300,
                           184100,184100,214800,214800),
                     lat=c(6628000,6649700,6649700,6628000,
                           6590000,6621000,6621000,6590000,
                           6545000,6568700,6568700,6545000),
                     omr=c(rep('indre',4),rep('midtre',4),rep('ytre',4))
)

bokses <- bokses %>%
  st_as_sf(coords = c("lon", "lat"), remove=F, crs = 25833) %>%
  group_by(omr) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# making the wide results object into a spatial object
res.natopen.GRUK_wide <- st_as_sf(res.natopen.GRUK_wide, coords = c("UTM33_E","UTM33_N"), remove=F, crs = 25833)
# adding region information
res.natopen.GRUK_wide <-  st_join(res.natopen.GRUK_wide, bokses)
res.natopen.GRUK_wide <-  st_drop_geometry(res.natopen.GRUK_wide)


#### plot ec.vs against område
res.natopen.GRUK_wide %>%
  ggplot(aes(x=omr,y=ec.vs,fill=omr)) + 
  geom_hline(yintercept=0.6, linetype="dashed") +
  geom_violin()+
  geom_point(size=1, shape=16, color="black", position = position_jitter(seed = 1, width = 0.05)) + 
  scale_x_discrete(labels=c('Indre Oslofjord', 'Midtre Oslofjord', 'Ytre Oslofjord') ) +
  xlab("GRUK område") + 
  ylab("Aggregert tilstandsindex (per sirkel)") + 
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )

#### Indikatorer på Mdir-variabler
res.natopen.GRUK %>%
  filter(ec.index %in% c("aliens","erosion","shrub")) %>%
  ggplot(aes(x=ec.index,y=scaled_value, fill=ec.index)) +
  geom_hline(yintercept=0.6, linetype="dashed") + 
  geom_violin(color=NA)+
  geom_point(size=1, shape=16, color="black", position = position_jitter(seed = 1, width = 0.05)) +
  scale_x_discrete(labels=c('Fremmede arter', 'Slitasje', 'Busker') ) +
  xlab("Areal uten dekning av...") + 
  ylab("Scalert indikator verdi") + 
  theme(legend.position="none", 
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold"))

#### Indikatorer på funksjonelle planteindikatorer
res.natopen.GRUK %>%
  filter(ec.index %in% c("CC1","SS1","RR1","Light1","Nitrogen1","Soil_disturbance1",
                         "CC2","SS2","RR2","Light2","Nitrogen2","Soil_disturbance2")) %>%
  mutate(dir= recode(ec.index,CC1="lower",SS1="lower",RR1="lower",Light1="lower",Nitrogen1="lower",Soil_disturbance1="lower",
                     CC2="upper",SS2="upper",RR2="upper",Light2="upper",Nitrogen2="upper",Soil_disturbance2="upper")) %>%
  mutate(ec.index= recode(ec.index,CC1="CSR-C (lower)",SS1="CSR-S (lower)",RR1="CSR-R (lower)",Light1="Light (lower)",Nitrogen1="Nitrogen (lower)",Soil_disturbance1="Soil disturbance (lower)",
                          CC2="CSR-C (upper)",SS2="CSR-S (upper)",RR2="CSR-R (upper)",Light2="Light (upper)",Nitrogen2="Nitrogen (upper)",Soil_disturbance2="Soil disturbance (upper)")) %>%
  mutate(dir = factor(dir,levels=c("upper","lower")) ) %>%
  mutate(ec.index = factor(ec.index,levels=c("CSR-C (lower)","CSR-S (lower)","CSR-R (lower)","Light (lower)","Nitrogen (lower)","Soil disturbance (lower)",
                                             "CSR-C (upper)","CSR-S (upper)","CSR-R (upper)","Light (upper)","Nitrogen (upper)","Soil disturbance (upper)")) ) %>%
  ggplot(aes(x=ec.index, y=scaled_value, fill=ec.index)) + 
  geom_hline(yintercept=0.6, linetype="dashed") + 
  geom_violin(color = NA) +
  #  geom_boxplot(width=0.2, color="grey") +
  geom_point(size=1, shape=16, color="black", position = position_jitter(seed = 1, width = 0.05)) +
  facet_wrap(~dir, nrow = 2, , scale="free", labeller = labeller(dir = c("upper"="Øvre indikator", "lower"="Nedre indikator"))) + 
  scale_x_discrete(labels=c('CSR-C', 'CSR-S', 'CSR-R', 'Lys', 'Nitrogen', 'Forstyrrelse',
                            'CSR-C', 'CSR-S', 'CSR-R', 'Lys', 'Nitrogen', 'Forstyrrelse') ) +
  xlab("") + 
  ylab("Skalert indikator verdi") + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.2, size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size = 20)
        )


#### Aggregated condition index
res.natopen.GRUK_wide %>%
  ggplot(aes(x=factor(Tilstand,levels=c("Dårlig","Moderat","God")),y=ec.vs, fill=Tilstand)) +
  geom_hline(yintercept=0.6, linetype="dashed") + 
  geom_violin(color=NA)+
  geom_point(size=1, shape=16, color="black", position = position_jitter(seed = 1, width = 0.05)) + 
  xlab("Tilstandsevaluering Mdir instruks (M-2209 )") + 
  ylab("Aggregert tilstandsindex") + 
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
        )



#### Plotting relevant scatterplots and models

## Light1 vs. Dekning__busker_busksjikt
# basic scatterplot
p_L1_busk <- res.natopen.GRUK %>%
  filter(ec.index=="Light1") %>%
  ggplot(aes(x=Dekning_busker_busksjikt, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "Lys (nedre)", x = "Busksjiktsdekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
        )

# model and model prediction
mod <- glmmTMB(scaled_value2 ~ Dekning_busker_busksjikt +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="Light1",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(Dekning_busker_busksjikt=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(Dekning_busker_busksjikt=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_L1_busk <- p_L1_busk +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  #  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],6)), size=6, col="firebrick" ) +
  annotate("text",x=90, y=0.1, label=paste("p < 0.01"), size=6, col="firebrick" )


## Light2 vs. erosjon_prosent
# basic scatterplot
p_L2_slit <- res.natopen.GRUK %>%
  filter(ec.index=="Light2") %>%
  ggplot(aes(x=erosjon_prosent, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "Lys (øvre)", x = "Slitasjedekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )

# model and model prediction
mod <- glmmTMB(scaled_value2 ~ erosjon_prosent +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="Light2",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(erosjon_prosent=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(erosjon_prosent=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_L2_slit <- p_L2_slit +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  #  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],6)), size=6, col="firebrick" ) +
  annotate("text",x=90, y=0.1, label=paste("p < 0.01"), size=6, col="firebrick" )

## Nitrogen2 vs. Totaldekning_fremmedearter
# basic scatterplot
p_N2_frem <- res.natopen.GRUK %>%
  filter(ec.index=="Nitrogen2") %>%
  ggplot(aes(x=Totaldekning_fremmedearter, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "Nitrogen (øvre)", x = "Fremmedartsdekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )

# model and model prediction
mod <- glmmTMB(scaled_value2 ~ Totaldekning_fremmedearter +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="Nitrogen2",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_N2_frem <- p_N2_frem +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  #  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],6)), size=6, col="firebrick" ) +
  annotate("text",x=90, y=0.1, label=paste("p < 0.01"), size=6, col="firebrick" )

## Soil_disturbance1 vs. Dekning_busker_busksjikt
# basic scatterplot
p_Sd1_busk <- res.natopen.GRUK %>%
  filter(ec.index=="Soil_disturbance1") %>%
  ggplot(aes(x=Dekning_busker_busksjikt, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "Forstyrrelse (nedre)", x = "Busksjiktsdekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )

# model and model prediction
mod <- glmmTMB(scaled_value2 ~ Dekning_busker_busksjikt +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="Soil_disturbance1",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(Dekning_busker_busksjikt=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(Dekning_busker_busksjikt=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_Sd1_busk <- p_Sd1_busk +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  #  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],6)), size=6, col="firebrick" ) +
  annotate("text",x=90, y=0.1, label=paste("p < 0.01"), size=6, col="firebrick" )

## Soil_disturbance2 vs. Dekning_fremmedearter
# basic scatterplot
p_Sd2_frem <- res.natopen.GRUK %>%
  filter(ec.index=="Soil_disturbance2") %>%
  ggplot(aes(x=Totaldekning_fremmedearter, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "Forstyrrelse (øvre)", x = "Fremmedartsdekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )

# model and model prediction
mod <- glmmTMB(scaled_value2 ~ Totaldekning_fremmedearter +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="Soil_disturbance2",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_Sd2_frem <- p_Sd2_frem +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  #  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],6)), size=6, col="firebrick" ) +
  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],2)), size=6, col="firebrick" )




## CC2 vs. Totaldekning_fremmedearter
# basic scatterplot
p_CC2_frem <- res.natopen.GRUK %>%
  filter(ec.index=="CC2") %>%
  ggplot(aes(x=Totaldekning_fremmedearter, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "CSR-C (øvre)", x = "Fremmedartsdekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )
# model and model prediction
mod <- glmmTMB(scaled_value2 ~ Totaldekning_fremmedearter +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="CC2",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_CC2_frem <- p_CC2_frem +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],2)), size=6, col="firebrick" )


## RR1 vs. Dekning_busker_busksjikt
# basic scatterplot
p_RR1_busk <- res.natopen.GRUK %>%
  filter(ec.index=="CC2") %>%
  ggplot(aes(x=Dekning_busker_busksjikt, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "CSR-R (nedre)", x = "Busksjiktsdekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )
# model and model prediction
mod <- glmmTMB(scaled_value2 ~ Dekning_busker_busksjikt +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="RR2",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(Dekning_busker_busksjikt=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(Dekning_busker_busksjikt=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_RR1_busk <- p_RR1_busk +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],2)), size=6, col="firebrick" )




## RR2 vs. Totaldekning_fremmedearter
# basic scatterplot
p_RR2_frem <- res.natopen.GRUK %>%
  filter(ec.index=="CC2") %>%
  ggplot(aes(x=Totaldekning_fremmedearter, y=scaled_value)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 100),ylim = c(0, 1)) +
  labs(y = "CSR-R (øvre)", x = "Fremmedartsdekning (%)") +
  theme(legend.position="none",
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title=element_text(size=14,face="bold")
  )
# model and model prediction
mod <- glmmTMB(scaled_value2 ~ Totaldekning_fremmedearter +(1|LokalitetID), family=beta_family(), data=res.natopen.GRUK[res.natopen.GRUK$ec.index=="RR2",])
pred <- data.frame(
  mu = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA),
  se = predict(mod,newdata=data.frame(Totaldekning_fremmedearter=0:100),type='link',re.form=NA,se.fit=T)$se.fit
)
pred <- pred %>%
  mutate(ci.lo = mu-1.96*se,
         ci.hi = mu+1.96*se)

# expit function for backtransforming from logit-link
expit <- function(L) exp(L) / (1+exp(L))

# add model line and 95%-CI to scatterplot
p_RR2_frem <- p_RR2_frem +
  geom_line(data=data.frame(x=0:100,y=expit(pred$mu)), aes(x,y),col="firebrick",linewidth=2) +
  geom_ribbon(data = pred, aes(x= 0:100, y = mu, ymin = expit(ci.lo), ymax = expit(ci.hi)), alpha = 0.3, fill = "firebrick") + 
  annotate("text",x=90, y=0.1, label=paste("p = ",round(summary(mod)$coefficients$cond[2,4],2)), size=6, col="firebrick" )




ggarrange(p_L1_busk, p_L2_slit, p_Sd1_busk, p_Sd2_frem, p_CC2_frem, p_N2_frem, p_RR1_busk, p_RR2_frem + rremove("x.text"), 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 2, nrow = 4)

ggarrange(p_N2_frem  + rremove("x.text"), 
          p_Sd1_busk + rremove("x.text") + rremove("y.text"), 
          p_L2_slit, 
          p_L1_busk + rremove("y.text"), 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
