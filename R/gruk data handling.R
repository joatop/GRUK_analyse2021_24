

### Regions - maps and statistics
Alternatively we can calculate and show the region-wise means and their related standard errors. But note that calculating a simple mean would be inappropriate for these data. This is because:
  (i) the scaled data are bound between 0 and 1, and thus follow a beta-distribution rather than a Gaussian one
(ii) both the ANO and the GRUK datasets have a nested structure

Therefore, we need to (i) use a beta-model, that (ii) can account for the nested structure of the data.
Here, we apply the following function using either a glmmTMB null-model with a beta-distribution, logit link, and the nesting as a random intercept, or a simple betareg null-model with logit link if the nesting is not extensive enough for a mixed model.
```{r}

expit <- function(L) exp(L) / (1+exp(L)) # since the beta-models use a logit link, we need to calculate the estimates back to the identity scale

# the function performs a glmmTMB if there's >= 5 random levels in the nesting structure
# if that is not the case, then the function performs a betareg if theres >= 2 observations
# if that is not the case either, then the function returns the value of the single observation with NA standard error

indmean.beta <- function(df) {
  
  st_geometry(df) <- NULL
  colnames(df) <- c("y","ran")
  
  if ( nrow(df[!is.na(df[,1]),]) >= 2 ) {
    
    if ( length(unique(df[!is.na(df[,1]),2])) >=5 ) {
      
      mod1 <- glmmTMB(y ~ 1 +(1|ran), family=beta_family(), data=df)
      
      return(c(
        expit(summary( mod1 )$coefficients$cond[1]),
        
        expit( summary( mod1 )$coefficients$cond[1] + 
                 summary( mod1 )$coefficients$cond[2] )-
          expit( summary( mod1 )$coefficients$cond[1] ),
        
        nrow(df[!is.na(df$y),]),
        summary( mod1 )$coefficients$cond[1],
        summary( mod1 )$coefficients$cond[2]
      ))
      
    } else {
      
      mod2 <- betareg(y ~ 1, data=df)
      
      return(c(
        expit(summary( mod2 )$coefficients$mean[1]),
        expit( summary( mod2 )$coefficients$mean[1] + 
                 summary( mod2 )$coefficients$mean[2] )-
          expit( summary( mod2 )$coefficients$mean[1] ),
        nrow(df[!is.na(df$y),]),
        summary( mod2 )$coefficients$mean[1],
        summary( mod2 )$coefficients$mean[2]
      ))
      
    }
    
  } else {
    
    return(c(df$y,NA,1,NA,NA))
    
  }
  
}

```

```{r}
# we have to join the ANO and GRUK results spatial objects with the Norway and region mask
res.natopen.ANO2 = st_join(res.natopen.ANO2, regnor, left = TRUE)
res.natopen.GRUK2 = st_join(res.natopen.GRUK2, regnor, left = TRUE)
# we check if all there's any site that did not get a region assigned
nrow(res.natopen.ANO2[is.na(res.natopen.ANO2$region),]) # no NA's for ANO
nrow(res.natopen.GRUK2[is.na(res.natopen.GRUK2$region),]) # some points didn't get assigned to a region. Why?

tm_shape(regnor, bbox = boks) +
  tm_fill('GID_0', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_shape(res.natopen.GRUK2[is.na(res.natopen.GRUK2$region),]) +
  tm_dots('Nitrogen2',midpoint=NA, palette=tmaptools::get_brewer_pal("YlOrRd", 7, plot = FALSE), scale=2, legend.show = FALSE) + # 
  tm_layout(main.title = "Nitrogen index (upper), natopen GRUK",legend.position = c("right", "bottom"), main.title.size=1.2) + 
  tm_add_legend(type = "fill", 
                col = c(tmaptools::get_brewer_pal("YlOrRd", 7, plot = FALSE),'grey'),
                labels = c("0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", 
                           "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0", "NA"),
                title = "index values")
# they seem to lie in water
# all sites but the southernmost one are in Eastern Norway, the remaining one in Southern Norway
summary(res.natopen.GRUK2[is.na(res.natopen.GRUK2$region),"y"])
res.natopen.GRUK2[is.na(res.natopen.GRUK2$region) & res.natopen.GRUK2$y<59.83,c("y","PolygonID")] # site 123-4 is in Southern Norway
res.natopen.GRUK2[res.natopen.GRUK2$PolygonID=="123-4","region"] <- "Southern Norway"
# and all the other region=NA observations are in Eastern Norway
res.natopen.GRUK2[is.na(res.natopen.GRUK2$region),"region"] <- "Eastern Norway"
nrow(res.natopen.GRUK2[is.na(res.natopen.GRUK2$region),]) # no NA's left

# now we can calculate regionwise means and standard errors with beta-regression null-models
# note that beta-models cannot handle observations that are exactly 0 or 1
res.natopen.ANO2$RR1[res.natopen.ANO2$RR1==1] <- 0.999
res.natopen.ANO2$RR1[res.natopen.ANO2$RR1==0] <- 0.001
res.natopen.GRUK2$Nitrogen2[res.natopen.GRUK2$Nitrogen2==1] <- 0.999
res.natopen.GRUK2$Nitrogen2[res.natopen.GRUK2$Nitrogen2==0] <- 0.001

regnor <- regnor %>%
  mutate(
    RR1.ANO.reg.mean = c(indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Northern Norway",c("RR1","ano_PolygonID")])[1],
                         indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Central Norway",c("RR1","ano_PolygonID")])[1],
                         indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Eastern Norway",c("RR1","ano_PolygonID")])[1],
                         indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Western Norway",c("RR1","ano_PolygonID")])[1],
                         indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Southern Norway",c("RR1","ano_PolygonID")])[1]
    ),
    RR1.ANO.reg.se = c(indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Northern Norway",c("RR1","ano_PolygonID")])[2]*2,
                       indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Central Norway",c("RR1","ano_PolygonID")])[2]*2,
                       indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Eastern Norway",c("RR1","ano_PolygonID")])[2]*2,
                       indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Western Norway",c("RR1","ano_PolygonID")])[2]*2,
                       indmean.beta(df=res.natopen.ANO2[res.natopen.ANO2$region=="Southern Norway",c("RR1","ano_PolygonID")])[2]*2
    ),
    RR1.ANO.reg.n = c(nrow(res.natopen.ANO2[res.natopen.ANO2$region=="Northern Norway" & !is.na(res.natopen.ANO2$RR1),]),
                      nrow(res.natopen.ANO2[res.natopen.ANO2$region=="Central Norway" & !is.na(res.natopen.ANO2$RR1),]),
                      nrow(res.natopen.ANO2[res.natopen.ANO2$region=="Eastern Norway" & !is.na(res.natopen.ANO2$RR1),]),
                      nrow(res.natopen.ANO2[res.natopen.ANO2$region=="Western Norway" & !is.na(res.natopen.ANO2$RR1),]),
                      nrow(res.natopen.ANO2[res.natopen.ANO2$region=="Southern Norway" & !is.na(res.natopen.ANO2$RR1),])
    ),
    Nitrogen2.GRUK.reg.mean = c(indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Northern Norway",c("Nitrogen2","PolygonID")])[1],
                                indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Central Norway",c("Nitrogen2","PolygonID")])[1],
                                indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Eastern Norway",c("Nitrogen2","PolygonID")])[1],
                                indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Western Norway",c("Nitrogen2","PolygonID")])[1],
                                indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Southern Norway",c("Nitrogen2","PolygonID")])[1]
    ),
    Nitrogen2.GRUK.reg.se = c(indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Northern Norway",c("Nitrogen2","PolygonID")])[2]*2,
                              indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Central Norway",c("Nitrogen2","PolygonID")])[2]*2,
                              indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Eastern Norway",c("Nitrogen2","PolygonID")])[2]*2,
                              indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Western Norway",c("Nitrogen2","PolygonID")])[2]*2,
                              indmean.beta(df=res.natopen.GRUK2[res.natopen.GRUK2$region=="Southern Norway",c("Nitrogen2","PolygonID")])[2]*2
    ),
    Nitrogen2.GRUK.reg.n = c(0,
                             0,
                             nrow(res.natopen.GRUK2[res.natopen.GRUK2$region=="Eastern Norway" & !is.na(res.natopen.GRUK2$Nitrogen2),]),
                             0,
                             nrow(res.natopen.GRUK2[res.natopen.GRUK2$region=="Southern Norway" & !is.na(res.natopen.GRUK2$Nitrogen2),])
    )
  )


## scaled value maps for CSR-R1 (lower indicator), ANO
# mean
tm_shape(regnor) +
  tm_polygons(col="RR1.ANO.reg.mean", title="CSR-R (lower), mean", style="quantile", palette=rev(get_brewer_pal(palette="OrRd", n=5, plot=FALSE))) +
  tm_text("RR1.ANO.reg.n",col="black",bg.color="grey")
```
Mean index value by region for the lower CSR-R indicator (i.e. index shows deviations towards less ruderal species) from the ANO monitoring data. Numbers in grey fields show the number of observations in the respective region.

```{r}
# se
tm_shape(regnor) +
  tm_polygons(col="RR1.ANO.reg.se", title="CSR-R (lower)", style="quantile", palette=(get_brewer_pal(palette="OrRd", n=5, plot=FALSE))) +
  tm_text("RR1.ANO.reg.n",col="black",bg.color="grey")

```
Standard error to the mean index value by region for the lower CSR-R indicator from the ANO monitoring data. Numbers in grey fields show the number of observations in the respective region.

And here are the corresponding maps for the upper Nitrogen indicator in the GRUK data:
  ```{r}
## scaled value maps for Nitrogen2 (upper indicator), ASO
# mean
tm_shape(regnor) +
  tm_polygons(col="Nitrogen2.GRUK.reg.mean", title="Nitrogen (upper), mean", style="quantile", palette=rev(get_brewer_pal(palette="OrRd", n=5, plot=FALSE))) +
  tm_text("Nitrogen2.GRUK.reg.n",col="black",bg.color="grey")
```
Mean index value by region for the upper Nitrogen indicator (i.e. index shows deviations towards increased Nitrogen affinity in the plant community) from the GRUK monitoring data. Numbers in grey fields show the number of observations in the respective region.

```{r}
# se
tm_shape(regnor) +
  tm_polygons(col="Nitrogen2.GRUK.reg.se", title="Nitrogen (upper), 2 SE", style="quantile", palette=(get_brewer_pal(palette="OrRd", n=5, plot=FALSE))) +
  tm_text("Nitrogen2.GRUK.reg.n",col="black",bg.color="grey")

```
Standard error to the mean index value by region for the upper Nitrogen indicator from the GRUK monitoring data. Numbers in grey fields show the number of observations in the respective region.

#### continue here ####

#### Unscaled values vs. reference
We can also compare the unscaled values to the reference distribution in order to identify ecosystem types and functional plant indicators showing a deviation from the expectation. Since CSR-R for ANO and nitrogen for GRUK show some deviations, we exemplify this with these indicators for unscaled values.

Light, GRUK
```{r}

# only two NiN types in GRUK, T2-C-7 and T2-C-8
par(mfrow=c(1,2))
for ( i in unique(res.natopen.GRUK$Kartleggingsenhet_1til5000) ) {
  
  tryCatch({
    
    plot(density( as.matrix(natopen.ref.cov[['Light']][,i]) ,na.rm=T),
         xlim=c(1,9), ylim=c(0,2), type="l", main=i,xlab='Light value')
    points(res.natopen.GRUK[res.natopen.GRUK$fp_ind=="Nitrogen1" & res.natopen.GRUK$Kartleggingsenhet_1til5000==i,]$original,
           rep(0,length(res.natopen.GRUK[res.natopen.GRUK$fp_ind=="Light1" & res.natopen.GRUK$Kartleggingsenhet_1til5000==i,]$original)),
           col="red")
    points(density(res.natopen.GRUK[res.natopen.GRUK$fp_ind=="Light1" & res.natopen.GRUK$Kartleggingsenhet_1til5000==i,]$original,na.rm=T),
           type="l", col="red")
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}
legend("topright", legend=c("reference","field data"), pch=c(NA,1), lty=1, col=c("black","red"), cex=1)

```
The GRUK figure shows that the distributions for the plant communities' nitrogen affinity in the limestone rich T2-areas (åpen grunnlendt mark) around Oslofjord are shifted towards higher nitrogen affinity. The ANO figure shows shifts towards less ruderal strategy in plant communities limestone poor T2-areas as well as T12 (strandeng) and T16 (rasmarkhei, -eng) types.
