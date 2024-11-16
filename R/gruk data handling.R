#### msaking the reference object for the scaling ####

# NiN-types where each type is represented by one species list (including when one species list represents two NiN-types)
names(natopen.ref.cov[["Light"]])
x <- 1:66

# checking the actual NiN-types in the natopen lists
natopen.NiNtypes <- colnames(natopen.ref.cov[["Light"]])
natopen.NiNtypes[-x] <- substr(natopen.NiNtypes[-x], 1, nchar(natopen.NiNtypes[-x])-1)
natopen.NiNtypes

# 7 indicator-value indicators: Grimes C, S and R, Tyler's Light, Nitrogen, and Soil_disturbance
indEll.n=6
# creating a table to hold:
# Tyler: the 0.5 quantile (median), 0.05 quantile and  0.95 quantile for each NiN-type
# for every nature type (nrows)
tab <- matrix(ncol=3*indEll.n, nrow=length(unique(natopen.NiNtypes)) ) # 66 basic ecosystem types
# coercing the values into the table


for (i in 1:length(x) ) {
  tab[i,1:3] <- quantile(as.matrix(natopen.ref.cov[["CC"]][,x[i]]),probs=c(0.025,0.5,0.975),na.rm=T)
  tab[i,4:6] <- quantile(as.matrix(natopen.ref.cov[["SS"]][,x[i]]),probs=c(0.025,0.5,0.975),na.rm=T)
  tab[i,7:9] <- quantile(as.matrix(natopen.ref.cov[["RR"]][,x[i]]),probs=c(0.025,0.5,0.975),na.rm=T)
  tab[i,10:12] <- quantile(as.matrix(natopen.ref.cov[["Light"]][,x[i]]),probs=c(0.025,0.5,0.975),na.rm=T)
  tab[i,13:15] <- quantile(as.matrix(natopen.ref.cov[["Nitrogen"]][,x[i]]),probs=c(0.025,0.5,0.975),na.rm=T)
  tab[i,16:18] <- quantile(as.matrix(natopen.ref.cov[["Soil_disturbance"]][,x[i]]),probs=c(0.025,0.5,0.975),na.rm=T)
  
}

tab <- as.data.frame(tab)
tab$NiN <- NA
tab$NiN[1:length(x)] <- names(natopen.ref.cov[[1]])[x]
tab


# making it a proper data frame
dim(tab)
round(tab[,1:18],digits=2)

colnames(tab) <- c("CC_q2.5","CC_q50","CC_q97.5",
                   "SS_q2.5","SS_q50","SS_q97.5",
                   "RR_q2.5","RR_q50","RR_q97.5",
                   "Light_q2.5","Light_q50","Light_q97.5",
                   "Nitrogen_q2.5","Nitrogen_q50","Nitrogen_q97.5",
                   "Soil_disturbance_q2.5","Soil_disturbance_q50","Soil_disturbance_q97.5",
                   "NiN")
summary(tab)
tab$NiN
#tab$NiN <- gsub("C", "C-", tab$NiN) # add extra hyphen after C for NiN-types
tab


# restructuring into separate indicators for lower (q2.5) and higher (q97.5) than reference value (=median, q50)
y.Light <- numeric(length=nrow(tab)*2)
y.Light[((1:dim(tab)[1])*2)-1] <- tab$Light_q2.5 
y.Light[((1:dim(tab)[1])*2)] <- tab$Light_q97.5 

y.CC <- numeric(length=nrow(tab)*2)
y.CC[((1:dim(tab)[1])*2)-1] <- tab$CC_q2.5 
y.CC[((1:dim(tab)[1])*2)] <- tab$CC_q97.5 

y.SS <- numeric(length=nrow(tab)*2)
y.SS[((1:dim(tab)[1])*2)-1] <- tab$SS_q2.5 
y.SS[((1:dim(tab)[1])*2)] <- tab$SS_q97.5 

y.Nitrogen <- numeric(length=nrow(tab)*2)
y.Nitrogen[((1:dim(tab)[1])*2)-1] <- tab$Nitrogen_q2.5 
y.Nitrogen[((1:dim(tab)[1])*2)] <- tab$Nitrogen_q97.5 

y.RR <- numeric(length=nrow(tab)*2)
y.RR[((1:dim(tab)[1])*2)-1] <- tab$RR_q2.5 
y.RR[((1:dim(tab)[1])*2)] <- tab$RR_q97.5 

y.Soil_disturbance <- numeric(length=nrow(tab)*2)
y.Soil_disturbance[((1:dim(tab)[1])*2)-1] <- tab$Soil_disturbance_q2.5 
y.Soil_disturbance[((1:dim(tab)[1])*2)] <- tab$Soil_disturbance_q97.5 

# creating final objects holding the reference and limit values for all indicators

# ref object for indicators
natopen.ref.cov.val <- data.frame(N1=rep('natopen',(nrow(tab)*2*indEll.n)),
                                  hoved=c(rep('NA',(nrow(tab)*2*indEll.n))),
                                  grunn=c(rep(rep(tab$NiN,each=2),indEll.n)),
                                  county=rep('all',(nrow(tab)*2*indEll.n)),
                                  region=rep('all',(nrow(tab)*2*indEll.n)),
                                  Ind=c(rep(c('CC1','CC2'),nrow(tab)),
                                        rep(c('SS1','SS2'),nrow(tab)),
                                        rep(c('RR1','RR2'),nrow(tab)),
                                        rep(c('Light1','Light2'),nrow(tab)),
                                        rep(c('Nitrogen1','Nitrogen2'),nrow(tab)),
                                        rep(c('Soil_disturbance1','Soil_disturbance2'),nrow(tab))
                                  ),
                                  Rv=c(rep(tab$CC_q50,each=2),
                                       rep(tab$SS_q50,each=2),
                                       rep(tab$RR_q50,each=2),
                                       rep(tab$Light_q50,each=2),
                                       rep(tab$Nitrogen_q50,each=2),
                                       rep(tab$Soil_disturbance_q50,each=2)
                                  ),
                                  Gv=c(y.CC,y.SS,y.RR,y.Light,y.Nitrogen,y.Soil_disturbance),
                                  maxmin=c(rep(c(0,1),nrow(tab)), # CC
                                           rep(c(0,1),nrow(tab)), # SS
                                           rep(c(0,1),nrow(tab)), # RR
                                           rep(c(1,7),nrow(tab)),  # 7 levels of Light
                                           rep(c(1,9),nrow(tab)),  # 9 levels of Nitrogen
                                           rep(c(1,9),nrow(tab))  # 9 levels of Soil_disturbance
                                  )
)

natopen.ref.cov.val
natopen.ref.cov.val$grunn <- as.factor(natopen.ref.cov.val$grunn)
natopen.ref.cov.val$Ind <- as.factor(natopen.ref.cov.val$Ind)
summary(natopen.ref.cov.val)
head(natopen.ref.cov.val)

### add scaling values for cover of slitasje, alien sp., problem sp., woody species
natopen.ref.cov.val <- natopen.ref.cov.val %>% add_row(N1="natopen",
                                hoved=rep("T2",6),
                                grunn=rep(c("T2-C-7","T2-C-8"),3),
                                county=rep("all",6),
                                region=rep("all",6),
                                Ind=c("aliens","aliens","erosion","erosion","shrub","shrub"),
                                Rv=c(100,100,100,100,90,90),
                                Gv=c(95,95,93.75,93.75,75,75),
                                maxmin=c(0,0,0,0,25,25)
                )


#### prepare dataframes for scaling ####
colnames(GRUK.variables)
levels(as.factor(GRUK.variables$Kartleggingsenhet_1til5000)) # NiN types in data
levels(natopen.ref.cov.val$grunn) # NiN types in reference
#### creating dataframe to hold the results for natopens ####
# all GRUK points
nrow(GRUK.variables)
GRUK.natopen <- GRUK.variables

head(GRUK.natopen)
# update row-numbers
row.names(GRUK.natopen) <- 1:nrow(GRUK.natopen)
head(GRUK.natopen)
dim(GRUK.natopen)
colnames(GRUK.natopen)

length(levels(as.factor(GRUK.natopen$PolygonID)))
length(levels(as.factor(GRUK.natopen$RuteID)))
summary(as.factor(GRUK.natopen$RuteID))
# none are double

unique(GRUK.natopen$Kartleggingsenhet_1til5000)
GRUK.natopen$Kartleggingsenhet_1til5000 <- as.factor(GRUK.natopen$Kartleggingsenhet_1til5000)
summary(GRUK.natopen$Kartleggingsenhet_1til5000)

results.natopen.GRUK <- list()
ind <- unique(natopen.ref.cov.val$Ind)
# choose columns for site description
colnames(GRUK.natopen)
results.natopen.GRUK[['original']] <- GRUK.natopen
# drop geometry
#st_geometry(results.natopen.GRUK[['original']]) <- NULL
results.natopen.GRUK[['original']] <- as.data.frame(results.natopen.GRUK[['original']])

# add columns for indicators
nvar.site <- ncol(results.natopen.GRUK[['original']])
for (i in 1:length(ind) ) {results.natopen.GRUK[['original']][,i+nvar.site] <- NA}
colnames(results.natopen.GRUK[['original']])[(nvar.site+1):(length(ind)+nvar.site)] <- paste(ind)
for (i in (nvar.site+1):(length(ind)+nvar.site) ) {results.natopen.GRUK[['original']][,i] <- as.numeric(results.natopen.GRUK[['original']][,i])}
summary(results.natopen.GRUK[['original']])
results.natopen.GRUK[['original']]$Kommune <- as.factor(results.natopen.GRUK[['original']]$Kommune)
results.natopen.GRUK[['original']]$GlobalID <- as.factor(results.natopen.GRUK[['original']]$GlobalID)
results.natopen.GRUK[['original']]$PolygonID <- as.factor(results.natopen.GRUK[['original']]$PolygonID)
results.natopen.GRUK[['original']]$uRute_ID <- as.factor(results.natopen.GRUK[['original']]$RuteID)
results.natopen.GRUK[['original']]$Kartleggingsenhet_1til5000 <- as.factor(results.natopen.GRUK[['original']]$Kartleggingsenhet_1til5000)

# roll out
results.natopen.GRUK[['scaled']] <- results.natopen.GRUK[['non-truncated']] <- results.natopen.GRUK[['original']]

#### scaling function ####
#### scaled values ####
r.s <- 1    # reference value
l.s <- 0.6  # limit value
a.s <- 0    # abscence of indicator, or indicator at maximum

#### function for calculating scaled values for measured value ####

## scaling function including truncation
scal <- function() {
  # place to hold the result
  x <- numeric()
  if (maxmin < ref) {
    # values >= the reference value equal 1
    if (val >= ref) {x <- 1}
    # values < the reference value and >= the limit value can be deducted from the linear relationship between these two
    if (val < ref & val >= lim) {x <- (l.s + (val-lim) * ( (r.s-l.s) / (ref-lim) ) )}
    # values < the limit value and > maxmin can be deducted from the linear relationship between these two
    if (val < lim & val > maxmin) {x <- (a.s + (val-maxmin) * ( (l.s-a.s) / (lim-maxmin) ) )}
    # value equals or lower than maxmin
    if (val <= maxmin) {x <-0}
  } else {
    # values <= the reference value equal 1
    if (val <= ref) {x <- 1}
    # values > the reference value and <= the limit value can be deducted from the linear relationship between these two
    if (val > ref & val <= lim) {x <- ( r.s - ( (r.s - l.s) * (val - ref) / (lim - ref) ) )}
    # values > the limit value and < maxmin can be deducted from the linear relationship between these two
    if (val > lim) {x <- ( l.s - (l.s * (val - lim) / (maxmin - lim) ) )}
    # value equals or larger than maxmin
    if (val >= maxmin) {x <-0}
  }
  return(x)
  
}

## scaling function without truncation
scal.2 <- function() {
  # place to hold the result
  x <- numeric()
  if (maxmin < ref) {
    # values >= the reference value estimated from the linear relationship for lim < x < ref (line below)
    if (val >= ref) {x <- (l.s + (val-lim) * ( (r.s-l.s) / (ref-lim) ) )}
    # values < the reference value and >= the limit value can be deducted from the linear relationship between these two
    if (val < ref & val >= lim) {x <- (l.s + (val-lim) * ( (r.s-l.s) / (ref-lim) ) )}
    # values < the limit value and > maxmin can be deducted from the linear relationship between these two
    if (val < lim & val > maxmin) {x <- (a.s + (val-maxmin) * ( (l.s-a.s) / (lim-maxmin) ) )}
    # value equal or lower than maxmin
    if (val <= maxmin) {x <-0}
  } else {
    # values <= the reference value estimated from the linear relationship for lim < x < ref (line below)
    if (val <= ref) {x <- ( r.s - ( (r.s - l.s) * (val - ref) / (lim - ref) ) )}
    # values > the reference value and <= the limit value can be deducted from the linear relationship between these two
    if (val > ref & val <= lim) {x <- ( r.s - ( (r.s - l.s) * (val - ref) / (lim - ref) ) )}
    # values > the limit value and < maxmin can be deducted from the linear relationship between these two
    if (val > lim & val < maxmin) {x <- ( l.s - (l.s * (val - lim) / (maxmin - lim) ) )}
    # value equal og larger than maxmin
    if (val >= maxmin) {x <-0}
  }
  return(x)
  
}

#### do the scaling ####
#### calculating scaled and non-truncated values for the indicators based on the dataset ####
for (i in 1:nrow(GRUK.natopen) ) {  #
  tryCatch({
    print(i)
    print(paste(GRUK.natopen$PolygonID[i]))
    print(paste(GRUK.natopen$RuteID[i]))
    #    GRUK.natopen$Hovedoekosystem_sirkel[i]
    #    GRUK.natopen$Hovedoekosystem_rute[i]
    
    
    
    # if the GRUK.hovedtype exists in the reference
    #    if (GRUK.natopen$hovedtype_rute[i] %in% unique(substr(natopen.ref.cov.val$grunn,1,2)) ) {
    
    # if there is any species present in current GRUK point  
    if ( length(GRUK.species.ind[GRUK.species.ind$ParentGlobalID==as.character(GRUK.natopen$GlobalID[i]),'Species']) > 0 ) {
      
      
      # Grime's C
      
      dat <- GRUK.species.ind[GRUK.species.ind$ParentGlobalID==as.character(GRUK.natopen$GlobalID[i]),c('art_dekning','CC')]
      results.natopen.GRUK[['original']][i,'richness'] <- nrow(dat)
      dat <- dat[!is.na(dat$CC),]
      
      if ( nrow(dat)>0 ) {
        
        val <- sum(dat[,'art_dekning'] * dat[,'CC'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
        # lower part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'CC1'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'CC1'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'CC1'] <- val 
        
        # upper part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'CC2'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'CC2'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'CC2'] <- val
        
      }
      
      
      # Grime's S
      dat <- GRUK.species.ind[GRUK.species.ind$ParentGlobalID==as.character(GRUK.natopen$GlobalID[i]),c('art_dekning','SS')]
      results.natopen.GRUK[['original']][i,'richness'] <- nrow(dat)
      dat <- dat[!is.na(dat$SS),]
      
      if ( nrow(dat)>0 ) {
        
        val <- sum(dat[,'art_dekning'] * dat[,'SS'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
        # lower part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'SS1'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'SS1'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'SS1'] <- val
        
        # upper part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'SS2'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'SS2'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'SS2'] <- val
        
      }
      
      
      # Grime's R
      dat <- GRUK.species.ind[GRUK.species.ind$ParentGlobalID==as.character(GRUK.natopen$GlobalID[i]),c('art_dekning','RR')]
      results.natopen.GRUK[['original']][i,'richness'] <- nrow(dat)
      dat <- dat[!is.na(dat$RR),]
      
      if ( nrow(dat)>0 ) {
        
        val <- sum(dat[,'art_dekning'] * dat[,'RR'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
        # lower part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'RR1'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'RR1'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'RR1'] <- val
        
        # upper part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'RR2'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'RR2'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'RR2'] <- val
        
      }
      
      
      # Light
      dat <- GRUK.species.ind[GRUK.species.ind$ParentGlobalID==as.character(GRUK.natopen$GlobalID[i]),c('art_dekning','Light')]
      results.natopen.GRUK[['original']][i,'richness'] <- nrow(dat)
      dat <- dat[!is.na(dat$Light),]
      
      if ( nrow(dat)>0 ) {
        
        val <- sum(dat[,'art_dekning'] * dat[,'Light'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
        # lower part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'Light1'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'Light1'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'Light1'] <- val
        
        # upper part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'Light2'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'Light2'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'Light2'] <- val
        
        
      }
      
      
      
      
      # Nitrogen
      dat <- GRUK.species.ind[GRUK.species.ind$ParentGlobalID==as.character(GRUK.natopen$GlobalID[i]),c('art_dekning','Nitrogen')]
      results.natopen.GRUK[['original']][i,'richness'] <- nrow(dat)
      dat <- dat[!is.na(dat$Nitrogen),]
      
      if ( nrow(dat)>0 ) {
        
        val <- sum(dat[,'art_dekning'] * dat[,'Nitrogen'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
        # lower part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'Nitrogen1'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'Nitrogen1'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'Nitrogen1'] <- val
        
        
        # upper part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'Nitrogen2'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'Nitrogen2'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'Nitrogen2'] <- val
        
      }
      
      
      
      
      
      
      # Soil_disturbance
      dat <- GRUK.species.ind[GRUK.species.ind$ParentGlobalID==as.character(GRUK.natopen$GlobalID[i]),c('art_dekning','Soil_disturbance')]
      results.natopen.GRUK[['original']][i,'richness'] <- nrow(dat)
      dat <- dat[!is.na(dat$Soil_disturbance),]
      
      if ( nrow(dat)>0 ) {
        
        val <- sum(dat[,'art_dekning'] * dat[,'Soil_disturbance'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
        # lower part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'Soil_disturbance1'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'Soil_disturbance1'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'Soil_disturbance1'] <- val
        
        
        # upper part of distribution
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),"_BN",sep=""),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'Soil_disturbance2'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'Soil_disturbance2'] <- scal.2() 
        results.natopen.GRUK[['original']][i,'Soil_disturbance2'] <- val
        
      }
      
    }
    #    }
    
       # Area cover without alien species
      dat <- GRUK.natopen[i,"Totaldekning_fremmedearter"]
      
      if ( !is.na(dat) ) {
        
        val <- 100-dat
        # one-sided indicator
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='aliens' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"])),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='aliens' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"])),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='aliens' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'aliens'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'aliens'] <- scal() 
        results.natopen.GRUK[['original']][i,'aliens'] <- val
        
      }    
      
      ## run the indicators that are not dependent on species in the plot
      # Area cover without erosion
      dat <- GRUK.natopen[i,"erosjon_prosent"]
      
      if ( !is.na(dat) ) {
        
        val <- 100-dat
        # one-sided indicator
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='erosion' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"])),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='erosion' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"])),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='erosion' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'erosion'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'erosion'] <- scal() 
        results.natopen.GRUK[['original']][i,'erosion'] <- val
        
      } 
      
      # area cover without shrub cover
      dat <- GRUK.natopen[i,"Dekning_busker_busksjikt"]
      
      if ( !is.na(dat) ) {
        
        val <- 100-dat
        # one-sided indicator
        ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='shrub' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"])),'Rv']
        lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='shrub' & natopen.ref.cov.val$grunn==paste(as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"])),'Gv']
        maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='shrub' & natopen.ref.cov.val$grunn==as.character(results.natopen.GRUK[['original']][i,"Kartleggingsenhet_1til5000"]),'maxmin']
        # coercing x into results.natopen.GRUK dataframe
        results.natopen.GRUK[['scaled']][i,'shrub'] <- scal() 
        results.natopen.GRUK[['non-truncated']][i,'shrub'] <- scal() 
        results.natopen.GRUK[['original']][i,'shrub'] <- val
        
      }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# for using both sides of the plant indicators
results.natopen.GRUK[['2-sided']] <- results.natopen.GRUK[['non-truncated']]

# remove values >1 for 2-sided indicators
results.natopen.GRUK[['2-sided']]$CC1[results.natopen.GRUK[['2-sided']]$CC1>1] <- NA
results.natopen.GRUK[['2-sided']]$CC2[results.natopen.GRUK[['2-sided']]$CC2>1] <- NA

results.natopen.GRUK[['2-sided']]$SS1[results.natopen.GRUK[['2-sided']]$SS1>1] <- NA
results.natopen.GRUK[['2-sided']]$SS2[results.natopen.GRUK[['2-sided']]$SS2>1] <- NA

results.natopen.GRUK[['2-sided']]$RR1[results.natopen.GRUK[['2-sided']]$RR1>1] <- NA
results.natopen.GRUK[['2-sided']]$RR2[results.natopen.GRUK[['2-sided']]$RR2>1] <- NA

results.natopen.GRUK[['2-sided']]$Light1[results.natopen.GRUK[['2-sided']]$Light1>1] <- NA
results.natopen.GRUK[['2-sided']]$Light2[results.natopen.GRUK[['2-sided']]$Light2>1] <- NA

results.natopen.GRUK[['2-sided']]$Nitrogen1[results.natopen.GRUK[['2-sided']]$Nitrogen1>1] <- NA
results.natopen.GRUK[['2-sided']]$Nitrogen2[results.natopen.GRUK[['2-sided']]$Nitrogen2>1] <- NA

results.natopen.GRUK[['2-sided']]$Soil_disturbance1[results.natopen.GRUK[['2-sided']]$Soil_disturbance1>1] <- NA
results.natopen.GRUK[['2-sided']]$Soil_disturbance2[results.natopen.GRUK[['2-sided']]$Soil_disturbance2>1] <- NA

#saveRDS(results.natopen.GRUK, "data/cache/results.natopen.GRUK.RDS")
