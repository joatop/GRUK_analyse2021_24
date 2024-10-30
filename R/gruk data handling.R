### Plant indicator data
# trimming away sub-species & co, and descriptor info
ind.Grime[,'species.orig'] <- ind.Grime[,'species']
ind.Grime[,'species'] <- word(ind.Grime[,'species'], 1,2)

# dealing with 'duplicates'
ind.Grime[duplicated(ind.Grime[,'species']),"species"]
ind.Grime.dup <- ind.Grime[duplicated(ind.Grime[,'species']),c("species")]
ind.Grime[ind.Grime$species %in% ind.Grime.dup,]
# getting rid of the duplicates
ind.Grime <- ind.Grime %>% filter( !(species.orig %in% list("Carex viridula brachyrrhyncha",
                                                            "Dactylorhiza fuchsii praetermissa",
                                                            "Medicago sativa varia",
                                                            "Montia fontana chondrosperma",
                                                            "Papaver dubium lecoqii",
                                                            "Sanguisorba minor muricata")
) )
#ind.Grime[duplicated(ind.Grime[,'species']),"species"]


names(ind.Tyler)[1] <- 'species'
ind.Tyler[,'species.orig'] <- ind.Tyler[,'species']
ind.Tyler <- ind.Tyler %>% mutate(species = str_remove(species, "sect. "))
ind.Tyler$species <- as.factor(ind.Tyler$species)
#summary(ind.Tyler$species)
ind.Tyler <- ind.Tyler[!is.na(ind.Tyler$species),]

ind.Tyler[,'species'] <- word(ind.Tyler[,'species'], 1,2)


#ind.Tyler2 <- ind.Tyler
#ind.Tyler <- ind.Tyler2
#ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
ind.Tyler.dup <- ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
#ind.Tyler[ind.Tyler$species %in% ind.Tyler.dup,c("Light","Moisture","Soil_reaction_pH","Nitrogen","species.orig","species")]
ind.Tyler <- ind.Tyler %>% filter( !(species.orig %in% list("Ammophila arenaria x Calamagrostis epigejos",
                                                            "Anemone nemorosa x ranunculoides",
                                                            "Armeria maritima ssp. elongata",
                                                            "Asplenium trichomanes ssp. quadrivalens",
                                                            "Calystegia sepium ssp. spectabilis",
                                                            "Campanula glomerata 'Superba'",
                                                            "Dactylorhiza maculata ssp. fuchsii",
                                                            "Erigeron acris ssp. droebachensis",
                                                            "Erigeron acris ssp. politus",
                                                            "Erysimum cheiranthoides L. ssp. alatum",
                                                            "Euphrasia nemorosa x stricta var. brevipila",
                                                            "Galium mollugo x verum",
                                                            "Geum rivale x urbanum",
                                                            "Hylotelephium telephium (ssp. maximum)",
                                                            "Juncus alpinoarticulatus ssp. rariflorus",
                                                            "Lamiastrum galeobdolon ssp. argentatum",
                                                            "Lathyrus latifolius ssp. heterophyllus",
                                                            "Medicago sativa ssp. falcata",
                                                            "Medicago sativa ssp. x varia",
                                                            "Monotropa hypopitys ssp. hypophegea",
                                                            "Ononis spinosa ssp. hircina",
                                                            "Ononis spinosa ssp. procurrens",
                                                            "Pilosella aurantiaca ssp. decolorans",
                                                            "Pilosella aurantiaca ssp. dimorpha",
                                                            "Pilosella cymosa ssp. gotlandica",
                                                            "Pilosella cymosa ssp. praealta",
                                                            "Pilosella officinarum ssp. peleteranum",
                                                            "Poa x jemtlandica (Almq.) K. Richt.",
                                                            "Poa x herjedalica Harry Sm.",
                                                            "Ranunculus peltatus ssp. baudotii",
                                                            "Sagittaria natans x sagittifolia",
                                                            "Salix repens ssp. rosmarinifolia",
                                                            "Stellaria nemorum L. ssp. montana",
                                                            "Trichophorum cespitosum ssp. germanicum")
) )
#ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
ind.Tyler.dup <- ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
#ind.Tyler[ind.Tyler$species %in% ind.Tyler.dup,c("Light","Moisture","Soil_reaction_pH","Nitrogen","species.orig","species")]

# only hybrids left -> get rid of these
ind.Tyler <- ind.Tyler[!duplicated(ind.Tyler[,'species']),]
#ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]

ind.Tyler$species <- as.factor(ind.Tyler$species)
#summary(ind.Tyler$species)
# no duplicates left

# merge indicator data
ind.dat <- merge(ind.Grime,ind.Tyler, by="species", all=T)
#summary(ind.dat)
#ind.dat[duplicated(ind.dat[,'species']),"species"]
ind.dat$species <- as.factor(ind.dat$species)
#summary(ind.dat$species)
head(ind.dat)



### GRUK data handling
names(GRUK.ruter)
names(GRUK.species)
names(GRUK.sirkler)
names(GRUK.polygoner)
#names(GRUK2021.condition)

## GRUK species data handling
names(GRUK.species)<-c("ObjectID","GlobalID","ParentGlobalID","PolygonID","RuteID","Norsknavn","Latinsknavn","art_dekning","RAkat","CreationDate","Creator","EditDate","Editor","Navn","karplantenavn","karplantedekning","veg_html_row","x")

# fix species names
unique(as.factor(GRUK.species$Latinsknavn))
GRUK.species <- GRUK.species %>%
  mutate(Species=word(Latinsknavn, 1, 2)) # lose subspecies
unique(as.factor(GRUK.species$Species))

# merge species data with indicators
GRUK.species.ind <- merge(x=GRUK.species[,c("Species", "art_dekning", "ParentGlobalID","PolygonID","RuteID")], 
                          y= ind.dat[,c("species","CC", "SS", "RR","Light", "Nitrogen", "Soil_disturbance")],
                          by.x="Species", by.y="species", all.x=T)
summary(GRUK.species.ind)

# checking which species didn't find a match
unique(GRUK.species.ind[is.na(GRUK.species.ind$Light & 
                                is.na(GRUK.species.ind$RR)),'Species'])

# fix species name issues
ind.dat <- ind.dat %>% 
  #  mutate(species=str_replace(species,"Aconitum lycoctonum", "Aconitum septentrionale")) %>% 
  #  mutate(species=str_replace(species,"Carex simpliciuscula", "Kobresia simpliciuscula")) %>%
  #  mutate(species=str_replace(species,"Carex myosuroides", "Kobresia myosuroides")) %>%
  #  mutate(species=str_replace(species,"Artemisia rupestris", "Artemisia norvegica")) %>%
  mutate(species=str_replace(species,"Cotoneaster simonsii", "Cotoneaster symondsii")) %>%
  mutate(species=str_replace(species,"Rosa vosagica", "Rosa vosagiaca"))

GRUK.species <- GRUK.species %>%
  mutate(species=str_replace(Species,"Acinos arvensis", "Clinopodium acinos")) %>%
  mutate(Species=str_replace(Species,"Arabis wahlenbergii", "Arabis hirsuta")) %>%
  #  mutate(Species=str_replace(Species,"Arctous alpinus", "Arctous alpina")) %>%
  #  mutate(Species=str_replace(Species,"Betula tortuosa", "Betula pubescens")) %>%
  #  mutate(Species=str_replace(Species,"Blysmopsis rufa", "Blysmus rufus")) %>%
  #  mutate(Species=str_replace(Species,"Cardamine nymanii", "Cardamine pratensis")) %>%
  #  mutate(Species=str_replace(Species,"Carex adelostoma", "Carex buxbaumii")) %>%
  #  mutate(Species=str_replace(Species,"Carex leersii", "Carex echinata")) %>%
  mutate(Species=str_replace(Species,"Carex paupercula", "Carex magellanica")) %>%
  #  mutate(Species=str_replace(Species,"Carex simpliciuscula", "Kobresia simpliciuscula")) %>%
  mutate(Species=str_replace(Species,"Carex viridula", "Carex flava")) %>%
  #  mutate(Species=str_replace(Species,"Chamaepericlymenum suecicum", "Cornus suecia")) %>%
  #  mutate(Species=str_replace(Species,"Cicerbita alpina", "Lactuca alpina")) %>%
  mutate(Species=str_replace(Species,"Cotoneaster scandinavicus", "Cotoneaster integerrimus")) %>%
  # mutate(Species=str_replace(Species,"Cotoneaster symondsii", "Cotoneaster integrifolius")) %>%
  mutate(Species=str_replace(Species,"Cyanus montanus", "Centaurea montana")) %>%
  #  mutate(Species=str_replace(Species,"Empetrum hermaphroditum", "Empetrum nigrum")) %>%
  mutate(Species=str_replace(Species,"Erysimum virgatum", "Erysimum strictum")) %>%
  #  mutate(Species=str_replace(Species,"Festuca prolifera", "Festuca rubra")) %>%
  mutate(Species=str_replace(Species,"Festuca trachyphylla", "Festuca brevipila")) %>%
  mutate(Species=str_replace(Species,"Galium album", "Galium mollugo")) %>%
  #  mutate(Species=str_replace(Species,"Galium elongatum", "Galium palustre")) %>%
  mutate(Species=str_replace(Species,"Helictotrichon pratense", "Avenula pratensis")) %>%
  mutate(Species=str_replace(Species,"Helictotrichon pubescens", "Avenula pubescens")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium alpina", "Hieracium Alpina")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium alpinum", "Hieracium Alpina")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium hieracium", "Hieracium Hieracium")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium hieracioides", "Hieracium umbellatum")) %>%
  mutate(Species=str_replace(Species,"Hieracium murorum", "Hieracium Hieracium")) %>%
  mutate(Species=str_replace(Species,"Hieracium vulgatum", "Hieracium Vulgata")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium oreadea", "Hieracium Oreadea")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium prenanthoidea", "Hieracium Prenanthoidea")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium vulgata", "Hieracium Vulgata")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium pilosella", "Pilosella officinarum")) %>%
  #  mutate(Species=str_replace(Species,"Hieracium vulgatum", "Hieracium umbellatum")) %>%
  #  mutate(Species=str_replace(Species,"Hierochloã« alpina", "Hierochloë alpina")) %>%
  #  mutate(Species=str_replace(Species,"Hierochloã« hirta", "Hierochloë hirta")) %>%
  #  mutate(Species=str_replace(Species,"Hierochloã« odorata", "Hierochloë odorata")) %>%
  mutate(Species=str_replace(Species,"Hylotelephium maximum", "Sedum telephium")) %>%
  #  mutate(Species=str_replace(Species,"Listera cordata", "Neottia cordata")) %>%
  #  mutate(Species=str_replace(Species,"Leontodon autumnalis", "Scorzoneroides autumnalis")) %>%
  mutate(Species=str_replace(Species,"Lepidotheca suaveolens", "Matricaria discoidea")) %>%
  #  mutate(Species=str_replace(Species,"Loiseleuria procumbens", "Kalmia procumbens")) %>%
  mutate(Species=str_replace(Species,"Malus ×domestica", "Malus domestica")) %>%
  #  mutate(Species=str_replace(Species,"Mycelis muralis", "Lactuca muralis")) %>%
  #  mutate(Species=str_replace(Species,"Omalotheca supina", "Gnaphalium supinum")) %>%
  #  mutate(Species=str_replace(Species,"Omalotheca norvegica", "Gnaphalium norvegicum")) %>%
  #  mutate(Species=str_replace(Species,"Omalotheca sylvatica", "Gnaphalium sylvaticum")) %>%
  #  mutate(Species=str_replace(Species,"Oreopteris limbosperma", "Thelypteris limbosperma")) %>%
  #  mutate(Species=str_replace(Species,"Oxycoccus microcarpus", "Vaccinium microcarpum")) %>%
  #  mutate(Species=str_replace(Species,"Oxycoccus palustris", "Vaccinium oxycoccos")) %>%
  #  mutate(Species=str_replace(Species,"Phalaris minor", "Phalaris arundinacea")) %>%
  mutate(Species=str_replace(Species,"Pilosella peletariana", "Pilosella officinarum")) %>%
  #  mutate(Species=str_replace(Species,"Pinus unicinata", "Pinus mugo")) %>%
  #  mutate(Species=str_replace(Species,"Poa alpigena", "Poa pratensis")) %>%
  mutate(Species=str_replace(Species,"Poa angustifolia", "Poa pratensis")) %>%
  mutate(Species=str_replace(Species,"Poa humilis", "Poa pratensis")) %>%
  #  mutate(Species=str_replace(Species,"Pyrola grandiflora", "Pyrola rotundifolia")) %>%
  mutate(Species=str_replace(Species,"Rosa dumalis", "Rosa vosagiaca")) %>%
  #  mutate(Species=str_replace(Species,"Rumex alpestris", "Rumex acetosa")) %>%
  mutate(Species=str_replace(Species,"Sorbus hybrida", "Hedlundia hybrida")) %>%
  mutate(Species=str_replace(Species,"Spergularia salina", "Spergularia marina")) %>%
  #  mutate(Species=str_replace(Species,"Syringa emodi", "Syringa vulgaris")) %>%
  #  mutate(Species=str_replace(Species,"Taraxacum crocea", "Taraxacum officinale")) %>%
  #  mutate(Species=str_replace(Species,"Taraxacum croceum", "Taraxacum officinale")) %>%
  #  mutate(Species=str_replace(Species,"Trientalis europaea", "Lysimachia europaea")) %>%
  mutate(Species=str_replace(Species,"Trifolium pallidum", "Trifolium pratense"))

# merge species data with indicators
GRUK.species.ind <- merge(x=GRUK.species[,c("Species", "art_dekning", "ParentGlobalID","PolygonID","RuteID")], 
                          y= ind.dat[,c("species","CC", "SS", "RR","Light", "Nitrogen", "Soil_disturbance")],
                          by.x="Species", by.y="species", all.x=T)
summary(GRUK.species.ind)
# checking which species didn't find a match
unique(GRUK.species.ind[is.na(GRUK.species.ind$Light & 
                                is.na(GRUK.species.ind$RR)),'Species'])

## GRUK ruter data handling
names(GRUK.ruter)
# make coordinates numeric
GRUK.ruter <- GRUK.ruter %>% 
  mutate( UTM33_E_ne=as.numeric(UTM33_E_ne) ) %>%
  mutate( UTM33_N_ne=as.numeric(UTM33_N_ne) ) %>%
  mutate( UTM33_E_sw=as.numeric(UTM33_E_sw) ) %>%
  mutate( UTM33_N_sw=as.numeric(UTM33_N_sw) )

# calculate central coordinates for each plot
GRUK.ruter <- GRUK.ruter %>% 
  mutate(UTM33_N = (UTM33_N_ne + UTM33_N_sw)/2) %>%
  mutate(UTM33_E = (UTM33_E_ne + UTM33_E_sw)/2)

# some of the calculations throw NA's because there's only one set of coordinates, coalesce that set into the calculation column 
GRUK.ruter <- GRUK.ruter %>% 
  mutate (UTM33_N = coalesce(UTM33_N,UTM33_N_ne) ) %>%
  mutate (UTM33_E = coalesce(UTM33_E,UTM33_E_ne) )
GRUK.ruter <- GRUK.ruter %>% 
  mutate (UTM33_N = coalesce(UTM33_N,UTM33_N_sw) ) %>%
  mutate (UTM33_E = coalesce(UTM33_E,UTM33_E_sw) )


## GRUK sirkler data handling

## GRUK polygoner data handling

## merge information on mapping units and condition variables from GRUK.sirkler into GRUK.ruter
names(GRUK.ruter)
names(GRUK.sirkler)
GRUK.variables <- merge(x=GRUK.ruter[,c(2,4:11,19:23,25,52:53)], 
                    y=GRUK.sirkler[,c("GlobalID",
                                      "Kartleggingsenhet 1:5000",
                                      "Spor etter slitasje og slitasjebetinget erosjon (%)",
                                      "Dekning % av nakent berg",
                                      "Total dekning % av vedplanter i feltsjikt",
                                      "Dekning % av busker i busksjikt",
                                      "Dekning % av tresjikt",
                                      "Dekning % av problemarter",
                                      "Total dekning % av fremmede arter")], 
                    by.x="GlobalID", by.y="GlobalID", all.x=T)
summary(GRUK.variables)

## merge information on condition and quality from GRUK.polygoner into GRUK.variables
# transform GRUK.variables into spatial object
GRUK.variables <- st_as_sf(GRUK.variables, coords = c("UTM33_E","UTM33_N"),remove=F, crs = 25833)

# transform GRUK.polygoner into spatial object
GRUK.polygoner <- st_as_sf(GRUK.polygoner, wkt = "WKT" ,remove=F, crs = 25833)

tm_shape(GRUK.polygoner) +
  tm_graticules() +
  tm_polygons("PolygonID") +
  tm_shape(GRUK.variables) +
  tm_dots("RuteID")
  
# run a spatial join to get columns from GRUK.polygoner into GRUK.variables
GRUK.variables2 <- st_join(GRUK.variables,GRUK.polygoner[,c(3:4,9,15,18,20,22,24,60)])
names(GRUK.variables2)[1:33]<-c("GlobalID","PolygonID.x","RuteID","RuteID_loknr",
                               "Dekning%avkarplanterifeltsjikt","Dekning%avmoser","Dekning%avlav","Dekning%avstrø",
                               "Dekning%avbarjord/grus/stein/berg","Precision","UTM33_E_ne","UTM33_N_ne",
                               "UTM33_E_sw","UTM33_N_sw","areal(m2)","UTM33_N","UTM33_E","Kartleggingsenhet1:5000",
                               "Sporetterslitasjeogslitasjebetingeterosjon(%)","Dekning%avnakentberg",
                               "Totaldekning%avvedplanterifeltsjikt","Dekning%avbuskeribusksjikt","Dekning%avtresjikt",
                               "Dekning%avproblemarter","Totaldekning%avfremmedearter","LokalitetID","PolygonID.y",
                               "Kartleggingsdato","Lokalitetskvalitet","Kommune","Tilstand","Naturmangfold","NiNKartleggingsenheter")

# check how good the spatial join worked
cbind(GRUK.variables2$PolygonID.x,GRUK.variables2$PolygonID.y)
GRUK.variables2[7,]
GRUK.polygoner[GRUK.polygoner$PolygonID=="46-2",]
# some points could not be matched to polygons -> merge by PolygonID instead, drop geometry of GRUK.variables first
GRUK.variables <- st_drop_geometry(GRUK.variables)
GRUK.variables2 <- merge(x=GRUK.variables, 
                        y=GRUK.polygoner[,c(3:4,9,15,18,20,22,24)], 
                        by.x="PolygonID", by.y="PolygonID", all.x=T)
summary(GRUK.variables2) 
summary(as.factor(GRUK.variables2$Tilstand)) # no unexpected NA's


## adding information on ecosystem and condition variables to species+indicator data
names(GRUK.species.ind)
names(GRUK.variables)
GRUK.species.ind <- merge(x=GRUK.species.ind, 
                          y=GRUK.variables[,-c(2:3)], 
                          by.x="ParentGlobalID", by.y="GlobalID", all.x=T)
summary(GRUK.species.ind)
# fixing variable names and types
names(GRUK.species.ind)<-c("ParentGlobalID","Species","art_dekning","PolygonID",
                           "RuteID","CC","SS","RR","Light","Nitrogen",
                           "Soil_disturbance","RuteID_loknr","Dekning_karplanter_feltsjikt",
                           "Dekning_moser","Dekning_lav","Dekning_strø",
                           "Dekning_bar_substrat","Precision",
                           "UTM33_E_ne","UTM33_N_ne","UTM33_E_sw","UTM33_N_sw",
                           "areal_m2","UTM33_N","UTM33_E","Kartleggingsenhet_1til5000",
                           "erosion_prosent",
                           "Dekning_nakentberg","Totaldekning_vedplanter_feltsjikt",
                           "Dekning_busker_busksjikt","Dekning_tresjikt",
                           "Dekning_problemarter","Totaldekning_fremmedearter")

GRUK.species.ind <- GRUK.species.ind %>% 
  mutate(Species = as.factor(Species)) %>%
  mutate(areal_m2 = as.numeric(areal_m2)) %>%
  mutate(Kartleggingsenhet_1til5000 = as.factor(Kartleggingsenhet_1til5000)) %>%
  mutate(Dekning_nakentberg = as.numeric(Dekning_nakentberg)) %>%
  mutate(Dekning_problemarter = as.numeric(Dekning_problemarter))
summary(GRUK.species.ind)

# trimming away the points without information on NiN, species or cover  
GRUK.species.ind <- GRUK.species.ind[!is.na(GRUK.species.ind$Species),]
GRUK.species.ind <- GRUK.species.ind[!is.na(GRUK.species.ind$art_dekning),]
# no NA's for kartleggingsenhet

#rm(GRUK.species)
#rm(GRUK.ruter)


summary(GRUK.species.ind)
head(GRUK.species.ind)



### reference data - data handling

head(natopen_NiN_ref)
head(natopen_NiN_ref_spInfo)

colnames(natopen_NiN_ref)[1] <- "sp"
head(natopen_NiN_ref)

natopen_NiN_ref <- merge(natopen_NiN_ref,natopen_NiN_ref_spInfo[,c(1,4)], by.x="sp", by.y="ScientificName", all.x=T)
unique(natopen_NiN_ref[is.na(natopen_NiN_ref$Phylum),'sp']) # Pucinella does not exist in ind.dat, so we don't care
# we're only interested in vascular plants and ferns, which we have indicators on
unique(natopen_NiN_ref$Phylum)
natopen_NiN_ref <- natopen_NiN_ref %>%
  filter(Phylum %in% c("Magnoliophyta","Pteridophyta"))
unique(natopen_NiN_ref$Phylum)



natopen_NiN_ref$sp
# only genus and species name
natopen_NiN_ref$sp.orig <- natopen_NiN_ref$sp
natopen_NiN_ref$sp <- word(natopen_NiN_ref$sp, 1,2)
natopen_NiN_ref <- natopen_NiN_ref[!is.na(natopen_NiN_ref$sp),]
# merging with indicator values
NiN.natopen <- merge(natopen_NiN_ref,ind.dat[,c(1,3:5,20,23,27)], by.x="sp", by.y="species", all.x=T)
head(NiN.natopen)
summary(NiN.natopen)



NiN.natopen
unique(NiN.natopen$sp)
#NiN.sp$spgr <- as.factor(as.vector(Eco_State$Concept_Data$Species$Species_List$art.code))

# checking which species didn't find a match
unique(NiN.natopen[is.na(NiN.natopen$Light) & is.na(NiN.natopen$Nitrogen) & is.na(NiN.natopen$RR),'sp'])

# fix species name issues
natopen_NiN_ref <- natopen_NiN_ref %>% 
  mutate(sp=str_replace(sp,"Acinos arvensis", "Clinopodium acinos")) %>%
  mutate(sp=str_replace(sp,"Aconitum septentrionale", "Aconitum lycoctonum")) %>%
  mutate(sp=str_replace(sp,"Anagallis minima", "Lysimachia minima")) %>%  
  mutate(sp=str_replace(sp,"Arabis wahlenbergii", "Arabis hirsuta")) %>% 
  mutate(sp=str_replace(sp,"Aristavena setacea", "Deschampsia setacea")) %>% 
  mutate(sp=str_replace(sp,"Atriplex lapponica", "Atriplex longipes")) %>% 
  mutate(sp=str_replace(sp,"Atriplex praecox", "Atriplex longipes")) %>% 
  mutate(sp=str_replace(sp,"Blysmopsis rufa", "Blysmus rufus")) %>% 
  mutate(sp=str_replace(sp,"Carex mackenziei", "Carex norvegica")) %>% 
  mutate(sp=str_replace(sp,"Carex xvacillans", "Carex vacillans")) %>% 
  mutate(sp=str_replace(sp,"Cicerbita alpina", "Lactuca alpina")) %>% 
  mutate(sp=str_replace(sp,"Cirsium acaulon", "Cirsium acaule")) %>%
  mutate(sp=str_replace(sp,"Cotoneaster scandinavicus", "Cotoneaster integerrimus")) %>%
  mutate(sp=str_replace(sp,"Dactylorhiza viridis", "Coeloglossum viride")) %>%
  mutate(sp=str_replace(sp,"Hylotelephium maximum", "Hylotelephium telephium")) %>%
  mutate(sp=str_replace(sp,"Poa alpigena", "Poa pratensis")) %>%
  mutate(sp=str_replace(sp,"Poa humilis", "Poa pratensis")) %>%
  mutate(sp=str_replace(sp,"Spergula marina", "Spergularia marina")) %>%
  mutate(sp=str_replace(sp,"Spergularia salina", "Spergularia marina"))

ind.dat[2556,'species'] <- "Saxifraga osloënsis"
ind.dat[17,'species'] <- "Hierochloë odorata"
ind.dat[9,'species'] <- "Hippophaë rhamnoides"


# merging with indicator values
NiN.natopen <- merge(natopen_NiN_ref,ind.dat[,c(1,3:5,20,23,27)], by.x="sp", by.y="species", all.x=T)
# checking which species didn't find a match
unique(NiN.natopen[is.na(NiN.natopen$Light) & is.na(NiN.natopen$Nitrogen) & is.na(NiN.natopen$RR),'sp'])
# ok now


# translating the abundance classes into %-cover
coverscale <- data.frame(orig=0:6,
                         cov=c(0, 1/32 ,1/8, 3/8, 0.6, 4/5, 1)
)

NiN.natopen.cov <- NiN.natopen
colnames(NiN.natopen.cov)
for (i in 2:71) {
  NiN.natopen.cov[,i] <- coverscale[,2][ match(NiN.natopen[,i], 0:6 ) ]
}

summary(NiN.natopen)
summary(NiN.natopen.cov)