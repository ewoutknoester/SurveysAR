
#'   
#'  Github packages 
#'   install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
#'   devtools::install_github("renatoamorais/rfishprod")
#' 
#' In the end we want to get a data frame in which for each survey executed, we will have values for 
#'   1) DIVERSITY (Bray-Curtis dissimilarity & Species richness)
#'   2) ABUNDANCE (Total individuals)
#'   3) BIOMASS (Total g/kg, divided by the area in m^2)
#'   4) PRODUCTIVITY (Change in biomass per day)
#' 
#' PREPARATION OF THE DATASET 
#' 
#' In order to get all of these parameters, we will need all individual fish observations, 
#' meaning that we will need to use the INPUT sheet of the fish_surveys_DATABASE file. 
#' However, it is difficult to directly read the sheet into R, because the first 5 rows 
#' only contain information on the meaning of the columns for rarity and trophic level. 
#' The sixth row binds together multiple columns which we need to use and the eighth row has loads of NAs
#' which makes our data frame less conveniently arranged. To overcome these issues, I just copy the whole 
#' sheet into a new sheet (you can do this in a new file) and remove rows 1-6 and 8. Make sure to use the most
#' recent version of the database, or a version containing the surveys you are interested in. Give this sheet a 
#' suitable name (e.g. diversity). Then, remove the columns containing anything else than numbers in the beginning, 
#' we will make an additional sheet containing all species and information needed and bind them later on. For the
#' analyses of abundance, biomass and production, we need to use only the instantaneous data. 
#' This means that the values with a colored background must be deleted. Copy the biomass sheet 
#' and give it a suitable name (e.g. abubioprod). Then, use the following macro to clear all colored cells (you may
#' have to adjust the range so that it includes data from all surveys):
#' 
#' 
#'    Sub RemoveAllColouredCells()
#'    
#'    Range("A1:UUJ300").Select
#'    For Each Cell In Selection
#'    If Not Cell.Interior.Color = RGB(255, 255, 255) Then
#'    Cell.Clear
#'    End If
#'    Next
#'    
#'    End Sub
#'  
#' 
#' After executing the macro (this may take about an hour), check if the colored cells are cleared and load the data of both sheets into R.
#' 

#' BRAY-CURTIS
#' We will now address the composition

#' For these analyses, take care of:
#'   1: Random effects
#'      We have random effects in our dataset (pseudoreplications and different observers).
#'      These random effects are (at least partly) accounted for in the other models, 
#'      but I don't know how to deal with them in a test like this. Easy solutions, such
#'      as taking a mean are not an option for count data of species. How do we fix this?
#'      (Only using the first observation might be an option, but this way we will lose 
#'      a lot of data, especially from the baseline study.)
#'   2: Significance
#'      There are very strong significant effects, these strong differences might be 
#'      partly because there are just so many species. How do we adjust for these effects,
#'      and is it possible to use a different p-value adjustment for the test on species
#'      and on group level (it would make sense to me to be more conservative on species
#'      level because the p-values are so small because of the above mentioned reason). 
#'   3: Dimensions
#'      Dimensions in metaMDS do not make use of eigenvalues, so just using more dimensions
#'      is not as easy as in PCA (alhough the fist axis still explains most of the data).
#'      Ideally we would use only 2 or 3 dimensions, this is possible, but plots look not
#'      as nice. For the species level, this one survey(No396) stands out too much to 
#'      see differences between the rest. Options to fix this are to leave it away, or 
#'      to add more dimensions (6+ seems to give a nice plot).
#'      For the family/diet level, this is less of an issue, and 3/4 dimensions already
#'      give a nice plot.
#'   4: Stress
#'      The model calculates the amount of stress, stress goes down with addition of 
#'      dimensions. Stress > 0.2 is bad, < 0.1 is good. Our stress is between 0.1 and 
#'      0.2 for the least amount of dimensions (2). The stress is lower for the grouped
#'      test. Do we use more dimensions to decrease stress, or do we accept relative high
#'      stress. Similarly to point 2: can we use more dimensions on species-level than 
#'      on group level? 
#' 

#' NDMS plots
#' 
#' It is common for NMDS analyses to start by running with 2-dimensions (k), 
#' but you want to increase the number of dimensions to ensure a minimized 
#' stress value. Keep in mind that anything more than 5-dimensions makes it 
#' difficult to interpret a 2-dimensional plot.
#' As a rule of thumb literature has identified the following cut-off values for 
#' stress-level:
#' Higher than 0.2 is poor (risks for false interpretation).
#' 0.1 - 0.2 is fair (some distances can be misleading for interpretation).
#' 0.05 - 0.1 is good (can be confident in inferences from plot).
#' Less than 0.05 is excellent (this can be rare).
#' 
#' From Rpubs.com: 'Running NMDS using 'metaMDS'' by Joe Brown




#' Figure 4
#' 


#' Figure 5
#' Try and get figure 5: a figure showing the relative abundances on different reefs
#' The figure 5A and 5B are based on only the last survey on a transect during each of the studies
#' To get the final set we used "subbcDiet <- divBrayDiet[-c(1:4,9,11:26,41,43,60:119),]"
#' 
selection <- as.numeric(paste(subbc$SurveyNo)) #The surveys we want to keep
reldiet <- as.data.frame(cbind(abp1$Survey[abp1$Survey %in% selection], 
                               paste(abp1$Transect[abp1$Survey %in% selection]),
                               paste(abp1$Family[abp1$Survey %in% selection]),
                               paste(abp1$Species[abp1$Survey %in% selection]),
                               paste(abp1$Diet[abp1$Survey %in% selection]),
                               abp1$Biomass[abp1$Survey %in% selection],
                               abp1$Abundance[abp1$Survey %in% selection],
                               abp1$prod[abp1$Survey %in% selection]))
colnames(reldiet) <- c('Survey', 'Transect', 'Family', 'Species', 'Diet', 'Biomass', 'Abundance', 'Productivity')
reldiet1 <- merge(reldiet, surveys, all.x=T)
reldiet2 <- reldiet1[,c(1:8, 11,30:33,38,39)]
reldiet2$Biomass <- as.numeric(paste(reldiet2$Biomass))
reldiet2$Biomass_ha <- reldiet2$Biomass/reldiet2$Area*10
reldiet2$Abundance <- as.numeric(paste(reldiet2$Abundance))
reldiet2$Abundance_m2 <- reldiet2$Abundance/reldiet2$Area
reldiet2$Productivity_ha <- as.numeric(paste(reldiet2$Productivity))/reldiet2$Area*10
reldietagg <- merge(aggregate(Biomass ~ Transect + Study , reldiet2, FUN=sum),
                    aggregate(Abundance_m2 ~ Transect+Study, reldiet2, FUN=sum)$Abundance_m2,
                    all.x=T)

library(viridis)
Figure5B <- ggplot(reldiet2[reldiet2$Study=='2021 / 2022',], aes(x='', y=Abundance_m2, fill=Diet)) +
  geom_bar(stat="identity", position='fill') +
  facet_wrap(~ReefType)+theme_bw()+ theme(legend.position = 'none')+
  scale_fill_viridis(option ='D', discrete = T) +
  coord_polar("y", start=0)+theme_void()+ggtitle('B  Relative abundance on diet level')+
  theme(plot.title = element_text(size=18),strip.text.x=element_text(size=14),legend.title = element_text( size=16), legend.text=element_text(size=12))

a<- aggregate(Abundance_m2 ~ Species, reldiet2[reldiet2$Study=='2021 / 2022',], sum)
a <- a[order(-a$Abundance_m2),]
relevantspecies <- a$Species[a$Abundance_m2>=sum(a$Abundance_m2)/100]

sapply(reldiet2$Species, function(x){
  if (any(x%in%relevantspecies)) return(x)
  else return ('Other')
}) -> reldiet2$Species2

co1 <- c('black','forestgreen', 'red2', 'orange', 'cornflowerblue', 
         'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue')
reldiet2$Species2 <- as.factor(reldiet2$Species2)
reldiet2$Species2 <- factor(reldiet2$Species2, levels=c('Dascyllus carneus', 'Chromis dimidiata', 'Dascyllus trimaculatus', 'Chromis lepidolepis','Chromis viridis', 'Parupeneus macronemus',
                                                        'Chromis ternatensis', 'Plectroglyphidodon lacrymatus', 'Lutjanus fulviflamma', 'Other'))
levels(reldiet2$Species2) <- c('Das. car.', 'Chr. dim.', 'Das. tri.', 'Chr. lep.', 'Chr. vir.', 'Par. mac.', 'Chr. ter.', 'Ple. lac.', 'Lut. ful.', 'Other')

Figure5A <- ggplot(reldiet2[reldiet2$Study=='2021 / 2022',], aes(x='', y=Abundance_m2, fill=Species2)) +
  geom_bar(stat="identity", position='fill') +
  facet_wrap(~ReefType)+theme_bw()+ theme(legend.position = 'none')+
  scale_fill_viridis(name = 'Species', option='H', discrete=T) +
  ggtitle('A   Relative abundance on species level')+
  coord_polar("y", start=0)+theme_void()+
  theme(plot.title = element_text(size=18),strip.text.x=element_text(size=14),legend.title = element_text( size=16), legend.text=element_text(size=12,face = "italic"))

grid.arrange(Figure5A,Figure4C, nrow=2)

#' Figure 6
(Prodplot <- ggplot(
  data = prodsumsubtransmeans[prodsumsubtransmeans$Study== '2021 / 2022',],
  aes(x = ReefType, y = prodstoch_ha, color = 'grey44')) +
    geom_jitter(width = 0.2, size=2) +
    ylab(expression(paste("Productivity (kg / ha / day)"))) +
    xlab(expression(paste('Reef types')))+
    theme_bw() + theme(legend.position = "right")+
    scale_y_continuous(trans='log10')+ ggtitle("A  Productivity")+
    theme(legend.position='none', legend.text=element_text(size=12))+
    scale_color_manual(values=co)+
    theme(plot.title = element_text(size = 18),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text = element_text(size = 14)))

emmProdsum <- as.data.frame(emmProd$`emmeans of ReefType | Study`[1:12,])
emmProdsum$emmean <- exp(emmProdsum$emmean)
emmProdsum$lower.CL <- exp(emmProdsum$lower.CL)
emmProdsum$upper.CL <- exp(emmProdsum$upper.CL)
emmProdsum$label <- c('ns', '**', 'ns', 'ns', 'ns', 'ns', 'b', 'b', 'b', 'b', 'a', 'b')

(Figure6A <- Prodplot+
    geom_point(data= emmProdsum[emmProdsum$Study=='2020',],
               aes(y = emmean), colour='darkorange', size=2.5)+
    geom_errorbar(data = emmProdsum[emmProdsum$Study == '2020',], 
                  aes(y = emmean,
                      ymin = lower.CL, 
                      ymax = upper.CL), width=0.1, size=0.5, colour='darkorange')+
    geom_point(data = emmProdsum[emmProdsum$Study == '2021 / 2022',],
               aes(y = emmean), 
               size = 2.5, colour='black') +
    geom_errorbar(data = emmProdsum[emmProdsum$Study == '2021 / 2022',], 
                  aes(y = emmean,
                      ymin = lower.CL, 
                      ymax = upper.CL), 
                  width = 0.1, size=0.8, colour='black')+
    geom_text(data=emmProdsum[emmProdsum$Study == '2021 / 2022',],
              aes(label=label, y=upper.CL*1.6), colour='black', size=7)+
    geom_text(data=emmProdsum[emmProdsum$Study=='2020',],
              aes(label=label, y=lower.CL*0.7), colour='darkorange', size=7))

abustats <- summary(log(relation$Abundance_m2)) #Get values for min, Q1, med, Q3 and max
abumin <- as.numeric(abustats[1])
abu1q <- as.numeric(abustats[2])
abumed <- as.numeric(abustats[3])
abu3q <- as.numeric(abustats[5])
abumax <- as.numeric(abustats[6])

(biomstats <- summary(log(relation$Biomass_ha))) #Define range
biomrange <- seq(from=-6, to=8, by = 0.1 )

fit <- lm(logProd_ha ~ logBiom_ha*logAbu_m2, data=relation)#Define the formula

predprodmin <- predict(fit, newdata=data.frame(logAbu_m2=abumin, logBiom_ha=biomrange)) #Fit the predicted productivity, based on biomass, for 5 different values of abundance
predprod1q <- predict(fit, newdata=data.frame(logAbu_m2=abu1q, logBiom_ha=biomrange))
predprodmed <- predict(fit, newdata=data.frame(logAbu_m2=abumed, logBiom_ha=biomrange))
predprod3q <- predict(fit, newdata=data.frame(logAbu_m2=abu3q, logBiom_ha=biomrange))
predprodmax <- predict(fit, newdata=data.frame(logAbu_m2=abumax, logBiom_ha=biomrange))

grays <- gray.colors(5,start=0.2, end=0.8)
(Figure6B <- ggplot(relation, aes(y=prodstoch_ha, x=Biomass_ha, color=log(Abundance_m2)))+geom_point()+
    ggtitle('B   Productivity vs abundance & biomass')+
    labs(x='Biomass (kg/ha)', y='Productivity (kg / ha / day)')+
    geom_line(data=data.frame(Biomass_ha=exp(biomrange), prodstoch_ha=exp(predprodmin)), color=grays[5], size=1)+
    geom_line(data=data.frame(Biomass_ha=exp(biomrange), prodstoch_ha=exp(predprod1q)), color=grays[4], size=1)+
    geom_line(data=data.frame(Biomass_ha=exp(biomrange), prodstoch_ha=exp(predprodmed)), color=grays[3], size=1)+
    geom_line(data=data.frame(Biomass_ha=exp(biomrange), prodstoch_ha=exp(predprod3q)), color=grays[2], size=1)+
    geom_line(data=data.frame(Biomass_ha=exp(biomrange), prodstoch_ha=exp(predprodmax)), color=grays[1], size=1)+
    scale_y_continuous(trans='log10')+ 
    scale_x_continuous(trans='log10')+theme_bw()+
    theme(plot.title = element_text(size = 18),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text = element_text(size = 14), 
          legend.position = 'right', legend.title = element_text(size=14), legend.text = element_text(size=12))+
    labs(colour='Abundance (log)'))

grid.arrange(Figure6A, Figure6B, nrow=2)

#' Appendix A1
Rugosity <- as.data.frame(read_excel("Metadata ARs.xlsx", sheet ="Metadata"))
Rugosity <- merge(Rugosity, reeftype, by="Transect")
ggplot(Rugosity, aes(x=ReefType, y=RugF))+
  geom_jitter(width =0.15,size=3, colour='grey44')+theme_bw()+xlab('Reef type')+ylab('Rugosity')+ 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
lm <- lm(RugF ~ ReefType, data=Rugosity)
anova(lm)
summary(lm)

#' Appendix A4
#Checking for observer bias 
abpsum[abpsum$Observer== 'Ewout Knoester',]
Observerbias <- subset(abpsum, Survey>446 & Survey<558)
Observerbias <- subset(Observerbias, Survey <462 |Survey>471)
Observerbias <- subset(Observerbias, Survey <488 |Survey>493)
Observerbias$Transect[Observerbias$Observer=='Ewout Knoester']==Observerbias$Transect[Observerbias$Observer=='Jelle Rienstra']

piaredtBiom <- t.test(Observerbias$Biomass_ha[Observerbias$Observer=='Ewout Knoester'],
                      Observerbias$Biomass_ha[Observerbias$Observer== 'Jelle Rienstra'], paired=T)

pdbiom <- data.frame(A = Observerbias$Biomass_ha[Observerbias$Observer=='Ewout Knoester']*10,
                     B = Observerbias$Biomass_ha[Observerbias$Observer== 'Jelle Rienstra']*10)
install.packages('ggpubr')
library(ggpubr)
paired1 <- ggpaired(pdbiom, cond1='A', cond2='B', fill='condition', line.size=0.01,
                    xlab='Observer', ylab='Biomass (kg/ha)', title='Observer bias Biomass')+
  scale_y_continuous(trans='log10')

#' Now abundance
#' 
pairedtAbu <- t.test(Observerbias$Abundance_m2[Observerbias$Observer=='Ewout Knoester'],
                     Observerbias$Abundance_m2[Observerbias$Observer== 'Jelle Rienstra'], paired=T)

pdabu <- data.frame(A = Observerbias$Abundance_m2[Observerbias$Observer=='Ewout Knoester'],
                    B = Observerbias$Abundance_m2[Observerbias$Observer== 'Jelle Rienstra'])
paired2 <- ggpaired(pdabu, cond1='A', cond2='B', fill='condition', line.size=0.01,
                    xlab='Observer', ylab='Abundance (individuals/m2)', title='Observer bias Abundance')+
  scale_y_continuous(trans='log10')
#' And finally S
obiasS <- subset(divsum, SurveyNo>446 & SurveyNo<558)
obiasS <- subset(obiasS, SurveyNo <462 |SurveyNo>471)
obiasS <- subset(obiasS, SurveyNo <488 |SurveyNo>493)
obiasS <- merge(obiasS, surveys, all.x=T)
obiasS$Transect[obiasS$Observer=='Ewout Knoester']==obiasS$Transect[obiasS$Observer=='Jelle Rienstra']

pairedtS <- t.test(obiasS$S[obiasS$Observer=='Ewout Knoester'],
                   obiasS$S[obiasS$Observer== 'Jelle Rienstra'], paired=T)

pdS <- data.frame(A = obiasS$S[obiasS$Observer=='Ewout Knoester'],
                  B = obiasS$S[obiasS$Observer== 'Jelle Rienstra'])
paired3 <- ggpaired(pdS, cond1='A', cond2='B', fill='condition', line.size=0.01,
                    xlab='Observer', ylab='Species richness', title='Observer bias Species richness')

grid.arrange(paired1,paired2,paired3, nrow=1)

#' Appendix A10 
#' 
Sanalysis <- merge(divgrouping, surveys, by='SurveyNo',  all.x=T)
Sanalysis <- Sanalysis[Sanalysis$Study=='2021 / 2022',]
Sanalysis <- Sanalysis[Sanalysis$Observer=='Jelle Rienstra',]
Sanalysis <- Sanalysis[Sanalysis$SpeciesAbundance>0,]

Bottle2022 <- unique(Sanalysis$Transect[Sanalysis$ReefType=='Bottle'])
Bottlerand <- sample(Bottle2022)
SBottle <- c()
SBottle[1] <- length(unique(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]]))
SBottle[2] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]])))
SBottle[3] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]])))
SBottle[4] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[4]])))
SBottle[5] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[4]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[5]])))
SBottle[10] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[4]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[5]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[6]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[7]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[8]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[9]],
                                   Sanalysis$Species[Sanalysis$Transect==Bottlerand[10]])))
SBottle[9] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[4]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[5]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[6]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[7]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[8]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[9]])))
SBottle[8] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[4]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[5]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[6]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[7]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[8]])))
SBottle[7] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[4]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[5]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[6]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[7]])))
SBottle[6] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Bottlerand[1]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[2]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[3]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[4]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[5]],
                                  Sanalysis$Species[Sanalysis$Transect==Bottlerand[6]])))

Cage2022 <- unique(Sanalysis$Transect[Sanalysis$ReefType=='Cage'])
Cagerand <- sample(Cage2022)
SCage <- c()
SCage[1] <- length(unique(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]]))
SCage[2] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]])))
SCage[3] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[3]])))
SCage[4] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[4]])))
SCage[5] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[5]])))
SCage[10] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[3]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[4]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[5]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[6]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[7]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[8]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[9]],
                                 Sanalysis$Species[Sanalysis$Transect==Cagerand[10]])))
SCage[9] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[6]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[7]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[8]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[9]])))
SCage[8] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[6]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[7]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[8]])))
SCage[7] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[6]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[7]])))
SCage[6] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cagerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cagerand[6]])))

Cake2022 <- unique(Sanalysis$Transect[Sanalysis$ReefType=='Cake'])
Cakerand <- sample(Cake2022)
SCake <- c()
SCake[1] <- length(unique(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]]))
SCake[2] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]])))
SCake[3] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[3]])))
SCake[4] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[4]])))
SCake[5] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[5]])))
SCake[10] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[3]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[4]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[5]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[6]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[7]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[8]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[9]],
                                 Sanalysis$Species[Sanalysis$Transect==Cakerand[10]])))
SCake[9] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[6]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[7]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[8]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[9]])))
SCake[8] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[6]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[7]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[8]])))
SCake[7] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[6]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[7]])))
SCake[6] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Cakerand[1]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[2]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[3]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[4]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[5]],
                                Sanalysis$Species[Sanalysis$Transect==Cakerand[6]])))

Compound2022 <- unique(Sanalysis$Transect[Sanalysis$ReefType=='Compound'])
Compoundrand <- sample(Compound2022)
SCompound <- c()
SCompound[1] <- length(unique(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]]))
SCompound[2] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]])))
SCompound[3] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]])))
SCompound[4] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[4]])))
SCompound[5] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[4]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[5]])))
SCompound[10] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[4]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[5]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[6]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[7]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[8]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[9]],
                                     Sanalysis$Species[Sanalysis$Transect==Compoundrand[10]])))
SCompound[9] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[4]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[5]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[6]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[7]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[8]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[9]])))
SCompound[8] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[4]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[5]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[6]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[7]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[8]])))
SCompound[7] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[4]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[5]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[6]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[7]])))
SCompound[6] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Compoundrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[3]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[4]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[5]],
                                    Sanalysis$Species[Sanalysis$Transect==Compoundrand[6]])))

degr <- unique(Sanalysis$Transect[Sanalysis$ReefType=='Negative'])
degrrand <- sample(degr)
Sdegr <- c()
Sdegr[1] <- length(unique(Sanalysis$Species[Sanalysis$Transect==degrrand[1]]))
Sdegr[2] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]])))
Sdegr[3] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[3]])))
Sdegr[4] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[3]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[4]])))
Sdegr[5] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[3]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[4]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[5]])))
Sdegr[10] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[3]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[4]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[5]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[6]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[7]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[8]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[9]],
                                 Sanalysis$Species[Sanalysis$Transect==degrrand[10]])))
Sdegr[9] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[3]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[4]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[5]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[6]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[7]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[8]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[9]])))
Sdegr[8] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[3]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[4]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[5]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[6]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[7]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[8]])))
Sdegr[7] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[3]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[4]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[5]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[6]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[7]])))
Sdegr[6] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==degrrand[1]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[2]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[3]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[4]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[5]],
                                Sanalysis$Species[Sanalysis$Transect==degrrand[6]])))

Healthy2022 <- unique(Sanalysis$Transect[Sanalysis$ReefType=='Positive'])
Healthyrand <- sample(Healthy2022)
SHealthy <- c()
SHealthy[1] <- length(unique(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]]))
SHealthy[2] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]])))
SHealthy[3] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]])))
SHealthy[4] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[4]])))
SHealthy[5] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[4]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[5]])))
SHealthy[10] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[4]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[5]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[6]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[7]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[8]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[9]],
                                    Sanalysis$Species[Sanalysis$Transect==Healthyrand[10]])))
SHealthy[9] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[4]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[5]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[6]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[7]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[8]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[9]])))
SHealthy[8] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[4]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[5]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[6]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[7]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[8]])))
SHealthy[7] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[4]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[5]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[6]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[7]])))
SHealthy[6] <- length(unique(paste(Sanalysis$Species[Sanalysis$Transect==Healthyrand[1]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[2]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[3]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[4]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[5]],
                                   Sanalysis$Species[Sanalysis$Transect==Healthyrand[6]])))

N <- c(1:10)


Sgraph <- as.data.frame(cbind(N, SBottle,SCage,SCake,SCompound, Sdegr, SHealthy))
Sgraph <- melt(setDT(Sgraph), id='N', variable.name = 'Reef type', value.name = 'Cumulative Species')
levels(Sgraph$`Reef type`) <- c('Bottle', 'Cage', 'Cake', 'Compound', 'Negative', 'Positive')
ggplot(data = Sgraph, aes(x=N, y=`Cumulative Species`)) + 
  geom_line(aes(colour=`Reef type`), size=1.2)+theme_bw()+xlab('Number of patches')+
  scale_color_manual(values=co)

Smax <- Sgraph$`Cumulative Species`[Sgraph$N=='20']
(figueSCum <- qplot(N, `Cumulative Species`, group=`Reef type`, colour=`Reef type`, data=Sgraph, main = 'Cumulative Species')+
    theme_bw()+ geom_line(aes(colour=`Reef type`), size=1)+
    xlab('Number of transects')+ylab('Cumulative species')+theme(legend.position = 'right')+
    scale_color_manual(values=co))

#' Appendix A12
#' 
diveffectstransAll <- divsumsubtransmeans
(Figurea12A <- p.Dpt.S + ggtitle('A  Effect of depth on species richness')+xlab('Reef depth (m)')+ylab('Species richness (S)'))
abueffectstrans <- abusumsubtransmeans
(FigureA12B <- qplot(`AR CC`, Abundance_m2, data=abueffectstrans)+
    geom_smooth(method='lm', colour='red')+theme_bw()+ggtitle('B  Effect of AR coral cover on abundance')+
    xlab('AR coral cover (%)') + ylab('Abundance (individuals/m2)')+ scale_y_continuous(trans='log10'))
bioeffectstrans <- biosumsubtransmeans
(FigureA12C <- qplot(`AR CC`, Biomass_ha, data=bioeffectstrans)+
    geom_smooth(method='lm', colour='red')+theme_bw()+ggtitle('C  Effect of AR coral cover on biomass')+
    xlab('AR coral cover (%)') + ylab('Biomass (kg/ha)') + scale_y_continuous(trans= 'log10'))
prodeffectstrans <- prodsumsubtransmeans
(FigureA12D <- qplot(`AR CC`, prodstoch_ha, data=prodeffects)+
    geom_smooth(method='lm', colour='red')+theme_bw()+ggtitle('D   Effect of AR coral cover on productivity')+
    xlab('AR coral cover (%)') + ylab('Productivity (kg/ha/day)')+ scale_y_continuous(trans='log10'))

#' Appendix A13
emmAbucorsum <- as.data.frame(emmAbucor$emmeans)
emmAbucorsum$emmean <- exp(emmAbucorsum$emmean)
emmAbucorsum$lower.CL <- exp(emmAbucorsum$lower.CL)
emmAbucorsum$upper.CL <- exp(emmAbucorsum$upper.CL)

(FigureA13A <- ggplot(
  data = emmAbucorsum,
  aes(x = ReefType, y = emmean)) +
    geom_bar(stat='identity', color='grey44') +
    ylab(expression(paste("Abundance (individuals/m2)"))) +
    xlab(expression(paste('Reef types')))+
    ggtitle('A  Abundance corrected')+
    theme_bw() + theme(legend.position = "none")+
    geom_errorbar(data = emmAbucorsum, 
                  aes(y = emmean,
                      ymin = lower.CL, 
                      ymax = upper.CL), 
                  width = 0.3,colour='black'))

emmBiocorsum <- as.data.frame(emmBiocor2$emmeans)
emmBiocorsum$emmean <- exp(emmBiocorsum$emmean)
emmBiocorsum$lower.CL <- exp(emmBiocorsum$lower.CL)
emmBiocorsum$upper.CL <- exp(emmBiocorsum$upper.CL)

(FigureA13B <- ggplot(
  data = emmBiocorsum,
  aes(x = ReefType, y = emmean)) +
    geom_bar(stat='identity', color='grey44') +
    ylab(expression(paste("Biomass (kg/ha)"))) +
    xlab(expression(paste('Reef types')))+
    ggtitle('B  Biomass corrected')+
    theme_bw() + theme(legend.position = "none")+
    geom_errorbar(data = emmBiocorsum, 
                  aes(y = emmean,
                      ymin = lower.CL, 
                      ymax = upper.CL), 
                  width = 0.3,colour='black'))

emmProdcorsum <- as.data.frame(emmProdcor$emmeans)
emmProdcorsum$emmean <- exp(emmProdcorsum$emmean)
emmProdcorsum$lower.CL <- exp(emmProdcorsum$lower.CL)
emmProdcorsum$upper.CL <- exp(emmProdcorsum$upper.CL)

(FigureA13C <- ggplot(
  data = emmProdcorsum,
  aes(x = ReefType, y = emmean)) +
    geom_bar(stat='identity', color='grey44') +
    ylab(expression(paste("Biomass productivity (kg/ha/day)"))) +
    xlab(expression(paste('Reef types')))+
    ggtitle('C  Productivity corrected')+
    theme_bw() + theme(legend.position = "none")+
    geom_errorbar(data = emmProdcorsum, 
                  aes(y = emmean,
                      ymin = lower.CL, 
                      ymax = upper.CL), 
                  width = 0.3,colour='black'))
#' Appendix A14
(nmdsplotCageSpecies <- ggplot(data=Diversity.scores[Diversity.scores$ReefType=='Cage',]) +
    stat_ellipse(aes(x=NMDS1,y=NMDS2, colour = Study),level = 0.5, size=1.5) +
    geom_point(aes(x=NMDS1,y=NMDS2, colour = Study),size=4) +
    theme_bw() + ggtitle(' A  Difference in fish composition (species level) on cage reefs')+
    theme(legend.position = "right", plot.title=element_text(size=18),
          legend.text=element_text(size=12))+ 
    scale_color_manual(values=c('darkorange', 'grey44')))
(nmdsplotCageDiet <- ggplot(data=Diversity.scoresDiet[Diversity.scoresDiet$ReefType=='Cage',]) +
    stat_ellipse(aes(x=NMDS1,y=NMDS2, colour = Study),level = 0.5, size=1.5) +
    geom_point(aes(x=NMDS1,y=NMDS2, colour = Study),size=4) +
    theme_bw() + ggtitle('B  Differences in fish composition (diet level) on cage reefs')+
    theme(legend.position = "right", plot.title=element_text(size=18),
          legend.text=element_text(size=12))+
    scale_color_manual(values=c('darkorange', 'grey44')))
grid.arrange(nmdsplotCageSpecies, nmdsplotCageDiet)

#' Appendix A15
#' Now, Ill look at older, and newer values to compare my results with really Positive baselines
ms1 <- abp[rowSums(is.na(abp[,3:14])) != 12,]
ms1 <- merge(ms1,SurveysDataPlus, by= 'SurveyNo',all.x=T)
ms1 <- merge(ms1, specieslist, all.x=T, by='Species')
ms1 <- melt(ms1, id.vars = c('SurveyNo', 'Transect' ,'Family', 'Species', 'Diet', 'a', 'b', 
                             'Area'), 
            measure.vars = c('1.25', '3.75', '6.25', '8.75', '12.5', '17.5', '25', '35', '45', '75', '125', '175'), variable.name = 'SizeClass', 
            value.name = 'Abundance')

ms1 <- ms1[!is.na(ms1$Abundance),] #This will remove rows for species that have not been sighted within a certain sizeclass in a certain survey
ms1 <- ms1[order(ms1$Species),]
ms1 <- ms1[order(ms1$SurveyNo),]

ms1$Length <- as.numeric(paste(ms1$SizeClass))
ms1$BiomassIndiv <- ms1$a * (ms1$Length ^ ms1$b) 
ms1$Biomass <- ms1$BiomassIndiv * ms1$Abundance
ms1$Biomass_ha <- (ms1$Biomass / ms1$Area)*10
ms1$Abundance_m2 <- ms1$Abundance/ms1$Area
moresurveys <- aggregate(Biomass_ha ~ SurveyNo, ms1, sum)
moresurveys <- merge(moresurveys, aggregate(Abundance_m2 ~ SurveyNo, ms1, sum))
moresurveys <- merge(moresurveys, SurveysDataPlus, all.x=T)

length(moresurveys$Biomass_ha[moresurveys$Location=='Wasini'])
ggplot(moresurveys[moresurveys$Location=='Wasini',],aes(x=Location,y=Biomass_ha))+ geom_boxplot(size=1) +
  theme_bw()+ scale_y_continuous(trans='log10')+ 
  geom_hline(yintercept=91.8, colour='grey44', linetype=4, size=2)+
  geom_hline(yintercept=1000, colour='darkgreen', size=1.5)+
  geom_hline(yintercept=100, colour='darkred', size=1.5)+
  geom_jitter(width = 0.1, size=2)+
  annotate("rect", xmin = -Inf, xmax = Inf ,
           ymin = 300, ymax = 450, fill = "goldenrod", 
           alpha = .6, color = NA) +xlab('Wasini MPA')+ylab('Biomass (kg / ha)')
#' Appendix A16
#' Now, we will look at older reef structures compared to the best Positive reefs
Data <- as.data.frame(read_excel("Fish surveys_DATABASE_Updated.xlsm", sheet = "Data", n_max = 639))
Data$SurveyNo <- as.factor(paste(Data$Survey))
Data1 <- merge(divsum,Data, all.x=T)
Data2 <- aggregate(Biomass_ha ~ SurveyNo, ms1, sum)
Data3 <- merge(Data2, Data1, all.x=T)
DataAbu <- aggregate(Abundance ~ SurveyNo, ms1, sum)
Data4 <- merge(DataAbu, Data3, all.x=T)
Data5 <- Data4[Data4$Transect=='F-R-1'|Data4$Transect=='(F-R-1)'|Data4$Transect=='F-R-3'|
                 Data4$Transect=='(F-R-3)'|Data4$Transect=='F-C-4'|Data4$Transect=='F-C-5',]
Data6 <- Data4[Data4$Transect=='F-C-4'|Data4$Transect=='F-C-5'|Data4$Transect=='Shallow cage',]
Data6$Abundance_m2 <- Data6$Abundance/Data6$Area
Data7 <- Data6[Data6$Observer=='Jelle Rienstra',]

grid.arrange(
  ggplot(Data7, aes(x=Transect, y=S, pch=Transect))+
    theme_bw()+geom_jitter(width=0.2, size=3)+ylim(0,50)+
    geom_hline(yintercept=22.1, colour='grey44', linetype=4, size=2)+
    xlab('Patch')+ylab('Species richness (individuals / survey)')+
    theme(legend.position = 'none', axis.text=element_text(size=12),
          axis.title=element_text(size=16)),
  ggplot(Data7, aes(x=Transect, y=Abundance_m2, pch=Transect))+
    geom_jitter(width=0.2, size=3)+theme_bw()+ylim(0,4)+
    geom_hline(yintercept=0.7258, colour='grey44', linetype=4, size=2)+
    xlab('Patch')+ylab('Abundance (individuals / m2)')+
    theme(legend.position = 'none', axis.text=element_text(size=12),
          axis.title=element_text(size=16)),
  ggplot(Data7, aes(x=Transect, y=Biomass_ha, pch=Transect))+
    geom_jitter(width=0.2,size=3)+theme_bw()+ ylim(0,800) +
    geom_hline(yintercept=65, colour='grey44', linetype=4, size=2)+
    xlab('Patch')+ylab('Biomass (kg / ha)')+
    theme(legend.position = 'none', axis.text=element_text(size=12),
          axis.title=element_text(size=16)),nrow=1)
#' Appendix A17
#' 
FigureA17B <- ggplot(reldiet2[reldiet2$Study=='2021 / 2022',], aes(x='', y=Biomass_ha, fill=Diet)) +
  geom_bar(stat="identity", position='fill') +
  facet_wrap(~ReefType)+theme_bw()+
  scale_fill_viridis(option ='D', discrete = T) +
  coord_polar("y", start=0)+theme_void()+ggtitle('Relative biomass on diet level')+
  theme(plot.title = element_text(size=18),strip.text.x=element_text(size=14),legend.title = element_text( size=16), legend.text=element_text(size=12))
grid.arrange(Figure5B, FigureA17B, nrow =2)
b <- merge(a,aggregate(Biomass_ha ~ Species, reldiet2[reldiet2$Study=='2021 / 2022',], sum), all.x=T)[order(-b$Biomass_ha),]
relspbiom <- b$Species[1:length(relevantspecies)]
sapply(reldiet2$Species, function(x){
  if (any(x%in%relspbiom)) return(x)
  else return ('Other')
}) -> reldiet2$SpeciesBiomass
ggplot(reldiet2[reldiet2$Study=='2021 / 2022',], aes(x='', y=Biomass_ha, fill=SpeciesBiomass)) +
  geom_bar(stat="identity", position='fill') +
  facet_wrap(~ReefType)+theme_bw()+ theme(legend.position = 'none')+
  scale_fill_viridis(discrete = T) +
  coord_polar("y", start=0)+theme_void()+ggtitle('C  Species biomass proportions on different reef types')

#' Appendix A18
ARCCdata <- metadata[metadata$ReefType!='Negative'&metadata$ReefType!='Positive',]
qplot(ReefType, `AR CC`, data = ARCCdata)+theme_bw()
ggplot(ARCCdata, aes(x=ReefType, y=`AR CC`))+
  theme_bw()+ geom_boxplot(color='grey44') +geom_jitter(width=0.2,size=2)+
  ggtitle('Variety in coral cover')+ylab('Coral cover on AR (%)')+ylim(-1,100)+
  theme(plot.title = element_text(size=12), axis.title = element_text(size=10))