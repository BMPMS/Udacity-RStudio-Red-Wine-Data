# Question: What variables contribute to making the best quality Red Wine?
# fetch the data
setwd('/Users/bryonymiles/documents/udacityprojects')
rwine <-read.csv('wineQualityReds.csv')
#Initial Analysis ----------------------------------------------

#dimensions?
dim(rwine)
#variables?
names(rwine)
#Field Descriptions:
#1 - fixed acidity: most wine acids involved are fixed or nonvolatile (do not evaporate readily)
#2 - volatile acidity: amount of acetic acid in wine - can be unpleasant, vinegary taste if too high? 
#3 - citric acid: found in small quantities, can add 'freshness' and flavor to wines
#4 - residual sugar: sugar remaining after fermentation stops, rare < 1 gram/liter, > 45 grams/liter are considered sweet
#5 - chlorides:  amount of salt in the wine
#6 - free sulfur dioxide: the free form of SO2 - prevents microbial growth and the oxidation of wine
#7 - total sulfur dioxide: free + bound forms of S02; in low concentrations, mostly undetectable in wine, free SO2 over 50 ppm, evident in the nose and taste of wine
#8 - density: the density of water is close to that of water (approx 1) depending on the percent alcohol and sugar content
#9 - pH: acidic  on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale
#10 - sulphates: anadditive which can contribute to S02 levels,  acts as an antimicrobial and antioxidant
#11 - alcohol: the percent alcohol content of the wine
#12 - quality (Output Variable) - sensory score between 0 and 10
#structure?
str(rwine)
summary(rwine)
#Thoughts at this stage - Quality range is between 3 and 8 - does this correlate with anything?
#Mean/Median seem to be relatively close on all variables except total.sulfur.dioxide and chlorides - long tailed?

#GGPairs ----------------------------------------------
library('GGally')
ggpairs(rwine)
#I've commented it out and attached it as "RedwineGGPlot.pdf"
#Pearson's R results are colour code - yellow(0.3-0.5 small meaning), orange(0.5-0.7 moderate), red(0.7+ pretty large)
#Thoughts at this stage:
#Normally distributed data: density, ph
#Positively Skewed: fixed.acidity, volatile.acidity,citric acid, residual sugar, free & total sulphur dioxide, sulphates, alcohol
#Correlations?  I highlighted anything with an R-score over or around 0.5
#fixed & volatile acidity
#fixed acidity & density 
#fixed acidity & pH
#citric acid & pH
#density & alcohol
#quality & alcohol

#GGPairs next step: trying to normalise the skewed data ----------------------------------------------
library(ggplot2)
library(gridExtra)
p1 <- qplot(data = rwine, x = fixed.acidity,fill = I('blue'),xlab = 'Fixed Acidity Histogram')
p2 <- qplot(data = rwine, x = fixed.acidity,fill = I('blue'),xlab = 'Fixed Acidity Log 10 Histogram') + scale_x_log10() 
p3 <- qplot(data = rwine, x = fixed.acidity,fill = I('blue'),xlab = 'Fixed Acidity Sqrt Histogram') + scale_x_sqrt()
p4 <- qplot(data = rwine, x = volatile.acidity,fill = I('orange'),xlab = 'Volatile Acidity Histogram')
p5 <- qplot(data = rwine, x = volatile.acidity,fill = I('orange'),xlab = 'Volatile Acidity Log 10 Histogram') + scale_x_log10() 
p6 <- qplot(data = rwine, x = volatile.acidity,fill = I('orange'),xlab = 'Volatile Acidity Sqrt Histogram') + scale_x_sqrt()
p7 <- qplot(data = rwine, x = citric.acid,fill = I('purple'),xlab = 'Citric Acid Histogram')
p8 <- qplot(data = rwine, x = citric.acid,fill = I('purple'),xlab = 'Citric Acid Log 10 Histogram') + scale_x_log10() 
p9 <- qplot(data = rwine, x = citric.acid,fill = I('purple'),xlab = 'Citric Acid Sqrt Histogram') + scale_x_sqrt()
p10 <- qplot(data = rwine, x = residual.sugar,fill = I('red'),xlab = 'Residual Sugar Histogram')
p11 <- qplot(data = rwine, x = residual.sugar,fill = I('red'),xlab = 'Residual Sugar Log 10 Histogram') + scale_x_log10() 
p12 <- qplot(data = rwine, x = residual.sugar,fill = I('red'),xlab = 'Residual Sugar Sqrt Histogram') + scale_x_sqrt()
p13 <- qplot(data = rwine, x = total.sulfur.dioxide,fill = I('pink'),xlab = 'Total SO2 Histogram')
p14 <- qplot(data = rwine, x = total.sulfur.dioxide,fill = I('pink'),xlab = 'Total SO2 Log 10 Histogram') + scale_x_log10() 
p15 <- qplot(data = rwine, x = total.sulfur.dioxide,fill = I('pink'),xlab = 'Total SO2 Sqrt Histogram') + scale_x_sqrt()
p16 <- qplot(data = rwine, x = free.sulfur.dioxide,fill = I('grey'),xlab = 'Free SO2 Histogram')
p17 <- qplot(data = rwine, x = free.sulfur.dioxide,fill = I('grey'),xlab = 'Free SO2 Log 10 Histogram') + scale_x_log10() 
p18 <- qplot(data = rwine, x = free.sulfur.dioxide,fill = I('grey'),xlab = 'Free SO2 Sqrt Histogram') + scale_x_sqrt()
p19 <- qplot(data = rwine, x = sulphates,fill = I('green'),xlab = 'Sulphates Histogram')
p20 <- qplot(data = rwine, x = sulphates,fill = I('green'),xlab = 'Sulphates Log 10 Histogram') + scale_x_log10() 
p21 <- qplot(data = rwine, x = sulphates,fill = I('green'),xlab = 'Sulphates Sqrt Histogram') + scale_x_sqrt()
p22 <- qplot(data = rwine, x = alcohol,fill = I('yellow'),xlab = 'Alcohol Histogram')
p23 <- qplot(data = rwine, x = alcohol,fill = I('yellow'),xlab = 'Alcohol Log 10 Histogram') + scale_x_log10() 
p24 <- qplot(data = rwine, x = alcohol,fill = I('yellow'),xlab = 'Alcohol Sqrt Histogram') + scale_x_sqrt()
p25 <- qplot(data = rwine, x = chlorides,fill = I('black'),xlab = 'Chlorides Histogram')
p26 <- qplot(data = rwine, x = chlorides,fill = I('black'),xlab = 'Chlorides Log 10 Histogram') + scale_x_log10() 
p27 <- qplot(data = rwine, x = chlorides,fill = I('black'),xlab = 'Chlorides Sqrt Histogram') + scale_x_sqrt()
grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27)

#Resulting decisions made to transform the data as follows
#1 - fixed acidity: Log 10 Transformation
#2 - volatile acidity: Log 10 Transformation
#3 - citric acid: No change -  132 0 values, transformations therefore not suitable.
#4 - residual sugar - Log 10 - this is long tailed but results could be significant 
#5 - chlorides: Log 10 transformation
#6 - free sulfur dioxide: Sqrt transformation - normalises better than log 10
#7 - total sulfur dioxide:  Log 10 Transformation - again long tailed but results may be significant
#8 - density: no change - normal already
#9 - pH:  no change - normal already
#10 - sulphates: log10 transformation
#11 - alcohol: no change - transformations have no significant impact
#12 - quality (Output Variable) - sensory score between 0 and 10
newrwine = data.frame(rwine$X,log10(rwine$fixed.acidity),
                      log10(rwine$volatile.acidity), rwine$citric.acid,
                      log10(rwine$residual.sugar),log10(rwine$chlorides),
                      sqrt(rwine$free.sulfur.dioxide),log10(rwine$total.sulfur.dioxide),
                      rwine$density,rwine$pH,log10(rwine$sulphates),rwine$alcohol,rwine$quality)
str(newrwine)
#New ggpairs - stored in Redwine_transformedGGPlot.pdf
ggpairs(newrwine)
#Pearson's R results are colour code - yellow(0.3-0.5 small meaning), orange(0.5-0.7 moderate), red(0.7+ pretty large)
#What does this mean though?  I ended up plotting it on a flow diagram using draw.io - see Red Wine Correlations.png  Colour coding is the same


#GGPairs Conclusions ----------------------------------------------

#1. Although Free and Total SO2 are highly correlated, they do not correlate with any other variables so I am not going to analyse them further
#2. Residual sugar seems to have a small correlation to density but nothing else.  I won't analyse that further either.
#3. Start off with 3 critical factors - Alcohol, Volatile Acid and Sulphates - facet wraps on quality...
#4. Link these graphs to related variables and see if I can see any further patterns.

#Quick look at Quality ----------------------------------------------

qplot(x=quality,data=rwine)
#There majority of the data has quality level 5,6,7


#Critical Factor Analysis ----------------------------------------------

#1. ALCOHOL

library(dplyr)
q_groups <- group_by(rwine,quality)
rwine.alc_by_q <- summarise(q_groups, 
                            alcohol_mean = mean(alcohol),
                            alcohol_median = median(alcohol),
                            n = n())
rwine.alc_by_q
(681+638+199)/1599
#95% of the data is in the quality bracket 5-7

ggplot(rwine, aes(x=alcohol)) + geom_histogram(binwidth=0.5) + facet_grid(~quality,scales="free")
#Alcohol - the alcohol content goes up with the quality.  Nothing <10 for top, nothing over 12 for bottom.
#Majority of mid range wines (5 & 6) = 10
#lets look in more detail..
ggplot(aes(y=alcohol,x=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  geom_line(stat = 'summary', fun.y = mean, linetype = 2, color = 'red') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1)) +
  geom_line(stat = 'summary',fun.y= quantile, fun.args = list(probs = .9)) 
#once the quality gets to 5 there is a clear positive trend 


#What about Density?, Fixed Acidity and pH - how does that affect the alcohol level.  what makes it high?
ggplot(aes(x=alcohol,y=density,color=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  scale_colour_gradient(low = I('yellow'), high = I('purple'), na.value = "grey50", guide = "colourbar") + 
  stat_smooth(method = lm)
#There seems to be a negative linear correlation between alcohol and density - the content is higher when the density is lower
#How about fixed acidity and density?
ggplot(aes(x=density,y=fixed.acidity,color=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  scale_colour_gradient(low = I('yellow'), high = I('purple'), na.value = "grey50", guide = "colourbar") + 
  stat_smooth(method = lm)
#There seems to be a positive linear correlation here.  The higher the density, the higher the fixed acidity.
#how about pH
ggplot(aes(x=fixed.acidity,y=pH,color=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +  
  scale_colour_gradient(low = I('yellow'), high = I('purple'), na.value = "grey50", guide = "colourbar") + 
  stat_smooth(method = lm)
#strong negative trend here...

ggplot(aes(x=fixed.acidity,y=density,color=pH),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) + xlim(7,15) + 
  scale_colour_gradient(low = I('blue'), high = I('red'), na.value = "grey50", guide = "colourbar") + 
  stat_smooth(method = lm)
#But does pH figure?  Tricky to tell.  The Ph levels do seem higher at the 'positive' end? 

#Possible hypothesis:  low density levels + low fixed.acidity + higher ph levels (over 3?) = higher alcohol content = better quality wine?

#2. VOLATILE ACIDITY

rwine.va_by_q <- summarise(q_groups, 
                            vol.acid_mean = mean(volatile.acidity),
                            vol.acid_median = median(volatile.acidity),
                            n = n())
rwine.va_by_q
#looks like a gradual descent in acidity from low to high quality
#mean and median quite similar

ggplot(rwine, aes(x=volatile.acidity)) + geom_histogram(binwidth = 0.05) + facet_wrap(~quality) 
#not much more to glean from the histograms really.  Distributions seem pretty similar on first glance.

ggplot(aes(x=quality,y=volatile.acidity),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  stat_smooth(method = lm) 
#this plot does reflect the downward trend but there is a lot of noise  
#smoother may be misleading?  let's knock off 1% (outliers) and have a look at mean and quantiles
#also let's check if it's worth adding log10 or sqrt transformations.
p1 <- ggplot(aes(x=quality,y=volatile.acidity),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  geom_line(stat = 'summary', fun.y = mean, linetype = 2, color = 'red') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1)) +
  geom_line(stat = 'summary',fun.y= quantile, fun.args = list(probs = .9)) +
  ylim(0,quantile(rwine$volatile.acidity,0.99)) 

p2 <- ggplot(aes(x=quality,y=volatile.acidity),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  geom_line(stat = 'summary', fun.y = mean, linetype = 2, color = 'red') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1)) +
  geom_line(stat = 'summary',fun.y= quantile, fun.args = list(probs = .9)) +
  coord_trans(y='sqrt') 

p3 <- ggplot(aes(x=quality,y=volatile.acidity),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  geom_line(stat = 'summary', fun.y = mean, linetype = 2, color = 'red') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1)) +
  geom_line(stat = 'summary',fun.y= quantile, fun.args = list(probs = .9)) +
  coord_trans(y='log10')

grid.arrange(p1,p2,p3)
#no to transformation.  slight downward trend still visible.

#Where's the correlation between Volatile and Fixed Acidity?
ggplot(aes(x=fixed.acidity,y=volatile.acidity),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))
#seems to be a bit of a cluster - the majority of results have low fixed and volatile acidity
#what if we knock out the outliers?
ggplot(aes(x=fixed.acidity,y=volatile.acidity,color=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  xlim(0,quantile(rwine$fixed.acidity,0.99)) +
  ylim(0,quantile(rwine$volatile.acidity,0.99)) +
  scale_colour_gradient(low = I('yellow'), high = I('purple'), na.value = "grey50", guide = "colourbar")
#it looks like fixed acidity and volatile acidity are related and pretty constant - as you'd expect
#there seems to be no particular correlation with quality so perhaps a dead end.

#Possible hypothesis - Volatile Acidity may have a small impact on quality - the lower the acidity, the better the wine
#this may or may not be significant.

#3. SULPHATES

rwine.sul_by_q <- summarise(q_groups, 
                           sulph_mean = mean(sulphates),
                           sulph_median = median(sulphates),
                           n = n())
rwine.sul_by_q
#looks like a pretty clear positive trend in both mean and median
ggplot(rwine, aes(x=sulphates)) + geom_histogram(binwidth=0.2) + facet_grid(~quality)
#histograms back this up.. 

ggplot(aes(x=quality,y=sulphates),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  stat_smooth(method = lm) 

#what about mean and quantiles and knocking off 1%
ggplot(aes(x=quality,y=sulphates),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2, color = 'red') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1)) +
  geom_line(stat = 'summary',fun.y= quantile, fun.args = list(probs = .9)) +
  ylim(0,quantile(rwine$sulphates,0.99))

#slight positive trend still visible..
#not trying transformations as the original data was normal


#What about Citric Acid?
ggplot(aes(x=citric.acid,y=sulphates,color=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  stat_smooth(method = lm) +
  scale_colour_gradient(low = I('yellow'), high = I('purple'), na.value = "grey50", guide = "colourbar")
#no very clear trends there...  pretty constand and quality well distributed

#or Chlorides?
ggplot(aes(x=chlorides,y=sulphates,color=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  stat_smooth(method = lm) +
  scale_colour_gradient(low = I('yellow'), high = I('purple'), na.value = "grey50", guide = "colourbar")
#this looks interesting.  Lets take out some outliers...
ggplot(aes(x=chlorides,y=sulphates,color=quality),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0))  + 
  stat_smooth(method = lm) +
  scale_colour_gradient(low = I('yellow'), high = I('purple'), na.value = "grey50", guide = "colourbar") +
  ylim(0,quantile(rwine$sulphates,0.95)) +
  xlim(0,quantile(rwine$chlorides,0.95))
#this backs up the theory that higher sulphate levels = better quality wine
#like volatile and fixed acidity there is a pretty constant and logical cluster between chlorides and sulphates but no further statistical information can be gleaned.
#Possible hypothesis - Sulphates may have a small impact on quality - the higher the sulphate level, the better the wine


#Conclusions:  What are my final three plots?

#What do they need to do:

#* Draw comparisons.
#* Identify trends.
#* Engage a wide audience.
#* Explain a complicated finding.
#* Clarify a gap between perception and reality.
#* Enable the reader to digest large amounts of information.

#Three possible hypotheses so far:

#1. Low density levels + low fixed.acidity + higher ph levels (over 3?) = higher alcohol content = better quality wine
#2. higher sulphate levels =   better the wine
#3. lower volatile acidity =  better the wine

#I'm less confident about the second two.  clearly H1 is the more complicated!
#I'm also not 100% sure that pH has a significant effect...

#Plot One: Alcohol, Quality and related values - following the trail

#Explanation: There is a clear positive trend between Alcohol Level and Quality (Pearson's R: 0.479)
#There is a clear negative correlation between Density and Alcohol (Pearson's R: 0.496) 
#There is a clear positive correlation between Density and Fixed Acidity (Pearson'R: 0.675)
#There is a clear negative correlation between Fixed Acidity and pH (Pearson's R: 0.706)
#Possible conclusion low density + low fixed acidity + high ph = high alcohol = better wine?
#Here's the graphic to back me up...

p1 = ggplot(aes(y=alcohol,x=density, color=quality),data = rwine) + 
  geom_jitter(alpha=1/2)  + 
  scale_colour_gradient(low = I('yellow'), high = I('purple'), guide = "legend") +
  stat_smooth(method = 'lm') +
  ggtitle("Alcohol v Density - entire dataset (1599 rows)")

rwine2 = subset(rwine, fixed.acidity < 9, pH >3)
p2 = ggplot(aes(y=alcohol,x=density, color=quality),data = rwine2) + 
  geom_jitter(alpha=1/2)  + 
  scale_colour_gradient(low = I('yellow'), high = I('purple'), guide = "legend") +
  stat_smooth(method='lm') + xlab("My x label") +
  ggtitle("Alcohol v Density - where Fixed.Acidity < 9 and pH > 3 (1132 rows)")

grid.arrange(p1,p2,p3)
#Conclusion: the graphs show a clear link between alcohol content, density and wine quality
#Factoring in a lower Fixed acidity level and a higher pH level makes the regression stronger.


#Plot Two: Sulphate and Quality

ggplot(aes(x=quality,y=sulphates),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2, color = 'red') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1)) +
  geom_line(stat = 'summary',fun.y= quantile, fun.args = list(probs = .9)) +
  ylim(0,quantile(rwine$sulphates,0.99))+
  ggtitle("Sulphate v Quality - 16 outliers removed")

#Conclusion: There is a link between Sulphates and Quality which is worth further statistical analysis

#Plot Three: Volatile Acidity and Quality

ggplot(aes(x=quality,y=volatile.acidity),data = rwine) + 
  geom_point(alpha = 1/5, position = position_jitter(h = 0)) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2, color = 'red') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1)) +
  geom_line(stat = 'summary',fun.y= quantile, fun.args = list(probs = .9)) +
  ylim(0,quantile(rwine$sulphates,0.99))+
  ggtitle("Volatile Acidity v Quality - 3 outliers removed")

#Conclusion: There is a link between Volatile Acidity and Quality which is worth further statistical analysis


#Overall Conclusion:

#The next step would a more indepth statistical analysis on whether the following factors contribute to a better quality wine:

#high alcohol level (over 10)
#low density (under 0.998)
#low fixed acidity (under 9)
#higher ph (over 3)
#low volatile acidity (under 0.75)
#higher sulphate levels (over 0.5)

#a more solid analysis could be made if we had access to data from different years.
#it is also worth baring in mind that the 'quality' is a sensory score given by 3 professionals.  
#if the same professionals could be involved each year that would make the analysis more robust.
