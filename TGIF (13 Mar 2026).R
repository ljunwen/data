# downloads the data file from the GitHub if it's not there
if (!file.exists("Dataset.RData")) {
   download.file("https://github.com/ljunwen/data/raw/main/Dataset.RData", "Dataset.RData", method = "libcurl")
}

load("Dataset.RData")


# looking at the data

# boxplots

Data <- tags[["hor"]]   # set this to the tag you're interested in
Data <- subset(Data, context == "convo")   # takes the tokens from the spontaneous conversation section
sum(Data$Freq)   # total number of tokens of the tag
sum(Data$Wordcount)   # total speaker wordcount

length(Data$SCD[Data$Freq > 0])   # number of speakers who did not use the tag
xtabs(~ Ethnicity, data = subset(Data, Data$Freq > 0))   # distribution of speakers who did use the tag by ethnicity


# histograms

Data$Freq.per.1000 <- Data$Freq / Data$Wordcount * 1000

if(!require(ggplot2)){
  install.packages("ggplot2")   # installs the 'ggplot2' package (for 'ggplot') if it isn't installed
  library(ggplot2)
}

ggplot(Data[Data$Freq > 0,], aes(x = Freq.per.1000)) + geom_histogram() +
  facet_grid(Ethnicity ~., labeller = label_both)   # used for interaction plots, e.g., with S.Education is the other interaction factor; 'labeller = label_both' includes the factor name with the levels in the labels


View(utterances[["hor"]])   # looking at the utterances in the dataset


# statistical analyses

Data <- tags[["hor"]]   # set this to the tag you're interested in

if(!require(lmerTest)){
  install.packages("lmerTest")   # installs the lmerTest package if it isn't installed
}

Data <- subset(Data, context == "convo")   # again, takes the tokens from the spontaneous conversation section
Data$Wordcount.scaled <- scale(Data$Wordcount, scale = TRUE)   # normalises the wordcounts for each speaker

full.glmm <- glmer(Freq ~ Ethnicity + Wordcount.scaled + (1|conv), data = Data, family = poisson)

# model diagnostics

if(!require(DHARMa)){
  install.packages("DHARMa")   # installs the 'DHARMa' package (for 'plotQQunif', 'simulateResiduals', and 'testZeroInflation') if it isn't installed
}

plotQQunif(simulateResiduals(full.glmm, n = 1000))
testZeroInflation(simulateResiduals(full.glmm, n = 1000))

# testing the significant of the independent variables

if(!require(car)){
  install.packages("car")   # installs the 'car' package (for 'Anova') if it isn't installed
}

car::Anova(full.glmm, type = "II")

# obtaining the marginal and conditional R-squared of the model

if(!require(MuMIn)){
  install.packages("MuMIn")   # installs the MuMIn package if it isn't installed
  library(MuMIn)   # loads the package
}

r.squaredGLMM(full.glmm)

# calculating the estimated marginal means

if(!require(emmeans)){
  install.packages("emmeans")   # installs the emmeans package if it isn't installed
}

(full.emm <- emmeans(full.glmm, pairwise ~ Ethnicity, type = "response"))

plot(full.emm)


# combined tags

Data <- tags[["totals"]]   # set this to the dataset with the totals

Data <- subset(Data, context == "convo")   # again, takes the tokens from the spontaneous conversation section
Data$Wordcount.scaled <- scale(Data$Wordcount, scale = TRUE)   # normalises the wordcounts for each speaker

if(!require(corrplot)){
  install.packages("corrplot")   # installs the corrplot package if it isn't installed
  library(corrplot)   # loads the package
}

corrplot(cor(Data[,c(9:13)]))   # the correlation plot

if(!require(glmmTMB)){
  install.packages("glmmTMB")   # installs the 'glmmTMB' package (for 'glmmTMB') if it isn't installed
}

full.glmm <- glmmTMB(Freq ~ Ethnicity + Wordcount.scaled + (1|conv), data = Data, family = nbinom2)
car::Anova(full.glmm, type = "II")
(full.emm <- emmeans(full.glmm, pairwise ~ Ethnicity, type = "response"))
plot(full.emm)