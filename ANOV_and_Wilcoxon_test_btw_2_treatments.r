##### ANOVA and Wilcoxon tests between two treatments (Low Nitrogen vs Optimal Nitrogen) ####
#############################################################################################

##### LN vs ON difference test #####
##### Chapter 3 Physiology project #####

##STEP0: Grouping into variables  ** (trait) X (LN/ON)
LN_height <- LNON[1:48,c("Height_45")]
ON_height <- LNON[49:76,c("Height_45")]

LN_SPAD <- LNON[1:48,c("SPAD_45")]
ON_SPAD <- LNON[49:76,c("SPAD_45")]

LN_growth <- LNON[1:48,c("X.45.15..30")]
ON_growth <- LNON[49:76,c("X.45.15..30")]

LN_V1 <- LNON[1:48,c("V1")]
ON_V1 <- LNON[49:76,c("V1")]

LN_V8 <- LNON[1:48,c("V8")]
ON_V8 <- LNON[49:76,c("V8")]

LN_Pn <- LNON[1:48,c("Photosynthesis")]
ON_Pn <- LNON[49:76,c("Photosynthesis")]

LN_Chl <- LNON[1:48,c("Chlorophyll")]
ON_Chl <- LNON[49:76,c("Chlorophyll")]

LN_Glu <- LNON[1:48,c("Glucose")]
ON_Glu <- LNON[49:76,c("Glucose")]

LN_Fru <- LNON[1:48,c("Fructose")]
ON_Fru <- LNON[49:76,c("Fructose")]

LN_Suc <- LNON[1:48,c("Sucrose")]
ON_Suc <- LNON[49:76,c("Sucrose")]

LN_Sug <- LNON[1:48,c("Total.sugar")]
ON_Sug <- LNON[49:76,c("Total.sugar")]

LN_sta <- LNON[1:48,c("Starch")]
ON_sta <- LNON[49:76,c("Starch")]

LN_Bio <- LNON[1:48,c("Biomass")]
ON_Bio <- LNON[49:76,c("Biomass")]

LN_N <- LNON[1:48,c("N_percentage")]
ON_N <- LNON[49:76,c("N_percentage")]
##STEP1: check the normality of our data **Shapiro test
shapiro.test(LN_height)
shapiro.test(ON_height)
shapiro.test(LN_SPAD)
shapiro.test(ON_SPAD)
shapiro.test(LN_growth)
shapiro.test(ON_growth)
shapiro.test(LN_V1)
shapiro.test(ON_V1)
shapiro.test(LN_V8)
shapiro.test(ON_V8)
shapiro.test(LN_Pn)
shapiro.test(ON_Pn)
shapiro.test(LN_Chl)
shapiro.test(ON_Chl)

shapiro.test(LN_Glu)
shapiro.test(ON_Glu)
shapiro.test(LN_Fru)
shapiro.test(ON_Fru)
shapiro.test(LN_Suc)
shapiro.test(ON_Suc)
shapiro.test(LN_Sug)
shapiro.test(ON_Sug)
shapiro.test(LN_sta)
shapiro.test(ON_sta)
shapiro.test(LN_Bio)
shapiro.test(ON_Bio)
shapiro.test(LN_N)
shapiro.test(ON_N)
##STEP2: Wilcoxon rank test
wilcox.test(LN_height, ON_height,correct = FALSE)
wilcox.test(LN_SPAD, ON_SPAD,correct = FALSE)
wilcox.test(LN_V1, ON_V1,correct = FALSE)
wilcox.test(LN_V8, ON_V8,correct = FALSE)
wilcox.test(LN_Chl, ON_Chl,correct = FALSE)
wilcox.test(LN_Glu, ON_Glu,correct = FALSE)
wilcox.test(LN_Fru, ON_Fru,correct = FALSE)
wilcox.test(LN_Suc, ON_Suc,correct = FALSE)
wilcox.test(LN_Sug, ON_Sug,correct = FALSE)
wilcox.test(LN_sta, ON_sta,correct = FALSE)

##STEP3: ANOVA
aov_growth <- aov(LNON$X.45.15..30~LNON$Treatment*LNON$genotype)
summary(aov_growth)
aov_Pn <- aov(LNON$Photosynthesis~LNON$Treatment*LNON$genotype)

aov_Bio <- aov(LNON$Biomass~LNON$Treatment*LNON$genotype)
aov_N <- aov(LNON$N_percentage~LNON$Treatment*LNON$genotype)

##STEP4:Posthoc
mean(LN_SPAD)
#39.21042
mean(ON_SPAD)
#41.625
mean(LN_Chl)
mean(ON_Chl)
mean(LN_N)
mean(ON_N)