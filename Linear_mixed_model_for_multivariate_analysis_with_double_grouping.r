#### Linear mixed model for multivariate analysis with double groupings for each genotype ####
###############################################################################################


### We have two levels of grouping, Ancestry group (aus, ind, wild), Oryza group (U_weed, SA_weed, cul). Here we try to make the two factors into a 3 X 3 group, while the 14 genotypes are treated as a random factor
### Genotypes: "arr29" "arr38" "arr43" "arr54" "arr70" "arr74" "rr01"  "rr05"  "rr09"  "rr20"  "sau71" "sau76" "sin11" "sin31"
### We treated genotypes as a random factor, in the "linear mixed effect" model we try to fit, we set the intercept as gentoypes 


#weed <- read.csv(file.choose())
weed <- read.csv("F:/Dropbox/Dropbox/Caicedo_Lab_Zhongyun_personal/Zhongyun/Physiology project/Files for Dr. Levine/physio_ANOV_04012016_LN_dropped_more_traits.csv")
summary(weed)
levels(weed$genotype)
levels(weed$Ances_group)
levels(weed$Oryza_group)

#Combine variables using PCA
pca1 <- prcomp(~ glucose + fructose + sucrose + starch, data=weed)
pc1 <- pca1$x[,1]

pca2 <- prcomp(~ SPAD_45 + Chlorophyll, data=weed)
pc2 <- pca2$x[,1]

pca3 <- prcomp(~ Height_45 + X.45.15..30, data=weed)
pc3 <- pca3$x[,1]

pca4 <- prcomp(~ V1 + V8, data=weed)
pc4 <- pca4$x[,1]

#Modeling

#for PC1 (sugar + starch)
library(lme4)
fit <- lmer(pc1 ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

#for starch in PC1
fit <- lmer(starch ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

#for glucose in PC1
fit <- lmer(glucose ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

'''
Linear mixed model fit by maximum likelihood  ['lmerMod']
Formula: glucose ~ Ances_group + Oryza_group + (1 | genotype)
   Data: weed

     AIC      BIC   logLik deviance df.resid 
    44.6     56.8    -15.3     30.6       35 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.7408 -0.4865 -0.2552  0.3394  2.3801 

Random effects:
 Groups   Name        Variance Std.Dev.
 genotype (Intercept) 0.0000   0.0000  
 Residual             0.1214   0.3484  
Number of obs: 42, groups:  genotype, 14

Fixed effects:
                   Estimate Std. Error t value
(Intercept)         0.26750    0.11612   2.304
Ances_groupind      0.12933    0.11612   1.114
Ances_groupwild     0.83958    0.18361   4.573
Oryza_groupSA_weed  0.05275    0.14222   0.371
Oryza_groupU_weed   0.18042    0.14222   1.269

Correlation of Fixed Effects:
            (Intr) Ancs_grpn Ancs_grpw Or_SA_
Ances_grpnd -0.500                           
Ancs_grpwld -0.158  0.316                    
Oryz_grpSA_ -0.612  0.000    -0.387          
Oryz_grpU_w -0.612  0.000     0.000     0.500
'''

#for fructose in PC1
fit <- lmer(fructose ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

#for sucrose in PC1
fit <- lmer(sucrose ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

#for PC2
fit <- lmer(pc2 ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

# for SPAD_45

fit <- lmer(SPAD_45 ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

# for Chlorophyll
fit <- lmer(Chlorophyll ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

# for PC3 (Height_45 + growth_rate)
fit <- lmer(pc3 ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

# for Height_45
fit <- lmer(Height_45 ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

# for growth rate
fit <- lmer(X.45.15..30 ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)


fit <- lmer(Height_45 ~ Ances_group + Oryza_group + (1|genotype),
            data=weed, REML=FALSE)
summary(fit)

coef(fit)
