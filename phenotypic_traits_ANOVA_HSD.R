## For SureSelect phenotypic data
# 1. Check normality  2. Check chamber effect 3. Check group effect

chamber_effect <- read.csv("F:/Dropbox/Dropbox/CaicedoLab/Zhongyun/Sureselect_results/SureSelect_paper/Phenotype_trait_list.csv")
head(chamber_effect)

library("agricolae")
library("pgirmess")
library("doBy")

##############################
###Trait 1: Flowering_date ###

shapiro.test(chamber_effect$Flowering_date)
#data:  chamber_effect$Flowering_date
#W = 0.88571, p-value = 3.562e-07
qqnorm(chamber_effect$Flowering_date)
#Not normally distributed, use Kruskal-Wallis

kruskal(chamber_effect$Flowering_date, chamber_effect$Chamber, alpha=0.05, p.adj=("BH"),console=TRUE)
#Study: chamber_effect$Flowering_date ~ chamber_effect$Chamber
#Kruskal-Wallis test's
#Ties or no Ties

#Value: 2.738054
#degrees of freedom: 1
#Pvalue chisq  : 0.09798404 

#chamber_effect$Chamber,  means of the ranks

#  chamber_effect.Flowering_date  r
#1                      53.45385 65
#2                      43.39706 34

#P value adjustment method: BH
#t-Student: 1.984723
#Alpha    : 0.05
#Minimum difference changes for each comparison

#Means with the same letter are not significantly different

#Groups, Treatments and mean of the ranks
#a      1 	 53.45 
#a 	 2 	 43.4 

## No chamber effect, test both chambers

kruskal(chamber_effect$Flowering_date, chamber_effect$Group, alpha=0.05, p.adj=("BH"),console=TRUE)
#Study: chamber_effect$Flowering_date ~ chamber_effect$Group
#Kruskal-Wallis test's
#Ties or no Ties

#Value: 12.75971
#degrees of freedom: 6
#Pvalue chisq  : 0.04701457 

#chamber_effect$Group,  means of the ranks

#chamber_effect.Flowering_date  r
#admixed                      55.57692 13
#al                           65.02632 19
#aus                          55.67857 14
#il                           47.38462 13
#indica                       35.00000 12
#wild                         47.76923 13
#wl                           37.03333 15

#P value adjustment method: BH
#t-Student: 1.986086
#Alpha    : 0.05
#Minimum difference changes for each comparison

#Means with the same letter are not significantly different

#Groups, Treatments and mean of the ranks
#a      al      	 65.03 
#ab 	 aus     	 55.68 
#ab 	 admixed 	 55.58 
#ab 	 wild    	 47.77 
#ab 	 il      	 47.38 
#b 	 wl      	 37.03 
#b 	 indica  	 35 

#Just to see stats
aov_oneway_flower = aov(Flowering_date~Group, data=chamber_effect)
summary(aov_oneway_flower)
#Df Sum Sq Mean Sq F value Pr(>F)
#Group        6  11575    1929   1.654  0.141
#Residuals   92 107303    1166     
flower_posthoc <- HSD.test(chamber_effect$Flowering_date,chamber_effect$Group, 92, 1166, group=T)
flower_posthoc
#chamber_effect$Flowering_date      std  r Min Max
#admixed                      135.7692 54.98963 13  63 237
#al                           132.0000 26.14490 19 103 213
#aus                          123.0357 23.48687 14  85 184
#il                           114.0000 18.92969 13  77 149
#indica                       104.0000 18.35508 12  70 128
#wild                         117.5385 44.48898 13  61 214
#wl                           108.8667 37.59154 15  54 202



##############################
###Trait 2: Tiller_number ###
shapiro.test(chamber_effect$Tiller_number)
#data:  chamber_effect$Tiller_number
#W = 0.90411, p-value = 2.714e-06

#Not normally distributed, use Kruskal-Wallis

#Check chamber effect
kruskal(chamber_effect$Tiller_number, chamber_effect$Chamber, alpha=0.05, p.adj=("BH"),console=TRUE)
#Study: chamber_effect$Tiller_number ~ chamber_effect$Chamber
#Kruskal-Wallis test's
#Ties or no Ties

#Value: 8.262909
#degrees of freedom: 1
#Pvalue chisq  : 0.004046325 

#chamber_effect$Chamber,  means of the ranks

#  chamber_effect.Tiller_number  r
#1                     55.49219 64
#2                     38.22059 34

#P value adjustment method: BH
#t-Student: 1.984984
#Alpha    : 0.05
#Minimum difference changes for each comparison

#Means with the same letter are not significantly different

#Groups, Treatments and mean of the ranks
#a      1 	 55.49 
#b 	 2 	 38.22 


#Chamber effect does exist, use data from Chamber 1 only

chamber1only <- read.csv("F:/Dropbox/Dropbox/CaicedoLab/Zhongyun/Sureselect_results/SureSelect_paper/Phenotype_trait_list_chamber1only.csv")
head(chamber1only)

kruskal(chamber1only$Tiller_number, chamber1only$Group, alpha=0.05, p.adj=("BH"),console=TRUE)
#Study: chamber1only$Tiller_number ~ chamber1only$Group
#Kruskal-Wallis test's
#Ties or no Ties

#Value: 6.173771
#degrees of freedom: 6
#Pvalue chisq  : 0.4040086 

#chamber1only$Group,  means of the ranks

#chamber1only.Tiller_number  r
#admixed                   25.42857  7
#al                        30.59375 16
#aus                       29.64286  7
#il                        35.05556  9
#indica                    23.50000  6
#wild                      35.88889  9
#wl                        42.55000 10

#P value adjustment method: BH
#t-Student: 2.002465
#Alpha    : 0.05
#Minimum difference changes for each comparison

#Only to get mean and SD
aov_oneway_tiller = aov(Tiller_number~Group, data=chamber1only)
summary(aov_oneway_tiller)
#Df Sum Sq Mean Sq F value Pr(>F)
#Group        6  200.7   33.46   1.482  0.201
#Residuals   57 1286.7   22.57     
tiller_posthoc <- HSD.test(chamber1only$Tiller_number,chamber1only$Group, 57, 22.57, group=T)
tiller_posthoc
#chamber1only$Tiller_number      std  r Min Max
#admixed                   7.571429 3.952094  7   4  15
#al                        8.437500 3.346018 16   3  14
#aus                       7.714286 1.496026  7   5   9
#il                       11.111111 6.679155  9   5  22
#indica                    7.000000 2.966479  6   4  11
#wild                     11.444444 7.551674  9   2  23
#wl                       11.500000 4.143268 10   5  19




##############################
###Trait 3: Shattering ###
shapiro.test(chamber_effect$Shattering)
#data:  chamber_effect$Shattering
#W = 0.79019, p-value = 1.582e-09
#Not normally distributed, only use Kruskal-Wallis

kruskal(chamber_effect$Shattering, chamber_effect$Chamber, alpha=0.05, p.adj=("BH"),console=TRUE)


#Shapiro-Wilk normality test



kruskal(chamber_effect$Shattering, chamber_effect$Group, alpha=0.05, p.adj=("BH"),console=TRUE)
#Study: chamber_effect$Shattering ~ chamber_effect$Group
#Kruskal-Wallis test's
#Ties or no Ties

#Value: 43.46849
#degrees of freedom: 6
#Pvalue chisq  : 9.421245e-08 

#chamber_effect$Group,  means of the ranks

#chamber_effect.Shattering  r
#admixed                  46.88889  9
#al                       26.16667 18
#aus                      61.61538 13
#il                       50.63636 11
#indica                   72.62500  8
#wild                     23.77273 11
#wl                       30.19231 13

#P value adjustment method: BH
#t-Student: 1.991673
#Alpha    : 0.05
#Minimum difference changes for each comparison

#Means with the same letter are not significantly different

#Groups, Treatments and mean of the ranks
#a      indica  	 72.62 
#ab 	 aus     	 61.62 
#bc 	 il      	 50.64 
#c 	 admixed 	 46.89 
#d 	 wl      	 30.19 
#d 	 al      	 26.17 
#d 	 wild    	 23.77 

# Only for getting the right mean and SD
aov_oneway_shatter = aov(Shattering~Group, data=chamber_effect)
summary(aov_oneway_shatter)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Group        6  10134  1689.1   8.987 2.04e-07 ***
#    Residuals   76  14283   187.9    
shatter_posthoc <- HSD.test(chamber_effect$Shattering,chamber_effect$Group, 76, 187.9, group=T)
shatter_posthoc
#$means
#chamber_effect$Shattering       std  r   Min   Max
#admixed                 14.703333 16.583640  9  0.00 49.81
#al                       2.136111  5.789736 18  0.00 24.38
#aus                     25.611538 17.921350 13  3.13 63.70
#il                      17.461818 16.705195 11  0.00 54.02
#indica                  35.957500  5.491952  8 26.62 45.49
#wild                     2.468182  5.513548 11  0.00 14.68
#wl                       8.357692 19.046547 13  0.00 66.89

##############################
###Trait 1: Seed_len_wid ####
shapiro.test(chamber_effect$Seed_len_wid)
#data:  chamber_effect$Seed_len_wid
#W = 0.97793, p-value = 0.1432
#Normally distributed, Use ANOVA

#Use one way ANOVA to test chamber effects
aov_oneway_Chamber_Seed = aov(Seed_len_wid~Chamber, data=chamber_effect)
summary(aov_oneway_Chamber_Seed)
#Df Sum Sq Mean Sq F value Pr(>F)
#Chamber      1   0.02  0.0215   0.053  0.819
#Residuals   85  34.68  0.4080 

##There is no Chamber effect for this one, first try two way ANOVA taking into account both chamber and group
aov_twoway_Chamber_Group_Seed = aov(Seed_len_wid~Chamber*Group, data=chamber_effect)
summary(aov_twoway_Chamber_Group_Seed)
#Chamber        1  0.022  0.0215   0.079    0.779    
#Group          6 14.442  2.4071   8.880 2.84e-07 ***
#    Chamber:Group  6  0.447  0.0744   0.275    0.947    
#Residuals     73 19.788  0.2711              

##There is only significance for Group factor, no Chamber, go to one-way ANOVA with only Group effect

aov_oneway_Group_Seed = aov(Seed_len_wid~Group, data=chamber_effect)
summary(aov_oneway_Group_Seed)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Group        6  14.46  2.4106   9.531 6.85e-08 ***
#    Residuals   80  20.23  0.2529  

Group_Seed_posthoc <- HSD.test(chamber_effect$Seed_len_wid,chamber_effect$Group,80,0.2529, group=T)
Group_Seed_posthoc
#$statistics
#Mean       CV MSerror       HSD r.harmonic
#3.934483 12.78165  0.2529 0.6270881   11.76713

#$parameters
#Df ntr StudentizedRange alpha  test               name.t
#80   7         4.277494  0.05 Tukey chamber_effect$Group

#$means
#chamber_effect$Seed_len_wid       std  r  Min  Max
#admixed                    3.485556 0.3511805  9 2.99 4.06
#al                         3.850556 0.4792331 18 2.94 4.59
#aus                        3.705000 0.6227822 14 2.73 5.10
#il                         3.485833 0.4277947 12 2.75 3.98
#indica                     3.891111 0.5118214  9 3.18 4.47
#wild                       4.730000 0.5920980 11 3.96 6.21
#wl                         4.347857 0.4546034 14 3.82 5.31

#$comparison
#NULL

#$groups
#trt    means  M
#1 wild    4.730000  a
#2 wl      4.347857 ab
#3 indica  3.891111 bc
#4 al      3.850556 bc
#5 aus     3.705000  c
#6 il      3.485833  c
#7 admixed 3.485556  c