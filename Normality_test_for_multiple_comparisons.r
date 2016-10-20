#### Pairwise comparison between groups for multiple measurements ####
#### Normality tests for pre pairwise t-test ####
#### weedy rice physiology project ####
#######################################

physio_ANOV_04012016_LN_dropped_more_traits_stat <- read.csv("F:/Dropbox/Dropbox/Caicedo_Lab_Zhongyun_personal/Zhongyun/Physiology project/Files for Dr. Levine/physio_ANOV_04012016_LN_dropped_more_traits_stat.csv")
#### Height_45
ind_cult_height <- physio_ANOV_04012016_LN_dropped_more_traits_stat[37:42,c("Height_45")]
ind_USW_height  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[7:12,c("Height_45")]
aus_cult_height  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[31:36,c("Height_45")]
aus_SAW_height  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[13:18,c("Height_45")]
# Mann-whitney_U-test
wilcox.test(ind_cult_height,ind_USW_height, correct=FALSE)
#Wilcoxon rank sum test

#data:  ind_cult_height and ind_USW_height
#W = 0, p-value = 0.002165
#alternative hypothesis: true location shift is not equal to 0


# Normality results
shapiro.test(ind_cult_height)  
#p-value = 0.08398
shapiro.test(ind_USW_height) 
#p-value = 0.3046
shapiro.test(aus_cult_height)
#p-value = 0.03526 **
shapiro.test(aus_SAW_height)
#p-value = 0.09221




## N_percentage
ind_cult_N <- physio_ANOV_04012016_LN_dropped_more_traits_stat[37:42,c("N_percentage")]
ind_USW_N  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[7:12,c("N_percentage")]
aus_cult_N  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[31:36,c("N_percentage")]
aus_USW_N  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[1:6,c("N_percentage")]
aus_SAW_N <- physio_ANOV_04012016_LN_dropped_more_traits_stat[13:18,c("N_percentage")]
#results
shapiro.test(ind_cult_N)
#p-value = 0.7213
shapiro.test(ind_USW_N)
#p-value = 0.5968
shapiro.test(aus_cult_N)
#p-value = 0.8469
shapiro.test(aus_USW_N)
#p-value = 0.3713
shapiro.test(aus_SAW_N)
#p-value = 0.08038