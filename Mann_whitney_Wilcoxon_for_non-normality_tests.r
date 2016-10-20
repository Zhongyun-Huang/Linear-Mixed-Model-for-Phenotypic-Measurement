#### Mann-Whitney/Wilcox rank test for non-normality ####
#### Weedy rice physiology project ####
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

wilcox.test(aus_cult_height,aus_SAW_height, correct=FALSE)
#Wilcoxon rank sum test

#data:  aus_cult_height and aus_SAW_height
#W = 27, p-value = 0.1797
#alternative hypothesis: true location shift is not equal to 0


## N_percentage
ind_cult_N <- physio_ANOV_04012016_LN_dropped_more_traits_stat[37:42,c("N_percentage")]
ind_USW_N  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[7:12,c("N_percentage")]
aus_cult_N  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[31:36,c("N_percentage")]
aus_USW_N  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[1:6,c("N_percentage")]
aus_SAW_N <- physio_ANOV_04012016_LN_dropped_more_traits_stat[13:18,c("N_percentage")]
# Mann-whitney_U-test
wilcox.test(ind_cult_N,ind_USW_N, correct=FALSE)
#data:  ind_cult_N and ind_USW_N
#W = 29, p-value = 0.09307
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(aus_cult_N,aus_USW_N, correct=FALSE)
#data:  aus_cult_N and aus_USW_N
#W = 30, p-value = 0.06494
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(aus_cult_N,aus_SAW_N, correct=FALSE)
#data:  aus_cult_N and aus_SAW_N
#W = 31, p-value = 0.04113
#alternative hypothesis: true location shift is not equal to 0


##SPAD
ind_cult_SPAD <- physio_ANOV_04012016_LN_dropped_more_traits_stat[37:42,c("SPAD_45")]
ind_USW_SPAD <- physio_ANOV_04012016_LN_dropped_more_traits_stat[7:12,c("SPAD_45")]
ind_SAW_SPAD <- physio_ANOV_04012016_LN_dropped_more_traits_stat[19:24,c("SPAD_45")]
aus_cult_SPAD <- physio_ANOV_04012016_LN_dropped_more_traits_stat[31:36,c("SPAD_45")]
aus_SAW_SPAD <- physio_ANOV_04012016_LN_dropped_more_traits_stat[13:18,c("SPAD_45")]
wilcox.test(ind_cult_SPAD, ind_USW_SPAD, correct=FALSE)
#data:  ind_cult_SPAD and ind_USW_SPAD
#W = 16, p-value = 0.8182
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(ind_cult_SPAD, ind_SAW_SPAD, correct=FALSE)
#In wilcox.test.default(ind_cult_SPAD, ind_SAW_SPAD, correct = FALSE) :
#W = 10.5, p-value = 0.229
#    cannot compute exact p-value with ties
wilcox.test(aus_cult_SPAD, aus_SAW_SPAD, correct=FALSE)
#W = 24, p-value = 0.335
#cannot compute exact p-value with ties

##Chloroplast
ind_cult_Chl <- physio_ANOV_04012016_LN_dropped_more_traits_stat[37:42,c("Chlorophyll")]
ind_USW_Chl <- physio_ANOV_04012016_LN_dropped_more_traits_stat[7:12,c("Chlorophyll")]
wilcox.test(ind_cult_Chl, ind_USW_Chl, correct=FALSE)
#W = 8, p-value = 0.1087
#Warning message:
#    In wilcox.test.default(ind_cult_Chl, ind_USW_Chl, correct = FALSE) :
#    cannot compute exact p-value with ties


##Total sugar
ind_cult_sugar <- physio_ANOV_04012016_LN_dropped_more_traits_stat[37:42,c("total_sugar")]
ind_USW_sugar <- physio_ANOV_04012016_LN_dropped_more_traits_stat[7:12,c("total_sugar")]
aus_cult_sugar  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[31:36,c("total_sugar")]
aus_USW_sugar  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[1:6,c("total_sugar")]
aus_SAW_sugar <- physio_ANOV_04012016_LN_dropped_more_traits_stat[13:18,c("total_sugar")]
wilcox.test(ind_cult_sugar, ind_USW_sugar, correct=FALSE)
#data:  ind_cult_sugar and ind_USW_sugar
#W = 12, p-value = 0.3939
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(aus_cult_sugar,aus_USW_sugar, correct=FALSE)
#data:  aus_cult_sugar and aus_USW_sugar
#W = 5, p-value = 0.04113
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(aus_cult_sugar,aus_SAW_sugar, correct=FALSE)
#data:  aus_cult_sugar and aus_SAW_sugar
#W = 17, p-value = 0.9372
#alternative hypothesis: true location shift is not equal to 0


##Growth rate
ind_cult_growth <- physio_ANOV_04012016_LN_dropped_more_traits_stat[37:42,c("X.45.15..30")]
ind_USW_growth <- physio_ANOV_04012016_LN_dropped_more_traits_stat[7:12,c("X.45.15..30")]
aus_cult_growth  <- physio_ANOV_04012016_LN_dropped_more_traits_stat[31:36,c("X.45.15..30")]
aus_SAW_growth <- physio_ANOV_04012016_LN_dropped_more_traits_stat[13:18,c("X.45.15..30")]

wilcox.test(ind_cult_growth,ind_USW_growth, correct=FALSE)
#data:  ind_cult_growth and ind_USW_growth
#W = 0, p-value = 0.002165
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(aus_cult_growth,aus_SAW_growth, correct=FALSE)
#data:  aus_cult_growth and aus_SAW_growth
#W = 22, p-value = 0.5887
#alternative hypothesis: true location shift is not equal to 0