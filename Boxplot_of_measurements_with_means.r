#### Boxplots for physiology measurements ####



boxplot(X.45.15..30 ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$X.45.15..30,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(Biomass ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$Biomass,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(N_percentage ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$N_percentage,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(SPAD_45 ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$SPAD_45,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(Chlorophyll ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$Chlorophyll,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(Pn ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$Pn,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(glucose ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$glucose,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(fructose ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$fructose,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(sucrose ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$sucrose,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(total_sugar ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$total_sugar,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(starch ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$starch,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(V1 ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$V1,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

boxplot(V8 ~ genotype, data = physio_ANOV_04012016_LN_dropped_more_traits, col = "lightgray")
means <- tapply(physio_ANOV_04012016_LN_dropped_more_traits$V8,physio_ANOV_04012016_LN_dropped_more_traits$genotype,mean)
points(means,col="red",pch=18)

