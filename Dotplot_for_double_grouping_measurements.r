#### Dotplots for multiple measurements with double grouping assignments of each accession ####
#### Use of ggplot2 to separate panels for pre-plotting to get an idea ####
#### Written by Zhongyun Huang ####


library(ggplot2)
huang <- read.csv("physio_ANOV_04012016_LN_dropped_more_traits_2.csv")



p <- ggplot ( huang, aes ( x = starch, y = V8 ) ) + theme_bw()
p + geom_point()
p + geom_point ( aes ( color = Oryza_group ) )
p + geom_point() + facet_grid ( Ances_group ~ Oryza_group )
p + geom_point ( aes ( color = genotype ) ) + facet_grid ( Ances_group ~ Oryza_group )



pheight <- ggplot ( huang, aes ( x=Height_45, y=genotype, color=Oryza_group ) ) + theme_bw()
pheight + geom_point() + facet_wrap ( ~ Ances_group, ncol=1, scales="free_y" )

pstarch <- ggplot ( huang, aes ( x=starch, y=genotype, color=Ances_group ) ) + theme_bw()
pstarch + geom_point() + facet_wrap ( ~ Oryza_group, ncol=1, scales="free_y" )


pBiomass <- ggplot ( huang, aes ( x=Biomass, y=genotype, color=Oryza_group ) ) + theme_bw()
pBiomass + geom_point() + facet_wrap ( ~ Ances_group, ncol=1, scales="free_y" )
pBiomass <- ggplot ( huang, aes ( x=Biomass, y=genotype, color=Ances_group ) ) + theme_bw()
pBiomass + geom_point() + facet_wrap ( ~ Oryza_group, ncol=1, scales="free_y" )


prate <- ggplot ( huang, aes ( x=X.45.15..30, y=genotype, color=Oryza_group ) ) + theme_bw()
prate + geom_point() + facet_wrap ( ~ Ances_group, ncol=1, scales="free_y" )

prate <- ggplot ( huang, aes ( x=X.45.15..30, y=genotype, color=Ances_group ) ) + theme_bw()
prate + geom_point() + facet_wrap ( ~ Oryza_group, ncol=1, scales="free_y" )



pdf(height=3,width=7)
for ( trait in 5:18 ) {
    p <- ggplot ( huang, aes ( x=huang[,trait], y=genotype, color=Oryza_group ) ) + theme_bw()
    print ( p + geom_point() +
                facet_wrap ( ~ Ances_group, ncol=1, scales="free_y" ) +
                xlab ( names(huang)[trait] )
    )
    
    p <- ggplot ( huang, aes ( x=huang[,trait], y=genotype, color=Ances_group ) ) + theme_bw()
    print ( p + geom_point() +
                facet_wrap ( ~ Oryza_group, ncol=1, scales="free_y" ) +
                xlab ( names(huang)[trait] )
    )
}
dev.off()