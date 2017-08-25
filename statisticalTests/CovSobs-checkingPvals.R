## or via method in page 256 of community ecology (gardener)
# using simpVals from mothur:

Sim = read.table("DNA-grouping.txt", sep = "\t", row.names=1, header=T)
SimVal = read.table("DNA-sobsCov.txt", sep = "\t", row.names=1, header=T)

site = Sim$Growth

Cov <- SimVal[,1]
tapply(Cov, INDEX=site, FUN=shapiro.test)

Haov = aov(Cov ~ site)
summary(Haov)


# output for coverage DNA
#             Df   Sum Sq   Mean Sq F value Pr(>F)
# site         2 0.000483 0.0002414   1.103  0.341
# Residuals   44 0.009633 0.0002189

Sob <- SimVal[,2]
tapply(Sob, INDEX=site, FUN=shapiro.test)

Haov = aov(Sob ~ site)
summary(Haov)

# output for sobs DNA
#             Df  Sum Sq Mean Sq F value Pr(>F)
# site         2  261453  130726   1.959  0.153
# Residuals   44 2935784   66722


##### now for cDNA

Sim = read.table("cDNA-invSimp-grouping.txt", sep = "\t", row.names=1, header=T)
SimVal = read.table("cDNA-sobsCov.txt", sep = "\t", row.names=1, header=T)

head(SimVal)
# invsimpson
# D1Highplot1a   314.2937
# D1Highplot1b   337.2637
# D1Highplot2a   265.8085
# D1Highplot2b   270.5671
# D1Highplot3a   267.3590
# D1Highplot3b   259.0361

site = Sim$Growth

covC <- SimVal[,1]
tapply(covC, INDEX=site, FUN=shapiro.test)


# if no p > 0.05 then normally distributed and can continue to aov

Haov = aov(covC ~ site)
summary(Haov)

              # Df   Sum Sq   Mean Sq F value Pr(>F)
# site         2 0.001222 0.0006112   1.125  0.334
# Residuals   44 0.023911 0.0005434

SobC <- SimVal[,2]
tapply(SobC, INDEX=site, FUN=shapiro.test)


# if no p < 0.01 then normally distributed and can continue to aov

Haov = aov(SobC ~ site)
summary(Haov)

# Df   Sum Sq Mean Sq F value Pr(>F)
# site         2  3057457 1528729    0.82  0.447
# Residuals   44 82023348 1864167

