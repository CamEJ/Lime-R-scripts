

## or via method in page 256 of community ecology (gardener)
# using simpsons Vals from mothur:

simEven1 = d[,c(1,5,6,7)]


simEvenDNA = subset(simEven1, NucType == "DNA")

simEvencDNA = subset(simEven1, NucType == "cDNA")

# define your variables for test
# how samples are grouped into tmts
site = simEvenDNA$Growth

H <- simEvenDNA[,2]
tapply(H, INDEX=site, FUN=shapiro.test)
# if no p > 0.05 then normally distributed and can continue to aov

Haov = aov(H ~ site)
summary(Haov)

# p is 0.000169 so carry out Tukey


TukeyHSD(Haov)

# OUTPUT:
# diff          lwr           upr     p adj
# Heading-Elongation   0.005890217 -0.000286168  0.0120666013 0.0643910
# Ripening-Elongation -0.005669283 -0.011845668  0.0005071013 0.0778016
# Ripening-Heading    -0.011559500 -0.017635449 -0.0054835511 0.0000995



##### now for cDNA


site = simEvencDNA$Growth

H <- simEvencDNA[,2]
# to for normality of distribution
tapply(H, INDEX=site, FUN=shapiro.test)


# if no p > 0.05 then normally distributed and can continue to aov

Haov = aov(H ~ site)
summary(Haov)

# p is 0.000169 so carry out Tukey

TukeyHSD(Haov)

# OUTPUT

# diff           lwr           upr     p adj
# Heading-Elongation   0.006338837 -0.0001988164  0.0128764914 0.0590888
# Ripening-Elongation -0.005897913 -0.0124355664  0.0006397414 0.0844143
# Ripening-Heading    -0.012236750 -0.0186680935 -0.0058054065 0.0000993

boxplot(H ~ site)


