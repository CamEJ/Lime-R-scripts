

d = read.table("LimeAllAlphaDiv_25Aug17.txt", sep = "\t", row.names=1, header=T)


## or via method in page 256 of community ecology (gardener)
# using simpsons Vals from mothur:

simEven1 = d[,c(1,5,6,7,8,9)]


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

##b------------------------ and for DNA and cDNA together

# define your variables for test
# how samples are grouped into tmts
site = simEven1$LimeByGrowth

H <- simEven1$simpEven
tapply(H, INDEX=site, FUN=shapiro.test)
# if no p > 0.05 then normally distributed and can continue to aov

Haov = aov(H ~ site)
summary(Haov)

# p is 0.000169 so carry out Tukey


TukeyHSD(Haov)

# OUTPUT:

# $site
# diff           lwr           upr
# ElongationLime -ElongationControl -0.001480321 -0.0090067387  0.0060460959
# HeadingControl-ElongationControl   0.006327616 -0.0011988012  0.0138540334
# HeadingLime -ElongationControl     0.004322429 -0.0032039887  0.0118488459
# RipeningControl-ElongationControl -0.003170509 -0.0106969262  0.0043559084
# RipeningLime -ElongationControl   -0.009975696 -0.0175021137 -0.0024492791
# HeadingControl-ElongationLime      0.007807938  0.0005367276  0.0150791474
# HeadingLime -ElongationLime        0.005802750 -0.0014684599  0.0130739599
# RipeningControl-ElongationLime    -0.001690187 -0.0089613974  0.0055810224
# RipeningLime -ElongationLime      -0.008495375 -0.0157665849 -0.0012241651
# HeadingLime -HeadingControl       -0.002005188 -0.0092763974  0.0052660224
# RipeningControl-HeadingControl    -0.009498125 -0.0167693349 -0.0022269151
# RipeningLime -HeadingControl      -0.016303313 -0.0235745224 -0.0090321026
# RipeningControl-HeadingLime       -0.007492937 -0.0147641474 -0.0002217276
# RipeningLime -HeadingLime         -0.014298125 -0.0215693349 -0.0070269151
# RipeningLime -RipeningControl     -0.006805188 -0.0140763974  0.0004660224
# p adj
# ElongationLime -ElongationControl 0.9925391
# HeadingControl-ElongationControl  0.1509842
# HeadingLime -ElongationControl    0.5529698
# RipeningControl-ElongationControl 0.8223464
# RipeningLime -ElongationControl   0.0028623
# HeadingControl-ElongationLime     0.0278952
# HeadingLime -ElongationLime       0.1952180
# RipeningControl-ElongationLime    0.9840228
# RipeningLime -ElongationLime      0.0124540
# HeadingLime -HeadingControl       0.9661773
# RipeningControl-HeadingControl    0.0034523
# RipeningLime -HeadingControl      0.0000001
# RipeningControl-HeadingLime       0.0394912
# RipeningLime -HeadingLime         0.0000020
RipeningLime -RipeningControl     0.0800958


