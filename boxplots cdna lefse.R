
library(phyloseq)

## plotting Lefse Data from cDNA 

sharedsubFile = read.table('stability.an.0.03.subsample.shared')
sharedsubFile = t(sharedsubFile)
rownames(sharedsubFile) = sharedsubFile[,1]
colnames(sharedsubFile) = sharedsubFile[2,]
dim(sharedsubFile) # use this to help changing vals below
sharedsubFile = sharedsubFile[,2:48]
sharedsubFile = sharedsubFile[4:54982,]
class(sharedsubFile) <- "numeric"
head(sharedsubFile)

## Import taxonomy file 
## As it is from mothur, there are not column headers for order, family genus etc
## in the cons.taxonomy file and this must be fixed first. This is how I did it:
## read cons.taxonomy file into excel and choose semicolon as a separator so each tax level
## is therefore in its own column. then delete header 'taxonomy' and put in appropriate
## header for each column (order or family or genus etc. 
## Copy and paste into notepad and save. Then carry on: 
taxFile = read.table('cDNAheadedFile.cons.taxonomy', header=T, sep='\t')
rownames(taxFile) = taxFile[,1]
taxFile = taxFile[,2:8]
taxFile = as.matrix(taxFile)
head(taxFile)

## import metadata file
metaFile = read.table('cDNA_metadata.txt', header=T, sep='\t')
rownames(metaFile) = metaFile[,1]
metaFile = metaFile[,2:3]
head(metaFile)

metaFile$GrowthStage <- factor(metaFile$GrowthStage, levels = metaFile$GrowthStage)

## Create phyloseq object

#OTU = otu_table(sharedFile, taxa_are_rows = TRUE)
OTUsub = otu_table(sharedsubFile, taxa_are_rows = TRUE)
TAX = tax_table(taxFile)
META = sample_data(metaFile)
#physeq = phyloseq(OTU, TAX, META)
physeqSub = phyloseq(OTUsub, TAX, META)


## Get rid of any OTUs not present in any samples and get relative abundance

microSub <- prune_taxa(taxa_sums(physeqSub) > 0, physeqSub)
microSubRel = transform_sample_counts(microSub, function(x) x / sum(x) )
microSubRelFilt = filter_taxa(microSubRel, function(x) mean(x) > 1e-5, TRUE)

# for subsampled shared file
#sharedSubRelAubd = transform_sample_counts(sharedsubFile, function(x) x / sum(x) )
microSubRelFiltFilt = filter_taxa(microSubRelFilt, function(x) mean(x) > 1e-2, TRUE)

dat <- psmelt(microSub)
write.csv(dat, file='untrimmedPhyloseqOTU-DNAlimeCOUNTS.csv')


dat <- psmelt(microSubRel)
write.csv(dat, file='untrimmedPhyloseqOTU-DNAlime.csv')


################

yy <- read.csv(file='untrimmedPhyloseqOTU-DNAlime.csv', header=T )

yy_df <- as_df(yy)
# get out all lefse's
# made this list of otus using concatenate in excel

microSubRelLefse = subset(yy, OTU=="Otu000002"|
                            OTU=="Otu000003"|
                            OTU=="Otu000004"|
                            OTU=="Otu000005"|
                            OTU=="Otu000006"|
                            OTU=="Otu000007"|
                            OTU=="Otu000010"|
                            OTU=="Otu000012"|
                            OTU=="Otu000013"|
                            OTU=="Otu000015"|
                            OTU=="Otu000019"|
                            OTU=="Otu000020"|
                            OTU=="Otu000021"|
                            OTU=="Otu000022"|
                            OTU=="Otu000028"|
                            OTU=="Otu000029"|
                            OTU=="Otu000033"|
                            OTU=="Otu000035"|
                            OTU=="Otu000042"|
                            OTU=="Otu000045"|
                            OTU=="Otu000046"|
                            OTU=="Otu000048"|
                            OTU=="Otu000052"|
                            OTU=="Otu000063"|
                            OTU=="Otu000073"|
                            OTU=="Otu000078"|
                            OTU=="Otu000095"|
                            OTU=="Otu000107"|
                            OTU=="Otu000175"|
                            OTU=="Otu000181"
                          )


write.csv(microSubRelLefse, file='microSubRelLefse-cDNA.csv')

## read into excel and added extra column saying which growth stage
# each otu was discriminatory for
#read back in 

pp <- read.csv("microSubRelLefse-cDNA.csv", header = TRUE)

# set factor orders
# function for this saved in github R basics folder
pp[["GrowthStage"]] <- setFactorOrder(pp[["GrowthStage"]], c("Elongation", "Heading", "Ripening"))

pp[["Discrim"]] <- setFactorOrder(pp[["Discrim"]], c("elongation", "heading", "ripening"))

yy_E = subset(pp, Discrim=="elongation")

yy_H = subset(pp, Discrim=="heading")

yy_R = subset(pp, Discrim=="ripening")


#### plotting elongation

k <- ggplot(yy_E, aes(x=OTU,y=Abundance, fill=GrowthStage)) +
  geom_boxplot(colour="black")
k


E_labs <- c("Myxococcales",
            "Rhizobiales",
            "Spartobacteria unclass",
            "Spartobacteria unclass",
            "Caulobacterales",
            "Caulobacterales",
            "Sphingobacteriales",
            "Bacteria unclass",
            "Acidobacteria Gp4 unclass",
            "Sphingobacteriales"
)

ki <- k + scale_x_discrete(labels= E_labs)


k2 <- ki + theme_bw() +
  theme(axis.text.x=element_text(angle=-90, size=13, hjust=0, vjust=0, colour="black"), 
        axis.text.y=element_text(hjust=1, size=14, vjust=0.4, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=15, colour="black")) +
  labs(fill="  Growth Stage\n", y = "Relative abundance\n") +
  theme(legend.title =element_text(size=15, colour="black"),
        legend.text = element_text(size=14, colour="black"), 
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"), # remove white from panel tops
        legend.key.size = unit(2, "cm"))

#k2 + theme(axis.text.x = element_text(hjust = 0))

#### plotting heading

k <- ggplot(yy_H, aes(x=OTU,y=Abundance, fill=GrowthStage)) +
  geom_boxplot(colour="black")
k


H_labs <- c("Verrucomicrobia unclass",
            "Acidobacteria Gp3 unclass",
            "Pseudomonadales",
            "Bacteria unclass",
            "Myxococcales")

ki <- k + scale_x_discrete(labels= H_labs)


ki + theme_bw() +
  theme(axis.text.x=element_text(angle=-90, size=12, hjust=0, vjust=0, colour="black"), 
        axis.text.y=element_text(hjust=1, size=14, vjust=0.4, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=15, colour="black")) +
  labs(fill="  Growth Stage\n", y = "Relative abundance\n") +
  theme(legend.title =element_text(size=15, colour="black"),
        legend.text = element_text(size=14, colour="black"), 
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"), # remove white from panel tops
        legend.key.size = unit(2, "cm"))




#### plotting ripening

k <- ggplot(yy_R, aes(x=OTU,y=Abundance, fill=GrowthStage)) +
  geom_boxplot(colour="black")
k


R_labs <- c("Flavobacteriales",
            "Myxococcales",
            "Acidobacteria Gp3 unclass",
            "Verrucomicrobiales",
            "Myxococcales",
            "Pseudomonadales",
            "Myxococcales",
            "Myxococcales",
            "Myxococcales",
            "Bacteroidetes unclass",
            "Bacteroidetes unclass",
            "Bacteroidetes unclass",
            "Flavobacteriales",
            "Myxococcales",
            "Myxococcales"
)

ki <- k + scale_x_discrete(labels= R_labs)


ki + theme_bw() +
  theme(axis.text.x=element_text(angle=-90, size=12, vjust=0, hjust=0, colour="black"), 
        axis.text.y=element_text(hjust=1, size=14, vjust=0.4, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=15, colour="black")) +
  labs(fill="  Growth Stage\n", y = "Relative abundance\n") +
  theme(legend.title =element_text(size=15, colour="black"),
        legend.text = element_text(size=14, colour="black"), 
        strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
        strip.background =  element_rect(fill = "white"), # remove white from panel tops
        legend.key.size = unit(2, "cm"))



