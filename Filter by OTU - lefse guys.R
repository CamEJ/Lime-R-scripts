## WP3 specific bacterial groups abundance w phyloseq 


## others can be installed using tools -> install packages on R studio
## If run into errors such as below the try this command:
## setInternet2(TRUE) 
## "Cannot open compressed file 'knitr/DESCRIPTION', probable reason 'No such file or directory'"

library("phyloseq")
library("ggplot2")
library("plyr")
library("vegan")
library("grid")
library("directlabels")
library("knitr")
library("clustsig")
library("ellipse")
library("ggthemr")
library("dplyr")
library(RColorBrewer)

# if not using WP3 colours or other ggthemr

mypal <- colorRampPalette( brewer.pal( 6 , "RdBu" ) )
mypal(25)


# define the four colours where black is control, choc4 is tmt2; slateblue is 
# treatment3 and olivedrab is treatment 4

WP3_colsA <- c("black", "chocolate4", "slateblue", "olivedrab")
# add background white (#55555)
WP3_cols <- c("#555555", WP3_colsA)

# define palette
WP3Cols <- define_palette(
  swatch = WP3_cols, # colours for plotting points and bars
  gradient = c(lower = WP3_cols[1L], upper = WP3_cols[2L]), #upper and lower colours for continuous colours
  background = "white" #defining a grey-ish background 
)

# set new col theme as current

ggthemr(WP3Cols) 

## for now on colour palette only has 4 colours
## if you want to plot sth with more than 4 colours it'll
## make an error

## Import subsampled otu matrix 

sharedsubFile = read.table('stability.an.0.03.subsample.shared')
sharedsubFile = t(sharedsubFile)
rownames(sharedsubFile) = sharedsubFile[,1]
colnames(sharedsubFile) = sharedsubFile[2,]
dim(sharedsubFile) # use this to help changing vals below
sharedsubFile = sharedsubFile[,2:48]
sharedsubFile = sharedsubFile[4:2659,]
class(sharedsubFile) <- "numeric"
head(sharedsubFile)

## Import taxonomy file 
## As it is from mothur, there are not column headers for order, family genus etc
## in the cons.taxonomy file and this must be fixed first. This is how I did it:
## read cons.taxonomy file into excel and choose semicolon as a separator so each tax level
## is therefore in its own column. then delete header 'taxonomy' and put in appropriate
## header for each column (order or family or genus etc. 
## Copy and paste into notepad and save. Then carry on: 
taxFile = read.table('HEADEDconsTax.txt', header=T, sep='\t')
rownames(taxFile) = taxFile[,1]
taxFile = taxFile[,2:8]
taxFile = as.matrix(taxFile)
head(taxFile)

## import metadata file
metaFile = read.table('DNA-metadata.txt', header=T, sep='\t')
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

dat <- psmelt(microSubRelFilt)
write.csv(dat, file='trimmed-5_PhyloseqOTU-DNAlime.csv')

yy <- read.csv("untrimmedPhyloseqOTU-DNAlime.csv", header = TRUE)

zz <- read.csv("untrimmedPhyloseqOTU-DNAlimeCOUNTS.csv", header = TRUE)

# set factor orders
# function for this saved in github R basics folder
yy[["GrowthStage"]] <- setFactorOrder(yy[["GrowthStage"]], c("Elongation", "Heading", "Ripening"))

zz[["GrowthStage"]] <- setFactorOrder(zz[["GrowthStage"]], c("Elongation", "Heading", "Ripening"))

# make sure a dframe
yy_df <- tbl_df(yy)

# make sure a dframe
zz_df <- tbl_df(zz)

require(ggplot2)
ggplot(yy_df, aes(x=Sample, y=Abundance, fill=Pylum)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~GrowthStage, scales="free") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1, size=9, vjust=0.4))

## filter for nitrifyers

#genera: Nitrosomonas, Nitrosococcus, Nitrobacter and Nitrococcus

## look Nitrosomonas
microSubRelNitrosomonas = subset_taxa(microSubRel, Genus=="Nitrosomonas(100)")
BarNitrosom <- plot_bar(microSubRelNitrosomonas, fill="Genus", title="Nitrosomonas")
## changes scale/day so cant see difference really so just use all together
BarNitrosom + facet_wrap(~SamplingTime, scales="free") + theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1, size=9, vjust=0.4))



microSubRelOTU2 = subset(yy_df, OTU=="Otu000002")

microSubRelOTU2

microSubOTU2 = subset(zz_df, OTU=="Otu000002")

microSubOTU2

k <- ggplot(microSubOTU2, aes(x=GrowthStage,y=Abundance, fill=GrowthStage)) +
  geom_boxplot()
k


k2 = k +

  
 theme_bw() +
  theme(axis.text.x=element_text(hjust=1, size=15, vjust=0.4, colour="black")) +
  theme(axis.text.y=element_text(hjust=1, size=14, vjust=0.4, colour="black")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(size=15, colour="black")) +
  labs(fill="  Growth Stage\n", y="Abundance (OTU 2)") +
  theme(legend.title =element_text(size=15, colour="black")) +
  theme(legend.text = element_text(size=14, colour="black")) +
  #guides(fill = guide_legend(override.aes = list(size=5)))
  theme(legend.key.size = unit(2, "cm"))
k2



