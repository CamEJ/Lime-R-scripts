## nmds for cDNA -- aug 2016
library("phyloseq")
library("ggplot2")
library("plyr")
library("vegan")
library("grid")
library("directlabels")
library("knitr")
library("clustsig")
library("ellipse")

# import tax file with correct tax order labels
taxFile = read.table('cDNAheadedFile2.cons.taxonomy.txt', header=T, sep='\t')
rownames(taxFile) = taxFile[,1]
taxFile = taxFile[,2:8]
taxFile = as.matrix(taxFile)
head(taxFile)

## import metadata file
metaFile = read.table('cDNA_metadata.txt', header=T, sep='\t')
rownames(metaFile) = metaFile[,1]
metaFile = metaFile[,2:3]
head(metaFile)

# import nmds
mothurNMDS <- as.data.frame(read.table("stability.an.thetayc.0.03.lt.ave.nmds.axes", header=T))

mothurNMDSspearman <- as.data.frame(read.table("stability.an.0.03.subsample.spearman.corr.axes", header=T))

## sort row names of each
rownames(mothurNMDS) <- mothurNMDS[,1] 
rownames(mothurNMDSspearman) <- mothurNMDSspearman[,1]

## merge nmds file and metadata file for labelling plot
mothurNMDSmeta <- merge(mothurNMDS, metaFile, by = "row.names")
rownames(mothurNMDSmeta) <- mothurNMDSmeta[,1]
## use head() to see how dataframe is looking
head(mothurNMDSmeta) ## did this and had sample name in three rows so trimmed table as below
mothurNMDSmeta = mothurNMDSmeta[,3:7]



## Plots
mothurNMDSplot <- ggplot(mothurNMDSmeta, title='nMDS of cDNA samples') +
  geom_point(aes(x=axis1, y=axis2, color=GrowthStage), size=3) + 
  theme_classic() +
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) 
  #scale_color_viridis(discrete=TRUE)

mothurNMDSplot


## Plots
#mothurNMDSplot <- ggplot(mothurNMDSmeta, title='nMDS of DNA samples') +
# geom_point(aes(x=axis1, y=axis2, color=GrowthStage))
#mothurNMDSplot

p2 <- mothurNMDSplot + 
  geom_polygon(aes(x=axis1, y=axis2, fill = GrowthStage, color=NA), alpha=0.5) 






ggtitle("nMDS of DNA samples") +
  
  
  ## Modify shapes so can display the different treatments within the days
  
  # mothurNMDSmeta.shape.names = unique(mothurNMDSmeta$LimeLoad)
  # mothurNMDSmeta.shape <- 1:(length(mothurNMDSmeta.shape.names))
  # names(mothurNMDSmeta.shape) <- mothurNMDSmeta.shape.names
  # mothurNMDSmeta.shape["samples"] <- 47
  # mothurNMDSplot <- ggplot(mothurNMDSmeta, title='nMDS of cDNA with lime symbols') +
  #   geom_point(aes(x=axis1, y=axis2, color=GrowthStage, shape = LimeLoad)) +
  #   scale_shape_manual(values = mothurNMDSmeta.shape) +
#   theme(legend.key.size=unit(0.3,"cm"))
# mothurNMDSplot



## Showing which OTUS are responsible for shifting communities with arrows

## originial tutorial uses output from indicator() in mothur
## definition of indicator value: "Indicator value The degree to which a species is indicator
## of (the conditions found in) a group of sites" Legendre, 2013
## I chose ones with P<0.05 and who had highest indicator values as:
## "IV ranges from 0 (no indication) to 100 (perfect indication)."
## from Dai et al., 2006

"Otu000168", "Otu000458", "Otu000607", "Otu000813", "Otu000331", "Otu000241", "Otu000136", "Otu000266", "Otu000026", "Otu000292"

## looked in indicator.summary file and got top10 from here 
# opened in excel, ordered by indicator column biggest to smallest. Chose
## top 10 biggest (all of which had sig p values)

# this time, these are using lefse from spring 2016 analysis
top10indicator = mothurNMDSspearman[c("Otu000002", "Otu000003", "Otu000004", "Otu000029", "Otu000005", "Otu000048", "Otu000022", "Otu000006", "Otu000019", "Otu000020"),]


#top10indicator = mothurNMDSspearman[c("Otu000567","Otu002988","Otu001424","Otu003534","Otu002069","Otu000875","Otu002937","Otu004324","Otu010194", "Otu002250"),]



## "Otu000336", "Otu000168", "Otu000656", "Otu000531", "Otu000458", "Otu001222", "Otu000607", "Otu000813", "Otu000331", "Otu000769", "Otu000241", "Otu000136", "Otu000621"

arrowmatrix = top10indicator
arrowdf <- data.frame(labels = rownames(arrowmatrix), arrowmatrix)

# get taxonomic information from the original tax file
## I chose order here due to so many unclassified at genus level

arrowdf <- data.frame(labels = taxFile[rownames(arrowmatrix),"Class"], arrowmatrix)

arrowmap <- aes(xend = axis1, yend = axis2, x = 0, y = 0, alpha=0.5, 
                shape = NULL, color = NULL, label = labels)
labelmap <- aes(x = axis1, y = axis2 + 0.04, shape = NULL, 
                color = NULL, label = labels, size=1.5)

##don't know what npc stands for,some kind of default
arrowhead = arrow(length = unit(0.02, "npc"))

## make plot
mothurNMDSplot <- ggplot(mothurNMDSmeta) +
  
  geom_point(aes(x=axis1, y=axis2, color=GrowthStage)) +
  xlim(-0.5, 0.6)
  

mothurNMDSplot

## add arrow
mothurNMDSplotArrow <- mothurNMDSplot + 
  geom_segment(arrowmap, size = 0.5, data = arrowdf, color = "black",  
               arrow = arrowhead, show.legend = FALSE) + 
  geom_text(labelmap, size = 3, data = arrowdf, colour="black")

mothurNMDSplotArrow

p2 <- mothurNMDSplotArrow


Prit <- p2 +
  labs(y="Axis 2", x="Axis 1") +
  theme(axis.text.x=element_text(size=12, vjust=0.5, colour = "black"), 
        axis.text.y=element_text(size=12, vjust=0.5, colour = "black"),
        axis.title.y=element_text(size=13, vjust=1, colour="black"),
        axis.title.x=element_text(size=13, vjust=1, colour="black"),
        legend.text=element_text(size=13, vjust=0.5),
        legend.title=element_blank())+
  guides(fill = guide_legend(override.aes = list(size=10)))
Prit


# Arrow_names <- c(
#   `Alphaproteobacteria(100)` = "Alphaproteobacteria",
#   `Acidobacteria_Gp3(100)` = "Acidobacteria Gp3",
#   `unclassified(100)` = "Unclass. bacteria",
#   `Gemmatimonadetes(100)` = "Gemmatimonadetes",
#   `Planctomycetacia(100)` = "Planctomycetacia",
#   `unclassified(100)` = "Unclass. bacteria",
#   `Alphaproteobacteria(100)` = "Alphaproteobacteria",
#   `Actinobacteria(100)` = "Actinobacteria",
#   `Holophagae(100)` = "Holophagae",
#   `Acidobacteria_Gp6(100)` = "Acidobacteria Gp6")





## if you get an error somewhere here, load the top10indicator matrix to check you have entered all OTU numbers
## correctly. If you didnt, one line or row entries will come as NA

## To plot all these together go to 'putting plots together_arrow_geom_nmds' script
## also shows does change in size of text etc here. 

poo <- mothurNMDSplotArrow + geom_polygon(aes(x=axis1, y=axis2, fill = GrowthStage, color=NA), alpha=0.5) 


#### 

taxFile = read.table('taxoForArrows.txt', header=T, sep='\t')
rownames(taxFile) = taxFile[,1]
taxFile = taxFile[,2:8]
taxFile = as.matrix(taxFile)
head(taxFile)

