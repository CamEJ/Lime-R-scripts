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
library("ggrepel")

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



## Showing which OTUS are responsible for shifting communities with arrows


## looked in lefse.summary file and got top10 from here 
# opened in excel, ordered by p value. trimmed for > 0.01 
# then ordered via LDA column biggest to smallest. Chose
## top 10 biggest (all of which had sig p values)

top10indicator = mothurNMDSspearman[c("Otu000002", "Otu000003", "Otu000004", 
                                      "Otu000029", "Otu000005", "Otu000048", 
                                      "Otu000022", "Otu000006", "Otu000019", 
                                      "Otu000020"),]



arrowmatrix = top10indicator
top10indicator = top10indicator[,c(1,3,5)]


arrowdf <- data.frame(labels = rownames(arrowmatrix), arrowmatrix)

# get taxonomic information from the original tax file
## I chose order here due to so many unclassified at genus level

head(taxFile)

arrowdf <- data.frame(labels = taxFile[rownames(arrowmatrix),"Family"], arrowmatrix)

# for arrowmap I've divided axis1 & 2 co-ords by 2 so direction is same 
# but length is half as was squishing nmds points together loads at arrows 
# too long for plot. 
arrowmap <- aes(xend = axis1/2, yend = axis2/2, x = 0, y = 0, alpha=0.001, 
                shape = NULL, color = NULL, label = labels)
labelmap <- aes(x = axis1/2, y = axis2/2, shape = NULL, 
                color = NULL, label = labels, size=1.5)


## read out arrow df and changed by hand the names to remove (100) etc

arrOh = read.table('Arrow-df-fixedTax.txt', header=T, sep='\t', row.names=1)
# check it's same layout etc then overwrite old arrow df
arrowdf <- arrOh

##don't know what npc stands for,some kind of default
arrowhead = arrow(length = unit(0.02, "npc"))

################# And now finally to plotting ################


## make basic nMDS plot

mothurNMDSplot <- ggplot(mothurNMDSmeta) +
  
  geom_point(aes(x=axis1, y=axis2, color=GrowthStage, shape=LimeLoad), size=5) 
  
  mothurNMDSplot
  
# add polygons
mothurNMDSpolyG <- mothurNMDSplot + 
  geom_polygon(aes(x=axis1, y=axis2, fill = GrowthStage, color=NA), alpha=0.5) 
mothurNMDSpolyG # print

## add arrow
mothurNMDSplotArrowPoly <- mothurNMDSpolyG + 
  geom_segment(arrowmap, size = 0.5, data = arrowdf, color = "black",  
               arrow = arrowhead, show.legend = FALSE) + 
  geom_text_repel(labelmap, size =4.5, data = arrowdf, colour="black")

mothurNMDSplotArrowPoly


# now modify font sizes etc: 

p2 <- mothurNMDSplotArrowPoly 


Prit <- p2 +
  labs(y="Axis 2", x="Axis 1") +
  theme(axis.text.x=element_text(size=12, vjust=0.5, colour = "black"), 
        axis.text.y=element_text(size=12, vjust=0.5, colour = "black"),
        axis.title.y=element_text(size=13, vjust=1, colour="black"),
        axis.title.x=element_text(size=13, vjust=1, colour="black"),
        legend.text=element_text(size=13, vjust=0.5),
        legend.title=element_blank())+
  guides(fill = guide_legend(override.aes = list(size=10)),
         shape = guide_legend(override.aes = list(size=10)))
Prit

## add text box showing stress and R2
# might look like theyre on top of each other 
# but fine when you zoom/export. 

PritStress <-  Prit + 
  annotate("text", colour="black", size = 4.5, x=0.24, y=0.47, 
           label= "R^{2}==0.92", parse=T) +
  annotate("text", colour="black",  size = 4.5, x=0.24, y=0.5, 
           label= "Stress==0.11", parse=T)
PritStress



