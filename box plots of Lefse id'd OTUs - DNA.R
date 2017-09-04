## box plots of Lefse id'd OTUs - DNA
library(ggplot2)

# read in OTU table (made in phyloseq; also has metadata and tax info)
yy <- read.csv(file='untrimmedPhyloseqOTU-DNAlime.csv', header=T )

# make sure it's a df. 
#yy_df <- as_df(yy)
# get out the OTUs identified by lefse as being differentially abundant.  

# made this list of otus using concatenate in excel to be faster with OTU==""|

microSubRelLefse = subset(yy, OTU=="Otu000086"|
                            OTU=="Otu000104"|
                            OTU=="Otu000061"|
                            OTU=="Otu000136"|
                            OTU=="Otu000037"|
                            OTU=="Otu000023"|
                            OTU=="Otu000041"|
                            OTU=="Otu000020"|
                            OTU=="Otu000002"|
                            OTU=="Otu000016"|
                            OTU=="Otu000022"|
                            OTU=="Otu000012"|
                            OTU=="Otu000003"|
                            OTU=="Otu000019"|
                            OTU=="Otu000006"|
                            OTU=="Otu000090"|
                            OTU=="Otu000111"|
                            OTU=="Otu000145"|
                            OTU=="Otu000014"|
                            OTU=="Otu000015"|
                            OTU=="Otu000025"|
                            OTU=="Otu000096"|
                            OTU=="Otu000043"|
                            OTU=="Otu000102"|
                            OTU=="Otu000008"|
                            OTU=="Otu000028"|
                            OTU=="Otu000031"|
                            OTU=="Otu000026"|
                            OTU=="Otu000005")


write.csv(microSubRelLefse, file='microSubRelLefse-DNA_ZERO.csv')

## write it out so if you need to do this again you don't need to waste
# time reading your whole otu table. 

dt <- read.table("lefse-OTUsandDiscrim.txt", header = TRUE)
head(dt) # looks like this
# OTU  Discrim
# 1 Otu000086 ripening
# 2 Otu000104 ripening
# 3 Otu000061 ripening
# 4 Otu000136 ripening
# 5 Otu000037 ripening
# 6 Otu000023 ripening

## now using this i can add an extra column to my lefse trimmed OTU table
## that i will call 'Discim' and it will say which growth phase that OTU 
# is differentially abundant in. 


pp$Discrim <- dt$Discrim[match(pp$OTU, dt$OTU)]

# fun the set factor order function
# found in github .

pp[["GrowthStage"]] <- setFactorOrder(pp[["GrowthStage"]], c("Elongation", "Heading", "Ripening"))

pp[["Discrim"]] <- setFactorOrder(pp[["Discrim"]], c("elongation", "heading", "ripening"))


# i have 3 different timepoints and therefore end up with 
# 3 diff box plots. Initially tried to plot these 3 with facet_wrapping
# by my timepoint - but, as i had a diff no of OTUs id'd per timepoint
# the boxes in each of the 3 plot were a diff width and it looked ugly.

# So i subset my data by the phase for which they are identifed as
# being differenitally abundant, and plot these 3 separately...

# subset for OTUs that were highly abundant in elongation
# based on my column that contains this info and is called 'Discrim'

yy_E = subset(pp, Discrim=="elongation")

yy_H = subset(pp, Discrim=="heading")

yy_R = subset(pp, Discrim=="ripening")




#### plotting elongation
# this will show the relative abundance of OTUs (id'd as differentially
# during the elongation phase) for all three phases so we can see what's
# actually happening. 


k <- ggplot(yy_E, aes(x=OTU,y=Abundance, fill=GrowthStage)) +
  geom_boxplot(colour="black")
k

# vector of labelling info. can leave this out if you
# want to see OTU instead of phylotype. 

E_labs <- c("Sphingobacteriales",
            "Burkholderiales",
            "Sphingobacteriales",
            "Burkholderiales",
            "Actinomycetales",
            "Sphingobacteriales",
            "Pseudomonadales",
            "Burkholderiales",
            "Verrucomicrobiae;Unclass",
            "Pseudomonadales",
            "Burkholderiales",
            "Sphingobacteriales",
            "Burkholderiales",
            "Sphingobacteriales"
)

# add the labels to your plot
ki <- k + scale_x_discrete(labels= E_labs)

# make it pretty. 

k2 <- ki + theme_bw() +
  theme(axis.text.x=element_text(angle=-90, size=13, hjust=0, vjust=0, colour="black"), 
        axis.text.y=element_text(hjust=1, size=14, vjust=0.4, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=15, colour="black")) +
  labs(fill="  Growth Stage\n", y = "Relative abundance\n") +
  theme(legend.title =element_text(size=15, colour="black"),
        legend.text = element_text(size=14, colour="black"), 
        legend.key.size = unit(2, "cm"))

#k2 + theme(axis.text.x = element_text(hjust = 0))
k2

#### plotting heading

k <- ggplot(yy_H, aes(x=OTU,y=Abundance, fill=GrowthStage)) +
  geom_boxplot(colour="black")
k


H_labs <- c("Acidobacteria_Gp4;Unclass ",
            "Planctomycetales",
            "Acidobacteria_Gp1;Unclass ",
            "Acidobacteria_Gp1;Unclass ",
            "Gemmatimonadales",
            "Bacteria;Unclassified"
)

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
            "Pseudomonadales",
            "Bacteroidetes;Unclassified",
            "Sphingobacteriales",
            "Bacteroidetes;Unclassified",
            "Verrucomicrobiales",
            "Bacteroidetes;Unclassified",
            "Bacteroidetes;Unclassified",
            "Sphingobacteriales"
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



## export all three boxplots to the same height, & adjust width
# in export to make the width of the actual boxes look the same
# then in a ppt you can put them together & get same effect as facet_wrap

