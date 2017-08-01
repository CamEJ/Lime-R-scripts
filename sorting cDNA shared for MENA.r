
# sorting cDNA shared for MENA

data <- read.table(file="table.from_biom_w_Tax2.txt", header=T, sep="\t")
head(dataT)

rownames(data) = data[,1]
colnames(sharedsubFile) = sharedsubFile[2,]
dataT = data[,2:49]    # define col no's
sharedsubFile = sharedsubFile[4:17770,] # define row no's use # from sub.sample + 1
class(dataT) <- "numeric"
head(sharedsubFile)


write.csv(data, file='cDNA-shared.csv')


dataZero = data[,c(10:17,26:33,42:49)]

head(dataZero)

write.csv(dataZero, file='cDNA-sharedZeroLime.csv')

dataHigh = data[,c(2:9,18:25,34:41)] 
head(dataHigh)

write.csv(data, file='cDNA-sharedHighLime.csv')


yy <- read.csv("cDNA-sharedZeroLime.csv", header = TRUE, row.names=1)

# Treatment1 is samples in columns 1 - 8 incl.
ElongcDNAZero = yy[,1:8]
write.csv(ElongcDNAZero, file='ElongcDNAZero.csv')


headcDNAZero = yy[,9:16]
write.csv(headcDNAZero, file='headcDNAZero.csv')

ripecDNAZero = yy[,17:24]
write.csv(ripecDNAZero, file='ripecDNAZero.csv')


## now for high lime

yy <- read.csv("cDNA-sharedHighLime.csv", header = TRUE, row.names=1)

# Treatment1 is samples in columns 1 - 8 incl.
ElongcDNAHigh = yy[,2:9]
write.csv(ElongcDNAHigh, file='ElongcDNAHigh.csv')


HeadcDNAHigh = yy[,18:25]
write.csv(HeadcDNAHigh, file='headcDNAHigh.csv')

RipencDNAHigh = yy[,34:41]
write.csv(RipencDNAHigh, file='ripecDNAHigh.csv')





##########


data <- read.table(file="HEADEDconsTax.txt", header=T)

data2 <- read.table(file="82-node-ids.txt", header=T)


zz <- merge(data2, data, by = "OTU")

write.csv(zz, file='ModuleTaxInfo_82cutoff.csv')



