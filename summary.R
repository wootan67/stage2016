# This script is used to extract the F-values of all performed classifications

# Input :
# a) A .txt file containg the codes of all classifications to analyse
# Output :
# a) A .txt file containing the F-values of all performed classifications


# Author : Quentin POTEREK (quentin.poterek@etu.unistra.fr)
# Version : 07/2016

cat("If the program sends an error, please check code (lines 26-36).")

doc <- read.table("path:/to/classifications/classifications.txt")
doc.loop <- 1

while (doc.loop <= nrow(doc)) {
	foo <- paste("path:/to/classifications/YYYYMMMDD/03_results/quality/",as.character(doc[doc.loop,1]),".txt",sep="")
	classification <- readLines(foo)
	assign(as.character(doc[doc.loop,1]), classification[length(classification)])
	doc.loop <- doc.loop+1
}

rm(doc,doc.loop,foo,classification)

classification.names <- c(ls(all.names = TRUE))
classification <- cbind(classification.names)
obj.list <- lapply(classification, get)
obj.loop <- 1
while (obj.loop <= length(obj.list)) {
	# If 2 spaces separate the string and numeric characters of "F-value:  0.8127589", uncomment the code below
	obj.list[obj.loop] <- as.numeric(sapply(strsplit(as.character(obj.list[obj.loop]), "  "), "[[", 2))
	# If 1 space separates the string and numeric characters of "F-value: 0.8127589", uncomment the code below
	## obj.list[obj.loop] <- as.numeric(sapply(strsplit(as.character(obj.list[obj.loop]), " "), "[[", 2))
	obj.loop <- obj.loop + 1
}

rm(obj.loop)

df <- as.data.frame(do.call(rbind, obj.list))
df <- cbind(classification,df)
names(df) <- c("Classification", "F-Value")

file.export <- write.table(df, file = "path:/to/classifications/YYYYMMMDD/03_results/quality/summary.txt")
rm(list = ls(all.names = TRUE))