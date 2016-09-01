# This script is used to perform 'Random Forest' and 'Support Vector Machine' classifications on remotely sensed data
# Both classifications are automated and run thanks to pre-processed Copernicus data ('Urban Atlas' and 'High Resolution Layers')

# Inputs : 
# a) Scene to classify (pixels or rasterized objects)
# b) Training/Validation dataset (rasterized 'Urban Atlas', 'High Resolution Layers', or a combination of both stored into a shapefile)
# Ouputs : 
# a) Binary classified scene (e.g. : 'Urban' and 'Not Urban')
# b) A quality assessment .txt file

# Author : Quentin POTEREK (quentin.poterek@etu.unistra.fr)
# Version : 07/2016

# Load required libraries
library(rgdal)
library(raster)

cat("
	\n# ------------------------------------------------------------------
	\nClassification possibilities: 
	\n1st character *_____ [Classification Type] : p (Pixel) - o (Object)
	\n2nd character _*____ [Image to classify] : P (N bands PCA) - R (Raw scene)
	\n3rd & 4th characters __**__ [Training/Validation dataset] : 10 (Urban Atlas) - 11 (HRL Imperviousness) - 12 (UA/HRL)
	\n5th character ____*_ [Sampling method] : b (Balanced) - u (Unbalanced)
	\n6th character _____* [Classification algorithm] : R (Random Forest) - S (Support Vector Machine)
	\n# ------------------------------------------------------------------
	\n")

print("Be careful when typing the 6-characters classification code, this program is case sensitive.")
classification <- readline(prompt="Enter classification code: ")
cat("Classification code:", classification, "\n")

# ----------------------------------------------------------------------------------------------------
# Import necessary data sets (scene, training and validation data) based on the classification code
# ----------------------------------------------------------------------------------------------------

# INFO
print("- Running process: Load required data for the land cover classification -")

# The brick() function takes a multiband raster as input. Use raster() for one band rasters.

if (substr(classification,1,1)=="p") {
	if (substr(classification,2,2)=="P") {
		scene <- brick("C:/Users/Quentin/Desktop/classifications/input/ACP.tif")
	} else if (substr(classification,2,2)=="R") {
		scene <- brick("C:/Users/Quentin/Desktop/classifications/input/BRUT.tif")
	} else {
		stop("The code is incorrect. Should be '_P____' (PCA) or '_R____' (Raw scene).")
	}
} else if (substr(classification,1,1)=="o") {
	if (substr(classification,2,2)=="P") {
		scene <- brick("C:/Users/Quentin/Desktop/classifications/input/SegmentsStats_ACP.tif")
	} else if (substr(classification,2,2)=="R") {
		scene <- brick("C:/Users/Quentin/Desktop/classifications/input/SegmentsStats_Brut.tif")
	} else {
		stop("The code is incorrect. Should be '_P____' (PCA) or '_R____' (Raw scene).")
	}
} else {
	stop("The code is incorrect. Should be 'p_____' (Pixel) or 'o_____' (Object).")
}
names(scene) <- c(paste0("band",1:nbands(scene),coll=""))

# Import a shapefile of the area of interest (e.g. Strasbourg) later used as a mask
strasbg <- shapefile("C:/Users/Quentin/Desktop/classifications/input/StrasbgEmprise_20110627_32632.shp")
scene <- mask(scene,strasbg)

# Import the data set used for training and validation
# Set a sample size based on the dataset used for the classification
if (substr(classification,3,4)=="10") {
	# Uncomment the code below if a shapefile is used as input
	## classes.tmp <- shapefile("C:/Users/Quentin/Desktop/classifications/input/UA2012.shp") 
	## classes.tmp <- rasterize(classes.tmp, scene, field='CODE', progress="text")
	classes.tmp <- raster("C:/Users/Quentin/Desktop/classifications/input/UA2012.tif")
	## sample.size <- 0.05 # Can be changed, but is set accordingly to the characteristics of the 2012 version of Urban Atlas
} else if (substr(classification,3,4)=="11") {
	classes.tmp <- raster("C:/Users/Quentin/Desktop/classifications/input/HRL_I.tif")
	classes.tmp <- classes.tmp/100
	# Reclassify the 'HRL Imperviousness' layer
	# According to 'Copernicus' metadata, any cell containing a value superior to 0 is meant to be Urban
	classes.tmp[classes.tmp>0] <- 1
	## sample.size <- 0.05 # Can be changed, but is set accordingly to the 2012 version of HRL Imperviousness
} else if (substr(classification,3,4)=="12") {
	# Uncomment the code below if a shapefile is used as input
	classes.tmp <- shapefile("C:/Users/Quentin/Desktop/classifications/input/UA2012_HRL.shp") 
	classes.tmp <- rasterize(classes.tmp, scene, field='CODE', progress="text")
	## classes.tmp <- raster("C:/Users/Quentin/Desktop/classifications/input/UA2012_HRL.tif")
	## sample.size <- 0.05 # Can be changed, but is set accordingly to the 2012 version of Urban Atlas and HRL Imperviousness
} else {
	stop("The code is incorrect. Should be '__10__' (Urban Atlas), '__11__' (HRL) or '__12__' (Mixed set UA/HRL).")
}
classes <- focal(classes.tmp, w=matrix(1, nrow=3, ncol=3), fun=mean)
classes <- mask(classes,strasbg)
names(classes) <- "class"
rm(classes.tmp)

# Create a mask of the scene, based on the training/validation dataset
sceneMask <- mask(scene, classes)

# INFO
print("- Running process: Merge input data sets into one layer -")

# Create a large dataset containing PCA layers and the rasterized land cover polygons
trainingBrick <- addLayer(sceneMask, classes)
rm(sceneMask, classes)

# Create a table containing PCA values and land cover for each pixel in the training dataset
# na.omit() allows to delete NA values in the table, generated when using getValues() on multiband data
valueTable <- getValues(trainingBrick)
valueTable <- na.omit(valueTable)
valueTable <- as.data.frame(valueTable)
valueTable$class <- factor(valueTable$class, levels=c(min(valueTable$class):max(valueTable$class)))
rm(trainingBrick)

# ----------------------------------------------------------------------------------------------------
# Subset and sample the Training/Validation dataset according to the land cover classes 
# ----------------------------------------------------------------------------------------------------

# INFO
print("- Running process: Create the training dataset based on a random sampling -")

# For reproducible results and similar sampling from a session to another, uncomment the set.seed() function in the code below

# Set a sample size based on the dataset used for the classification
# Do NOT uncomment, except if the sample size has not been already been set (see lines 39-58) 
## sample.size <- 0.05

# Create the training dataset
urban <- subset(valueTable, class==1)
notUrban <- subset(valueTable, class==0)
if (substr(classification,5,5)=="b") {
	sample.size <- 0.05
	set.seed(0)
	urbanSample <- urban[sample(1:nrow(urban), nrow(urban)*sample.size,replace=FALSE),]
	set.seed(0)
	notUrbanSample <- notUrban[sample(1:nrow(notUrban), nrow(urban)*sample.size,replace=FALSE),]
} else if (substr(classification,5,5)=="u") {
	sample.size <- 0.025
	set.seed(0)
	urbanSample <- urban[sample(1:nrow(urban), nrow(urban)*sample.size,replace=FALSE),]
	set.seed(0)
	notUrbanSample <- notUrban[sample(1:nrow(notUrban), nrow(notUrban)*sample.size,replace=FALSE),]
} else {
	stop("The code is incorrect. Should be '____b_' (balanced sampling) or '____u_' (unbalanced sampling).")
}

# Create the validation dataset used for the SVM classification
if (substr(classification,6,6)=="S") {
	if (substr(classification,5,5)=="b") {
		sample.size <- 0.05
		set.seed(1)
		urbanVerif <- urban[sample(1:nrow(urban), nrow(urban)*sample.size,replace=FALSE),]
		set.seed(1)
		notUrbanVerif <- notUrban[sample(1:nrow(notUrban), nrow(urban)*sample.size,replace=FALSE),]
		valueVerif <- rbind(urbanVerif, notUrbanVerif)
		rm(urbanVerif, notUrbanVerif)
	} else if (substr(classification,5,5)=="u") {
		sample.size <- 0.025
		set.seed(1)
		urbanVerif <- urban[sample(1:nrow(urban), nrow(urban)*sample.size,replace=FALSE),]
		set.seed(1)
		notUrbanVerif <- notUrban[sample(1:nrow(notUrban), nrow(notUrban)*sample.size,replace=FALSE),]
		valueVerif <- rbind(urbanVerif, notUrbanVerif)
		rm(urbanVerif, notUrbanVerif)
	} else {
		stop("The code is incorrect. Should be '____b_' (balanced sampling) or '____u_' (unbalanced sampling).")
	}
} else if (substr(classification,6,6)=="R") {
	print("No validation dataset is required for the Random Forest classification. Check source: ***")
} else {
	stop("The code is incorrect. Should be '_____R' (Random Forest) or '_____S' (Support Vector Machine).")
}

valueSample <- rbind(urbanSample, notUrbanSample)
rm(urban, notUrban, urbanSample, notUrbanSample)

print(paste("The sample size is set to:", sample.size))

# ----------------------------------------------------------------------------------------------------
# Perform the land cover classification - Random Forest (RF) or Support Vector Machine (SVM)
# ----------------------------------------------------------------------------------------------------

# Create a file containing all the informations dealing with current classification
# sink() allows to write lines in the file
file <- paste("C:/Users/Quentin/Desktop/classifications/results/quality/",classification,".txt",sep="")
sink(file,append=TRUE)
cat("Classification code:", classification, "\n")
cat("The sample size is set to:", sample.size, "\n")
sink()

# INFO
print("- Running process: Perform the classification -")

if (substr(classification,6,6)=="R") {
	library(randomForest)
	modelRF <- randomForest(x=valueSample[,c(1:(ncol(valueSample)-1))], y=valueSample$class,importance = TRUE)
	modelPred <- predict(scene, model=modelRF, na.rm=TRUE)

	# Print a confusion matrix and the OOB error for the Random Forest classification
	# Class 1 [raster value = 0] : Not Urban | Class 2 [raster value = 1] : Urban
	colnames(modelRF$confusion) <- c(paste("Class", 1:nrow(modelRF$confusion)), "Error")
	rownames(modelRF$confusion) <- c(paste("Class", 1:nrow(modelRF$confusion)))
	sink(file,append=TRUE)
	cat("Confusion matrix for the classification: \n")
	print(modelRF$confusion)
	sink()
	# OOB error : In random forests, there is no need for cross-validation or a separate test set to get an unbiased estimate of the test set error
	# Source : http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
	oobError <- sum(modelRF$confusion[,ncol(modelRF$confusion)])/nrow(modelRF$confusion)
	sink(file,append=TRUE)
	cat("OOB error:",oobError, "\n")
	sink()

	confusion.tab <- modelRF$confusion[,-ncol(modelRF$confusion)]
} else if (substr(classification,6,6)=="S") {
	library(e1071)
	modelSVM <- svm(class ~ ., data=valueSample, kernel="linear")
	# Comment out the line above and uncomment the line below if the best fitting model is required (WARNING: slow computation), otherwise, keep the basic classification
	## modelSVM <- best.svm(class ~ ., data=valueSample, kernel="linear", progress="text")
	
	# Print a confusion matrix and the Overall Accuracy for the Support Vector Machine classification
	# Class 1 [raster value = 0] : Not Urban | Class 2 [raster value = 1] : Urban
	svmPred <- predict(modelSVM, valueVerif[,c(1:(ncol(valueVerif)-1))])
	confusion.tab <- table(pred = svmPred, true = valueVerif[,ncol(valueVerif)])
	colnames(confusion.tab) <- c(paste("Class",1:nrow(confusion.tab)))
	rownames(confusion.tab) <- c(paste("Class",1:nrow(confusion.tab)))
	sink(file,append=TRUE)
	cat("Confusion matrix for the classification:\n")
	print(confusion.tab)
	sink()
	accuracy <- (sum(confusion.tab[1:nrow(confusion.tab),])-(sum(confusion.tab[1:nrow(confusion.tab),])-sum(diag(confusion.tab))))/sum(confusion.tab[1:nrow(confusion.tab),])
	sink(file,append=TRUE)
	cat(paste("Accuracy:", accuracy,"\n"))
	sink()
	
	# Perform the SVM classification
	scene.df <- data.frame(getValues(scene))
	names(scene.df) <- names(scene)
	## svmPred <- predict(modelSVM, scene.df)
	svmPred <- predict(modelSVM, scene.df[!rowSums(is.na(scene.df)),])

	# Create the classification map and fill its pixels with predicted values created by the SVM algorithm
	modelPred <- raster(scene)
	## values(modelPred) <- svmPred
	modelPred[!is.na(modelPred)] <- svmPred
	# Correct raster values
	modelPred <- modelPred-1
} else {
	stop("The code is incorrect. Should be '_____R' (Random Forest) or '_____S' (Support Vector Machine).")
}

# Perform and print the quality assessment
falsePositives <- falseNegatives <- falsePositivesPerc <- falseNegativesPerc <- numeric(0)
for (i in 1:nrow(confusion.tab)) { falsePositives[i] <- rowSums(confusion.tab[,])[i]-confusion.tab[i,i] }
for (i in 1:nrow(confusion.tab)) { falseNegatives[i] <- colSums(confusion.tab[,])[i]-confusion.tab[i,i] }
for (i in 1:nrow(confusion.tab)) { falsePositivesPerc[i] <- falsePositives[i]/rowSums(confusion.tab[,])[i]*100 }
for (i in 1:nrow(confusion.tab)) { falseNegativesPerc[i] <- falseNegatives[i]/colSums(confusion.tab[,])[i]*100 }
names(falsePositives) <- names(falseNegatives) <- names(falsePositivesPerc) <- names(falseNegativesPerc) <- rownames(confusion.tab)
falsePN.df <- data.frame(falsePositives, falsePositivesPerc, falseNegatives, falseNegativesPerc)
names(falsePN.df) <- c("False positives", "False positives (%)", "False negatives", "False negatives (%)")
rm(falsePositives, falsePositivesPerc, falseNegatives, falseNegativesPerc)
sink(file,append=TRUE)
cat("False negatives and false positives data frame:\n")
print(falsePN.df)
sink()

userAccuracy.vector <- producerAccuracy.vector <- diag(confusion.tab)
for (i in 1:nrow(confusion.tab)) { userAccuracy.vector[i] <- userAccuracy.vector[i]/sum(confusion.tab[i,1:ncol(confusion.tab)]) }
for (i in 1:nrow(confusion.tab)) { producerAccuracy.vector[i] <- producerAccuracy.vector[i]/sum(confusion.tab[,i]) }
sink(file,append=TRUE)
cat("User Accuracy:",paste("Class",1:length(userAccuracy.vector),userAccuracy.vector),"\n")
cat("Producer Accuracy:",paste("Class",1:length(producerAccuracy.vector),producerAccuracy.vector),"\n")
sink()

fValue <- (2*mean(userAccuracy.vector)*mean(producerAccuracy.vector)/(mean(userAccuracy.vector)+mean(producerAccuracy.vector)))
sink(file,append=TRUE)
cat("F-value:", fValue,"\n")
sink()

# Comment out when a mask of the city is not necessary
## modelPred <- mask(modelPred,strasbg)

lancCoverMap <- writeRaster(modelPred, filename=paste("C:/Users/Quentin/Desktop/classifications/results/",classification,".tif", sep=""), format="GTiff", overwrite=TRUE)

# Show the quality assessment file
file.show(file)

# Comment out to not clear the workspace
rm(list = ls(all.names = TRUE))
gc()

# INFO
print("- Done -")