origData <- read.csv('C:\\Users\\Sujay\\Desktop\\ML with R\\800770272_T_ONTIME.csv', sep=",", header = TRUE, stringsAsFactors = FALSE)

nrow(origData)

airports <- c('ATL', 'LAX', 'ORD', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')

origData <- subset(origData, DEST %in% airports & ORIGIN %in% airports)

nrow(origData)

head(origData, 2) # Display the first 2 rows from the new data frame

tail(origData, 2) # Display the last 2 rows from the new data frame

origData$x <- NULL # We can remove the column from a data frame by setting its value to NULL

head(origData)

# Check if any 2 attributes are correlated by using the cor() function
cor(origData[c("ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_AIRPORT_ID")])

cor(origData[c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")])

# Since they do not provide any valuable/changing data, we drop them
origData$ORIGIN_AIRPORT_SEQ_ID <- NULL
origData$DEST_AIRPORT_SEQ_ID <- NULL

mismatched <- origData[origData$CARRIER != origData$UNIQUE_CARRIER]
# Find the number of mismatched rows between CARRIER and UNIQUE_cARRIER
# Since its 0 drop them
origData$UNIQUE_CARRIER <- NULL

head(origData, 2)

# During import, from DOT website to .zip it may add an empty string if
# no data is present, otherwise R amy add NA field if no data
# We need to start droppinig these rows

onTimeData <- origData[!is.na(origData$ARR_DEL15) & origData$ARR_DEL15!="" & !is.na(origData$DEP_DEL15) & origData$DEP_DEL15!="",]

# There may be some fields within the original data set or the data set after importing into R
# where the field may indicate an integer value, but is present as a string
# We need to convert this string data to integer values. This is done using as.integer().
onTimeData$DISTANCE <- as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED <- as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED <- as.integer(onTimeData$DIVERTED)
onTimeData$ARR_DEL15 <- as.integer(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15 <- as.integer(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID <- as.integer(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID <- as.integer(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DEST <- as.integer(onTimeData$DEST)
onTimeData$ORIGIN <- as.integer(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK <- as.integer(onTimeData$DEP_TIME_BLK)
onTimeData$CARRIER <- as.integer(onTimeData$CARRIER)

# Dowload and install the Caret Package
install.packages('caret')

# Load the Caret package for use
library(caret)

# Set the seed to a number. This is to fix the random number sequence used in the algorithm
set.seed(122515)

# Keep only the necessary features within your data frame
featureCols <- c("ARR_DEL15", "CARRIER", "DEST", "ORIGIN", "DEP_TIME_BLK")

# Create a subset of the original onTime data using featureCols
onTimeDataFiltered <- onTimeData[,featureCols]

# Splitting the data into training and testing data and tell the method createDataPartition()
# what proportion it must split the data in
inTrainRows <- createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.70, list=FALSE)

# Looking at the indices using the head() function
head(inTrainRows, 10)

# Now to take partition the data into training data using tthe inTrainRows
trainDataFiltered <- onTimeDataFiltered[inTrainRows]

# Now to obtain the remaining data that is the testing data
testDataFiltered <- onTimeDataFiltered[-inTrainRows]

# Check if the data has been split appropriately by calculating the proportions of each of the data
nrow(trainDataFiltered)/nrow((testDataFiltered) + nrow(trainDataFiltered))

# Check if the data has been split appropriately by calculating the proportions of each of the data
nrow(testDataFiltered)/nrow((testDataFiltered) + nrow(trainDataFiltered))

# Use the logistic regression algorithm to create a model
logisticRegmodel <- train(ARR_DEL15 ~ ., data = trainDataFiltered, method = "glm",
                          family = "binomial")
