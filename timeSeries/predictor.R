setwd("/home/vvasuki/vishvas/work/statistics/timeSeries/")
# source("/home/vvasuki/vishvas/work/statistics/timeSeries/predictor.R")

fileName <- "training.csv";
csvData <- read.csv(fileName, header = FALSE, stringsAsFactors = FALSE);

# INPUTS:
#     locationIds: Locations we are interested in.
# OUTPUT: values occuring on a certain day in past years.
getOldData <- function(data = csvData, date, locationIds, curYear = "2010")
{
    monthDayStr <- format(date, "%m-%d");
    dayMatchFn <- function(potentialDateStr, dateToMatch = date, curYearTmp = curYear){
        potentialDate = as.Date(potentialDateStr,format="%Y-%m-%d");
        (format(potentialDate, "%m-%d") == monthDayStr) & (format(potentialDate, "%Y") < curYearTmp)
    }
    oldDates = sapply(data[1], dayMatchFn)
    oldData <- data[oldDates, locationIds]
    numYears = sum(oldDates)
    # print(paste("Num entries: ", as.character(sum(oldDates))))
    oldData
}

# INPUTS:
#       predictorFn: A function which operates on measurements from previous years.
# OUTPUT: predictions for the given year.
predictorWholeYear <- function(startDateStr = "2010-01-01", data = csvData, predictorFn) {
    date = as.Date(startDateStr,format="%Y-%m-%d")-1;
    curYear = format(date+1, "%Y")
    locationIds = 2:ncol(data);
#     locationIds = 2:3;
    predictedValues = data.frame(matrix(nrow = 365, ncol = max(locationIds)));
    for(day in 1:365) {
        date = date + day;
        predictedValues[day, 1] <- format(date, format="%Y-%m-%d");
        oldData = getOldData(date = date, locationIds = locationIds, curYear = curYear)
        values <- predictorFn(oldData)
        # browser();
        predictedValues[day, locationIds] <- values;
    }
    predictedValues
}

#OUTPUT: the mean squared error.
errorChecker <- function(predictedValues, data=csvData){
    print("Error computation")
    locationsSelector = 2:ncol(predictedValues);
    startDate = predictedValues[1, 1];
    endDate = predictedValues[nrow(predictedValues), 1];
    
    actualValues= data[(data[1]>=startDate) & (data[1]<=endDate), locationsSelector];
    predictedValues = predictedValues[, locationsSelector]
    # print(cbind(actualValues, predictedValues))
    sqError = (actualValues - predictedValues)^2;
    meanError = mean(sqError)
    print(meanError)
    meanError
}

startDateValidationStr = "2009-01-01"

## A simple model which posits that the observation on a certain day will be centered around the average of past years' observations.
getErrorSimpleAveraging <- function(startDate = startDateValidationStr){
    predictedValuesMean <- predictorWholeYear(startDate, predictorFn = Matrix::colMeans)
    error <- errorChecker(predictedValues = predictedValuesMean)
    error
}

## A general weighted averaging model.
# INPUTS:
#         X: numYears * numLocations. Observations for past years, with older years data appearing earlier.
#         weightsFn: A function which generates the weight vector to be assigned to observations from past years.
# OUTPUT: Error.
getErrorWeightedMean = function(weightsFn, startDate = startDateValidationStr) {
    predictedValues <- predictorWholeYear(startDate, predictorFn = function(X){t(weightsFn(X))%*%data.matrix(X)/sum(weightsFn(X))})
    error <- errorChecker(predictedValues = predictedValues)
    error
}

##Validation to pick the best weights from a family of harmonically decreasing weights (The weight for the observation k years old would be 1/(1+(k-1)d)). So d is the model parameter.
dChoices = seq(0.5, 1, 0.5)
harmonicWeightsFn <- function(X, d){
    numYears = nrow(X)
    1/seq(1+(numYears-1)*d, 1, -d)
}
errors = 0*dChoices;
for(d in dChoices){
    print("d loop")
    errors[dChoices == d] = getErrorWeightedMean(function(X){harmonicWeightsFn(X, d)})
}
print(errors)
dBest <- dChoices[errors == min(errors)]
weightsFnBest <- function(X){harmonicWeightsFn(X, dBest)}

#Get the final result.
startDate = "2010-01-01";
predictedValuesBest <- predictorWholeYear(startDate, predictorFn = function(X){t(weightsFnBest(X))%*%data.matrix(X)/sum(weightsFnBest(X))})

# Record output.
outputFileName = 'predictions.csv';
write.csv(predictedValuesBest, outputFileName, quote = FALSE, row.names = FALSE);
