fileName <- "~/stumblerRetention/retentionDataNoMissing.csv"
tblRet <- read.csv (fileName, header=TRUE, stringsAsFactors = FALSE)
X<- as.matrix(tblRet[,names(tblRet) != "Retained"])
X <- scale(X, center = TRUE, scale = TRUE)
z <- factor(tblRet$Retained)

#library("glmnet")
#ret <- cv.glmnet(x=X, y=z, family = "binomial")

ret <- glm.fit(x=X, y=z, family=binomial())
l <- factor(as.double(ret$fitted.values > 0.5))
sum(l == z)/length(l)
fileNameOut <- "~/stumblerRetention/tmp.csv"
write.csv(ret$coefficients, fileNameOut)

library("rpart")
tblRet[,"Retained"] <- z
model <- Retained ~ S1NumStumbles + Age + S1NumThumbdowns + NumShares + HasPic + Xrated + NumComments + NumFriends + Adult + NumHits + S1NumThumbups + NumThumbdowns + InstallDevice + S1NumTopics + NumThumbups + S1Length + NumStumbles
ret <- rpart(model, tblRet)
print(ret)

fullDataFile <- "~/stumblerRetention/retention_data.csv"
tblRetFull <- read.csv (fullDataFile, header=TRUE, stringsAsFactors = TRUE)
tblRetFull["Retained"] <- factor(tblRetFull$Retained)
model <- Retained ~ Age + Gender + City + State + Country + Email + NumStumbles + NumThumbdowns + NumThumbups + Adult + Xrated + NumComments + NumHits + NumFriends + NumShares + HasPic + SignupBrowser + S1Length + S1NumStumbles + S1NumTopics + S1NumThumbups + S1NumThumbdowns + S1FirstRating + S1FirstThumbup + S1FirstThumbdown + InstallDevice + AvgUrlScore20 + NumTopics20 + NumThumbdowns20 + NumThumbups20 + NumRatings20 + HasTwoRatings20 + HasVideo20 + NumVideos20 + NumDomains20 + AvgUrlScore10 + NumTopics10 + NumThumbdowns10 + NumThumbups10 + NumRatings10 + HasTwoRatings10 + HasVideo10 + NumVideos10 + NumDomains10 + AvgUrlScore5 + NumTopics5 + NumThumbdowns5 + NumThumbups5 + NumRatings5 + HasTwoRatings5 + HasVideo5 + NumVideos5 + NumDomains5 
ret <- rpart(model, tblRetFull)
print(ret)

tblRetFull["Retained"] <- factor(tblRetFull$Retained)
model <- Retained ~ Age + NumStumbles + NumThumbdowns + NumThumbups + Adult + Xrated + NumComments + NumHits + NumFriends + NumShares + HasPic + S1Length + S1NumStumbles + S1NumTopics + S1NumThumbups + S1NumThumbdowns + InstallDevice + Gender + AvgUrlScore5 + NumTopics5 + NumThumbdowns5 + NumThumbups5 + NumRatings5 + HasTwoRatings5 + HasVideo5 + NumVideos5 + NumDomains5 + AvgUrlScore10 + NumTopics10 + NumThumbdowns10 + NumThumbups10 + NumRatings10 + HasTwoRatings10 + HasVideo10 + NumVideos10 + NumDomains10 + AvgUrlScore20 + NumTopics20 + NumThumbdowns20 + NumThumbups20 + NumRatings20 + HasTwoRatings20 + HasVideo20 + NumVideos20 + NumDomains20 + S1FirstRating + S1FirstThumbup + S1FirstThumbdown
ret <- rpart(model, tblRetFull)
print(ret)

tblRetFull <- read.csv (fullDataFile, header=TRUE, stringsAsFactors = TRUE)
nonPolynominals <- c("Age", "NumStumbles", "NumThumbdowns", "NumThumbups", "Adult", "Xrated", "NumComments", "NumHits", "NumFriends", "NumShares", "HasPic", "S1Length", "S1NumStumbles", "S1NumTopics", "S1NumThumbups", "S1NumThumbdowns", "InstallDevice", "Gender", "AvgUrlScore5", "NumTopics5", "NumThumbdowns5", "NumThumbups5", "NumRatings5", "HasTwoRatings5", "HasVideo5", "NumVideos5", "NumDomains5", "AvgUrlScore10", "NumTopics10", "NumThumbdowns10", "NumThumbups10", "NumRatings10", "HasTwoRatings10", "HasVideo10", "NumVideos10", "NumDomains10", "AvgUrlScore20", "NumTopics20", "NumThumbdowns20", "NumThumbups20", "NumRatings20", "HasTwoRatings20", "HasVideo20", "NumVideos20", "NumDomains20", "S1FirstRating", "S1FirstThumbup", "S1FirstThumbdown", "Retained")
corr <- cor(tblRetFull[, colnames(tblRetFull) %in% nonPolynominals], use="pairwise.complete.obs")
fileNameOut <- "~/stumblerRetention/tmp.csv"
write.csv(covar, fileNameOut)

nominals <- c("Retained", "Email", "HasPic", "Url1", "SignupBrowser", "Url2", "Country", "Gender", "Url3", "Url4", "HasTwoRatings5", "HasVideo5", "Url5", "HasTwoRatings10", "HasVideo10", "State", "HasTwoRatings20", "HasVideo20", "City")
for(tmp in nominals) {tblRetFull[[tmp]] <- factor(tblRetFull[[tmp]])}

interestsFileName <- "~/stumblerRetention/interests.csv"
interests <- read.csv (interestsFileName, header=TRUE, stringsAsFactors = TRUE)
interestMeans = as.matrix(lapply(interests, mean))
write.csv(interestMeans, fileNameOut)

getMI = function (x) {
  y = (interests$Retained == "1")
  M = matrix(c(sum(x & y), sum(x & !y), sum(!x & y), sum(!x & !y)), nrow=2, ncol=2, byrow=TRUE) 
  M = M / sum(M)
  prX = apply(M, 1, sum)
  prY = apply(M, 2, sum)
  fn = function(i, j) {if(M[i, j] == 0) 0 else M[i, j]*log(M[i, j]/prX[i]/prY[j], 2)}
  mi = fn(1,1) + fn(1,2) + fn(2,1) + fn(2,2)
  mi
}
getMI(interests$Retained==1)
mutualInformation = lapply(interests, getMI)
write.csv(as.matrix(mutualInformation), fileNameOut)

library("rpart")
model <- Retained ~ Humor + Bizarre.Oddities +Magic.Illusions +Quotes +Music +Alcoholic.Drinks +Psychology +Photography +Chaos.Complexity +Comedy.Movies
interests$Humor = as.factor(interests$Humor)
interests$Bizarre.Oddities = as.factor(interests$Bizarre.Oddities)
ret <- rpart(model, interests)
print(ret)

tblRetLow = tblRet[tblRet$NumStumbles<20,]
corr <- cor(tblRetLow[, colnames(tblRetLow) %in% nonPolynominals], use="pairwise.complete.obs")
write.csv(covar, fileNameOut)
