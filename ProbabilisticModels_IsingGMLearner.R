className <- "ProbabilisticModels_IsingGMLearner.R"
ProbabilisticModels_IsingGMLearner <- new.env()

ProbabilisticModels_IsingGMLearner$isingModelFromSamples<- function(Samples){
    # xRange <- unique(Samples);
    # assert(min(xRange ) == -1 && numel(xRange) == 2, 'Improper Samples$');
    numNodes <- dim(Samples)[1];
    numSamples <- dim(Samples)[2];
    EdgeCouplings <- matrix(0,nrow=numNodes,ncol=numNodes);


    getLogisticRegressionInput <- function(node, Samples){
        numNodes = dim(Samples)[1];
        y = t(Samples[node,]);
        X = Samples[-node,];
        returnList = list(y = y, X = X);
    }

    thresholdingFn <- function(v, node) {
        v[abs(v)< max(v)/3] = 0;
    }

    learnNodeNbd <- function(node){
        logRegInput <- getLogisticRegressionInput(node, Samples);
        returnList <- cv.glmnet(t(logRegInput$X), logRegInput$y, family = "binomial");
        lambda.min = returnList$lambda.min;
        print(lambda.min)
        returnList <- returnList$glmnet.fit;
        # print(returnList)
        # browser()
        beta <- returnList$beta[,returnList$lambda == lambda.min];
        # Do thresholding as optimization problems is not solved to complete accuracy
        beta <- thresholdingFn(beta, node);
        # As beta corresponds to 2*EdgeCouplings, we do the following:
        beta <- beta/2;
        EdgeCouplings[node, -node] <<- beta;
        EdgeCouplings[node, node] <<- returnList$a0[returnList$lambda == lambda.min];

    }
    
    for(node in 1:numNodes){
        learnNodeNbd(node);
    }

    # Symmetrification:
    EdgeCouplings <- 0.5*(EdgeCouplings + t(EdgeCouplings));

    AdjMatrix <- (EdgeCouplings != 0);
    AdjMatrix <- AdjMatrix - diag(diag(AdjMatrix));
    numEdges <- sum(AdjMatrix)/2;
    numStates <- 2;
    model <- list();
    model$NodePotentials <- matrix(1, nrow=numNodes, ncol=numStates);
    model$AdjMatrix <- AdjMatrix;
    model$EdgeParameters <- EdgeCouplings;
    # model <- DiscreteRVGraphicalModel$fillIsingModelProperties(model);
	model
}

ProbabilisticModels_IsingGMLearner$getStructureRecoveryRate<- function(numNodes, topology){
    actualModel <- ProbabilisticModels_DiscreteGM$getTestIsingModel(numNodes, 3);
    print(actualModel)
    experimentSettings <- list();
    experimentSettings$numNodes <- numNodes;
    experimentSettings$topology <- topology;
    experimentSettings$sampleSizeRange <- seq(50, 100, 1000);
    experimentSettings$numSampleSets <- 15;

    checkStructureRecovery <- function(model){
        all(model$AdjMatrix | actualModel$AdjMatrix)
    }
    successRate = rep(0, length(experimentSettings$sampleSizeRange));
    for(sampleSize in experimentSettings$sampleSizeRange){
        numSuccesses = 0
        for(i in 1:experimentSettings$numSampleSets){
            Sample = ProbabilisticModels_DiscreteGM$sample(actualModel, sampleSize);
            model = ProbabilisticModels_IsingGMLearner$isingModelFromSamples(Samples);
            if(checkStructureRecovery(model))
                numSuccesses = numSuccesses + 1;
        }
        successRate(experimentSettings$sampleSizeRange == sampleSize) = numSuccesses/experimentSettings$numSampleSets;
    }
    plot(experimentSettings$sampleSizeRange, successRate);
}

ProbabilisticModels_IsingGMLearner$test <- function(){
    print(paste(className, " loaded successfully"))
}

