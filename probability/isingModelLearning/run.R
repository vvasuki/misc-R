workDir = "~/vishvas/work/software/R";
setwd(workDir)
# source("~/vishvas/work/software/R/run.R")
library("R.utils");
library("glmnet");
sourceDirectory(workDir, modifiedOnly=TRUE, pattern="P+.*[.]R$", recursive=FALSE);

model <- list();
model$numNodes <- 3;
numSamples <-10;
ProbabilisticModels_IsingGMLearner$getStructureRecoveryRate(2, 'chain')