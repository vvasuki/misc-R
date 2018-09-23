className <- "ProbabilisticModels_DiscreteGM.R";
ProbabilisticModels_DiscreteGM <- list();

ProbabilisticModels_DiscreteGM$makeEdgeStruct<-function(adj, nStates){

#edgeEnds = adjacency list (nEdges x 2 matrix)
#deg = max degree of graph
#nNodes = number of nodes
    makeEdgeVE<-function(edgeEnds, deg, nNodes){
        V<-c()
        E<-c()

        nEdges = length(edgeEnds[,1])
        nNei<-matrix(0,nNodes,1)
        nei<-matrix(0,nNodes,deg)
        for(e in 1:nEdges){
            n1 = edgeEnds[e,1]
            n2 = edgeEnds[e,2]
            nNei[n1]=nNei[n1]+1
            nNei[n2]=nNei[n2]+1
            nei[n1,nNei[n1]]=e
            nei[n2,nNei[n2]]=e
        }

        edge=1
        for(n in 1:nNodes){
            V[n]=edge
            for(i in 1:length(nei[n,])){
                if(nei[n,i]!=0){
                    E<-c(E,nei[n,i])
                    edge=edge+1
                }
            }
        }
        V[nNodes+1]=edge
        list(V=V,E=E)
    }


    nNodes = length(adj[1,])
#find nonzero indices
    I<-c() #row indices
    J<-c() #column indices
    adj[nNodes,1]
    nNodes
    deg<-0 #max degree
    for(i in 1:nNodes){
        tempDeg<-0
        for(j in 1:nNodes){
            if(adj[i,j] != 0){
                tempDeg=tempDeg+1
                I<-c(I,i)
                J<-c(J,j)
            }
        }
        if(tempDeg>deg){
            deg<-tempDeg
        }
    }

    nEdges = length(I)/2
    nEdges
    edgeEnds<-matrix(0, nEdges, 2)
    eNum<-1
    for( e in 1:nEdges*2 ){
        if( J[e] < I[e]){
           edgeEnds[eNum,]=c(J[e],I[e])
           eNum=eNum+1
        }
    }

    VE = makeEdgeVE(edgeEnds, deg, nNodes)

    edgeStruct<-list(edgeEnds=edgeEnds, V=VE$V, E=VE$E, nNodes, length(edgeEnds[,1]), nStates)

    edgeStruct
}

ProbabilisticModels_DiscreteGM$sample <- function(model, numSamples){

    edgeStruct <- ProbabilisticModels_DiscreteGM$makeEdgeStruct(model$AdjMatrix, model$numStates);
    y = apply(model$NodePotentials,2,max);

    burnin = 1000;

    dyn.load("UGMWrapper.so")
    Sample <- .C("getSamples", as.integer(model$numNodes), as.integer(model$numEdges), as.double(model$NodePotentials), as.double(model$EdgePotentials), as.integer(edgeStruct$edgeEnds), as.integer(model$numStates), as.integer(edgeStruct$V), as.integer(edgeStruct$E), as.integer(numSamples), as.integer(burnin), Sample = as.integer(y))$Sample;

    if(model$numStates == 2){
        Sample[Sample == 2] = -1;
    }

    # randomNumbers <- sample(c(-1, 1), model$numNodes * numSamples, replace='TRUE');
    # print(randomNumbers)
    # Sample <- array(randomNumbers, c(model$numNodes, numSamples))
    Sample
}

ProbabilisticModels_DiscreteGM$getTestIsingModel <- function(numNodes, couplingStrength){
    chainAdj<-function(n)
    {
        adj<-matrix(0,n,n)
        for(i in 1:(n-1))
        {
            adj[i,i+1]=1
            adj[i+1,i]=adj[i,i+1]
        }
        adj
    }
    model$numNodes = numNodes;
    model$numStates = 2;
    model$AdjMatrix = chainAdj(numNodes);
    model$numEdges = sum(model$AdjMatrix)/2;
    model$NodePotentials = matrix(1, nrow = model$numNodes, ncol = model$numStates);
    model$EdgeParameters = array(0, c(model$numStates, model$numStates, model$numEdges))

    for(edge in 1:model$numEdges){
        model$EdgeParameters[,,edge] = couplingStrength*array(c(1,-1,-1,1), c(model$numStates, model$numStates));
    }
    model$EdgePotentials = exp(model$EdgeParameters);
    model
}


ProbabilisticModels_DiscreteGM$test <- function(){
    print(paste(className, " loaded successfully"))
}


# while(className %in% search())
#     detach(className);
# attach(className);
