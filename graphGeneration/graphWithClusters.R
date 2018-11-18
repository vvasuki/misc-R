#source("/u/vvasuki/vishvas/work/graphTheory/graphGeneration/graphWithClusters.R")
library("igraph")
DATA_DIR="/u/vvasuki/vishvas/work/graphTheory/graphGeneration/"

numClusters=2
A = matrix(c(.5, .01, .01, .48), nrow=numClusters)

numNodes = 10000

forestFire.fwProb=0.37
forestFire.bwProb=0.32/0.37


sum_A_ii = sum(diag(A))

component <- list()

for(i in c(1:numClusters)) {
    component[[i]] <- forest.fire.game(A[i,i]*numNodes, fw.prob=forestFire.fwProb, bw.factor=forestFire.bwProb)
}
graph <- as.undirected(graph.disjoint.union(component))

clusterEdges <- ecount(graph)
print(paste("clusterEdges: ", clusterEdges))
totalEdges <- clusterEdges * sum(A)/ sum_A_ii

cumVerteces = cumsum(diag(A))*numNodes
for(clusterA in c(2:numClusters)){
for(clusterB in c(1:(clusterA-1))){
    interclusterEdges <- round(totalEdges*A[clusterA, clusterB], 0)
    clusterAstart <- round(cumVerteces[clusterA-1]+1,0)
    clusterAend <- clusterAstart + vcount(component[[clusterA]])-1
    clusterBstart <- 1
    if(clusterB>1)
        clusterBstart = round(cumVerteces[clusterB-1]+1,0)
    clusterBend <- clusterBstart + vcount(component[[clusterB]])-1
    print(paste("clusterAstart: ", clusterAstart))
    print(paste("clusterBstart: ", clusterBstart))
    print(paste("clusterAend: ", clusterAend))
    print(paste("clusterBend: ", clusterBend))
    startVerteces <- sample(c(clusterAstart:clusterAend), interclusterEdges, replace=TRUE)
    endVerteces <- sample(c(clusterBstart:clusterBend), interclusterEdges, replace=TRUE)
    graph = add.edges(graph, c(rbind(startVerteces, endVerteces)))
}}

graphUndir <- as.undirected(graph)
clusters(graphUndir)
is.connected(graphUndir, "strong")


d <- degree(graphUndir, mode="in")
dd <- degree.distribution(graphUndir, mode="in", cumulative=TRUE)
degDistrGraphFile = paste(DATA_DIR,"degDistr.png")
alpha <- power.law.fit(d, xmin=20)
png(degDistrGraphFile)
plot(dd, log="xy", xlab="degree", ylab="cumulative frequency",
     col=1, main="")
lines(10:500, 10*(10:500)^(-coef(alpha)+1))
dev.off()

adjMatrix = get.adjacency(graph, type="both", sparse=TRUE)
adjMatrixFile=paste(DATA_DIR,"graphGeneration")
# writeMat(adjMatrixFile, adjMatrix=adjMatrix )
# plot(graph)
# get.shortest.paths(graphUndir, 1, 8000)

