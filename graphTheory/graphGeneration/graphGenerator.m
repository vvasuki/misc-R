function [degrees numEdges] = graphGenerator(affNwNodes)
probabilityAddingActor = 0.8;
minDegreeActor = 3;
minDegreeSociety = 5;

affiliationNW_0 = ones(minDegreeSociety,minDegreeActor);
affiliationNW_i = affiliationNW_0;
numEdges = zeros(1,1);
for i=1:affNwNodes
    bAddActor = binornd(1,probabilityAddingActor);
    if(bAddActor== 1)
        affiliationNW_i = addActor(affiliationNW_i, minDegreeActor);
        numEdges(1,i)=nnz(affiliationNW_i*affiliationNW_i');
    else
        affiliationNW_i = addSociety(affiliationNW_i, minDegreeSociety);
    end
end
socialNW=makeSocialNW(affiliationNW_i);
degrees=sum(socialNW);
% checkPowerLaw(degrees);
checkDensification(numEdges);

function checkDensification(numEdges)
loglog(numEdges);

function checkPowerLaw(degrees)
degreeRanges = min(degrees):200:max(degrees);
freq=hist(degrees, degreeRanges)
avgDegrees = degreeRanges + 100*ones(1,numel(degreeRanges))
loglog(avgDegrees, freq)

function socialNW=makeSocialNW(affiliationNW)
A=affiliationNW;
socialNW=A*A';
socialNW(socialNW > 0) = 1;

function affiliationNW = addActor(affiliationNW, minDegreeActor)
[numActors, numSocieties] = size(affiliationNW);
% Add node.
affiliationNW(numActors+1,:) = zeros(1, numSocieties);
% Pick prototype.
actorDegrees = sum(affiliationNW, 2);
prototypeActor = randp(actorDegrees,1);
% Pick random set of edges
prototypeEdges = find(affiliationNW(prototypeActor,:));
selectEdges = sampleWithoutReplacement(prototypeEdges,minDegreeActor);
% Add edges to node
for i=1:numel(selectEdges)
    affiliationNW(numActors+1,selectEdges(1,i)) = 1;
end

function affiliationNW = addSociety(affiliationNW, minDegreeSociety)
[numActors, numSocieties] = size(affiliationNW);
% Add node.
affiliationNW(:,numSocieties+1) = zeros(numActors, 1);
% Pick prototype.
societyDegrees = sum(affiliationNW, 1);
prototype = randp(societyDegrees',1);
% Pick random set of edges
prototypeEdges = find(affiliationNW(:,prototype)');
selectEdges = sampleWithoutReplacement(prototypeEdges,minDegreeSociety);
% Add edges to node
for i=1:numel(selectEdges)
    affiliationNW(selectEdges(1,i),numSocieties+1) = 1;
end
