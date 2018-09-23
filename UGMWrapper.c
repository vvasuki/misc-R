/*
 *  UGMWrapper.c
 *  
 *
 *  Created by Christopher Johnson on 12/19/10.
 *  Copyright 2010 __MyCompanyName__. All rights reserved.
 *
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h> 

void getSamples(int *nodes, int *edges, double *nodePot, double *edgePot, int *edgeEnds, int *nStates, int *V, int *E, int *max, 
				int *burn, int *y, double *S){
	
	int nNodes, nEdges, maxIter, burnIn, i,n,s,e,n1,n2,Vind, maxState, sizeS[2];
	
	double *pot, z,U,u;
	
	/* Compute Sizes */
    
	nNodes = *nodes;
	nEdges = *edges;
	maxIter = *max;
	burnIn = *burn;
	
    maxState = getMaxState(nStates, nNodes);
    decrementEdgeEnds(edgeEnds,nEdges);
    decrementVector(V,nNodes+1);
    decrementVector(E,nEdges*2);
    decrementVector(y,nNodes);
	
	/* Output */
	pot = (double*)malloc(maxState * sizeof(double));
    sizeS[0] = nNodes;
    sizeS[1] = maxIter;
    
	/* Initialize to States with highest node potentials*/
	/* for(n = 0; n < nNodes; n++)
	 {
	 u = -1;
	 U = 0;
	 for(s = 0; s < nStates[n]; s++)
	 {
	 if(nodePot[n+nNodes*s] > u)
	 {
	 u = nodePot[n+nNodes*s];
	 U = s;
	 }
	 }
	 y[n] = U;
	 }
	 */
    
    for(i = 0; i < burnIn+maxIter; i++)
    {
		
        for(n = 0; n < nNodes; n++)
        {
            /* Compute Node Potential */
            for(s = 0; s < nStates[n]; s++)
            {
                pot[s] = nodePot[n + nNodes*s];
            }
            
			/* Iterate over Neighbors */
            for(Vind = V[n]; Vind < V[n+1]; Vind++)
            {
                e = E[Vind];
                n1 = edgeEnds[e];
                n2 = edgeEnds[e+nEdges];
				
                /* Multiply Edge Potentials */
                if(n == n1)
                {
					for(s = 0; s < nStates[n]; s++)
					{
                        pot[s] *= edgePot[s+maxState*(y[n2] + maxState*e)];
					}
                    
                }
                else
                {
                    for(s = 0; s < nStates[n]; s++)
                    {
                        pot[s] *= edgePot[y[n1]+maxState*(s + maxState*e)];
                    }
                }
                
            }
            
            /* Normalize */
            z = 0;
            for(s = 0; s < nStates[n]; s++)
                z = z + pot[s];
            for(s = 0; s < nStates[n]; s++)
                pot[s] /= z;
            
            /* Display */
            for(s = 0; s < nStates[n]; s++)
				
            /* Sample Discrete State */
				U = rand()/((double)RAND_MAX + 1);
            u = 0;
            for(s = 0; s < nStates[n]; s++)
            {
                u += pot[s];
                if(u > U)
                {
                    break;
                }
            }
            y[n] = s;
        }
        
        if(i >= burnIn)
        {        /* Record Sample */
            for(n = 0; n < nNodes; n++)
            {
                S[n + nNodes*(i-burnIn)] = y[n]+1;
            }
        }
    }
    
	/* Free memory */
	
	
}

int getMaxState(int *nStates,int nNodes)
{
	int n, maxState=0;
	
	for(n = 0; n < nNodes; n++)
	{
		if(nStates[n] > maxState)
			maxState = nStates[n];
	}
	return maxState;
}

void decrementEdgeEnds(int* edgeEnds,int nEdges)
{
	int e;
	for(e = 0; e < nEdges; e++)
	{
		edgeEnds[e]--;
		edgeEnds[e+nEdges]--;
	}
}

void decrementVector(int* vector, int nElements)
{
    int v;
    for(v = 0; v < nElements; v++)
        vector[v]--;
}

double absDif(double a, double b)
{
	if (a > b)
	{
		return a-b;
	}
	else
	{
		return b-a;
	}
}
