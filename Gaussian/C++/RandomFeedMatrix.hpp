#pragma once
#include "RandomFeedMatrix.hpp"
#include "Array.hpp"
void RandomFeedMatrix(Array<double,2>& M,rando& R)
{
    int n=M.maxs(0), m=M.maxs(1);
    for(int i=0;i<n;i++)
        for(int j=0;j<m;j++)
            M(i,j)= R.fv();
}
