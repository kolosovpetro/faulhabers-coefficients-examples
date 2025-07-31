(* ::Package:: *)

BeginPackage["GithubLatexTemplateMathematicaPackage`"]

FaulhaberCoefficients::usage="Faulhaber coefficients https://arxiv.org/pdf/math/9207222 page 14."
FaulhaberSum::usage="Faulhaber's sum of odd powers https://arxiv.org/pdf/math/9207222 page 16."
SumOfOddPowers::usage="Sum of odd powers."

Begin["`Private`"]

Unprotect[Power];
Power[0|0., 0|0.] = 1;
Protect[Power];

FaulhaberCoefficients[n_, k_]:= 0;
FaulhaberCoefficients[n_, k_]:= (-1)^(n-k) * Sum[Binomial[2n, n-k-j]* Binomial[n-k+j, j] * (n-k-j)/(n-k+j) * BernoulliB[n+k+j], {j, 0, n-k}] /; 0 <= k < n;
FaulhaberCoefficients[n_, k_]:= BernoulliB[2n] /; k == n;

FaulhaberSum[p_, m_]:= 1/(2m)* Sum[FaulhaberCoefficients[m, r] * (p^2+p)^(m-r), {r, 0, m-1}];
SumOfOddPowers[p_, m_]:= Sum[k^(2m-1), {k, 1, p}];

End[ ]

EndPackage[ ]




