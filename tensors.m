(* ::Package:: *)

e1={};
e2={};
e3={};
e4={};


SetBasis[arg_]:=
Block[{},
	If[arg=="R4",
		e1={1,0,0,0};
		e2={0,1,0,0};
		e3={0,0,1,0};
		e4={0,0,0,1};		
	]
]


Rank1LT[ind_,comp_,cov_]:={index->ind,components->comp,covariant->cov}


A=Rank1LT[mu,{A0,A1,A2,A3},true]
