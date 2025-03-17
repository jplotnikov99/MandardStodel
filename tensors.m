(* ::Package:: *)

e1={};
e2={};
e3={};
e4={};
g={{1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1}};


SetBasis[arg_]:=
Block[{},
	If[arg=="R4",
		e1={1,0,0,0};
		e2={0,1,0,0};
		e3={0,0,1,0};
		e4={0,0,0,1};		
	]
]


Rank1LT[ind_,comp_,cov_,symb_]:={index->ind,components->comp,covariant->cov,symbol->symb};


CheckOperator[expr__]:=Block[{full},
	full=ToString[FullForm[expr]];
	If[StringContainsQ[full,"CirclePlus"],Return["+"]];
	If[StringContainsQ[full,"CenterDot"],Return[""]];
	If[StringContainsQ[full,"CircleTimes"],Return["\[CircleTimes]"]];
	Return[Null];
]


Symbolic[expr__,cur_]:=Block[{res=cur,len,cov,ind,symb,operator},
	If[expr[[1,1]]===index,len=1,len=Length[expr]];
	Do[
		cov=covariant/.expr[[it]];
		ind=index/.expr[[it]];
		symb=symbol/.expr[[it]];
		If[it!=1,
			operator=CheckOperator[expr[[{it-1,it}]]],
			operator="";
		];
		If[cov,
			res=res<>operator<>ToString[Superscript[symb,ToString[ind]],FormatType->StandardForm],
			res=res<>operator<>ToString[Subscript[symb,ToString[ind]],FormatType->StandardForm]
		];
		Print[res];
	,{it,len}];
	Return[res];
]


CenterDot[x__,y__]:=
Block[{ind1,ind2,c1={},c2={},res},
	ind1=index/.x;
	ind2=index/.y;
	If[ind1===ind2,
		c1=components/.x;
		c2=components/.y;
		Return[c1 . g . c2];
		,
		Print["The Vectors ", symbol/.x, " and ", symbol/.y, " have different indices and cannot be contracted."];
		Return[Null];
	]
]


A=Rank1LT[\[Mu],{A0,A1,A2,A3},False,"A"];
B=Rank1LT[\[Mu],{A0,A1,A2,A3},True,"B"];


res=A\[CirclePlus]B\[CirclePlus]A


Symbolic[res,"L="]



