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


RankNLT[ind_,comp_,cov_,symb_]:={index->ind,components->comp,covariant->cov,symbol->symb};


CheckOperator[expr__]:=Block[{head},
	head=Head[expr];
	If[head==CirclePlus,Return["+"]];
	If[head==CenterDot,Return[""]];
	If[head==CircleTimes,Return["\[CircleTimes]"]];
	Return[""];
]


TensorString[expr__]:=Block[{cov=covariant/.expr,ind=index/.expr, symb=symbol/.expr},
	Assert[Length[cov]==Length[ind]];
	Do[
		If[cov[[jt]],
			symb=ToString[Superscript[symb,ToString[ind[[jt]]]],FormatType->StandardForm],
			symb=ToString[Subscript[symb,ToString[ind[[jt]]]],FormatType->StandardForm]
		];
	,{jt,Length[ind]}];
	Return[symb];
]


Symbolic[expr__,cur_]:=Block[{res=cur,operator},
	If[Head[expr]==List,Return[res<>TensorString[expr]]];
	operator=CheckOperator[expr];
	Do[
		If[it!=1,res=res<>operator];
		res=Symbolic[expr[[it]],res];
		(*res=res<>TensorString[expr[[it]]];*)
	,{it,Length[expr]}];
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


A=RankNLT[{\[Mu],\[Nu]},{{A00,A01,A02,A03},{A10,A11,A12,A13},{A20,A21,A22,A23},{A30,A31,A32,A33}},{False,True},"A"];
B=RankNLT[{\[Nu],\[Mu]},{{A00,A01,A02,A03},{A10,A11,A12,A13},{A20,A21,A22,A23},{A30,A31,A32,A33}},{False,True},"B"];


res=B\[CenterDot]A\[CirclePlus]A;


Symbolic[res,"L="]
