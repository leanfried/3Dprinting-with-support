(* ::Package:: *)

(* ::Title:: *)
(*File management*)


(* ::Text:: *)
(*Functions for converting MATLAB csv files into bigtable2 and for converting bigtable2 into changetable*)
(*Last modified November 2019 by Leanne Friedrich*)


<<"Mathematica\\master.wl"


(* ::Subsubsection:: *)
(*ster*)


(* ::Text:: *)
(*standard error of list l*)


ster[l_]:=If[Length[l]>1, N[StandardDeviation[l]/Sqrt[Length[l]]], 0]


(* ::Subsubsection:: *)
(*weightedaveno0*)


(* ::Text:: *)
(*weighted average, without zeros, where N is two columns after the value to be averaged*)


weightedaveno0[l_, k_]:=Module[{l1, m, se}, 
l1 = Select[l, #[[k+2]]!=0&]; 
If[Length[l1]>0
,m = Total[l1[[;;,k]]*l1[[;;,k+2]]]/Total[l1[[;;,k+2]]];
se = Sqrt[Total[(l1[[;;,k]]-m)^2*l1[[;;,k+2]]]/Total[l1[[;;,k+2]]]];
{m , se}
,{0,0}
]]


(* ::Subsubsection:: *)
(*meanno0*)


(* ::Text:: *)
(*means, without zeros*)


meanno0[l_, k_]:=Module[{l1}, 
l1 = Select[l[[;;,k]], #!=0&]; 
If[Length[l1]>0
, {Mean[l1]- Switch[k, 48,0.3, 51, 0.6, 54, 0.3,57, 0.6, _, 0],ster[l1]}
, {0,0}
]]


(* ::Subsubsection:: *)
(*meandifno0*)


(* ::Text:: *)
(*mean change, without zeros*)


meandifno0[l_, {i_, j_}]:=Module[{l1}, 
l1 = Select[l, #[[i]]!=0 && #[[j]]!=0&]; 
If[Length[l1]>0
, {Mean[l1[[;;,i]] - l1[[;;,j]]], ster[l1[[;;,i]] - l1[[;;,j]]]}
, {0,0}]]


(* ::Subsubsection:: *)
(*combfile2bt2*)


combfile2bt2[file_]:=Module[{im1, rc, rc2, t1, vibeg, viend, vitog, inf},
im1 = Import[file];
rc = GatherBy[im1, #[[1;;10]]&];
t1 = Table[
	vibeg = removestartcorners[t];
	viend = removeendcorners[t];
	vitog = Intersection[vibeg, viend];
	inf = t[[1, 1;;10]];
	inf[[1]] = ToExpression[inf[[1]]]-800;
	Join[inf
		,If[Length[vibeg]>0, SetPrecision[Flatten[Table[weightedaveno0[vibeg, k],{k, 15, 44,3}],1],5], ConstantArray[0, 20]]
		,If[Length[vibeg]>0, SetPrecision[ Flatten[Table[meanno0[vibeg, k], {k, 45, 53}],1],5], ConstantArray[0, 18]]
		,If[Length[viend]>0, SetPrecision[ Flatten[Table[meanno0[viend, k], {k,54, 59}],1],5], ConstantArray[0, 12]]
		,If[Length[vitog]>0, SetPrecision[ Flatten[Table[meandifno0[vitog, {k+6, k}], {k, 48, 53}],1],5], ConstantArray[0, 12]]
	]
,{t, rc}]
]


(* ::Subsubsection:: *)
(*combfile2bt3*)


(* ::Text:: *)
(*convert header2 file to bigtable2 table, only select middle 2 mm*)


combfile2bt3[file_]:=Module[{im1, rc, rc2, t1, vitog, inf},
im1 = Import[file];
rc = GatherBy[im1, #[[1;;10]]&];
t1 = Table[
	vitog = Select[t, #[[4]]/2-1<#[[14]]<#[[4]]/2+1&]; (*select the middle 2mm *)
	inf = t[[1, 1;;10]];
	inf[[1]] = ToExpression[inf[[1]]]-800;
	Join[inf
		,If[Length[vitog]>0, SetPrecision[Flatten[Table[weightedaveno0[vitog, k],{k,15, 44,3}],1],5], ConstantArray[0, 20]]
		,If[Length[vitog]>0, SetPrecision[ Flatten[Table[meanno0[vitog, k], {k, 45, 53}],1],5], ConstantArray[0, 18]]
		,If[Length[vitog]>0, SetPrecision[ Flatten[Table[meanno0[vitog, k], {k,54, 59}],1],5], ConstantArray[0, 12]]
		,If[Length[vitog]>0, SetPrecision[ Flatten[Table[meandifno0[vitog, {k+6, k}], {k, 48, 53}],1],5], ConstantArray[0, 12]]
]
,{t, rc}]
]


(* ::Subsubsection:: *)
(*chchchanges*)


(* ::Text:: *)
(*list of changes of interest*)


chchchanges[g_]:=Module[{gsort, y1, y2, x1, x2},
If[Length[g]==3,
gsort = Sort[g];
(*vars, relax close 2, relax close 3, relax far 3, shear close 2, shear close 3, shear far 3*)
Flatten[{
gsort[[1, {1,2,3,4,5,6,7,8,10}]]
,Table[
	Table[
		y1 =  k[[2]]+2*i;
		y2 =  k[[4]]+2*i;
		x1 = k[[1]];
		x2 = k[[3]];
	{gsort[[x1, y1]] - gsort[[x2, y2]], Sqrt[(gsort[[x1, y1+1]])^2 +(gsort[[x2, y2+1]])^2]}
	, {i,0,2}]
	,{k, {{2,49,1,31}, {3,49, 2, 31}, {3,55, 2, 37}, {2, 37, 2, 49}, {3, 37, 3, 49}, {3, 43, 3, 55}}}]
}]
, 
ConstantArray["", Length[CHANGEHEADER]]
]
]


(* ::Subsubsection:: *)
(*constructchangetable*)


(* ::Text:: *)
(*use bigtable2 to determine changes over the 3 passes*)


constructchangetable[bigtable2_]:=Module[{g1, changetable},
g1 = GatherBy[bigtable2, #[[1;;8]]&];
changetable = Prepend[chchchanges/@Select[g1, Length[#]==3&], CHANGEHEADER];
changetable
]
