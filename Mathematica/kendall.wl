(* ::Package:: *)

(* ::Title:: *)
(*Kendall \[Tau] tests*)


(* ::Text:: *)
(*Functions for working with Kendall Tau Tests*)
(*Generally, Kendall tau tests are stored here under a 5-parameter convention:*)
(*{tau, p-value, long sign (shows magnitude), short sign (shows just the sign), bool significance}*)
(*Often, these tests are store in sets of two, for the two types of support*)
(*Last updated November 2019 by Leanne Friedrich*)


(* ::Section:: *)
(*Tests*)


(* ::Subsubsection:: *)
(*kall*)


(* ::Text:: *)
(*list = list of data*)
(*xvar = column number 1*)
(*yvar = column number 2*)
(*OUTPUT: {tau, p-value, sign w/ magnitude, short sign, bool significant}*)
(*CALLED BY: tplist (correlations.wl), tplist2 (correlations.wl), tpkvar (means.wl), initpossplit (means.wl), dpospass (means.wl), dpossplit (means.wl), finalpospass (means.wl), finalpossplit (means.wl)*)


kall[list_, xvar_, yvar_]:=Module[{t, p, s1, s2, sig},
{t, p} = SetAccuracy[KendallTauTest[list[[;;, xvar]], list[[;;,yvar]], {"TestStatistic", "PValue"}],5];
If[p<0.05
	,
	s2 = If[t>0,"+","-"];
	s1 = StringJoin[ConstantArray[s2,Ceiling[Abs[t/0.1]]]];
	sig = True;
	, 
	s2 = ""; s1 = ""; sig = False;
];
{t, p, s1, s2, sig}	
]


(* ::Subsubsection:: *)
(*constructk*)


(* ::Text:: *)
(*table of Kendall tau correlations, split by support type*)
(*table = data table*)
(*header = header for data*)
(*list = list of column numbers to compare*)
(*OUTPUT: table of correlations*)
(*CALLS: kall*)
(*CALLED BY: (corrrelations2.nb)*)


constructk[table_, header_, list_]:=Module[{ktable, ki, c, s1},
ktable = ConstantArray["", {Length[list]*Length[list]*2+1, 10}];
ki = 1;
Do[
	Do[
		If[i<j,
			c = {"",""};
			Do[
				s1 = Select[table, #[[i]]!=0 && #[[j]]!=0 && #[[3]]==m &][[;;,{i,j}]];
				ktable[[ki]] = Join[{i,j, header[[i]], header[[j]], m}, kall[s1,1,2]];
				ki = ki+1;
			,{m,{3,2}}];
		];
	,{i, list}]
, {j, list}];
Prepend[Select[ktable, NumberQ[#[[1]]]&],{"i", "j","i title", "j title",  "mode", "tau", "p", "sign", "sign+", "significant"}]
]


(* ::Subsubsection:: *)
(*constructknn*)


(* ::Text:: *)
(*this treats the nearest neighbor and second nearest neighbor as the same metric, so instead of finding a correlation between column A and B, it throws A-B pairs and A-C pairs into the same pool and finds the correlation*)
(*table = list of data*)
(*header = header for data*)
(*lists = list of pairs of column numbers*)
(*jlist = list of columns*)
(*rsheader = header for the pairs*)
(*OUTPUT: table of correlations*)
(*CALLS: kall*)
(*CALLED BY: (corrrelations2.nb)*)


constructknn[table_, header_, lists_, jlist_, rsheader_]:=Module[{ktable, ki, c, s1, rslist, rslist1, rslist2},
Catch[
If[Length[lists]!=Length[rsheader], 
	Throw["Number of lists must equal length of header"]
];
ktable = ConstantArray["", {Length[lists]*Length[jlist]*2 + Length[lists]^2*2+1, 10}];
ki = 1;
Do[
	Do[
		rslist = lists[[rsindex]];
		c = {"",""};
		Do[
			s1 = Flatten[Table[
					Select[table, #[[i]]!=0 && #[[j]]!=0 && #[[3]]==m &][[;;,{i,j}]]
					,{i, rslist}],1];
			ktable[[ki]] = Join[{j, rsindex+Length[header], header[[j]], rsheader[[rsindex]], m}, kall[s1,1,2]];
			ki = ki+1;
		,{m,{3,2}}];
	,{rsindex, Length[lists]}]
, {j, jlist}];
Do[
	Do[
		rslist1 = lists[[rsindex1]];
		rslist2 = lists[[rsindex2]];
		c = {"",""};
		Do[
			s1 = Flatten[
					Table[
						Table[
							Select[table, #[[i]]!=0 && #[[j]]!=0 && #[[3]]==m &][[;;,{i,j}]]
						,{i, rslist1}]
					, {j, rslist2}]
				,2];
			ktable[[ki]] = Join[{rsindex1+Length[header], rsindex2+Length[header], rsheader[[rsindex1]], rsheader[[rsindex2]], m}, kall[s1,1,2]];
			ki = ki+1;
		,{m,{3,2}}];
	,{rsindex1, rsindex2-1}]
,{rsindex2, Length[lists]}];
Prepend[Select[ktable, NumberQ[#[[1]]]&],{"i", "j","i title", "j title",  "mode", "tau", "p", "sign", "sign+", "significant"}]
]]


(* ::Section:: *)
(*Plots*)


(* ::Subsubsection:: *)
(*kendallcircles*)


(* ::Text:: *)
(*kendallcircles shows circles indicating the sign and significance of the tau test*)
(*kendall = {bath row, layer-by-layer row}*)
(*OUTPUT: graphics*)
(*CALLED BY: vplot (means.wl), distplot (means.wl), finalplot (means.wl)*)


kendallcircles[kendall_]:=Module[{},
	Graphics[
		{
		If[NumberQ[kendall[[1,1]]] && ToExpression[kendall[[1,5]]]
			, {Gray, Disk[{2,0}, 1], Text[Style[kendall[[1,4]], White, 15], {2,0}]}
			, {White, Disk[{2,0}, 1]}
		]
		, If[NumberQ[kendall[[1,1]]] && ToExpression[kendall[[2,5]]]
			, {Black, Circle[{0,0},1], Text[Style[kendall[[2,4]], Black, 15], {0,0}]}
			, {White, Circle[{0,0},1]}
		]
		}
	, ImageSize->40]
]


(* ::Subsubsection:: *)
(*kendallcircles2*)


(* ::Text:: *)
(*kendallcircles2 shows circles indicating the sign, magnitude and significance of the tau test*)
(*kendall = {layer-by-layer row, bath row}*)
(*OUTPUT: graphics*)
(*CALLED BY: rotatedplot4 (correlations.wl)*)


kendallcircles2[kendall_]:=Module[{s1, s2, s},
s1 = Max[StringLength[kendall[[2,3]]]/2, 0.75];
s2 = Max[StringLength[kendall[[1,3]]]/2, 0.75];
	Graphics[
		{
		If[ToExpression[kendall[[2,5]]]
			, {Gray, Disk[{s2+s1,0}, s1], Text[Style[kendall[[2,3]], White, 8*PS], {s2+s1,0}]}
			, {White, Disk[{s2+s1,0}, s1]}
		]
		, If[ToExpression[kendall[[1,5]]]
			, {Black, Circle[{0,0},s2], Text[Style[kendall[[1,3]], Black, 8*PS], {0,0}]}
			, {White, Circle[{0,0},s2]}
		]
		}
	, ImageSize->16*(s1+s2)]
]
