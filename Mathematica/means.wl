(* ::Package:: *)

(* ::Title:: *)
(*Plotting means*)


(* ::Text:: *)
(*Functions for plotting mean distribution and velocity values as a function of printing parameters and matching experimental processing-structure correlations to theoretical correlations. Used for "Changes in filament microstructures during direct ink writing with yield stress fluid support"*)
(*Last updated November 2019 by Leanne Friedrich*)


<<"Mathematica\\kendall.wl";
<<"Mathematica\\master.wl";


(* ::Section:: *)
(*velocity data selection*)


(* ::Subsubsection::Closed:: *)
(*outswitch*)


(* ::Text:: *)
(*switch output by mode*)
(*OUTPUT: list*)
(*CALLED BY: tpkmean, initposave, initpossplit, dposave, dpospass, dpossplit, finalposave, finalpospass*)


outswitch[mode_, tplist_, kendalllist_]:=
Switch[mode
	,1, tplist
	,2, kendalllist
	,3, {tplist, kendalllist}
]


(* ::Subsection:: *)
(*points and kendall stats for average indep values*)


(* ::Subsubsection::Closed:: *)
(*mer*)


(* ::Text:: *)
(*mean and error*)
(*yvar = dependent variable to plot, as a column number in bigtable2*)
(*OUTPUT : {mean, error}*)
(*CALLS : ster (master.wl), accumulatederror (master.wl)*)
(*CALLED BY: s1mmean, dposave, finalposave*)


mer[s1_, yvar_]:=Module[{mean, error},
mean = Mean[s1[[;;,yvar]]];
error = ster[s1[[;;,yvar]]] + accumulatederror[s1[[;;,yvar+1]]];
{mean, error}
]


(* ::Subsubsection::Closed:: *)
(*s1mmean*)


(* ::Text:: *)
(*get the mean and error of a list of points*)
(*m = support mode, 2 for layer - by - layer, 3 for bath*)
(*bigtable2 = bigtable2.csv table*)
(*yvar = dependent variable to plot, as a column number in bigtable2*)
(*OUTPUT : {mean, error}*)
(*CALLS : mer*)
(*CALLED BY: tpkmean, initposave*)


s1mmean[m_, bigtable2_, yvar_]:=Module[{s1, mean, error},
s1 = Select[bigtable2, #[[3]]==m && #[[yvar]]!=0&];
mer[s1, yvar]
]


(* ::Subsubsection::Closed:: *)
(*tppoint*)


(* ::Text:: *)
(*from the mean and error, get a point for the overall mean*)
(*OUTPUT: {{x,y}, error bar}*)
(*CALLED BY: tpkmean, initposave, dposave, finalposave*)


tppoint[mean_, error_]:={{{0, mean}, ErrorBar[error]}}


(* ::Subsubsection::Closed:: *)
(*kmeanm*)


(* ::Text:: *)
(*from the mean and error, get a list of "kendall stats" for the overall mean*)
(*OUTPUT: list of 5 stats*)
(*CALLED BY: tpkmean, initposave, dposave, finalposave*)


kmeanm[mean_, error_]:=Module[{if, sign, kendalllist},
if = True;
sign =  If[mean-error>0
			, "+"
			, If[mean+error<0
				, "-"
				, if = False; ""
			]
		];
kendalllist = {mean, 0,sign,sign, if};
kendalllist
]


(* ::Subsubsection::Closed:: *)
(*tpkmean*)


(* ::Text:: *)
(*get the points to plot and the kendall stats for the average across all variables*)
(*m = support mode, 2 for layer-by-layer, 3 for bath*)
(*bigtable2 = bigtable2.csv table*)
(*yvar = dependent variable to plot, as a column number in bigtable2*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: {list of one point with error, list of "kendall" stats}*)
(*CALLS: s1mmean, tppoint, kmeanm, outswitch, vplotcol*)
(*CALLED BY: (means4.nb)*)


tpkmean[m_, bigtable2_, yvar_, mode_]:=Module[{s1, mean, error, tplist, kendalllist, if, sign},
{mean, error} = s1mmean[m, bigtable2, yvar];
tplist = If[mode==1 || mode==3, tppoint[mean, error], {}];
kendalllist = If[mode==2 || mode==3, kmeanm[mean, error], {}];
outswitch[mode, tplist, kendalllist]
];


(* ::Subsection:: *)
(*points and kendall stats for indep/dep correlations, split by indep variable*)


(* ::Subsubsection::Closed:: *)
(*tpvarm*)


(* ::Text:: *)
(*points to plot from initial points*)
(*s1 = list of all points*)
(*xvar = x variable column number*)
(*yvar = y variable column number*)
(*OUTPUT: list of points*)
(*CALLS: ster (master.wl), accumulatederror (master.wl)*)
(*CALLED BY: initpossplit, dpospass, dpossplit, finalpospass*)


tpvarm[s1_, xvar_, yvar_]:=Module[{tplist},
Sort[
			{{#[[1, xvar]], Mean[#[[;;, yvar]]]}
				, ErrorBar[ster[#[[;;,yvar]]] + accumulatederror[#[[;;,yvar+1]]]]
			}&/@ GatherBy[s1, #[[xvar]]&]
		]
]


(* ::Subsubsection::Closed:: *)
(*tpkvar*)


(* ::Text:: *)
(*get the points to plot and the kendall stats as a function of a printing parameter*)
(*m = support mode, 2 for layer-by-layer, 3 for bath*)
(*bigtable2 = bigtable2.csv table*)
(*xvar = independent variable to plot*)
(*yvar = dependent variable to plot, as a column number in bigtable2*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: {list of points with errors, list of kendall stats}*)
(*CALLS: kall (kendall.wl), tpvarm, outswitch, vplotcol*)
(*CALLED BY: (means4.nb)*)


tpkvar[m_, bigtable2_, xvar_, yvar_, mode_]:=Module[{s1, mean, error, tplist, kendalllist, if, sign},
s1 = Select[bigtable2, #[[3]]==m && #[[yvar]]!=0&];
kendalllist = If[mode==2 || mode==3, kall[s1, xvar, yvar], {}];
tplist = If[mode==1 || mode==3, tpvarm[s1, xvar, yvar], {}];
outswitch[mode, tplist, kendalllist]
];


(* ::Section:: *)
(*distribution data selection*)


(* ::Subsubsection::Closed:: *)
(*label*)


(* ::Text:: *)
(*convert the mmode to a label*)
(*mmode = 1-8: 1 = initial position, 2 = dp relax NN, 3 = dp relax 2nn, 4 = dp shear, 5 = initial width, 6 = dw relax NN, 7 = dw relax 2nn, 8 = dw shear*)


swepar[n_]:=If[n==0,4,n]
tlist = {"Initial", "\[CapitalDelta]relax NN","\[CapitalDelta]relax 2nd NN",  "\[CapitalDelta]shear"};
label[mmode_]:=tlist[[swepar[Mod[mmode,4]]]]<>" "<>({"pos", "width"}[[Ceiling[mmode/4]]])


(* ::Subsubsection::Closed:: *)
(*tpkdist*)


(* ::Text:: *)
(*points to plot and kendall stats for particle distributions*)
(*bigtable2 = bigtable2.csv table*)
(*changetable = changetable.csv table*)
(*mmode = 1-8: 1 = initial position, 2 = dp relax NN, 3 = dp relax 2nn, 4 = dp shear, 5 = initial width, 6 = dw relax NN, 7 = dw relax 2nn, 8 = dw shear*)
(*xvar = x variable to plot over*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: list of points and kendall stats*)
(*CALLS: initposave, initpossplit, dposave, dpospass, dpossplit*)
(*CALLED BY: (means4.nb)*)


tpkdist[bigtable2_, changetable_, mmode_, xvar_, m_, mode_]:=Module[{pos, NN, relax},Catch[
If[m<2 || m>3, Print["m must be 2 or 3"]; Throw[{}]];
If[mmode<=4, pos = True, pos = False];
If[Mod[mmode,4]==3, NN = False, NN = True;];
If[Mod[mmode,4]==0, relax = False, relax = True];
If[Mod[mmode,4]==3 && xvar==9
	, {}
	, If[mmode==1 || mmode==5
		, If[xvar==0
			, initposave[bigtable2, m, pos, mode]
			, initpossplit[bigtable2, m, xvar, pos, mode]
			]
		, Switch[xvar
			, 0, dposave[changetable, m, relax, pos, NN, mode]
			, 9, dpospass[changetable, m,  relax, pos, mode]
			, _, dpossplit[changetable, m, xvar, relax, pos, NN, mode]
		]
	]
]
]]


(* ::Subsection:: *)
(*initial*)


(* ::Subsubsection::Closed:: *)
(*initposave*)


(* ::Text:: *)
(*select points and kendall statistics for the initial particle distribution*)
(*bigtable2 = bigtable2.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*pos = True for position, False for width*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: s1mmean, tppoint, kmeanm, outswitch*)
(*CALLED BY: tpkdist*)


initposave[bigtable2_, m_, pos_, mode_]:=Module[{a, mean, error, tplist, kendalllist},
a = If[pos, 0, 4];
{mean, error} = s1mmean[m, bigtable2, 31+a];
tplist = If[mode==1 || mode==3, tppoint[mean, error], {}];
kendalllist = If[mode==2 || mode==3, kmeanm[mean, error], {}];
outswitch[mode, tplist, kendalllist]
]


(* ::Subsubsection::Closed:: *)
(*initpossplit*)


(* ::Text:: *)
(*select points and kendall statistics for the initial particle distribution, split by variable*)
(*bigtable2 = bigtable2.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*xvar = x variable to split by*)
(*pos = True for position, False for width*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: kall (kendall.wl), tpvarm, outswitch*)
(*CALLED BY: tpkdist*)


initpossplit[bigtable2_, m_, xvar_, pos_, mode_]:=Module[{s1, mean, error, tplist, if, sign, kendalllist, a},
a = If[pos, 0, 4];
s1 = Select[bigtable2, #[[3]]==m && #[[31+a]]!=0 &];
tplist = If[mode==1 || mode==3, tpvarm[s1, xvar, 31+a], {}];
kendalllist = If[mode==2 || mode==3, kall[s1, xvar, 31+a], {}];
outswitch[mode, tplist, kendalllist]
]


(* ::Subsection:: *)
(*change*)


(* ::Subsubsection:: *)
(*timebetween*)


(* ::Text:: *)
(*calculates the time between measurements of the initial and relaxed distributions*)
(*row = row of data from BIGTABLE2*)
(*OUTPUT: number*)
(*CALLED BY: ctselect*)


timebetween[row_]:=row[[4]]*row[[5]]/row[[2]]


(* ::Subsubsection::Closed:: *)
(*ctselect*)


(* ::Text:: *)
(*select points from the changetable*)
(*pos = True for position, False for width*)
(*relax = true to plot change during relaxation, false for shear*)
(*NN = True for nearest neighbor, False for 2nd nearest neighbor*)
(*changetable = changetable.csv table*)
(*OUTPUT: list of indices, table of points*)
(*CALLS: timebetween (master.wl)*)
(*CALLED BY: dposave, dpospass, dpossplit*)


ctselect[pos_, relax_, NN_, changetable_]:=Module[{a, indices, ct},
a = If[pos, 0, 4];
indices = If[relax, If[NN, {10, 16}, {22}], {28, 34, 40}]+a;
If[relax
	, ct = Select[changetable, timebetween[#]>7.5&]
	, ct = changetable
];
{indices, ct}
]


(* ::Subsubsection::Closed:: *)
(*dposave*)


(* ::Text:: *)
(*select points and kendall statistics for the change in particle distribution,  averaged over all printing parameters*)
(*changetable = changetable.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*relax = true to plot change during relaxation, false for shear*)
(*pos = True for position, False for width*)
(*NN = True for nearest neighbor, False for 2nd nearest neighbor*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: mer, tppoint, ctselect, kmeanm, outswitch*)
(*CALLED BY: tpkdist*)


dposave[changetable_, m_, relax_, pos_, NN_, mode_]:=Module[{s1, mean, error, tplist, if, sign, kendalllist
																, a, indices, ct},
{indices, ct} = ctselect[pos, relax, NN, changetable];
s1 = Flatten[
		Table[
			Select[ct, #[[3]]==m && #[[j]]!=0 &][[;;, {j, j+1}]]
		, {j, indices}]
	,1];
{mean, error} = mer[s1, 1];
tplist = If[mode==1 || mode==3, tppoint[mean, error], {}];
kendalllist = If[mode==2 || mode==3, kmeanm[mean, error], {}];
outswitch[mode, tplist, kendalllist]
]


(* ::Subsubsection::Closed:: *)
(*dpospass*)


(* ::Text:: *)
(*select points and kendall statistics for the change in particle distribution, split over pass number*)
(*changetable = changetable.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*relax = true to plot change during relaxation, false for shear*)
(*pos = True for position, False for width*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: ctselect, kall (kendall.wl), tpvarm, outswitch*)
(*CALLED BY: tpkdist*)


dpospass[changetable_, m_,  relax_, pos_, mode_]:=Module[{s1, mean, error, tplist, if, sign, kendalllist, ilist, a, ct},
{ilist, ct} = ctselect[pos, relax, True, changetable];
s1 = Flatten[
		Table[
			{j+1, #[[ilist[[j]]]], #[[ilist[[j]]+1]]}
			&/@ Select[ct, #[[3]]==m && #[[ilist[[j]]]]!=0 &]
		, {j,2}]
	,1];
tplist = If[mode==1 || mode==3, tpvarm[s1, 1, 2], {}];
kendalllist = If[mode==2 || mode==3, kall[s1, 1,2], {}];
outswitch[mode, tplist, kendalllist]
]


(* ::Subsubsection::Closed:: *)
(*dpossplit*)


(* ::Text:: *)
(*select points and kendall statistics for the change in particle distribution, split over printing parameter*)
(*changetable = changetable.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*xvar = changetable column number to split over*)
(*relax = true to plot change during relaxation, false for shear*)
(*pos = True for position, False for width*)
(*NN = True for nearest neighbor, False for 2nd nearest neighbor*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: ctselect, kall (kendall.wl), tpvarm, outswitch*)
(*CALLED BY: tpkdist*)


dpossplit[changetable_, m_, xvar_, relax_, pos_, NN_, mode_]:=Module[{s1, mean, error, tplist, if, sign, kendalllist, a, indices, ct},
{indices, ct} = ctselect[pos, relax, NN, changetable];
s1 = Flatten[
		Table[
			{#[[xvar]], #[[j]], #[[j+1]]}&/@Select[ct, #[[3]]==m && #[[j]]!=0 &]
		, {j, indices}]
	,1];
tplist = If[mode==1 || mode==3, tpvarm[s1, 1, 2], {}];
kendalllist = If[mode==2 || mode==3, kall[s1, 1, 2], {}];
outswitch[mode, tplist, kendalllist]
]


(* ::Subsection:: *)
(*final*)


(* ::Subsubsection::Closed:: *)
(*finalposave*)


(* ::Text:: *)
(*select points and kendall statistics for the final particle distribution,  averaged over all printing parameters*)
(*bigtable2 = bigtable2.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*pos = True for position, False for width*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: mer, tppoint, kmeanm, outswitch*)
(*CALLED BY: (means4.nb)*)


finalposave[bigtable2_, m_, pos_, mode_]:=Module[{s1, mean, error, tplist, if, sign, kendalllist, a},
a = If[pos, 0, 4];
s1 = Select[bigtable2, #[[3]]==m && #[[9]]==3 && #[[31+a]]!=0 && #[[37+a]]!=0 && #[[43+a]]!=0 &];
s1 = Transpose[{Flatten[s1[[;;,{31+a, 37+a, 43+a}]]], Flatten[s1[[;;,{31+a, 37+a, 43+a}+1]]]}];
{mean, error} = mer[s1, 1];
tplist = If[mode==1 || mode==3, tppoint[mean, error], {}];
kendalllist = If[mode==2 || mode==3, kmeanm[mean, error], {}];
outswitch[mode, tplist, kendalllist]
]


(* ::Subsubsection::Closed:: *)
(*finalpospass*)


(* ::Text:: *)
(*select points and kendall statistics for the final particle distribution, as a function of pass number*)
(*bigtable2 = bigtable2.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*pos = True for position, False for width*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: tpvarm, kall (kendall.wl), outswitch*)
(*CALLED BY: (means4.nb)*)


finalpospass[bigtable2_, m_, pos_, mode_]:=Module[{s1, mean, error, tplist, if, sign, kendalllist, a, s2},
	a = If[pos, 0, 4];
	s1 = Select[Flatten[{{3, #[[31+a]], #[[32+a]]}, {2, #[[37+a]], #[[38+a]]}, {1, #[[43+a]], #[[44+a]]}}
				&/@Select[bigtable2, #[[3]]==m && #[[9]]==3&],1], #[[2]]!=0&];
	tplist = If[mode==1 || mode==3, tpvarm[s1, 1, 2], {}];
	kendalllist = If[mode==2 || mode==3, kall[s1, 1,2], {}];
	outswitch[mode, tplist, kendalllist]
]


(* ::Subsubsection::Closed:: *)
(*finalpossplit*)


(* ::Text:: *)
(*select points and kendall statistics for the final particle distribution, as a function of printing parameter*)
(*bigtable2 = bigtable2.csv table*)
(*m = 2 for layer-by-layer, 3 for bath*)
(*pos = True for position, False for width*)
(*xvar = column number for x variable*)
(*mode = 1 for points to plot, 2 for kendall stats, 3 for both*)
(*OUTPUT: points, stats, or points and stats*)
(*CALLS: ster (master.wl), accumulatederror (master.wl), kall (kendall.wl)*)
(*CALLED BY: (means4.nb)*)


finalpossplit[bigtable2_, m_, pos_, xvar_, mode_]:=Module[{s1, mean, error, tplist, if, sign, kendalllist, a, s2},
	a = If[pos, 0, 4];
	s1 = (#[[{xvar, 31+a, 37+a, 43+a,  32+a, 38+a, 44+a}]])
			&/@Select[bigtable2, #[[3]]==m&&#[[9]]==3 && #[[31+a]]!=0 && #[[37+a]]!=0 && #[[43+a]]!=0 &];
	s2 = Flatten[Table[s1[[i,{1,j}]], {j, 2,4}, {i, Length[s1]}],1];
	tplist = If[mode==1 || mode==3, Sort[{{#[[1,1]], Mean[Flatten[#[[;;, 2;;4]]]]}
				, ErrorBar[ster[Flatten[#[[;;, 2;;4]]]] + accumulatederror[Flatten[#[[;;, 5;;]]]]]}
				&/@GatherBy[s1, #[[1]]&]], {}];
	kendalllist = If[mode==2 || mode==3, kall[s2, 1,2], {}];
	outswitch[mode, tplist, kendalllist]
]


(* ::Section:: *)
(*error list plots*)


(* ::Subsection:: *)
(*general error list plot settings*)


(* ::Subsubsection::Closed:: *)
(*elpsettings*)


(* ::Text:: *)
(*CALLS: markerlist (master.wl)*)
(*CALLED BY: (means4.nb)*)


elpsettings = <|"bottom"->True
				, "colorsi"->{Black}
				, "joined"->True
				, "left"->True
				, "legend"->True
				, "legendfunc"->(Graphics)
				, "markersi"->markerlist[8]
				, "pmode"->1
				, "pr"->All
				, "signs"->{}
				, "title"->None
				, "xlist"->{0,1}
				, "xlabel"->""
				, "ylabel"->""
				|>; 


(* ::Subsubsection::Closed:: *)
(*pmode2fs*)


(* ::Text:: *)
(*gets a font size given a plot mode*)
(*pmode = 1 for powerpoints, 2 for paper plots*)
(*CALLED BY: elpdefault*)


pmode2fs[pmode_]:=Switch[pmode,1,18,2,10];


(* ::Subsubsection::Closed:: *)
(*elpdefault*)


(* ::Text:: *)
(*default error list plot*)
(*list = data to plot*)
(*elpset = settings struct*)
(*OUTPUT: plot*)
(*CALLS: pmode2fs, getColors (master.wl), markerlist (master.wl)*)
(*CALLED BY: plotallpositions*)


elpdefault[list_, elpset_]:=Module[{fs, is, lp, ll, colors, rl, markers, pmode, colorsi, markersi, xlist, left, xlabel, ylabel, legendfunc, legend, legendsize, pr, titl, title, signs},
ll = Length[list];
pmode = elpset["pmode"];
colorsi = elpset["colorsi"];
fs = pmode2fs[pmode];
If[pmode==1
	,
	(* powerpoint *)
	is = 350;
	lp = 120;
	If[Length[colorsi]<ll
		,
		colors = getColors[ll];
		If[OddQ[ll], colors[[Ceiling[ll/2]]]=Darker[colors[[Ceiling[ll/2]]]]];
		,
		colors = colorsi
	];
	rl = False;
	legendsize = 60;
	,
	(* paper *)
	
	lp = 38;
	is = (6.5*72-45-2*lp)/4;
	colors = GrayLevel/@Range[0, 0.66, 0.66/(ll-1)];
	rl = True;
	legendsize = 30;
	fs = 8;
];	
markersi = elpset["markersi"];
If[Length[markersi]!=ll
	, markers = Transpose[{markerlist[ll], ConstantArray[fs*1.25*100/72, ll]}];
	, If[Length[markersi[[1]]]==0
		, markers = Transpose[{markersi, ConstantArray[fs*1.25*100/72, ll]}];
		, markers = markersi
	];
];
left = elpset["left"];
xlist = elpset["xlist"];
pr = elpset["pr"];
titl = elpset["title"];
If[!NumberQ[titl], title = Style[titl, fs], title = None];
If[Length[elpset["signs"]]>0,
	signs = Row[Table[
				Graphics[{
						If[elpset["signs"][[i]]!="", colors[[i]], White]
						, Circle[{0,0},1]
						, Text[Style[elpset["signs"][[i]], colors[[i]], 15], {0,0}]
					}, ImageSize->20]
		, {i, Length[elpset["signs"]]}]];
		,
		signs = Graphics[{}]
	];
Show[
	ErrorListPlot[list
			, AspectRatio -> 1
			, Axes -> None
			, Epilog->Inset[signs, Scaled[{0.70, 0.1}]]
			, Frame -> True
			, FrameLabel -> {elpset["xlabel"],  elpset["ylabel"]}
			, FrameTicks -> {{Automatic, Automatic},{xlist, None}}
			, GridLines -> {None, {0}}
			, ImagePadding -> ({{If[left, lp, 0], 2}, {If[elpset["bottom"], fs*4, 2], 2}}*100/72)
			, ImageSize -> (is+If[left, lp, 0])*100/72
			, Joined->elpset["joined"]
			, LabelStyle -> Directive[fs*100/72, Black]
			, PlotLabel->title
			, PlotLegends -> None
			, PlotMarkers -> markers
			, PlotRangePadding -> {{Scaled[0.1], Scaled[0.1]}, {Scaled[0.2], Scaled[0.1]}}
			, PlotStyle -> ({Directive[Thin] , #}&/@colors)
			, RotateLabel -> rl
			, PlotRange->All
	]
, PlotRange -> pr]
]


(* ::Subsection:: *)
(*velocity plots*)


(* ::Subsubsection::Closed:: *)
(*xlabelrow*)


(* ::Text:: *)
(*CALLS: s8*)
(*CALLED BY: vplotcol*)


xlabelrow[xvar_, spacer_]:=Row[
					Prepend[
						Table[
							Graphics[
								Text[
									s8[
										Switch[xvar, 9, "Pass", 1, "w% TEGDMA", 2, "Print speed (mm/s)", 0, "Average"]
									]
									, {0,0}
									, {Left, Bottom}
								]
							, ImageSize->{(6.5*72-36)/10*PS*j, 10*PS}
							, PlotRange->{{0,(6.5*72-36)/10*PS*j},{0,10*PS}}
							, ImagePadding->0]
						, {j, {3,4,3}}]
					, spacer]
				]


(* ::Subsubsection::Closed:: *)
(*vplot*)


(* ::Text:: *)
(*one plot of transverse flow velocity*)
(*tp = list of points to plot*)
(*xvar = colum number of x variable in bigheader2*)
(*yvar = column number of y variable in bigheader2*)
(*kendall = list of kendall stats {bath, layer-by-layer}*)
(*OUTPUT: one plot*)
(*CALLS: kendallcircles (kendall.wl), s8 (master.wl), ylabelvt11 (master.wl)*)
(*CALLED BY: vplotcol*)


vplot[tp_, xvarindex_, yvar_, kendall_, xvarlist_]:=Module[{ticks, colors , signs, leftpad, rightpad, toppad, botpad, is, allticks
												, xticksbot, xtickstop, yticksleft, yticksright, lp, pl, pr1, mv
												, framelabel, xvar},
xvar = xvarlist[[xvarindex]];
If[xvar>0
	, 
	ticks = DeleteDuplicates[tp[[1, ;;, 1,1]]];
	xticksbot = If[yvar==11||yvar==17||yvar==25, ticks, {#, ""}&/@ticks];
	xtickstop = {#, ""}&/@ticks;
	, 
	xticksbot = xtickstop = None;
];

yticksleft = {#, If[#==0, "0.00", SetPrecision[#, 2]]}&/@{-0.15, 0, 0.15, 0.3};
yticksright = {#, ""}&/@{-0.15, 0, 0.15, 0.3};
allticks = {{yticksleft, yticksright},{xticksbot, xtickstop}};
colors = {Gray, Black};
signs = kendallcircles[kendall];
lp = 36;
leftpad = Round[If[yvar==11, lp,0]*PS];
rightpad =  Round[ If[yvar==29,10,0]*PS];
toppad = Round[If[xvar>0,11,1]*PS];
botpad = Round[1*PS];
is = Round[(6.5*72-lp)/10*PS]+leftpad+rightpad;
pl = Pane[Row[{
				If[MemberQ[{11,17,25}, yvar]
					, Style[Join[ToUpperCase[Alphabet[]], ToUpperCase[#<>#&/@Alphabet[]]]
							[[(xvarindex-1)*3+Switch[yvar, 11, 1, 17, 2, 25, 3]]]
						, 11*PS, FontFamily->"Arial"]
					, ""
				]
				, Show[signs, ImageSize->25*PS]
		}], Alignment->Left, ImageSize->is-leftpad-rightpad];
pr1 = MinMax[tp[[;;,;;, 1, 1]]];
If[pr1[[1]]==pr1[[2]],
	pr1[[1]] = pr1[[1]]-0.5;
	pr1[[2]] = pr1[[2]]+0.5;
];
mv = pr1[[2]]+0.3*(pr1[[2]]-pr1[[1]]);
framelabel = {None, If[yvar==11, ylabelvt11, None]};
Show[
	Graphics[If[yvar==29
				, {Text[Rotate[s8["out"], Pi/2], {mv, -0.2}, {Left, Bottom}]
					, Text[Rotate[s8["in"], Pi/2], {mv, 0.32}, {Left, Top}]}
				,{}]]
	, ErrorListPlot[tp
		, Joined-> (ToExpression/@kendall[[;;, 5]])
		, PlotRange-> All
		, PlotStyle-> colors
		, PlotMarkers-> ({#, 8*PS}&/@{\[FilledCircle],\[EmptyCircle]}) 
	]
	, AspectRatio->1
	, Axes-> None
	, Frame-> {True, yvar==11||yvar==17||yvar==25, True, yvar==29||yvar==15||yvar==23}
	, FrameLabel-> framelabel
	, FrameTicks-> allticks
	, GridLines-> {{}, { 0}}
	, ImageSize-> is
	, ImagePadding-> {{leftpad,rightpad}, {toppad, botpad}}
	, LabelStyle-> Directive[8*PS, Black]
	, PlotLabel-> pl
	, PlotRange-> {pr1,{-0.13, 0.23}}
	, PlotRangePadding-> Scaled[0.175]
]
]


(* ::Subsubsection::Closed:: *)
(*vplotcol*)


(* ::Text:: *)
(*Get a grid of vtplots*)
(*xvarlist= list of column numbers to plot in x*)
(*kvecs = kvectable.csv table*)
(*bigtable2 = bigtable2.csv table*)
(*OUTPUT: grid of graphics*)
(*CALLS: plegend (master.wl), rlegends (master.wl), tpkmean, tpkvar, xlabelrow, vplot*)
(*CALLED BY: (means4.nb)*)


vplotcol[xvarlist_, kvecs_, bigtable2_]:=Module[{tp, kendall, spacer, rlegend, rlegend2, xvar},
{spacer, rlegend, rlegend2} = rlegends[bigtable2, 36];
Column[
	Join[
		{plegend[Row], rlegend, rlegend2}
		, Table[
			xvar = xvarlist[[xvi]];
			Column[{
				Row[
					Table[
						tp = Table[
							If[xvar==0
								, tpkmean[m, bigtable2, yvar, 1]
								, tpkvar[m, bigtable2, xvar, yvar,1]
							]
						, {m, {3,2}}];
						kendall = Select[kvecs, #[[2]]==yvar && #[[3]]==xvar&][[;;, 5;;]];
						vplot[tp, xvi, yvar, kendall, xvarlist]
					, {yvar, 11, 29, 2}]]
				, xlabelrow[ xvar, spacer]
			}, Alignment->Left]
		,{xvi, Length[xvarlist]}]
	]
, Alignment->Right]
]


(* ::Subsection:: *)
(*distributions*)


(* ::Subsubsection::Closed:: *)
(*plotallpositions*)


(* ::Text:: *)
(*bigtable2 = subset of bigtable2*)
(*left = bool left axes*)
(*legend = bool legend*)
(*mode = 1 for position, 2 for stdev (mm), 3 for stdev (uniform)*)
(*pr = plot range*)
(*plotlist = lines to plot e.g. {1,2,3}*)
(*step = 1-4*)
(*elpst = error list plot settings struct*)
(*OUTPUT: one plot*)
(*CALLS: ster (master.wl), accumulatederror (master.wl), getColors (master.wl), markerlist (master.wl), highlightedwriting (master.wl), elpdefault*)
(*CALLED BY: (means4.nb)*)


plotallpositions[bigtable2_, mode_,  plotlist_, step_, elpst_]:=Module[{t1, b1, firstline, secondline, thirdline, colors, s,  p, pl, tp, label, ylabel, elpset, ml, pmode, col},
t1 = bigtable2;
elpset = elpst;
p = (mode-1)*2;
b1= Flatten[Table[
			Table[
			Table[
				col = 31 + (j-1)*6 + 12*k + p;
				s = Select[t1, #[[9]]==i && #[[col]]!=0&];
				{{i - 0.4*If[col>=49, 1, 0], Mean[s[[;;, col]]]}
						, ErrorBar[ster[s[[;;,col]]]+accumulatederror[s[[;;, col+1]]]]}
			, {i,j,3}]
			, {j, 1+k,3}]
			,{k,0,1}],1];
firstline = {b1[[1,1]], b1[[4,1]], b1[[2,1]], b1[[5,1]], b1[[3,1]]};
secondline = {b1[[1,2]], b1[[4,2]], b1[[2,2]]};
thirdline = {b1[[1,3]]};

pl = Flatten[{If[MemberQ[plotlist,1], {1,4},{}]
				, If[MemberQ[plotlist,2], {2,5},{}]
				, If[MemberQ[plotlist,3],{3},{}]
		}];
tp = ({firstline[[{1,3,5}]], secondline[[{1,3}]], thirdline, firstline[[{2,4}]], secondline[[{2}]]}[[pl]]);
Switch[step
	,1, tp[[1]] = tp[[1, {1}]];
		tp[[2]] = tp[[2,{1}]];
	,2, tp[[1]] = tp[[1, {1,2}]];
		tp[[2]] = tp[[2,{1}]];
	,3, tp[[1]] = tp[[1, {1,2}]];
		tp[[2]] = tp[[2,{1,2}]];
	,4, tp[[1]] = tp[[1, {1,2,3}]];
		tp[[2]] = tp[[2,{1,2}]];
];
Switch[mode
	,1, ylabel = {"Position", " (mm)"};
	,2, ylabel = {"Width", " (mm)"};
	,3, ylabel = {"Width", " (normalized)"};
];
pmode = elpset["pmode"];
elpset["ylabel"] =  If[pmode==1, Column[ylabel], Row[ylabel]];
If[pmode==1
	, colors = getColors[4][[1;;3]];
	, colors = GrayLevel/@{0, 0.33, 0.66}
];
elpset["xlist"] = {{1, "1b"},{1.6, "2a"}, {2, "2b"},{2.6, "3a"}, {3, "3b"}};
elpset["xlabel"] =  "Pass";
elpset["colorsi"] = Join[colors, colors[[1;;2]]][[pl]];
ml = markerlist[8];
elpset["markersi"] = Join[ml[[1;;3]], ml[[5;;6]]][[pl]];
elpset["legendfunc"] = highlightedwriting[#[[{1,3,5}]], 100, Switch[pmode, 1, 18*1.25, 2, 10*1.25]]&;
elpset["joined"] = False;
Show[elpdefault[tp, elpset]
	, Graphics[
			Flatten[Riffle[
				Table[{{Dashing[{1,0}], Dashing[{0.05, 0.02}], Dashing[{1,0}]}[[plotlist]][[i]]
						, colors[[plotlist]][[i]]}
					, {i,Length[plotlist]}]
				, Line[#[[;;,1]]]&/@
					({firstline[[1;;If[1<=step && step<=4, step+1, 5]]], secondline, thirdline}[[plotlist]])
			]]
		]]		
]


(* ::Subsubsection::Closed:: *)
(*distplot*)


(* ::Text:: *)
(*plot particle distributions as a function of printing parameters*)
(*xvar = x variable column number*)
(*mmode = 1-8: 1 = initial position, 2 = dp relax NN, 3 = dp relax 2nn, 4 = dp shear, 5 = initial width, 6 = dw relax NN, 7 = dw relax 2nn, 8 = dw shear*)
(*kendall = {bath, layer-by-layer} kendall stats*)
(*tp = {bath, layer-by-layer} points*)
(*OUTPUT: plot*)
(*CALLS: kendallcircles (kendall.wl), s12 (master.wl)*)


distplot[xvar_, mmode_, kendall_, tp_]:=Module[{ticks, colors, signs, lp, yticks, pr, ticklist, leftpad, rightpad
												, botpad, toppad, is, allticks, xtickstop, xticksbot, yticksleft
												, yticksright, xlabel, ylabel, plotlabel},
lp = 36; (*left padding*)
pr = If[mmode<5
		, {-0.15, 0.12}
		, If[mmode==5
			, {0.75, 0.87}
			, {0, 0.1}
		]
	]; (*plot range*)
yticks = If[mmode<5
			, {-0.15, 0, 0.15, 0.3}
			, Range[-1, 1, 0.05]
		];
yticksleft = {#, If[#==0, "0.00", SetAccuracy[#, 3]]}&/@yticks;
yticksright = {#, ""}&/@yticks;
If[xvar>0 && Length[tp]>0
	, 
	ticklist = DeleteDuplicates[tp[[1, ;;, 1,1]]];
	xticksbot = ticklist;
	xtickstop = {#, ""}&/@ticklist;
	, 
	ticklist = {}; 
	ticks = None;
	xticksbot = None;
	xtickstop = None;
];
allticks = {{yticksleft, yticksright}, {xticksbot, xtickstop}};
colors = {Gray, Black};
signs = kendallcircles[kendall];
leftpad = Round[If[xvar==0,lp,0]*PS];
rightpad = Round[If[xvar==2,1,0]*PS];
botpad = Round[If[mmode==4||mmode==8, 45, 11]*PS];
toppad = Round[1*PS];
is = (Round[(3.25*72-lp)/3.4*PS]*If[xvar==0,0.4,1]+leftpad+rightpad);
xlabel = If[mmode==8||mmode==4
				, Switch[xvar
					, 0, "Mean"
					, 1, "w% TEGDMA"
					, 2, Column[{"Print speed","(mm/s)"}]
					, 9, "Pass"
					]
		];
ylabel = Switch[mmode, 1, "Initial P", 2, "\[CapitalDelta]P relax NN", 3, "\[CapitalDelta]P relax 2nd NN", 4, "\[CapitalDelta]P shear"
						, 5, "Initial W", 6, "\[CapitalDelta]W relax NN", 7, "\[CapitalDelta]W relax 2nd NN", 8, "\[CapitalDelta]W shear"];
plotlabel = Pane[
				Row[{
					If[xvar==0
						, s12[ToUpperCase[Alphabet[]][[mmode]]]
						, ""]
					,"  "
					, Show[signs, ImageSize->25*PS]
				}]
			, Alignment->Left, ImageSize->is-rightpad];
Show[ErrorListPlot[tp
, AspectRatio->If[xvar==0,1/0.4,1]
, Axes->None
, Frame->{True, xvar==0, True, xvar==2}
, FrameLabel->{xlabel, ylabel}
, FrameTicks->allticks
, GridLines->{{}, {0}}
, ImageSize->is
, ImagePadding->{{leftpad,rightpad}, {botpad, toppad}}
, Joined->(ToExpression/@kendall[[;;, 5]])
, LabelStyle->Directive[8*PS, Black]
, PlotLabel->plotlabel
, PlotRange->All
, PlotRangePadding->{If[Length[ticklist]>2,Scaled[0.175], Scaled[0.3]], Scaled[0.175]}
, PlotStyle->colors
, PlotMarkers->({#, 8*PS}&/@{\[FilledCircle],\[EmptyCircle]}) 
], PlotRange->pr]
]


(* ::Subsubsection::Closed:: *)
(*finalplot*)


(* ::Text:: *)
(*plot final particle distributions as a function of printing parameters*)
(*xvar = x variable column number*)
(*kendall = {bath, layer-by-layer} kendall stats*)
(*tp = {bath, layer-by-layer} points*)
(*pos = true to plot position*)
(*OUTPUT: plot*)
(*CALLS: kendallcircles (kendall.wl), s12 (master.wl)*)
(*CALLED BY: (means4.nb)*)


finalplot[xvar_, kendall_, tp_, pos_]:=Module[{ticks, colors, signs,lp, yticks, pr, ticklist, leftpad, rightpad
												, botpad, toppad, is, allticks, xtickstop, xticksbot, yticksleft
												, yticksright, xsize, ysize, framelabel, plotlabel},
lp= 35;
pr = If[pos, {-0.005, 0.07}, {0.81, 0.92}];
yticks = If[pos
			, Range[-0.03, 0.1, 0.03]
			, Range[0,1,0.05]
		];
yticksleft = {#, If[Round[#, 0.0001]==0, "0.00", SetAccuracy[#, 3]]}&/@yticks;
yticksright = {#, ""}&/@yticks;
If[xvar>0 && Length[tp]>0
	, 
	ticklist = DeleteDuplicates[tp[[1, ;;, 1,1]]];
	xticksbot = ticklist; 
	xtickstop = {#, ""}&/@ticklist;
	, 
	ticklist = {}; 
	ticks = None;
];
allticks = {{yticksleft, yticksright}, {xticksbot, xtickstop}};
colors = {Gray, Black};
signs = kendallcircles[kendall];
leftpad = Round[If[xvar==0,lp,0]*PS];
rightpad = Round[If[xvar==2,1,0]*PS];
botpad = Round[45*PS];
toppad = Round[1*PS];
is = Round[(6.5*72-lp*2)/7.2*PS]*If[xvar==0,0.6,1]+leftpad+rightpad;
framelabel = {Switch[xvar, 0, "Average", 1, "w% TEGDMA", 2, Column[{"Print speed","(mm/s)"}], 9, "Line"]
				, If[pos, "Final position (mm)", "Final width (normalized)"]};
plotlabel = Pane[Row[{
					s12[Join[ToUpperCase[Alphabet[]], ToUpperCase[#<>#&/@Alphabet[]]][[4*If[pos,0,1]+Switch[xvar, 0,1,9,2,1,3,2,4]]]]
					,"  "
					, Show[signs, ImageSize->25*PS]}]
			, Alignment->Left, ImageSize->(is-20)];
Show[ErrorListPlot[tp
, Axes->None
, AspectRatio->If[xvar==0,1/0.6,1]
, Frame->{True, xvar==0, True, xvar==2}
, FrameLabel->framelabel
, FrameTicks->allticks
, GridLines->{{}, { 0}}
, ImageSize->is
, ImagePadding->{{leftpad,rightpad}, {botpad, toppad}}
, Joined->(ToExpression/@kendall[[;;, 5]])
, LabelStyle->Directive[8*PS, Black]
, PlotLabel->plotlabel
, PlotRange->All
, PlotRangePadding->{If[Length[ticklist]>2,Scaled[0.175], Scaled[0.3]], Scaled[0.175]}
, PlotStyle->colors
, PlotMarkers->({#, 8*PS}&/@{\[FilledCircle],\[EmptyCircle]}) 
], PlotRange->pr]
]


(* ::Section:: *)
(*Comparing theory to experiments*)


DISTSLIST = {"Initial position", "$\\Delta$position relax NN",  "$\\Delta$position relax 2NN",  "$\\Delta$position shear", "Initial width", "$\\Delta$width relax NN",  "$\\Delta$width relax 2NN",  "$\\Delta$width shear"};


(* ::Subsubsection::Closed:: *)
(*multiheader*)


(* ::Text:: *)
(*var = independent variable being compared*)
(*m = "LBL" for layer-by-layer, "Bath" for bath support*)
(*OUTPUT: string for latex*)
(*CALLED BY: exp2latex, matches*)


multiheader[var_, m_]:=Switch[var
							, 0, Switch[m
									, "LBL", "\\multicolumn{18}{l}{\\textbf{Layer by layer}}\\\\\n"
									, "Bath", "\\multicolumn{18}{l}{\\textbf{Bath}}\\\\\n"]
								<>"Average"
							, 9, "$\\Delta$, pass"
							, 1, "$\\Delta$, TEGDMA"
							, 2, "$\\Delta$, speed"
						]


(* ::Subsubsection::Closed:: *)
(*importrr*)


(* ::Text:: *)
(*import replacement rules from replacementrules.xlsx*)
(*CALLED BY: (means4.nb)*)


importrr[f_]:= (#[[1]]->#[[2]])&/@#&/@Import[f];


(* ::Subsection:: *)
(*theoretical correlations as a latex table*)


(* ::Subsubsection::Closed:: *)
(*theory2latex*)


(* ::Text:: *)
(*convert an excel spreadsheet to latex tables*)
(*theotable = imported excel spreadsheet theorymasterstraights.xlsx*)
(*mode = 1 to write velocities and distributions separately, 2 together*)
(*export = true to export results*)
(*rr = replacement rules*)
(*OUTPUT: exports latex tables*)
(*CALLS: listindex (master.wl), tb2lx (master.wl)*)
(*CALLED BY: (means4.nb)*)


theory2latex[theotable_, mode_, export_, rr_]:=Module[{pzf, theory, explanations, tables, sgn, exp, ex, list, clist},
clist = Switch[mode
		,1, {Range[4,13], Range[14, 21]}
		,2, {Range[4, 21]}
		];
Do[
	pzf = theotable[[Join[{1}, Range[initrow, initrow+17]], 1;;21]];
	theory = StringReplace[pzf[[2,1]], {" "->"_"}];
	pzf[[{3,5,7,9,12,14,16,18}, 3;;]] = StringReplace[#, Switch[initrow, 2, rr[[1]], 20, rr[[2]], 38, rr[[3]], 56, rr[[4]], 74, rr[[5]]]]&/@#&/@pzf[[{3,5,7,9,12,14,16,18}, 3;;]];
	explanations = listindex[Select[DeleteDuplicates[Flatten[pzf[[{3,5,7,9,12,14,16,18}, 3;;]]]], StringLength[#]>0&]];
	tables = Table[
		"\\centering\\begin{tabular}{ll"<>StringJoin[ConstantArray["c", Length[row]]]<>"}\n"<>
		tb2lx[
		Prepend[
			Table[
				Join[
					{ Switch[i,2, "\\multirow{4}{*}{Layer by layer}", 11, "\\hline\n\\multirow{4}{*}{Bath}", _,""], StringReplace[pzf[[i,3]], {"change"->"Change"}]}
					,
					Table[
						sgn = pzf[[i,j]];
						exp = pzf[[i+1,j]];
						If[StringLength[exp]>0
							, ex = "$^{"<>ToString[Select[explanations, #[[2]]==pzf[[i+1,j]]&][[1,1]]]<>"}$";
							, ex = ""
						];
						sgn<>ex
					,{j,row}]
				]
			,{i, {2, 4, 6, 8, 11, 13, 15,17}}]
		, Join[{"",""},("\\rotatebox{90}{"<>#<>"}")&/@pzf[[1,row]]]]]<>"\n\\end{tabular}"
	, {row, clist}];
	If[mode==1
		, tables[[2]] = StringReplace[tables[[2]], {"dpos"->"$\\Delta$position", "dwidth"->"$\\Delta$width"}];
		, tables[[1]] = StringReplace[tables[[1]], {"dpos"->"$\\Delta$position", "dwidth"->"$\\Delta$width", "Change w/"->"$\\Delta$,"}];
	];
	list = "\\begin{enumerate}\n"<>Table["\t\\item "<>explanations[[i,2]]<>"\n", {i, Length[explanations]}]<>"\\end{enumerate}";
	If[export,
		If[mode==1
			, Print@Export["Figures\\vtheory_"<>theory<>".txt", tables[[1]]];
			Print@Export["Figures\\dtheory_"<>theory<>".txt", tables[[2]]];
			Print@Export["Figures\\listtheory_"<>theory<>".txt", list];
			,
			Print@Export["figures\\boththeory_"<>theory<>".txt", tables[[1]]];
		];
	];
	{tables, list}
,{initrow, {2, 20, 38, 56, 74}}]];


(* ::Subsection:: *)
(*experimental correlations as a latex table*)


(* ::Subsubsection::Closed:: *)
(*exp2latex*)


(* ::Text:: *)
(*convert two csvs of experimental correlations to latex tables*)
(*kvecs = list of velocity correlations*)
(*kdist = list of particle distribution correlations*)
(*OUTPUT: latex table string*)
(*CALLS: multiheader, mspace (master.wl)*)
(*CALLED BY: (means4.nb)*)


exp2latex[kvecs_, kdist_]:=Module[{t3, t1, t2},
t3 = Prepend[
	Flatten[Table[
		t1 = Select[kvecs, #[[4]]==m&];
		t2 = Select[kdist, #[[4]]==m&];
		Table[
			Join[
				{multiheader[var, m]}
				, mspace/@ SortBy[Select[t1, #[[3]]==var&], #[[2]]&][[;;, 7]]
				, mspace/@ Select[t2, #[[3]]==var&][[;;, 7]]
			]
		,{var, {0, 9, 1, 2}}]
	,{m, {"LBL", "Bath"}}],1]
,"\\rotatebox{90}{"<>#<>"}" &/@ Join[{""}, DeleteDuplicates[kvecs[[;;,1]]], DISTSLIST]];
"\\centering\\begin{tabular}{l"<>StringJoin[ConstantArray["c", 10], "|", ConstantArray["c", 8]]<>"}\n"<>tb2lx[t3]<>"\n\\end{tabular}"
]


(* ::Subsection:: *)
(*qualitative matching*)


(* ::Subsubsection::Closed:: *)
(*matches*)


(* ::Text:: *)
(*theotable = imported table from theorymasterstraights*)
(*kvecs = list of vector kendall stats*)
(*kdist = list of distribution kendall stats*)
(*mode = 1 for vecs, 2 for dists*)
(*OUTPUT: latex table string*)
(*CALLS: tb2lx (master.wl), multiheader*)
(*CALLED BY: (means4.nb)*)


matches[theotable_, kvecs_, kdist_, mode_]:=Module[{theoryletters, t3, t1, tpk, theories, tbtxt},
theoryletters = {"p", "d", "c", "g", "s"};
t3 = Prepend[
		Flatten[
			Table[
				t1 = Select[If[mode==1, kvecs, kdist], #[[4]]==m&];
				Table[
					tpk = SortBy[Select[t1, #[[3]]==var&], #[[2]]&][[;;, 8]];
						Prepend[
							Table[
								theories = theotable[[{2, 20, 38, 56, 74}+Switch[m, "LBL", 0, "Bath", 9]
													 + Switch[var, 0, 0, 9, 2, 1, 4, 2, 6], 3+i]];
								StringJoin[Sort[Table[
									If[theories[[j]]==""
										,""
										, If[tpk[[i]]==""
											, "\\textcolor{Gray}{"<>theoryletters[[j]]<>"}"
											, If[tpk[[i]]==theories[[j]]
												, (*"\\textbf{"<>*)theoryletters[[j]](*<>"}"*)
												,"\\textcolor{Red}{\\textit{"<>theoryletters[[j]]<>"}}"
											]
										]
									]
								,{j, 5}]]]
							,{i, Length[tpk]}]
						, multiheader[var, m]
						]
				,{var, {0, 9, 1, 2}}]
			,{m, {"LBL", "Bath"}}],1]
	,"\\rotatebox{90}{"<>#<>"}"&/@Join[{""}, If[mode==1, DeleteDuplicates[kvecs[[;;,1]]], DISTSLIST]]];
tbtxt = "\\centering\\begin{tabular}{l"<>StringJoin[ConstantArray["c", 10]]<>"}\n"<>tb2lx[t3]<>"\n\\end{tabular}"
]


(* ::Subsection:: *)
(*numerical matching*)


(* ::Subsubsection::Closed:: *)
(*mncell*)


(* ::Text:: *)
(*OUTPUT: one cell of the table*)
(*CALLED BY: matchesnum*)


mncell[i_, j_, k_, m_, theotable_, tvmaxima_, tpmaxima_, twmaxima_, tvselect_, tdselect_]:=Module[{theory, max, exp},
theory = theotable[[{2, 20, 38, 56, 74}[[k]]+Switch[m, "LBL", 0, "Bath", 9] + {0, 2, 4, 6}[[j]], 3+i]];
max = If[i<=10
		, tvmaxima[[j]]
		, If[i<=14
			, tpmaxima[[j]]
			, twmaxima[[j]]
		]
];
exp = If[i<=10
	, tvselect[[j,i]]
	, tdselect[[j,i-10]]
];
If[(theory!="+"&&theory!="-")||exp[[9]]!="True"
	, 0
	, Switch[theory, "+", 1, "-", -1]* exp[[5]]/max/4*10
]
]


(* ::Subsubsection::Closed:: *)
(*stringnum*)


(* ::Text:: *)
(*given a \[Tau]all value n, output a string*)
(*CALLED BY:  matchesnum*)


stringnum[n_]:=If[Abs[n]<0.5
					, ""
					, If[n<0
						, "\\textcolor{Gray}{\\textit{"<>ToString[Round[n]]<>"}}"
						, ToString[Round[n]]
					]
				]


(* ::Subsubsection::Closed:: *)
(*matchesnum*)


(* ::Text:: *)
(*table of matches between theory and experiment, expressed as a numerical \[Tau]all value*)
(*grps = list of dependent variable index lists to probe as columns in theotable.*)
(*theotable = import from theorymasterstraights.xlsx*)
(*kvecs = import from kvectable.csv*)
(*kdist = import from kdisttable.csv*)
(*headerlist = list of header strings for the variables to probe*)
(*flownum = number of transverse flow velocity headers*)
(*distnum = number of distribution headers*)
(*OUTPUT: latex table as txt*)
(*CALLS: tb2lx (master.wl), mncell, stringnum*)
(*CALLED BY: matchesnumadd*)


matchesnum[grps_, theotable_, kvecs_, kdist_, headerlist_, flownum_, distnum_]:=Module[{t3, tv, td
					, tvselect, tvmaxima, tdselect, tpmaxima, twmaxima, t4, theory, max, exp, intro, row1, row2, closing},
intro = "\\centering\\begin{tabular}{l"<>StringJoin[ConstantArray["c",flownum]]<>"|"
			<>StringJoin[ConstantArray["c",distnum]];
row1 = "}\n&\\multicolumn{"<>ToString[flownum]<>"}{c|}{Transverse flows}&\\multicolumn{"
			<>ToString[distnum]<>"}{c}{Particle distributions}\\\\\n";
row2 = StringJoin["\\rotatebox{90}{"<>#<>"}"&/@Join[{""}, headerlist]<>"\\\\\n"];
closing = "\n\\end{tabular}";
t3 = tb2lx[
		StringRiffle[
			Table[
				tv = Select[kvecs, #[[4]]==m&];
				td = Select[kdist, #[[4]]==m&];
				tvselect = Table[Select[tv, #[[3]]==var&], {var, {0,9,1,2}}];
				tvmaxima = Max[Abs[Select[#[[;;,5]], NumberQ]]]&/@tvselect;
				tdselect = Table[Select[td, #[[3]]==var&], {var, {0,9,1,2}}];
				tpmaxima = Max[Abs[Select[#, NumberQ[#[[5]]]&& #[[2]]<=4&][[;;,5]]]]&/@tdselect;
				twmaxima = Max[Abs[Select[#, NumberQ[#[[5]]]&&#[[2]]>= 5&][[;;,5]]]]&/@tdselect;
				twmaxima[[1]] = Max[Abs[Select[tdselect[[1]], NumberQ[#[[5]]]&&#[[2]]>= 6&][[;;,5]]]];
				Switch[m
					, "LBL", "\\multicolumn{9}{l}{\\textbf{Layer-by-layer}}\\\\\n"
					,"Bath", "\\multicolumn{9}{l}{\\textbf{Bath}}\\\\\n" ]
					<>tb2lx[
						Table[
							Prepend[
								Table[
									t4 = Mean[Table[
										Total[Table[
											mncell[i,j,k,m, theotable, tvmaxima, tpmaxima, twmaxima, tvselect, tdselect]
										,{j, 4}]]
									,{i,groups}]];
									stringnum[t4]
								,{groups, grps}]
							, {"Plastic zone", "Disturbed zone", "Capillary spreading", "Gravity spreading", "Settling"}[[k]]]
						,{k,5}]
					]
			,{m, {"LBL", "Bath"}}]
	, "\\\\\n"]
];
intro<>row1<>row2<>t3<>closing
]


(* ::Subsubsection:: *)
(*matchesnumadd*)


(* ::Text:: *)
(*table of matches between theory and experiment, expressed as a numerical \[Tau]all value*)
(*theotable = import from theorymasterstraights.xlsx*)
(*kvecs = import from kvectable.csv*)
(*kdist = import from kdisttable.csv*)
(*vindices = list of velocity index lists to probe as columns in theotable.*)
(*dindices = list of distribution index lists to probe as columns in theotable.*)
(*vheader = list of velocity header strings for the variables to probe*)
(*dheader = list of distribution header strings for the variables to probe*)
(*OUTPUT: latex table as txt*)
(*CALLS: matchesnum*)
(*CALLED BY: (means4.nb)*)


matchesnumadd[theotable_, kvecs_, kdist_, vindices_, dindices_, vheader_, dheader_]:=Module[{}, Catch[
If[Length[vindices]!=Length[vheader] || Length[dindices]!=Length[dheader]
	, Throw["Length of indices doesn't match length of header"];];
matchesnum[Join[vindices, dindices]
			, theotable
			, kvecs
			, kdist
			, Join[vheader, dheader]
			, Length[vindices]
			, Length[dindices]
		]
]]
