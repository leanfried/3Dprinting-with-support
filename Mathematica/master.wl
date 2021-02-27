(* ::Package:: *)

(* ::Title:: *)
(*Master functions for data collected from PIV and digital image analysis*)


(* ::Text:: *)
(*Used for:*)
(*"Changes in filament microstructures during direct ink writing with yield stress fluid support"*)
(*"Printing direction dependent microstructure in direct ink writing"*)
(*"Corner accuracy in direct ink writing with support material"*)
(*Last updated November 2019 by Leanne Friedrich*)


(* ::Chapter:: *)
(*General utilities*)


(* ::Subsubsection:: *)
(*listindex*)


listindex[l_]:=Transpose[{Range[Length[l]], l}]


(* ::Chapter:: *)
(*Data labels*)


(* ::Section:: *)
(*Region info*)


(* ::Subsection:: *)
(*Text labels*)


(* ::Subsubsection:: *)
(*REGIONLIST*)


(* ::Text:: *)
(*CALLED BY: correlations.wl, *)


REGIONLIST = {"nozzle", "ahead", "just behind", "behind outer", "far behind", "behind NN", "behind 2NN"
				, "outer", "NN", "2NN"};


REGIONASSOC[n_]:=REGIONLIST[[(n-9)/2]]


(* ::Subsection:: *)
(*Categories of regions*)


(* ::Subsubsection:: *)
(*isv*)


(* ::Text:: *)
(*isv determines if a column in bigtable2 refers to a transverse flow velocity*)
(*CALLED BY: legcol (orientation.wl), yaxislabel (orientation.wl), linelist (orientation.wl), ylab (orientation.wl), seppr (orientation.wl), septicks (orientation.wl), seplistplot (orientation.wl), rotamp (orientation.wl)*)


isv[bt2n_]:=bt2n<=30


(* ::Subsubsection:: *)
(*I0, IX, IB, IL, IR*)


(* ::Text:: *)
(*I0 is the vertical, IX is horizontal, IB is behind, IL is left, IR is right*)
(*all numbers are column numbers from BIGHEADER2*)


(* ::Input::Initialization::Plain:: *)
(*HEADER2 NUMBERS*)
(*I0 = {18, 15, 21, 27};
IX =  {36, 15, 39, 42};
IB =  {24, 27, 30, 33};
IL = {36, 24};
IR = {39, 42, 30, 33};*)


(*REGION NUMBERS*)
(*I0 = {2, 1, 3, 5};
IX = {8, 1, 9, 10};
IB = {4, 5, 6, 7};
IL = {8, 4};
IR = {9, 10, 6, 7};*)


(*BIGTABLE2 NUMBERS*)
I0 = {13, 11, 15, 19};
IX = {25, 11, 27, 29};
IB = {17, 19, 21, 23};
IL = {25, 17};
IR = {27, 29, 21, 23};


(* ::Subsection:: *)
(*Region dimensions*)


(* ::Subsubsection:: *)
(*vregion*)


(* ::Text:: *)
(*vregion is an association that describes the size of a region in w, given a column from BIGHEADER2*)
(*CALLED BY: regionlegend, detectbasis (orientation.wl)*)


(*HEADER2 NUMBERS*)
(*vregion = <|15-> {-1, 1, -1, 1}
			, 18-> {-2, 2, 1, 4}
			, 21-> {-2, 2, -4, -1}
			, 24-> {-5, -1, -10, -3}
			, 27-> {-1, 1, -10, -3}
			, 30-> {1, 3, -10, -3}
			, 33-> {3, 5, -10, -3}
			, 36-> {-5, -1, -3, 3}
			, 39-> {1, 3, -3, 3}
			, 42-> {3, 5, -3, 3}|>;*)


(*REGION NUMBERS*)
(*vregion = <|1-> {-1, 1, -1, 1}
			, 2-> {-2, 2, 1, 4}
			, 3-> {-2, 2, -4, -1}
			, 4-> {-5, -1, -10, -3}
			, 5-> {-1, 1, -10, -3}
			, 6-> {1, 3, -10, -3}
			, 7-> {3, 5, -10, -3}
			, 8-> {-5, -1, -3, 3}
			, 9-> {1, 3, -3, 3}
			, 10-> {3, 5, -3, 3}|>;*)


(*BIGTABLE2 NUMBERS*)
vregion = <|11-> {-1, 1, -1, 1}
			, 13-> {-2, 2, 1, 4}
			, 15-> {-2, 2, -4, -1}
			, 17-> {-5, -1, -10, -3}
			, 19-> {-1, 1, -10, -3}
			, 21-> {1, 3, -10, -3}
			, 23-> {3, 5, -10, -3}
			, 25-> {-5, -1, -3, 3}
			, 27-> {1, 3, -3, 3}
			, 29-> {3, 5, -3, 3}|>;


(* ::Section:: *)
(*Headers*)


(* ::Subsubsection:: *)
(*HEADER2: raw data from OpenPIV*)


(* ::Text:: *)
(*HEADER2 is for v2 of the OpenPIV output files, e.g. *_32_16_3_2_vecs2 and *_32_16_3_2_combined*)


(* ::Input::Initialization::Plain:: *)
HEADER2=Join[
{"Prism",  "Line ",  "Edge ",  "Time (s) ",  "Distance (mm) ",  "Time from corner (s) ",  "Distance from corner (mm) "}
, Flatten[Table[{"vT", "vP", "N"}[[i]]<>" "<>REGIONLIST[[j]]<>{" (mm/s)", " (mm/s)", ""}[[i]]
, {j, 10}, {i, 3}]]
, Table[{"Position ", "Width ", "Normalized width "}[[i]]<>{"init", "sheared NN", "sheared 2NN"}[[j]]<>{" (mm)", " (mm)", ""}[[i]]
, {j, 3}, {i,3}]
, Table[{"Position ", "Width ", "Normalized width "}[[i]]<>{"relaxed NN", "relaxed 2NN"}[[j]]<>{" (mm)", " (mm)", ""}[[i]]
, {j, 2}, {i,3}]
];


printheader2:=Module[{},
Print["HEADER2"];
Print[listindex[HEADER2[[1;;14]]]];
Print[Grid@Partition[listindex[HEADER2][[15;;44]],3]];
Print[Grid@Partition[listindex[HEADER2][[45;;59]],3]];
]


(* ::Subsubsection:: *)
(*BIGHEADER2: averaged over edges*)


(* ::Text:: *)
(*BIGHEADER2 is for bigtable2.csv*)


BIGHEADER2 = Flatten[{"% TEGDMA","v (mm/s)","type","edge length (mm)","sides","corner angle (\[Degree])","phi (\[Degree])"
				,"Prism","Line ","Edge "
				, Table[{"vx "<>s<>" (mm/s)", "ster "<>s<>" (mm/s)"}, {s, {"nozzle", "ahead", "just behind", "behind left", "behind 0", "behind 1", "behind 2", "left", "right 1", "right 2"}}]
				, Table[{s[[1]]<>" "<>ToString[i]<>" behind "<>s[[2]], "ster "<>s[[1]]<>" "<>ToString[i]<>" behind "<>s[[2]]},{i,3}, {s, {{"position", "(mm)"}, {"wF", "(mm)"}, {"wF", "(normalized)"}}}]
				, Table[{s[[1]]<>" "<>ToString[i]<>" ahead "<>s[[2]], "ster "<>s[[1]]<>" "<>ToString[i]<>" ahead "<>s[[2]]},{i,2,3}, {s, {{"position", "(mm)"}, {"wF", "(mm)"}, {"wF", "(normalized)"}}}]
				, Table[{"\[CapitalDelta]"<>s[[1]]<>" "<>ToString[i]<>" "<>s[[2]], "ster \[CapitalDelta]"<>s[[1]]<>" "<>ToString[i]<>" "<>s[[2]]},{i,2,3}, {s, {{"position", "(mm)"}, {"wF", "(mm)"}, {"wF", "(normalized)"}}}]
				}];


printbigheader2:=Module[{},
Print["BIGHEADER2"];
Print[listindex[BIGHEADER2[[1;;10]]]];
Print[Grid@Partition[listindex[BIGHEADER2][[11;;30]],2]];
Print[Grid@Partition[listindex[BIGHEADER2][[31;;]],6]];
]


(* ::Text:: *)
(*CALLED BY: tplist (correlations.wl), tplist2 (correlations.wl)*)


BTNNINDICES:={{49, 55}, {37, 43}, {53, 59}, {41, 47}}


BTNNHEADER = {{73,"Relaxed position"},{74,"Sheared position"},{75,"Relaxed width"},{76,"Sheared width"}};


(* ::Subsubsection:: *)
(*CHANGEHEADER: changes over the print*)


(* ::Text:: *)
(*CHANGEHEADER is for changetable.csv*)


CHANGEHEADER = Flatten[{"% TEGDMA","v (mm/s)","type","edge length (mm)","sides","corner angle (\[Degree])","phi (\[Degree])", "Prism",  "Edge "
					,Table[
						Table[
							Table[
								j<>"\[CapitalDelta]"<>i<>" "<>k
							, {j, {"", "ster "}}]
						, {i,{"pos (mm)", "stdev (mm)", "stdev (normalized)"}}]
						,{k, {"relax close 2", "relax close 3", "relax far 3", "shear close 2", "shear close 3", "shear far 3"}}]
				}];


printchangeheader:=Module[{},
Print["CHANGEHEADER"];
Print[listindex[CHANGEHEADER[[1;;9]]]];
Print[Grid@Partition[listindex[CHANGEHEADER][[10;;]],6]];]


(* ::Subsubsection:: *)
(*COMBINEHEADER: combine bigtable2 and changetable*)


COMBINEHEADER = Join[CHANGEHEADER[[1;;9]], BIGHEADER2[[11;;30]], 
					Flatten[Table[{i<>" "<>j, "ster "<>i<>" "<>j}
						, {j,{"init",  "\[CapitalDelta]relax NN", "\[CapitalDelta]relax 2nd NN", "\[CapitalDelta]shear"}}
						, {i, {"pos (mm)" ,"width (mm)", "width (normalized)"}}]]];


printcombineheader:=Module[{li},
li = listindex[COMBINEHEADER];
Column[{Row[li[[1;;9]]], Grid[Partition[li[[10;;29]], 2]], Grid[Partition[li[[30;;]], 6]]}]]


(* ::Subsubsection:: *)
(*ORIENTHEADER2: print direction dependence*)


ORIENTHEADER2 = {"xvar", "mode", "subvar", "subset", "x0", "y0", "motor c", 
					"disturbed zone amp", "disturbed zone effect", "disturbed zone error", "disturbed zone vi",
					"solid rotation amp", "solid rotation effect", "solid rotation error", "solid rotation vi",
					"fluid reshaping amp", "fluid reshaping effect", "fluid reshaping error",  "fluid reshaping vi",
					"best fit", "rot amp", "best effect", "best error", "best vi", "best effect-vi"};


printorientheader:=Module[{li},
li = listindex[ORIENTHEADER2];
Column[{Row[li[[1;;4]]], Row[li[[5;;7]]], Grid[Partition[li[[8;;19]], 4]], Row[li[[20;;]]]}]]


(* ::Chapter:: *)
(*Data processing*)


(* ::Section:: *)
(*averages*)


(* ::Subsubsection:: *)
(*ster, mstd, meb*)


(* ::Text:: *)
(*standard error of list l*)


ster[l_]:=If[Length[l]>1, N[StandardDeviation[l]/Sqrt[Length[l]]], 0]


(* ::Text:: *)
(*mean and standard deviation of list l*)


mstd[l_]:=If[Length[l]>0, {Mean[l], StandardDeviation[l]}, {"", ""}]


(* ::Text:: *)
(*mean and standard error bar of list l*)


meb[l_]:=If[Length[l]>1, {{l[[1,1]], N[Mean[l[[;;,2]]]]}, ErrorBar[ster[l[[;;,2]]]]}, {}]


(* ::Subsubsection:: *)
(*accumulatederror, aveacum, avea*)


(* ::Text:: *)
(*propagated standard error from list of standard errors l*)
(*OUTPUT: number*)


accumulatederror[l_]:=Sqrt[Total[(l)^2]]/Length[l]


(* ::Text:: *)
(*given a list of points and errors l, get the mean and accumulated error*)
(*OUTPUT: list of points and errors*)
(*CALLS: ster, accumulatederror (in prismSummaries.wl)*)


aveacum[l_]:={{Mean[l[[;;,1]]], Mean[l[[;;,2]]]}, ErrorBar[ster[l[[;;,1]]]+accumulatederror[l[[;;,3]]], ster[l[[;;,2]]]+accumulatederror[l[[;;,4]]]]}


(* ::Text:: *)
(*given a list of points l, get the mean and standard error*)
(*OUTPUT: list of points and errors*)
(*CALLS: ster*)
(*CALLED BY: tplist (correlations.wl), tplist2 (correlations.wl)*)


avea[l_]:={{Mean[l[[;;,1]]], Mean[l[[;;,2]]]}, ErrorBar[ster[l[[;;,1]]], ster[l[[;;,2]]]]}


(* ::Section:: *)
(*filters*)


(* ::Subsubsection:: *)
(*linedist*)


(* ::Text:: *)
(*linedist finds the distance between the nozzle and the edge of the image along the line length*)
(*phi = angle between printing direction and x-axis in degree*)
(*OUTPUT: number, distance to edge of the image along the print path*)
(*CALLED BY: removestartcorners, removeendcorners*)


linedist[phi_]:=Module[{h,w},
	w = 4.59/2; (*image dimension in mm*)
	h = 2.87/2;
	If[Mod[phi,180]==0
		, h
		, If[Mod[phi,180]==90
			, w
			, Min[Abs[h/Cos[phi*Degree]], Abs[w/Sin[phi*Degree]]]
		]
	]
]


(* ::Subsubsection:: *)
(*removestartcorners*)


(* ::Text:: *)
(*removestartcorners removes frames where the starting corner was within the image frame*)
(*csvlist = input from MATLAB csv export*)
(*OUTPUT: shorter list along the edge*)
(*CALLS: linedist*)
(*CALLED BY: (timeseries.nb), combfile2bt2 (filemanagement.wl)*)


removestartcorners[csvlist_]:=Module[{h, w},
	Select[csvlist, #[[14]]>=linedist[#[[7]]]&]
]


(* ::Subsubsection:: *)
(*removeendcorners*)


(* ::Text:: *)
(*removeendcorners removes frames where the ending corner was within the image frame*)
(*csvlist = input from MATLAB csv export*)
(*OUTPUT: shorter list along the edge*)
(*CALLS: linedist*)
(*CALLED BY: (timeseries.nb), combfile2bt2 (filemanagement.wl)*)


removeendcorners[csvlist_]:=Module[{h, w},
	Select[csvlist, #[[14]]<=#[[4]]-2*0.3*(3-#[[9]])*Tan[Pi/#[[5]]] - linedist[#[[7]]]&]
]


(* ::Subsubsection:: *)
(*removeallcorners*)


(* ::Text:: *)
(*remove starting and ending corners*)
(*csvlist = input from MATLAB csv export*)
(*OUTPUT: shorter list along the edge*)
(*CALLS: removeendcorners, removestartcorners*)


removeallcorners[csvlist_]:=removeendcorners[removestartcorners[csvlist]]


(* ::Chapter:: *)
(*Plots*)


(* ::Section:: *)
(*plot defaults*)


Needs["ErrorBarPlots`"] (*need to run this line to make plots with error bars*)


SetOptions[ErrorListPlot, Joined->True
			, Frame->True
			, AxesStyle->Black
			, FrameStyle->Black
			, AspectRatio->1
			, PlotMarkers->Automatic
			, LabelStyle->Directive[12, Black]];


SetOptions[ListPlot, Joined->True
			, Frame->True
			, AxesStyle->Black
			, FrameStyle->Black
			, AspectRatio->1
			, PlotMarkers->Automatic
			, LabelStyle->Directive[12, Black]];


SetOptions[Plot
			, Frame->True
			, AxesStyle->Black
			, FrameStyle->Black
			, AspectRatio->1
			, LabelStyle->Directive[12, Black]];


(* ::Section:: *)
(*fonts and colors*)


(* ::Subsubsection:: *)
(*Plot scaling*)


(* ::Text:: *)
(*PS is a scaling factor for all graphics so they export in the actual dimensions we asked for*)


PS = 100/72;


(* ::Subsubsection:: *)
(*Font scaling and styling*)


(* ::Text:: *)
(*write strings in Arial in scaled font sizes*)
(*c = color*)
(*m = math*)


s7[st_]:=Style[st, 7*PS, FontFamily->"Arial"];


s8[st_]:=Style[st, 8*PS, FontFamily->"Arial"]


s8m[st_]:=Style[st, 8*PS, Italic, FontFamily->"Times"]


s8c[st_, c_]:=Style[st, c, 8*PS, FontFamily->"Arial"]


s10[s_]:=Style[s, 10*PS, FontFamily->"Arial"]


s10m[s_]:=Style[s, 10*PS, FontFamily->"Times", Italic]


s12[st_]:=Style[st, 12*PS, FontFamily->"Arial"]


s18[s_]:=Style[s, 18*PS, FontFamily->"Arial"]


(* ::Subsubsection:: *)
(*getColors, dark5colors, markerlist*)


(* ::Text:: *)
(*CALLED BY: elpdefault (means.wl), plotallpositions (means.wl), dsrcolorlist (orientation.wl), dark5colors, (timeseries.nb)*)


(* ::Input::Initialization::Plain:: *)
getColors[n_]:=ColorData["RedBlueTones"][#/(n-1)]&/@Range[0,n-1]


(* ::Text:: *)
(*CALLED BY: prismgraphics (orientation.wl), rowprismlegend (orientation.wl)*)


dark5colors:=Module[{colors}, 
colors = getColors[5];
colors[[3]] = Darker[colors[[3]]];
colors]


(* ::Text:: *)
(*CALLED BY: elpsettings (means.wl), elpdefault (means.wl), plotallpositions (means.wl), highlightedwriting, relaxplotgrid (relaxresults.wl), logrelax (relaxresults.wl)*)


markerlist[n_] := {\[FilledCircle], \[FilledSquare], \[FilledUpTriangle], \[FilledDiamond], \[EmptyCircle], \[EmptySquare], \[EmptyUpTriangle], \[EmptyDiamond]}[[1;;n]];


(* ::Subsubsection:: *)
(*ylabelvt*)


(* ::Text:: *)
(*just the transverse flow, but with no region label*)
(*OUTPUT: row of strings*)
(*CALLED BY: yaxislabel (orientation.wl)*)


ylabelvt[n_] := Row[{Style[Subscript["v", "T"], n*PS, FontFamily-> "Times", Italic], Style[" (mm/s)", n*PS, FontFamily->"Arial"]}];


ylabelvt11 = Row[{Style[Subscript["v", "T"], 11*PS, Italic, FontFamily->"Times"], s8["(mm/s)"]}];


(* ::Text:: *)
(*given a string name of region regionstring, get a label for transverse flow velocity*)
(*OUTPUT: row of strings*)


vtlabel[regionstring_]:=Row[{Style[Subscript["v", "T"], FontFamily->"Times", Italic, 9*PS]
								, s8[" "<>regionstring<>" (mm/s)"]}]


(* ::Section:: *)
(*Legends*)


(* ::Subsubsection:: *)
(*plegend, columnexp*)


(* ::Text:: *)
(*f = Row or Column, get a point legend for the support type*)
(*OUTPUT: column or row of text*)
(*CALLED BY: (correlations2.nb), vplotcol (means.wl), (means4.nb), spliteffects (orientation.wl), (orientation.nb)*)


plegend[f_]:=f[{Style["\[EmptyCircle] Layer-by-layer", Black, 8*PS, FontFamily->"Arial"
					],"  "
					, Style["\[FilledCircle] Bath", Gray , 8*PS, FontFamily->"Arial"]}];


(* ::Text:: *)
(*put the point legend at the top right of the figure fig*)
(*OUTPUT: column of graphics*)


columnexp[fig_]:=Column[{plegend[Row], fig}, Alignment->Right]


(* ::Subsubsection:: *)
(*regionlegend*)


(* ::Text:: *)
(*regionlegend is a graphics object that shows outlines of all the regions and fills in the designated ones with designated colors*)
(*indices = list of columns from HEADER2*)
(*colors = list of colors*)
(*OUTPUT: graphics object*)
(*CALLS: vregion*)
(*CALLED BY: legcol (orientation.wl), rlegends, septicks (orientation.wl)*)


regionlegend[indices_, colors_]:=Module[{vregions, allregions, theseregions, nozzle, vr, pr},
vregions = Values[vregion];
allregions = {Graphics[{Thin, Dotted, Gray
					, Table[
						Line[{{x, -10}, {x, 3}}]
						, {x, {-1, 1, 3, 5}}]
				}]};
theseregions = Graphics[Table[
		vr = vregion[indices[[i]]];
		{colors[[i]], Rectangle[{vr[[1]], vr[[3]]}, {vr[[2]], vr[[4]]}]}
		, {i, Length[indices]}]];
nozzle = Graphics[{EdgeForm[Black], FaceForm[], 
				Rectangle[{#[[1]], #[[3]]}, {#[[2]], #[[4]]}]&/@{vregions[[1]], vregions[[1]]*2}}];
pr = {{Min[vregions[[;;, 1]]], Max[vregions[[;;,2]]]}
	, {Min[vregions[[;;,3]]], Max[vregions[[;;,4]]]}};
Show[allregions, theseregions, nozzle
	, PlotRange->pr
	, PlotRangePadding->Scaled[0.001]]
]


(* ::Subsubsection:: *)
(*rlegends*)


(* ::Text:: *)
(*OUTPUT: three graphics objects: a spacer, one with graphical representations of the regions, and one with text of the regions*)
(*CALLS: vregion, regionlegend, s8m, s8, PS, REGIONLIST*)
(*CALLED BY: vplotcol (means.wl), (orientation.nb)*)


rlegends[bigtable2_, lp_]:=Module[{vtab, rleglist, vr, dims, v, spacer, spacer2, rlegend, rlegend2, arrows},
arrows = Length[bigtable2]>0;
If[arrows,(*get the average transverse flow velocity within each region*)
vtab = Table[
			{j, Mean[Select[bigtable2, NumberQ[#[[j]]] && #[[j]]!=0&][[;;, j]]]}
		, {j, 11, 29, 2}];
];
(*list of transverse flow velocities superimposed on cartoons of the regions*)
rleglist = Table[
			vr = vregion[9+2*j];
			If[arrows
				, dims = {(vr[[1]]+vr[[2]])/2, (vr[[3]]+vr[[4]])/2};
				v = {vtab[[j,2]]/0.01, 0};
			];
			Show[
				regionlegend[{9+2*j}, {Gray}]
				, If[arrows,
					Graphics[{
					Thick, Arrowheads[0.2], Arrow[{dims-v/2, dims+v/2}]
					, Text[s8m[Subscript["v", "T"]], dims+v*0.6, If[v[[1]]>0,Left, Right]]
					}]
					,
					{}
				]
				, ImageSize->(6.5*72-lp)/10*PS
				, PlotRange->{{-8, 8}, {-11,5}}
				, AspectRatio->1
			]
	,{j, 10}];

(*spacers*)
spacer2 = Graphics[{Black, Thick, Arrowheads[0.3]
			, Arrow[{{0,0},{0,-2}}]
			,  Arrow[{{-0.75,0.4},{0.4,0.4}}]
			,  Arrow[{{-1,0.4},{-2.15,0.4}}]
			, Text[s8["stage\nmvmt"], {-0.2,-1.3}, Right]
			, Text[s8["in"], {0,0.5}, Bottom]
			, Text[s8["out"], {-1.5, 0.5}, Bottom]}
	, PlotRange->{{-2.25,0.25}, {-2.5, 1.5}}
	, ImageSize->{lp*PS, lp*PS}];
spacer = Graphics[{}, ImageSize->{lp*PS, 8}];

(* header that shows the regions with arrows*)
rlegend = Row[Prepend[rleglist, spacer2]];

(*header that shows the region names*)
rlegend2 =Row[Join[{spacer}, Graphics[Text[s8[#]], ImageSize->{(6.5*72-lp)/10*PS, 10}]&/@REGIONLIST]];

{spacer, rlegend, rlegend2}
]


(* ::Subsubsection:: *)
(*highlightedwriting*)


(* ::Text:: *)
(*highlightedwriting is an image of the nozzle on the third line, with the three lines in colors*)
(*colors = list of colors, e.g. {Gray, Blue, Blue}*)
(*is = image size in px*)
(*fs = font size*)
(*CALLS: markerlist*)
(*OUTPUT: graphics object*)
(*CALLED BY: plotallpositions (means.wl)*)


highlightedwriting[colors_, is_, fs_]:=Module[{markers},
markers = markerlist[3];
	Graphics[{EdgeForm[Black], FaceForm[]
				, Rectangle[{0, 1}, {2/3, 5/3}]
				, Rectangle[{1/6, 7/6}, {1/2, 3/2}]
				, Text["Line", {1/2, 2}]
				, Thickness[0.3]
				, Table[
					{colors[[4-i]]
					, Line[{{i/3,0},{i/3,1}}]
					, Style[Text[markers[[4-i]], {i/3, -1/2}], fs]
					, Style[Text[4-i, {i/3, -1}], fs]}
				, {i, 3}]
			}, ImageSize->is]
			];


(* ::Subsubsection:: *)
(*highlightedline*)


(* ::Text:: *)
(*highlightedline is a prism, where the designated line is thicker*)
(*numedges = # of sides*)
(*line = line #*)
(*color = color of whole graphic*)
(*OUTPUT: graphics object*)
(*CALLED BY: legcol (orientation.wl), hllegend (orientation.wl)*)


highlightedline[numedges_, line_, color_]:=Module[{ls},
Graphics[{EdgeForm[color], color, FaceForm[]
	, Table[
		{If[i==line
			, EdgeForm[Thick]
			, EdgeForm[Thin]
		]
		, RegularPolygon[{i/3,Pi/numedges}, numedges]
	}, {i,line}]
}, PlotRange->(1.05*{{-1,1},{-1,1}})]];


(* ::Section:: *)
(*Ticks*)


(* ::Subsubsection:: *)
(*accu*)


(* ::Text:: *)
(*accu finds the # of decimal points in a number*)
(*n = number*)
(*OUTPUT: # of decimal points*)
(*CALLED BY: ticksfunc (correlations.wl), ticksfunc2 (relaxresults.wl)*)


accu[n_]:=Module[{m, mod},
m = -5;
mod = n;
While[mod>0,
	m = m+1;
	mod = Mod[n, 10^-(m-1)];
];
m]


(* ::Chapter:: *)
(*Tables*)


(* ::Section:: *)
(*Latex tables*)


(* ::Subsubsection:: *)
(*mspace*)


(* ::Text:: *)
(*insert spaces into string st so latex doesn't turn them into en dashes and em dashes*)
(*OUTPUT: string*)
(*CALLED BY: corrtable (correlations.wl), exp2latex (means.wl)*)


mspace[st_]:=StringReplace[st, Reverse[{"--"->"- -", "---"->"- - -", "----"->"- - - -", "-----"->"- - - - -", "------"->"- - - - - -"}]]


(* ::Subsubsection:: *)
(*tb2lx*)


(* ::Text:: *)
(*convert array t into a latex table*)
(*OUTPUT: string*)
(*CALLED BY: corrtable (correlations.wl), exp2latex (means.wl), theory2latex (means.wl), matches (means.wl), matchesnum (means.wl)*)


tb2lx[t_]:=StringRiffle[StringRiffle[#, " & "]&/@t, "\\\\\n"]
