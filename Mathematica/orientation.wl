(* ::Package:: *)

(* ::Title:: *)
(*Print direction dependence*)


(* ::Text:: *)
(*Used for "Printing direction dependent microstructure in direct ink writing"*)
(*Last updated November 2019 by Leanne Friedrich*)


<<"Mathematica\\master.wl"


(* ::Section:: *)
(*Polygon info*)


(* ::Subsubsection:: *)
(*prismangles*)


(* ::Text:: *)
(*prismangles is a list of \[Phi] printed in these experiments*)
(*CALLED BY: orientplotdefault (orientation.wl), ttheo (orientation.wl)*)


prismangles = {Range[30, 270, 120]
				, Range[0, 270, 90]
				, Range[54, 342, 72]
				, Range[30, 330, 60]
				, Range[0, 315, 45]};


(* ::Section:: *)
(*utilities*)


cosd[phi_]:=Cos[phi*Degree];


sind[phi_]:=Sin[phi*Degree];


cs[angle_]:=Cos[angle]*Sin[angle];


(* ::Text:: *)
(*ALPHA is the nozzle angle in degree*)


ALPHA = 17;


(* ::Section:: *)
(*plots*)


(* ::Subsection:: *)
(*Cos, Sin, CosSin general format*)


(* ::Subsubsection::Closed:: *)
(*legcol*)


(* ::Text:: *)
(*legend graphics*)
(*var = column number in BIGTABLE2*)
(*type = support type (2 or 3)*)
(*OUTPUT: graphics*)
(*CALLS: regionlegend (master.wl), highlightedlinewrite (master.wl)*)
(*CALLED BY: dbdisp*)


legcol[var_, type_]:=Show[If[isv[var]
	, regionlegend[{var}, {Lighter[Gray]}]
	, highlightedline[5, (49-var)/6, Black]
], ImageSize->50];


(* ::Subsubsection::Closed:: *)
(*yaxislabel*)


(* ::Text:: *)
(*convert variable column number var in bigtable2 to a y axis label*)
(*OUTPUT: string or row of strings*)
(*CALLS: ylabelvt, isv*)
(*CALLED BY: dbdisp, rotamp, detectbasis*)


yaxislabel[var_]:=If[isv[var], ylabelvt[8], "Position (mm)"]


(* ::Subsubsection::Closed:: *)
(*linelist*)


(* ::Text:: *)
(*select which lines to use*)
(*OUTPUT: list*)
(*CALLS: isv*)
(*CALLED BY: detectbasis*)


linelist[var_]:=If[isv[var], {1,2,3}, {3}]


(* ::Subsubsection::Closed:: *)
(*prismgraphics*)


(* ::Text:: *)
(*prismgraphics is a list of colored polygons that look like the polygons printed in this study*)
(*OUTPUT: list of graphics*)
(*CALLS: dark5colors*)
(*CALLED BY: rowprismlegend, orientplotdefault*)


prismgraphics:= Module[{nlist, colors},
nlist = {3,4,5,6,8};
colors = dark5colors;
Table[
	Graphics[{
		Opacity[0.7]
		, colors[[i]]
		, RegularPolygon[{0.01,Pi/nlist[[i]]}, nlist[[i]]]
	}]
, {i,5}]];


(* ::Subsubsection::Closed:: *)
(*rowprismlegend*)


(* ::Text:: *)
(*OUTPUT: graphics row*)
(*CALLS: prismgraphics, dark5colors (master.wl), s8c (master.wl)*)
(*CALLED BY: dbdisp, wfplotgrid, (orientation.nb)*)


rowprismlegend:=Module[{c, p},
p = prismgraphics;
c = dark5colors;
Row[
	Flatten[{
		s8["# of sides"]
		, Flatten[
			Table[{
				Show[p[[i]], ImageSize->12]
				, s8c[{3,4,5,6,8}[[i]], c[[i]]]
				, " "
			}, {i,5}]
		]
	}]
, " "]
]


(* ::Subsubsection:: *)
(*orientplotdefault*)


(* ::Text:: *)
(*tp is a list to plot*)
(*pri is the plot range*)
(*xlabel is the x axis label string*)
(*yl is y axis label string*)
(*plotlabel is string label or number for no label*)
(*left is bool to include y axis ticks and labels*)
(*bottom is bool to include x axis ticks and labels*)
(*legend is bool to include legend*)
(*joined is bool to join points with lines*)
(*OUTPUT: one plot*)
(*CALLS: dark5colors (master.wl), prismgraphics, prismangles (master.wl), s10m (master.wl)*)
(*CALLED BY: orientrowdefault, theogrid*)


orientplotdefault[tp_, pri_, xlabel_, yl_, plotlabel_, left_, bottom_, legend_, rl_, joined_]:=Module[{colors, fs, ls, is, lp, bp},
colors = dark5colors;
fs = 8;
ls = If[rl, 4*fs, 6.5*fs];
lp = If[left, ls, 0];
is = ((72*6.5-2*ls)/6+lp);
bp = If[bottom, fs*3.2, 2];
If[Length[tp[[1,1,1]]]==0, ListPlot, ErrorListPlot][tp
		, Axes->None
		, FrameLabel->{s8m[xlabel], yl}
		, FrameTicks->{Automatic, {{-1, -0.5, 0, 0.5, 1}, None}}
		, GridLines->{{0},{0}}
		, ImagePadding->{{lp, 2}, {bp, 2}}*PS
		, ImageSize-> is*PS
		, Joined->joined
		, LabelStyle->Directive[fs*PS, Black]
		, PlotLabel->If[StringQ[plotlabel] || (!NumberQ[plotlabel] && plotlabel!=None), s8[plotlabel], None]
		, PlotLegends->If[legend, {3,4,5,6,8}, None]
		, PlotMarkers->Table[{prismgraphics[[j]], 0.085}, {j, Length[prismangles]}]	
		, PlotRange->pri
		, PlotRangePadding->Scaled[0.1]
		, PlotStyle->colors
		, RotateLabel->rl
	]	
]


(* ::Subsubsection::Closed:: *)
(*orientrowdefault*)


(* ::Text:: *)
(*tp3 is a list to plot*)
(*pri is the plot range*)
(*yl is y axis label string*)
(*bottom is bool to include x axis ticks and labels*)
(*legendoverride is bool to include legend*)
(*OUTPUT : list of 3 plots*)
(*CALLS : orientplotdefault*)
(*CALLED BY: dbdisp, wfplots, rotamp*)


orientrowdefault[tp3_, pri_, yl_, bottom_, legendoverride_, rl_]:=Module[{caplist},
caplist = {"Cos\[Phi]", "Sin\[Phi]", "Cos\[Phi]Sin\[Phi]"};
Table[
	orientplotdefault[
		tp3[[i]]
		, pri
		, caplist[[i]]
		, yl
		, None
		, i==1
		, bottom 
		, If[legendoverride, i==3, False]
		, rl
		, True
	]
, {i, 3}]
]


(* ::Subsubsection::Closed:: *)
(*dbdisp*)


(* ::Text:: *)
(*detect basis display function*)
(*theo3 = fitted theoretical profile*)
(*et1 = original experimental profile data*)
(*et3 = error corrected experimental profile data*)
(*type = support type, 2 for layer-by-layer, 3 for bath*)
(*var = dependent variable column number in bigtable2*)
(*CALLS: legcol, orientrowdefault, yaxislabel, rowprismlegend, s8 (master.wl)*)
(*CALLED BY: detectbasis*)


dbdisp[theo3_, et1_, et3_, type_, var_]:=Module[{mm, tlist, rl},
tlist = {et1, theo3, et3};
mm = MinMax[Flatten[(#[[;;,;;,;;,2]])&/@tlist]];
rl = legcol[var, type];
Column[{
	Row[{
		Column[{
			If[var<31, s8[REGIONASSOC[var]], s8["Line "<>ToString[(49-var)/6]]]
			, s8[Switch[type, 2, "Layer-by-layer", 3, "Bath"]]
			, rowprismlegend
		}, Alignment->Right]
		,
		rl
	}, "  "]
	,
	Grid[
		Table[
			Prepend[
				orientrowdefault[tlist[[i]]
					, mm
					, yaxislabel[var]
					, i==Length[tlist]
					, False
					, False
				]
				,
				Rotate[Text[s8[{"Experimental", "Fitted", "Corrected"}[[i]]]], Pi/2]
			]
		, {i, Length[tlist]}]
	]
}, Alignment->Right]
];


(* ::Subsection:: *)
(*Compare fits of disturbed zone, solid rotation, and fluid reshaping models*)


(* ::Subsubsection::Closed:: *)
(*lprpis*)


(* ::Text:: *)
(*left padding, right padding, and image size*)
(*ticks = bool to plot ticks*)
(*right = bool to plot right border*)
(*totalwidth = total width of image*)
(*numplots = number of plots in image*)
(*OUTPUT: {left pad, right pad, image size}*)
(*CALLED BY: bfcatplot, DSRerrplot, hllegend, fitplot*)


lprpis[ticks_, right_, totalwidth_, numplots_]:=Module[{lp, rp, is},
lp = If[ticks, 50, 0];
rp = If[right, 1, 0];
is = (72*totalwidth-50-1)/numplots + lp+rp;
{lp, rp, is}]


(* ::Subsubsection::Closed:: *)
(*lpbfdsr*)


(* ::Text:: *)
(*listplot for best fit and dsr plots*)
(*tp = points to plot*)
(*ar = aspectratio*)
(*ticks = bool to plot ticks*)
(*ylabel = string y label*)
(*right = bool farthest right*)
(*yticks = list of yticks*)
(*lp = left padding in px*)
(*rp = right padding in px*)
(*is = image size in px*)
(*colorlist = list of colors*)
(*markerlist = list of markers*)
(*pr = plot range*)
(*OUTPUT: one plot*)
(*CALLED BY: bfcatplot, DSRerrplot, fitplot*)


lpbfdsr[tp_, ar_, ticks_, ylabel_, right_, yticks_, lp_, rp_, is_, colorlist_, markerlist_, pr_]:=
ListPlot[tp
	, AspectRatio->ar
	, Axes->None
	, Frame->{{True, right}, {True, True}}
	, FrameStyle->{{If[NumberQ[ticks] || ticks, Black, GrayLevel[0.8]], If[right, Black, Gray]}, {Black, Black}}
	, FrameLabel->{None, If[!NumberQ[ticks]&&ticks,ylabel, None]}
	, FrameTicks->{{If[!NumberQ[ticks] && ticks, yticks, None], None}, {None, None}}
	, GridLines->{None, {0}}
	, ImagePadding->{{lp, rp}, {3, 3}}*PS
	, ImageSize->is*PS
	, Joined -> False
	, LabelStyle->Directive[8*PS, Black]
	, PlotRange->pr
	, PlotStyle-> colorlist
	, PlotMarkers-> markerlist
]


(* ::Subsubsection::Closed:: *)
(*bfcatplot*)


(* ::Text:: *)
(*orients = orients table*)
(*var = variable number as bigtable2 header index*)
(*ticks = bool true to show left ticks*)
(*right = bool true to plot right frame*)
(*numplots = number of plots in the whole plot*)
(*totalwidth = total width of whole plot in inches*)
(*OUTPUT: graphics of one plot*)
(*CALLS: lprpis, lpbfdsr*)
(*CALLED BY: (orientation.nb)*)


bfcatplot[orients_, var_, ticks_, right_, numplots_, totalwidth_]:=Module[{rp, colorlist, markerlist, tp, yticks
																			, lp, is, ar, ylabel, pr},
{lp, rp, is} = lprpis[ticks, right, totalwidth, numplots];
colorlist = {Gray, Black};
markerlist = {#, 15}&/@{\[FilledCircle],\[EmptyCircle]};
yticks = {{1,"DZ"},{2, "SR"},{3, "FR"}};
tp = Table[
		Select[orients, #[[3]]==0 && #[[2]]==j && #[[1]]==var&][[;;,{1, 20}]]
	, {j, {3,2}}];
ar = 1;
ylabel = "Best Fit";
pr = {0.5, 3.5};
lpbfdsr[tp, ar, ticks, ylabel, right, yticks, lp, rp, is, colorlist, markerlist, pr]
]


(* ::Subsubsection::Closed:: *)
(*dsrcolorlist*)


(* ::Text:: *)
(*CALLED BY: DSRerrplot, DSRlegend*)


dsrcolorlist:=Module[{gc6},
gc6 = getColors[6];
{Gray, Black, gc6[[2]], gc6[[1]], gc6[[5]], gc6[[6]]}];


(* ::Subsubsection::Closed:: *)
(*DSRlegend*)


(* ::Text:: *)
(*f = Row or Column*)
(*CALLS: dsrcolorlist*)
(*CALLED BY: (orientation.nb)*)


DSRlegend[f_]:=Module[{colorlist},
colorlist = dsrcolorlist;
f[{s8c["\[FilledCircle] Disturbed zone (D) ", Black]
	, s8c["\[FilledSquare] Solid rotation (S) ", colorlist[[4]]]
	, s8c["\[FilledUpTriangle] Fluid reshaping (R) " ,colorlist[[6]]]}]]


(* ::Subsubsection::Closed:: *)
(*DSRerrplot*)


(* ::Text:: *)
(*orients = orients table*)
(*var = variable number as bigtable2 header index*)
(*ticks = bool true to show left ticks*)
(*right = bool true to plot right frame*)
(*numplots = number of plots in the whole plot*)
(*totalwidth = total width of whole plot in inches*)
(*yi = initial ORIENTHEADER2 index to probe*)
(*OUTPUT: graphics of one plot*)
(*CALLS: lprpis, dsrcolorlist, isv, lpbfdsr*)
(*CALLED BY: (orientation.nb)*)


DSRerrplot[orients_, var_, ticks_, right_, numplots_, totalwidth_, yi_]:=Module[{rp, colorlist, markerlist, tp, yticks
																			, lp, is, mm, dy, ar, ylabel, pr},
{lp, rp, is} = lprpis[ticks, right, totalwidth, numplots];
colorlist = dsrcolorlist;
markerlist = {#, 16}&/@{\[FilledCircle],\[EmptyCircle], \[FilledSquare], \[EmptySquare], \[FilledUpTriangle], \[EmptyUpTriangle]};
yticks = Switch[yi
			, 8, If[isv[var], {-0.3, {0, "0.0"}, 0.3, 0.6}, {-0.02, {0, "0.00"}, 0.02}]
			, 10, If[isv[var], {{0,"0.00"}, 0.01, 0.02}, {{0, "0.000"}, 0.002, 0.004}]
		];
tp = {#}&/@Flatten[Table[
		Table[
			{yvar, Select[orients, #[[1]]==var && #[[2]]==j && #[[3]]==0&][[1, yvar]]}
		, {j, {3,2}}]
	, {yvar, yi, yi+8, 4}],1];
ar = 1;
ylabel =  If[yi==8, Column[{"D, S, R", "(mm/s)"}], Column[{"Error","(mm/s)"}]];
pr = {{yi-2, yi+10}, Switch[yi
						, 8, If[isv[var], {-0.35, 0.65}, {-0.05, 0.05}]
						, 10, If[isv[var], {0, 0.023}, {0, 0.005}]]};
lpbfdsr[tp, ar, ticks, ylabel, right, yticks, lp, rp, is, colorlist, markerlist, pr]
]


(* ::Subsubsection::Closed:: *)
(*hllegend*)


(* ::Text:: *)
(*highlighted line list*)
(*totalwidth = total width of image*)
(*OUTPUT: row of images*)
(*CALLS: highlightedline (master.wl), lprpis*)
(*CALLED BY: (orientation.nb)*)


hllegend[totalwidth_]:=Module[{lp, rp, is},
{lp, rp, is} = lprpis[True, True, totalwidth, 3];
Row[
	Prepend[
		Table[Show[highlightedline[5, i, Black], ImageSize->(is-lp-rp)*PS], {i, 3}]
	, Graphics[{}, ImageSize->{lp*PS, 8}]
	]
]
]


(* ::Subsection:: *)
(*Plot coefficients*)


(* ::Subsubsection::Closed:: *)
(*ylab*)


(* ::Text:: *)
(*ylabels for fitting coefficients*)
(*var = dependent variable (flow or dist metric) column number in BIGTABLE2*)
(*param = fitting parameter column number in ORIENTHEADER2*)
(*CALLS: isv (master.wl), s8m (master.wl)*)
(*CALLED BY: fitplot, seplistplot, (orientation.nb)*)


ylab[var_, param_]:=Module[{vti},
vti = If[isv[var], Subscript["v", "Ti"], Subscript["L", "i"]];
Row[{s8m[Switch[param
			, 5, Subscript["O", "x"]
			, 6, Subscript["O", "y"]
			, 7, "M"
			, 8, "D"
			, 9, vti
			, 12, "S"
			, 13, vti
			, 16, "R"
			, 17, vti
			, 21, "DSR"
			, 22, vti
		]], s8[If[isv[param], " (mm/s)", " (mm)"]]}]
]


(* ::Subsubsection::Closed:: *)
(*fitplot*)


(* ::Text:: *)
(*orients = orients table*)
(*var = variable number as bigtable2 header index*)
(*ticks = bool true to show left ticks*)
(*right = bool true to plot right frame*)
(*numplots = number of plots in the whole plot*)
(*totalwidth = total width of whole plot in inches*)
(*param = ORIENTHEADER2 index to plot*)
(*OUTPUT: graphics of one plot*)
(*CALLS: lprpis, isv (master.wl), ylab, lpbfdsr*)
(*CALLED BY:  (orientation.nb)*)


fitplot[orients_, var_, t1_, right_, numplots_, totalwidth_, param_]:=Module[{oselect, rp, colorlist, markerlist, tp, yticks
																			, lp, is, ar, ylabel, pr, min, max, dm, vti, ticks},
If[NumberQ[t1]
	, ticks = False;
	, ticks = t1;
];
{lp, rp, is} = lprpis[ticks, right, totalwidth, numplots];
oselect = Select[orients, #[[3]]==0 &];
colorlist = {Gray, Black};
markerlist = {#, 15}&/@{\[FilledCircle],\[EmptyCircle]};
If[isv[var],
	pr = MinMax[oselect[[;;, param]]];
	pr = pr + {-(pr[[2]]-pr[[1]])*0.2, (pr[[2]]-pr[[1]])*0.2};
	min = Ceiling[pr[[1]],  0.1];
	max = Floor[pr[[2]], 0.1];
	If[max-min<=0.3
		, dm = 0.1
		, dm = 0.3
	];
	yticks = DeleteDuplicates[Join[Range[dm, max, dm], -Range[dm, -min, dm], {{0, "0.0"}}]];
	,
	pr = {-0.15, 0.15};
	yticks = {-0.15, {0, "0.00"}, 0.15};
];
tp = Table[
		{{1, Select[oselect, #[[2]]==j && #[[1]]==var&][[1,param]]}}
	, {j, {3,2}}];
ar = If[isv[var], 1, 3];
ylabel = ylab[var, param];
lpbfdsr[tp, ar, t1, ylabel, right, yticks, lp, rp, is, colorlist, markerlist, pr]
]


(* ::Subsection:: *)
(*Plot coefficients split by printing parameter*)


(* ::Subsubsection::Closed:: *)
(*seppr*)


(* ::Text:: *)
(*split effects plot range*)
(*svar = split variable column number in BIGHEADER2*)
(*j = column number in ORIENTHEADER2*)
(*var = fitted variable column number in BIGHEADER*)
(*OUTPUT: plot range*)
(*CALLS: isv (master.wl)*)
(*CALLED BY: seplistplot*)


seppr[svar_, j_, var_]:=If[svar==0 || isv[var]
						, All
						, Switch[j
								, 5, {-0.05, 0.09}
								, 6, {-0.12, 0.00}
								, 7, {-0.04, 0.03}
								, 8, {-0.3, 0.34}
								, 9, {-0.1, 0.25}
								, 12, {-0.05, 0.08}
								, 13, {-0.04, 0.09}]
					]


(* ::Subsubsection::Closed:: *)
(*septicks*)


(* ::Text:: *)
(*ticks for split effects plot*)
(*split effects plot range*)
(*svar = split variable column number in BIGHEADER2*)
(*tp = points to plot*)
(*j = column number in ORIENTHEADER2*)
(*dd = list of dependent variable column values (e.g. 11 to 29)*)
(*var = fitted variable column number in BIGHEADER2*)
(*OUTPUT: ticks list*)
(*CALLS: REGIONLIST (master.wl), regionlegend (master.wl), isv (master.wl)*)
(*CALLED BY: seplistplot*)


septicks[svar_, tp_, j_, dd_, var_]:=Module[{dmf, yticks, ticks},
	dmf = Differences[MinMax[Append[Flatten[tp,1][[;;,2]], 0]]][[1]];
	yticks = If[svar==0 || isv[var]
				, Range[-0.32, 0.32
					, If[dmf>0.24
						, 0.16
						, If[dmf>0.12
							, 0.08
							, If[dmf>0.06
								, 0.04
								, If[dmf>0.03
									, 0.02
									, 0.01
								]
							]
						]
					]
				]
				, Range[-0.2, 0.2
					, Switch[j
						, 5, 0.05
						, 6, 0.05
						, 7, 0.04
						, 8, 0.16
						, 9, 0.08
						, 12, 0.04
						, 13, 0.05
					]
				]
	];
	ticks = {
				{{#, If[#==0, "0.00", #]}&/@yticks, None}
				, {Table[{k, If[isv[var]
										, Column[{REGIONLIST[[(dd[[k]]-9)/2]]
												, Show[regionlegend[{dd[[k]]}, {GrayLevel[0.7]}], ImageSize->40]
											}, Alignment->Center]
										, k
									]},{k,Length[dd]}], None}
			};
	ticks
]


(* ::Subsubsection::Closed:: *)
(*septp*)


(* ::Text:: *)
(*select points to plot for split effects plot*)
(*orients = orients table*)
(*svar = split variable column number in BIGHEADER2*)
(*dd = list of dependent variable column values (e.g. 11 to 29)*)
(*j = column number in ORIENTHEADER2*)
(*OUTPUT: list of points fitted variable 1 LBL, fitted variable 1 bath, fitted variable 2 LBL, etc.*)
(*CALLED BY: spliteffectspos, spliteffects*)


septp[orients_, svar_, dd_, j_]:=Module[{s1},
Flatten[
		Table[
			Table[
				s1 = SortBy[Select[orients, #[[1]]==dd[[i]] && #[[2]]==m && #[[3]]==svar&], #[[4]]&];
				Table[
					{i + (k-(Length[s1]+1)/2)/(Length[s1]+1), s1[[k,j]]}
				, {k, Length[s1]}]
			, {m, {3,2}}]
		, {i, Length[dd]}]
	,1]
]


(* ::Subsubsection::Closed:: *)
(*seplistplot*)


(* ::Text:: *)
(*plot points*)
(*svar = split variable column number in BIGHEADER2*)
(*tp = points to plot*)
(*j = column number in ORIENTHEADER2*)
(*dd = list of dependent variable column values (e.g. 11 to 29)*)
(*jlist = list of all coefficient column numbers in ORIENTHEADER2 to plot*)
(*var = fitted variable column number in BIGHEADER2*)
(*OUTPUT: one graphics plot*)
(*CALLS: seppr, septicks, ylab, isv (master.wl)*)
(*CALLED BY: spliteffectspos, spliteffects*)


seplistplot[svar_, tp_, j_, dd_, jlist_, var_]:=Module[{ticks, lp, pr, ip, ylabel, joined, gl, is},
pr = seppr[svar,j, var];
ticks = septicks[svar, tp, j, dd, var];
lp = If[isv[var], 40, Switch[svar, 0, 30, 1, 40, _, 2]];
ip = {{lp, 2}, {If[j==jlist[[-1]] || svar==0, If[isv[var], 70, 30], 2], 3}};
is = (72*6.5-40)/If[isv[var], 1, 3] + lp;
ylabel = ylab[var, j];
joined = Table[If[Length[tp[[i]]]>1, KendallTauTest[tp[[i, ;;, 1]],tp[[i,;;,2]], "PValue"]<0.05, False], {i, Length[tp]}];
gl = Table[(tp[[i, -1, 1]] + tp[[i+1, 1, 1]])/2 ,{i, 2, Length[tp]-1, 2}];
ListPlot[tp
	, AspectRatio->If[isv[var], 1/8, If[svar>0, 1/3, 1]]
	, Axes->None
	, FrameLabel->{If[isv[var], None, "Line"], If[svar>0, ylabel, None]}
	, FrameTicks->ticks
	, GridLines->{If[svar>0, gl, None], {0}}
	, ImagePadding->ip*PS
	, ImageSize->is*PS
	, Joined->joined
	, LabelStyle->Directive[8*PS, Black]
	, PlotLabel->If[svar>0, None, s8@ylabel]
	, PlotLegends->None
	, PlotMarkers->({#, 8*PS}&/@{"\[FilledCircle]", "\[EmptyCircle]"})
	, PlotRange->{All, pr}
	, PlotRangePadding->{1/Length[tp[[1]]], Scaled[0.1]}
	, PlotStyle->{Gray, Black}
]]


(* ::Subsubsection:: *)
(*spliteffectspos*)


(* ::Text:: *)
(*plot coefficients as a function of printing parameters for distribution positions*)
(*orients = orient table*)
(*svar = split variable as column number in BIGTABLE2 (independent variable/printing parameter)*)
(*OUTPUT: grid of plots*)
(*CALLS: s8 (master.wl), septp, seplistplot*)
(*CALLED BY: (orientation.nb)*)


spliteffectspos[orients_, svar_]:=Module[{dd, jlist, vplot, tp, s1, mind, diffd},
dd = {43, 37, 31}; (*list of BIGHEADER2 columns to plot*)
jlist = {13, 5, 6, 7, 12}; (*list of ORIENTHEADER2 columns to plot*)
vplot = If[svar>0, Column, Row][
			Join[
				{s8[Switch[svar, 1, "TEGDMA 20, 25, 30, 35%", 2, "Print speed 3, 6, 9, 12 mm/s", 4, "Edge length 6, 8, 10, 12 mm", 0, ""]]}
				, Table[
					tp = septp[orients, svar, dd, j];
					seplistplot[svar, tp, j, dd, jlist, 31]
				,{j,jlist}]
			]
		, Alignment->Center];
vplot
]


(* ::Subsubsection:: *)
(*spliteffects*)


(* ::Text:: *)
(*plot coefficients as a function of printing parameters for flow speeds*)
(*orients = orient table*)
(*svar = split variable as column number in BIGTABLE2 (independent variable/printing parameter)*)
(*OUTPUT: grid of plots*)
(*CALLS: s8 (master.wl), septp, seplistplot, plegend (master.wl)*)
(*CALLED BY: (orientation.nb)*)


spliteffects[orients_, svar_]:=Module[{dd, jlist, dd2, vplot, tp, dmf, yticks, ticks, ip, pl, mind, diffd, g1},
dd = Range[11, 29, 2];
jlist = {13, 5, 6, 7, 12};
pl = Row[{s8[Switch[svar, 1, "TEGDMA 20, 25, 30, 35%", 2, "Print speed 3, 6, 9, 12 mm/s", 4, "Edge length 6, 8, 10, 12 mm", 0, ""]]
			, plegend[Row]}, "\t"];
vplot = Column[{
			pl
			,Column[Table[
				tp = septp[orients, svar, dd, j];
				seplistplot[svar, tp, j, dd, jlist, 11]
			,{j,jlist}]]
		}, Alignment->Right];
vplot
]


(* ::Subsection:: *)
(*Distribution widths*)


(* ::Subsubsection::Closed:: *)
(*wfplots*)


(* ::Text:: *)
(*row of particle distribution width plots*)
(*bigtable = subset of bigtable2*)
(*bot = bool to include bottom labels*)
(*pr = plot range*)
(*OUTPUT: row of plots*)
(*CALLS: orientrowdefault, s8m (master.wl), accumulatederror (master.wl)*)
(*CALLED BY: wfplotgrid, (orientation.nb)*)


wfplots[bigtable_, bot_, pr_]:=Module[{bt, s2, g1},
bt = Select[bigtable, #[[35]]!=0&];
s2 = Table[
		Table[
			g1 = GatherBy[{N[f[#[[7]]*Degree]], #[[35]], #[[36]]}&/@Select[bt, #[[5]]==sides&], #[[1]]&];
			Sort[{{#[[1,1]], Mean[#[[;;,2]]]}, ErrorBar[accumulatederror[#[[;;,3]]]]}&/@g1]
		,{sides, {3,4,5,6,8}}]
	,{f,{Cos, Sin, cs}}];
Row[orientrowdefault[s2, pr, s8m[Column[{Subscript["w", "Fp"], OverBar[Subscript["w", "u"]]}]]
		, bot, False, False]]
]


(* ::Subsubsection::Closed:: *)
(*wfplotgrid*)


(* ::Text:: *)
(*grid of particle distribution width plots split by printing parameters*)
(*bigtable = subset of bigtable2*)
(*indepvar = independent variable column number to split by*)
(*pr = plot range*)
(*OUTPUT: grid of plots*)
(*CALLS: s8 (master.wl), wfplots, rowprismlegend*)
(*CALLED BY: (orientation.nb)*)


wfplotgrid[bigtable2_, indepvar_, pr_]:=Module[{varlist},
varlist = Sort[DeleteDuplicates[bigtable2[[2;;, indepvar]]]];
Row[Table[
	Column[
		{Row[{s8[Switch[mode,2, "Layer-by-layer", 3, "Bath"]],"\t", If[mode==3, rowprismlegend, ""]}]
		,
		Column[Table[
			Column[{
				s8[Switch[indepvar, 1, "", 2, "Print speed ", 4, "Edge length "]<>ToString[t]<>Switch[indepvar, 1, " w% TEGDMA", 2, " (mm/s)", 4, " (mm)"]]
				,
				wfplots[Select[bigtable2, #[[3]]==mode && #[[indepvar]]==t&], t==varlist[[-1]], pr]
			}, Alignment->Left]
		,{t, varlist}]
		]
	}, Alignment->Left]
, {mode, 2,3}]]]


(* ::Subsection:: *)
(*Theory*)


theophiplot[yfunc_, title_, period_, pr_, left_]:=Module[{points, colors, fs, ls, lp, is, bp},
points = Table[
				Table[
						{\[Phi], SetAccuracy[yfunc[\[Phi]],6]}
					, {\[Phi],  Select[alist, #<period&]}]
			, {alist, prismangles}];
colors = dark5colors;
fs = 8;
ls = 6.5*fs;
lp = If[left, ls, 0];
is = ((72*6.5-ls)/4+lp);
bp = fs*3.2;
Show[
	Plot[yfunc[phi], {phi, 0, period}
		, Axes->None
		, FrameLabel->{s8m["\[Phi] (\[Degree])"], title}
		, FrameTicks->{Automatic, {Range[0, 360, 90], None}}
		, GridLines->{{0},{0}}
		, ImagePadding->{{lp, 2}, {bp, 2}}*PS
		, ImageSize-> is*PS
		, LabelStyle->Directive[fs*PS, Black]
		, PlotRange->pr
		, PlotRangePadding->Scaled[0.1]
		, PlotStyle->{Thin, Black, Dashed}
		, RotateLabel->False
	]
	,
	ListPlot[points
		, PlotMarkers->Table[{prismgraphics[[j]], 0.06}, {j, Length[prismangles]}]	
		, Joined-> False
	]
]]


(* ::Subsubsection::Closed:: *)
(*theogrid*)


(* ::Text:: *)
(*theogrid is a list of plots*)
(*yfunc is a theoretical function e.g. rigidmodel*)
(*pr is plot range*)
(*forceleft is 1 to always plot left, -1 to never plot left, 0 to only plot left on the first plot*)
(*forcelegend is 1 to always plot legend, -1 to never plot legend, 0 to only plot legend on the last plot*)
(*title is string, or a number for no title*)
(*bottom is bool to include xaxis labels*)
(*is is image size*)
(*ylabel is y axis label*)
(*OUTPUT: list of plots*)
(*CALLS: ttheo, orientplotdefault*)
(*CALLED BY:  (orientation.nb)*)


theogrid[yfunc_, pr_, forceleft_, forcelegend_, title_, bottom_, ylabel_]:=Module[{ttheos, colors, pri, ls, labels},
ttheos = Table[ttheo[xfunc, yfunc], {xfunc, {Cos, Sin, cs}}];
labels = {"cos\[Phi]", "sin\[Phi]", "cos\[Phi]sin\[Phi]"};
If[Length[pr]!=2
		, pri = {All, MinMax[Flatten[ttheos,2][[;;,2]]]};
		, pri = pr;
];
Table[
	orientplotdefault[
		ttheos[[i]]
		, pri
		, labels[[i]]
		, ylabel
		, If[i==1, title, 0]
		, If[forceleft==1, True, If[forceleft==-1, False, i==1]]
		, bottom
		, If[forcelegend==1, True, If[forcelegend==-1, False, i==3]]
		, False
		, True
	]
, {i, Length[ttheos]}]
]


(* ::Subsubsection::Closed:: *)
(*theogridraw*)


(* ::Text:: *)
(*theogridraw is a list of plots, not averaged in x*)
(*yfunc is a theoretical function e.g. rigidmodel*)
(*pr is plot range*)
(*forceleft is 1 to always plot left, -1 to never plot left, 0 to only plot left on the first plot*)
(*forcelegend is 1 to always plot legend, -1 to never plot legend, 0 to only plot legend on the last plot*)
(*title is string, or a number for no title*)
(*bottom is bool to include xaxis labels*)
(*is is image size*)
(*ylabel is y axis label*)
(*OUTPUT: list of plots*)
(*CALLS: ttheo, orientplotdefault*)
(*CALLED BY:  (orientation.nb)*)


theogridraw[yfunc_, pr_, forceleft_, forcelegend_, title_, bottom_, ylabel_]:=Module[{ttheos, colors, pri, ls, labels, ttheosall},
ttheos = Table[ttheoraw[xfunc, yfunc], {xfunc, {Cos, Sin, cs}}];
ttheosall = Table[ttheoall[xfunc, yfunc], {xfunc, {Cos, Sin, cs}}];
labels = {"cos\[Phi]", "sin\[Phi]", "cos\[Phi]sin\[Phi]"};
If[Length[pr]!=2
		, pri = {All, MinMax[Flatten[ttheos,2][[;;,2]]]};
		, pri = pr;
];
Table[
	Show[
	orientplotdefault[
		ttheos[[i]]
		, pri
		, labels[[i]]
		, ylabel
		, If[i==1, title, 0]
		, If[forceleft==1, True, If[forceleft==-1, False, i==1]]
		, bottom
		, If[forcelegend==1, True, If[forcelegend==-1, False, i==3]]
		, False
		, False
	]
	,
	ListLinePlot[ttheosall[[i]], PlotStyle->{Thin, Dashed, Black}]
	]
, {i, Length[ttheos]}]
]


(* ::Section:: *)
(*theoretical profiles*)


(* ::Subsection:: *)
(*motor*)


(* ::Subsubsection::Closed:: *)
(*vxcomponent*)


(* ::Text:: *)
(*perceived transverse flow from extra vx*)
(*\[Phi] is in degrees*)
(*OUTPUT: number*)
(*CALLS: cosd, sind*)


vxcomponent[\[Phi]_]:=Sign[sind[\[Phi]]]*cosd[\[Phi]]


(* ::Subsubsection::Closed:: *)
(*vycomponent*)


(* ::Text:: *)
(*perceived transverse flow from extra vy*)
(*\[Phi] is in degrees*)
(*OUTPUT: number*)
(*CALLS: cosd, sind*)


vycomponent[\[Phi]_]:=Sign[cosd[\[Phi]]]*sind[\[Phi]]


(* ::Subsubsection::Closed:: *)
(*cxcomponent*)


(* ::Text:: *)
(*perceived transverse flow from scaled v*)
(*\[Phi] is in degrees*)
(*OUTPUT: number*)
(*CALLS: cosd, sind*)
(*CALLED BY: detectbasis, (orientation.nb)*)


cxcomponent[\[Phi]_]:=2*cosd[\[Phi]]*sind[\[Phi]]


(* ::Subsection:: *)
(*disturbed zone*)


(* ::Subsubsection::Closed:: *)
(*slidingmid*)


(* ::Text:: *)
(*sliding along the middle of the nozzle*)
(*pma is in degrees*)
(*OUTPUT: number*)
(*CALLS: sind, cosd*)
(*CALLED BY: slidingx3*)


slidingmid[pma_] := Module[{a, rotatedbox, boxlines, b, m, p}, 
b = Sqrt[2]*Sin[(pma-45)*Degree];
If[45/2<pma<90-45/2,  
m = (1/2-b)/cosd[pma];
p = (1/2+b)/sind[pma];];
Piecewise[{
	{1, pma<=45/2}
	, {cosd[pma]*m - sind[pma]*p, 45/2<pma<90-45/2}
	, {-1, pma>=90-45/2}
	}]];


(* ::Subsubsection::Closed:: *)
(*slidingside*)


(* ::Text:: *)
(*sliding along the sides of the nozzle*)
(*pma is in degrees*)
(*OUTPUT: number*)
(*CALLS: sind, cosd*)
(*CALLED BY: slidingx3*)


slidingside[pma_]:=Module[{a, rotatedbox, boxlines, b, m, p, df}, 
b = Sqrt[2]*Sin[(pma-45)*Degree];
Piecewise[{
	{2*cosd[pma] - 1 - sind[pma]*2, pma<45/2}
	, {0, 45/2<=pma<=90-45/2}
	, {-2*sind[pma]+1 + cosd[pma]*2, pma>90-45/2}
	}]];


(* ::Subsubsection::Closed:: *)
(*slidingx3*)


(* ::Text:: *)
(*disturbed zone*)
(*\[Phi] is in degrees*)
(*ind is a column number from bigheader2*)
(*OUTPUT: number*)
(*CALLS: slidingmid, slidingside*)
(*CALLED BY: rotamp, (orientation.nb)*)


slidingx3[\[Phi]_, ind_]:=Module[{pma, vxa, df, b, m,p, d, e, region, aregion, vx, v, dr, w},
pma = Mod[(\[Phi]-ALPHA),90];
If[MemberQ[I0, ind] || ind>=31
	, slidingmid[pma]
	, slidingside[pma]
]
]


(* ::Subsection:: *)
(*x0, y0 nozzle origin miscalibration*)


(* ::Subsubsection::Closed:: *)
(*vxdx*)


(* ::Text:: *)
(*transverse flow from miscalibration of nozzle origins*)
(*\[Phi] is in degrees*)
(*lines is a list of lines (e.g. {1} or {1,2,3})*)
(*dx = any number, positive or negative*)
(*OUTPUT: number*)
(*CALLS: sind*)
(*CALLED BY: detectbasis, (orientation.nb)*)


vxdx[\[Phi]_, lines_, dx_]:=Module[{c, linenum},
linenum = If[Length[lines]==1
			, Floor[lines[[1]]/3]
			, 1/3
		];
sind[\[Phi]]*(- Sign[sind[\[Phi]]] - Sign[dx] + linenum*(Sign[sind[\[Phi]]] - Sign[dx]))/2
];
(*Plot[{vxdx[p, {1,2,3}, 1],vxdx[p, {1,2,3}, -1]}, {p, 0, 360}, PlotLegends\[Rule]{1,-1}]*)


(* ::Subsubsection::Closed:: *)
(*vxdy*)


(* ::Text:: *)
(*transverse flow from miscalibration of nozzle origins*)
(*\[Phi] is in degrees*)
(*lines is a list of lines (e.g. {1} or {1,2,3})*)
(*dy = any number, positive or negative*)
(*OUTPUT: number*)
(*CALLS: cosd*)
(*CALLED BY: detectbasis, (orientation.nb)*)


vxdy[\[Phi]_, lines_, dy_]:=Module[{c, linenum},
linenum = If[Length[lines]==1
			, Floor[lines[[1]]/3]
			, 1/3
		];
-cosd[\[Phi]]*(Sign[cosd[\[Phi]]]-Sign[dy] + linenum*(-Sign[cosd[\[Phi]]] - Sign[dy]))/2
];
(*Plot[{vxdy[p, {1,2,3}, 1],vxdy[p, {1,2,3}, -1]}, {p, 0, 360}, PlotLegends\[Rule]{1,-1}]*)


(* ::Subsection:: *)
(*solid rotation*)


(* ::Subsubsection::Closed:: *)
(*rigidmodel*)


(* ::Text:: *)
(*rotation of a rigid filament onto the substrate*)
(*\[Phi] is in degrees*)
(*OUTPUT: number*)
(*CALLS: cosd*)
(*CALLED BY: rotamp, (orientation.nb)*)


rigidmodel[\[Phi]_]:= Module[{rotatedfilament, frontedge, angle2, pma, vt},
	pma = Mod[\[Phi]-ALPHA, 90];
	If[pma==45
		, vt = 0;
		, vt = (If[pma>45, -1, 1]*cosd[45]-cosd[45+pma])*Sqrt[2];
	];
	vt
]


(* ::Subsection:: *)
(*fluid reshaping*)


(* ::Subsubsection::Closed:: *)
(*faststa*)


(* ::Text:: *)
(*fast side triangle area: excess flow from rotation of the nozzle*)
(*\[Phi] is in degrees*)
(*region is a column number from BIGHEADER2*)
(*OUTPUT: number*)
(*CALLS: sind*)
(*CALLED BY: rotamp, (orientation.nb)*)


faststa[\[Phi]_, region_]:=Module[{pma},
pma = Mod[\[Phi]-ALPHA,90]+ALPHA;
If[pma==ALPHA, 
0
,
If[MemberQ[I0, region] || region>=31
,	(-Sign[sind[pma-17-45]]*1/48 Abs[Csc[17 \[Degree]-\[Degree] pma] Sec[\[Degree] (-17+pma)] (-5 Cos[\[Degree] (-17+pma)]+4 Cos[2 \[Degree] (-17+pma)]-Cos[3 \[Degree] (13+pma)]+Sin[3 \[Degree] (13+pma)]-5 Sin[17 \[Degree]-\[Degree] pma])]*
							Abs[Csc[17 \[Degree]-\[Degree] pma] Sec[\[Degree] (-17+pma)] (1-Cos[\[Degree] (-17+pma)]+Sin[17 \[Degree]-\[Degree] pma])^2])/0.023216366605
,	(If[MemberQ[IL, region], -1, 1]*1/2 Abs[Csc[17 \[Degree]-\[Degree] pma] Sec[\[Degree] (-17+pma)] (1-Cos[\[Degree] (-17+pma)]+Sin[17 \[Degree]-\[Degree] pma])^2])/-(-1+Sqrt[2])^2
]
]]


(* ::Subsection:: *)
(*anisotropic focusing zone*)


(* ::Subsubsection::Closed:: *)
(*focusxy*)


(* ::Text:: *)
(*direction dependence from the establishment of a rectangular particle distribution in the nozzle*)
(*phi is in degree*)
(*scale = 1 for very narrow focusing in y, 0.5 for equal in both dimensions*)
(*OUTPUT: number*)


focusxy[phi_, scale_]:=Module[{pi, fx, fy},
pi = (phi-ALPHA)*Degree;
fx = scale;
fy = 1-scale;
fy*Abs[Cos[pi]]+fx*Abs[Sin[pi]] 
]


(* ::Section:: *)
(*fitting experimental profiles*)


(* ::Subsection:: *)
(*get experimental data*)


(* ::Subsubsection::Closed:: *)
(*exptp*)


(* ::Text:: *)
(*get list of points to plot from bigtable*)
(*yindex = column to plot from BIGHEADER2*)
(*OUTPUT: list of 3 lists of points*)
(*CALLS: cs, accumulatederror (master.wl)*)
(*CALLED BY: detectbasis*)


exptp[bigtable_, yindex_]:=Module[{b0table, tp, b1, bsides, flist, g1},
b1 = Select[bigtable, NumberQ[#[[yindex]]] && #[[yindex]]!=0&];
bsides = SortBy[GatherBy[b1, #[[5]]&], #[[1,5]]&];
flist = {Cos, Sin, cs};
Table[
	Table[
		g1 = GatherBy[{flist[[j]][#[[7]]*Degree], #[[yindex]], #[[yindex+1]]}&/@(bsides[[i]]), #[[1]]&];
		Sort[{{N[#[[1,1]]], Mean[#[[;;,2]]]}, ErrorBar[accumulatederror[#[[;;, 3]]]]}&/@g1]
	,{i, 5}]
, {j, 3}]
]


(* ::Subsection:: *)
(*fit slopes (origin miscalibration)*)


(* ::Subsubsection::Closed:: *)
(*slopealldetect*)


(* ::Text:: *)
(*measure the slope of an experimental profile*)
(*et1 = experimental profile*)
(*var = variable number*)
(*OUTPUT: slope as a number*)
(*CALLED BY: detectbasis*)


slopealldetect[et1_, var_]:=Module[{et1a, slopeoriginy, et1m},
	et1a = Flatten[et1[[{2,4,5}]],1];
	slopeoriginy = D[LinearModelFit[et1a, x, x][x],x];
	If[var<=29
		, et1m = Mean[et1a[[;;,2]]];
		et1a = If[(If[slopeoriginy>0, 1, -1]*#[[1]])>0
				, {#[[1]], et1m + 3*(#[[2]]-et1m)}
				, #
				]&/@et1a;
		slopeoriginy = D[LinearModelFit[et1a,x, x][x],x]
	]; (*make 1/3 scaling adjustment for velocities*)
	slopeoriginy
]


(* ::Subsection:: *)
(*construct fitted theory data*)


(* ::Subsubsection::Closed:: *)
(*ttheo*)


(* ::Text:: *)
(*ttheo outputs a table of theoretical values*)
(*xfunc is the x function (e.g. Cos), inputs in radians*)
(*yfunc is the y function (e.g. rigidmodel), inputs in degrees*)
(*OUTPUT: table of numbers*)
(*CALLS: prismangles (master.wl)*)
(*CALLED BY: theogrid, theoscale*)


ttheo[xfunc_, yfunc_]:=Table[
				Sort[Mean/@GatherBy[
					Table[
						{SetAccuracy[xfunc[\[Phi]*Degree],6], SetAccuracy[yfunc[\[Phi]],6]}
					, {\[Phi],  alist}]
				, #[[1]]&]]
			, {alist, prismangles}]


(* ::Subsubsection:: *)
(*ttheoraw*)


(* ::Text:: *)
(*ttheoraw outputs a table of theoretical values, not averaged over x*)
(*xfunc is the x function (e.g. Cos), inputs in radians*)
(*yfunc is the y function (e.g. rigidmodel), inputs in degrees*)
(*OUTPUT: table of numbers*)
(*CALLS: prismangles (master.wl)*)
(*CALLED BY: theogridraw*)


ttheoraw[xfunc_, yfunc_]:=Table[
				Table[
						{SetAccuracy[xfunc[\[Phi]*Degree],6], SetAccuracy[yfunc[\[Phi]],6]}
					, {\[Phi],  alist}]
			, {alist, prismangles}]


(* ::Subsubsection:: *)
(*ttheoall*)


(* ::Text:: *)
(*ttheoall outputs a table of theoretical values for phi = 0 to 360*)
(*xfunc is the x function (e.g. Cos), inputs in radians*)
(*yfunc is the y function (e.g. rigidmodel), inputs in degrees*)
(*OUTPUT: table of numbers*)
(*CALLS: prismangles (master.wl)*)
(*CALLED BY: theogrid, theoscale*)


ttheoall[xfunc_, yfunc_]:=
				Table[
						{SetAccuracy[xfunc[\[Phi]*Degree],6], SetAccuracy[yfunc[\[Phi]],6]}
					, {\[Phi],  0, 360}]


(* ::Subsubsection:: *)
(*theoscale, theoscaletable*)


(* ::Text:: *)
(*fitted theoretical profile*)
(*xfunc = function to plot in x (e.g. Sin, Cos)*)
(*yfuncs = list of basis functions (e.g. slopeoriginy)*)
(*scales = list of numbers to scale basis functions by*)
(*OUTPUT: table of numbers*)
(*CALLS: ttheo*)
(*CALLED BY: detectbasis, rotamp*)


theoscale[xfunc_, yfuncs_, scales_]:=Module[{t, t1},
t = ttheo[xfunc, #]&/@yfuncs;
t1 = t[[1]];
t1[[;;,;;,2]] = Sum[scales[[i]]*t[[i,;;,;;,2]], {i, Length[scales]}];
t1]


(* ::Text:: *)
(*fitted set of theoretical profiles*)
(*yfuncs = list of basis functions (e.g. slopeoriginy)*)
(*scales = list of numbers to scale basis functions by*)
(*OUTPUT: table of tables of numbers*)
(*CALLS: theoscale, cs*)
(*CALLED BY: detectbasis, rotamp*)


theoscaletable[yfuncs_, scales_]:=Table[theoscale[f, yfuncs, scales], {f, {Cos, Sin, cs}}]


(* ::Subsubsection::Closed:: *)
(*subtracttheo, addtheo*)


(* ::Text:: *)
(*subtract the fitted theoretical profile theo from the experimental profile et*)
(*OUTPUT: table of numbers*)
(*CALLED BY: detectbasis, rotamperror*)


subtracttheo[et_, theo_]:=Module[{et2},
If[Length[et[[1,1,1,1]]]==0
	, et2 = et
	, et2 = et[[;;,;;,;;,1]];
];
et2[[;;,;;,;;,2]] = et2[[;;,;;,;;,2]] - theo[[;;,;;,;;,2]];
et2
]


(* ::Text:: *)
(*add the fitted theoretical profile theo to the experimental profile et*)
(*OUTPUT: table of numbers*)
(*CALLED BY: rotamperror*)


addtheo[et_, theo_]:=Module[{et2},
If[Length[et[[1,1,1,1]]]==0
, et2 = et
, et2 = et[[;;,;;,;;,1]];
];
et2[[;;,;;,;;,2]] = et2[[;;,;;,;;,2]] + theo[[;;,;;,;;,2]];
et2
]


(* ::Subsection:: *)
(*fit \[Phi]-\[Alpha] dependent effects (solid rotation, fluid reshaping, disturbed zone)*)


(* ::Subsubsection:: *)
(*rotamperror*)


(* ::Text:: *)
(*find the final error between the fitted profile and the experimental profile*)
(*theo2 = amplitude-corrected fitted profile*)
(*theo1 = slope-corrected fitted profile*)
(*et2 = slope-corrected experimental profile*)
(*et1 = original experimental profile*)
(*vi = center of the theoretical profile in velocity*)
(*OUTPUT: {direction-independent velocity, error, fitted theoretical profile, corrected experimental profile}*)
(*CALLS: subtracttheo, addtheo*)
(*CALLED BY: rotamp*)


rotamperror[theo2_, theo1_, et2_, et1_, vi_]:=Module[{et3, ey, effect, theo3, ty, error},
	et3 = subtracttheo[et2, theo2];
	ey = Flatten[et3[[;;,;;,;;,2]]];
	effect = Mean[ey];
	theo3 = addtheo[theo1, theo2];
	theo3[[;;,;;,;;,2]] = theo3[[;;,;;,;;,2]] + effect;
	ty = Flatten[theo3[[;;,;;,;;,2]]];
	ey = Flatten[et1[[;;,;;,;;,2]]];
	error = Sqrt[Total[(ey - ty)^2]/(Length[ey]-1)]/Sqrt[Length[ey]];
	et3 = (# - {0, vi})&/@#&/@#&/@et3;
	{effect, error, theo3, et3}
]


(* ::Subsubsection:: *)
(*rotamp*)


(* ::Text:: *)
(*find the amplitude of the experimental profile in order to fit models which depend on the difference in angle between the nozzle orientation and the printing direction. Essentially, fit to solid rotation, fluid reshaping, and disturbed zone models and determine which one fits best*)
(*et2 = slope corrected experimental profile*)
(*var = column number in bigtable2*)
(*theo1 = slope fitted theoretical profile*)
(*superprint = bool true to print diagnostics*)
(*mm = minmax for plotting*)
(*et1 = original experimental profile*)
(*OUTPUT: row of fitting parameters*)
(*CALLS: theoscaletable, slidingx3, rotamperror, yaxislabel, orientrowdefault, rigidmodel, faststa, isv (master.wl), I0 (master.wl)*)
(*CALLED BY: detectbasis*)


rotamp[et2_, var_, theo1_, superprint_, mm_, et1_]:=Module[{coshex, cosoct, sinhex, sinoct, dzhexamp, dzoctamp, dzamp, theodz, dzeffect, dzerror, srhexamp, sroctamp, sramp
									, theosr, sreffect, srerror, frhexamp, froctamp, framp, theofr, freffect, frerror, best, theo3, dzet3, sret3, fret3, et3, effect, error, amp
									, dzhexcent, dzoctcent, srhexcent, sroctcent, frhexcent, froctcent, dzvi, srvi, frvi, vi},
(*experimental*)
coshex = Mean[et2[[1, 4, {1,3},2]]] - et2[[1,4,2,2]];
cosoct = Mean[et2[[1, 5, {2,4},2]]] - Mean[et2[[1,5,{1,3,5},2]]];
sinhex = et2[[2,4,2,2]] - Mean[et2[[2, 4, {1,3},2]]];
sinoct = Mean[et2[[2, 5, {2,4},2]]] - Mean[et2[[2,5,{1,3,5},2]]];

(*disturbed zone*)
dzhexamp = If[MemberQ[I0, var] || !isv[var], 1.54936, 0.5773];
dzoctamp = If[MemberQ[I0, var] || !isv[var], 1.8270, 0.3279]; (*amplitudes*)
dzhexcent = If[MemberQ[I0, var] || !isv[var], -0.22532, -0.0392];
dzoctcent = If[MemberQ[I0, var] || !isv[var], -0.0865, -0.1639]; (*centers*)
dzamp = Mean[{coshex/dzhexamp, cosoct/dzoctamp, sinhex/dzhexamp, sinoct/dzoctamp}]; (*scaling coefficient based on amplitudes*)
dzvi = dzamp*(dzhexcent+dzoctcent)/2; (*shifted center based on scaling coefficient*)
theodz = theoscaletable[{slidingx3[#, var]&, -1&}, {dzamp, dzvi}]; (*subtract center from scaled basis function*)
{dzeffect, dzerror, theodz, dzet3} = rotamperror[theodz, theo1, et2, et1, dzvi];
If[superprint,
		Print[Row@orientrowdefault[dzet3, mm, yaxislabel[var], False, False, False], "dz expcorrect"
		, Row@orientrowdefault[theodz, mm, yaxislabel[var], False, False, False], "DZ theosum"
			];
];

(*solid rotation*)
srhexamp = 0.94;
sroctamp = 0.92;
srhexcent = 0.13228;
sroctcent = 0.12523;
sramp = Mean[{coshex/srhexamp, cosoct/sroctamp, sinhex/srhexamp, sinoct/sroctamp}];
srvi = sramp*(srhexcent+sroctcent)/2;
theosr = theoscaletable[{rigidmodel[#]&,  -1&}, {sramp, srvi}];
{sreffect, srerror, theosr, sret3} = rotamperror[theosr, theo1, et2, et1, srvi];
If[superprint,
		Print[Row@orientrowdefault[sret3, mm, yaxislabel[var], False, False], "sr expcorrect"
			, Row@orientrowdefault[theosr, mm, yaxislabel[var], False, False], "sr theo sum"
			];
];

(*fluid reshaping*)
frhexamp = If[MemberQ[I0, var] || !isv[var], 1.52, If[MemberQ[IL, var], 0.12, -0.12]];
froctamp = If[MemberQ[I0, var] || !isv[var], 1.82, If[MemberQ[IL, var], 0.23, -0.23]];
frhexcent = If[MemberQ[I0, var] || !isv[var], -0.23746, If[MemberQ[IL, var], 0.704, -0.704]];
froctcent = If[MemberQ[I0, var] || !isv[var], -0.08657, If[MemberQ[IL, var], 0.759, -0.759]];
framp = Mean[{coshex/frhexamp, cosoct/froctamp, sinhex/frhexamp, sinoct/froctamp}];
frvi = framp*(frhexcent+froctcent)/2;
theofr = theoscaletable[{faststa[#, var]&,  -1&}, {framp, frvi}];
{freffect, frerror, theofr, fret3} = rotamperror[theofr, theo1, et2, et1, frvi];
If[superprint,
		Print[Row@orientrowdefault[fret3, mm, yaxislabel[var], False, False], "fr expcorrect"
			, Row@orientrowdefault[theofr, mm, yaxislabel[var], False, False], "fr theosum"
		];
];

(*determine which has best fit*)
If[dzerror<srerror && dzerror<frerror
	, best = 1; theo3 = theodz; et3 = dzet3; effect = dzeffect; error = dzerror; amp = dzamp; vi = dzvi;
	, If[srerror<frerror
		, best = 2; theo3 = theosr; et3 = sret3; effect = sreffect; error = srerror; amp = sramp; vi = srvi;
		, best = 3; theo3 = theofr; et3 = fret3; effect = freffect; error = frerror; amp = framp; vi = frvi;
		]
];

{dzamp, dzeffect, dzerror, dzvi, sramp, sreffect, srerror
		, srvi, framp, freffect, frerror, frvi, best, theo3, et3, amp, effect, error, vi}
]


(* ::Subsection:: *)
(*full fitting process*)


(* ::Subsubsection:: *)
(*detectbasis*)


(* ::Text:: *)
(*bigtable = big table from bigtable2.csv*)
(*var = dependent variable column to probe (see BIGHEADER2)*)
(*type = 2 or 3 for type of support*)
(*disp = bool true to print diagnostics*)
(*subvar = subset variable, column number from BIGHEADER2, set to 0 to use everything*)
(*sub = subset variable value*)
(*OUTPUT: {graphics table, row of data}*)
(*CALLS: cs, cxcomponent, dbdisp, exptp, linelist, orientrowdefault, rotamp, slopealldetect, subtracttheo, theoscale, theoscaletable, vregion, vxdx, vxdy, yaxislabel*)
(*CALLED BY:  (orientation.nb)*)


detectbasis[bigtable_, var_, type_, disp_, subvar_, sub_]:=Module[{b1, slopeoriginy, slopeoriginx, slopemotor, theo1, theo2, et1, et1a, et2, et3
																			, dbd, error, ey, ty, et1m, dzvi, srvi, frvi, vi
																			, dzamp, dzeffect, dzerror, sramp, sreffect, srerror, framp, freffect, frerror
																			, best, theo3, effect, ramp, superprint, mm, theoy, et1y, theox, et1x},
Catch[
If[(!MemberQ[Keys[vregion], var] && isv[var]) || (!MemberQ[{31, 37, 43}, var] && !isv[var])
	, Throw["Var # must be vT or position column from BIGHEADER2"]
];
b1 = Select[bigtable, #[[3]]==type&]; (*select only bath or layer-by-layer*)
If[!isv[var]
	, b1 = Select[b1, #[[9]]==3&] (*only use 3rd pass for distributions*)
];
If[subvar>0,
	b1 = Select[b1,  #[[subvar]]==sub&] (*select a subset*)
];
et1 = exptp[b1, var]; (*group data by polygon shape and cos sin cossin*)
et1 = et1[[;;,;;,;;,1]]; (*throw out error bars*)
superprint = disp && False;
If[superprint
	, mm = MinMax[Flatten[(#[[;;,;;,;;,2]])&@et1]]+{-0.05, 0.05}
	, mm = 0
];
If[superprint,
	Print[Row@orientrowdefault[et1, mm, yaxislabel[var], False, False, False], "Init"];
];

slopeoriginy = slopealldetect[et1[[1]], var];
If[superprint,
	theoy = theoscaletable[{vxdy[#, linelist[var], If[slopeoriginy>0, 1, -1]]&}, {Abs[slopeoriginy]}];
	et1y = subtracttheo[et1, theoy];
	Print[Row@orientrowdefault[et1y, mm, yaxislabel[var], False, False, False], "y0"];
];
slopeoriginx = slopealldetect[et1[[2]], var];
If[superprint,
	theox = theoscaletable[{vxdx[#, linelist[var], If[slopeoriginx>0, -1, 1]]&}, {Abs[slopeoriginx]}];
	et1x = subtracttheo[et1y, theox];
	Print[Row@orientrowdefault[et1x, mm, yaxislabel[var], False, False, False], "x0"];
];

slopemotor = D[LinearModelFit[Flatten[et1[[3,{5}]],1],x, x][x],x]/2;
theo1 = Table[theoscale[f, 
						{vxdy[#, linelist[var], If[slopeoriginy>0, 1, -1]]&
							,  vxdx[#, linelist[var], If[slopeoriginx>0, -1, 1]]&
							, cxcomponent}
						, {Abs[slopeoriginy], Abs[slopeoriginx], slopemotor}
					], {f, {Cos, Sin, cs}}]; 
et2 = subtracttheo[et1, theo1];
If[superprint,
		Print[Row@orientrowdefault[et2, mm, yaxislabel[var], False, False, False], "motor"];
];

{dzamp, dzeffect, dzerror, dzvi, sramp, sreffect, srerror, srvi
		, framp, freffect, frerror, frvi, best, theo3, et3, ramp, effect, error, vi} = 
				rotamp[et2, var, theo1, superprint, mm, et1];

If[superprint,
		Print[Row@orientrowdefault[et3, mm, yaxislabel[var], True, True, False], "rot"];
];

If[disp, dbd = dbdisp[theo3, et1, et3, type, var], dbd = ""];
{dbd, {var, type, subvar, sub, slopeoriginx, slopeoriginy, slopemotor
	, dzamp, dzeffect, dzerror, dzvi, sramp, sreffect, srerror, srvi
	, framp, freffect, frerror, frvi, best, ramp, effect, error, vi, effect-vi}}
]]
