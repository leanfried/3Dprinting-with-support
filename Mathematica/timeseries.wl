(* ::Package:: *)

(* ::Title:: *)
(*Time series*)


(* ::Text:: *)
(*Functions used for "Corner accuracy in direct ink writing with support material"*)
(*Last updated November 2019 by Leanne Friedrich*)


<<"Mathematica\\master.wl";


(* ::Section:: *)
(*utilities*)


(* ::Subsubsection:: *)
(*getdarkcolors*)


(* ::Text:: *)
(*getdarkcolors gets a list of n colors and darkens the middle one if n is odd*)


getdarkcolors[n_]:=Module[{colors},
	colors = getColors[n];
	If[OddQ[n], colors[[Ceiling[n/2]]] = Darker[colors[[Ceiling[n/2]]]]];
	colors
]


(* ::Subsubsection:: *)
(*material parameters*)


(* ::Text:: *)
(*material parameters for these experiments as a function of TEGDMA composition*)
(*CALLED BY: energyink, dinit, dshear, drelax*)


\[Theta]adlist = <|20->58.59, 25->59.05, 30->49.83, 35->50.52, 0->45.14|>; (*advancing contact angle*)
\[Theta]eqlist = <|20->42.50, 25->37.23, 30->34.50, 35->32.68, 0->35.64|>; (*equilibrium contact angle*)
\[Gamma]list = <|20-> 41.239, 25-> 40.233, 30-> 37.893, 35-> 34.648|>; (*surface tension*)
viscosity = <|20->6.420, 25->5.558, 30->5.503, 35->1.622|>;


(* ::Section:: *)
(*theories*)


(* ::Subsection:: *)
(*swell model*)


(* ::Subsubsection::Closed:: *)
(*vdouble*)


(* ::Text:: *)
(*vdouble is the double deposition volume*)
(*OUTPUT: number*)
(*CALLED BY: dswell*)


vdouble[\[Theta]_]:=Module[{U, v, D, h, W},
U = 1;
v = 1;
D = 0.3; (*mm*)
h = 0.3; (*mm*)
W = 0.3;
0.3*W^2/4(Cot[\[Theta]/2]+\[Theta]/2 - Pi/2) (*mm*)
]


(* ::Subsubsection::Closed:: *)
(*dswell*)


(* ::Text:: *)
(*dswell is the change in line width and line position due to swelling*)
(*\[Theta] is the corner angle in degree*)
(*OUTPUT: {line width, line position}*)
(*CALLS: vdouble*)
(*CALLED BY: SWELLTABLEV*)


dswell[\[Theta]_]:=Module[{v1, v2, wo, h, \[Alpha]list, denom, dp, A, Add, L, we, pt, atot},
	L = 0;
	v1 = 0;
	v2 = vdouble[\[Theta]*Degree];
	wo = 0.3;
	pt = Pi-\[Theta]*Degree;
	atot = (v1+v2)/wo;
	we = (-(pt*wo/2)+Sqrt[(pt*wo/2)^2 + 4(pt/2+1)atot])/(pt+2);
	{we, -we/2}
]


(* ::Subsubsection::Closed:: *)
(*SWELLTABLEV*)


(* ::Text:: *)
(*construct table of predicted changes due to swelling*)


SWELLTABLEV = Flatten[Table[
						Join[{v, angle}, dswell[angle]]
				, {angle, {60, 90, 108, 120, 135}}, {v, 3, 12, 3}],1];
Grid[Prepend[SWELLTABLEV, {"v", "angle", "change width", "change pos"}]]


(* ::Subsection:: *)
(*Huang model*)


(* ::Subsubsection::Closed:: *)
(*energy*)


(* ::Text:: *)
(*energy gives the energy of the system as a function of the *)
(*corner bulge radius ab, *)
(*the corner angle \[Theta], *)
(*and the edge half/length L*)
(*\[Theta]ad advancing contact angle*)
(*\[Theta]eq equilibrium contact angle*)
(*OUTPUT: energy*)
(*CALLED BY: energyink*)


energy[ab_, \[Theta]_, L_, \[Theta]ad_, \[Theta]eq_]:=Module[{w1, Vtot, Ab, Abb, Vb, V1, \[Theta]1, Alb, A1, Etotgam},
w1 = 0.3; (*mm*)
Vtot = 2*w1^2*L; (*mm^3*)
Ab = \[Theta]/(2*Pi)*Pi*ab^2*(1+Tan[\[Theta]ad/2]^2); (*mm^2*)
Abb = \[Theta]/(2*Pi)*Pi*ab^2; (*mm^2*)
Vb = (\[Theta]/(2*Pi))^(3/2)(Pi*Tan[\[Theta]ad/2](3+Tan[\[Theta]ad/2]^2)/6)ab^3; (*mm^3*)
V1 = Vtot-Vb; (*mm^3*)
\[Theta]1 = \[Theta]1/.FindRoot[(\[Theta]1-Sin[\[Theta]1]*Cos[\[Theta]1])/Sin[\[Theta]1]^2 == V1/(2L (w1/2)^2), {\[Theta]1, Pi/2}][[1]];
If[\[Theta]1<0,
	Etotgam = 100
	,
	Alb = 2*w1*(L-ab); (*mm^2*)
	A1 = V1/(w1/2)*2\[Theta]1*Sin[\[Theta]1]/(\[Theta]1 - Sin[\[Theta]1]Cos[\[Theta]1]); (*mm^2*)
	Etotgam = Ab + A1 - Cos[\[Theta]eq](Abb+Alb) (*mm^2*)
]
];


(* ::Subsubsection:: *)
(*energyink*)


(* ::Text:: *)
(*corner bulge radius ab, *)
(*the corner angle \[Theta], *)
(*ink = % TEGDMA in ink*)
(*OUTPUT: number*)
(*CALLS: energy, \[Theta]adlist, \[Theta]eqlist*)
(*CALLED BY: (timeseries.nb)*)


energyink[ab_, \[Theta]_, ink_]:=energy[ab, \[Theta]*Degree, 5, \[Theta]adlist[ink]*Degree, \[Theta]eqlist[ink]*Degree]


(* ::Subsection:: *)
(*Corner Smoothing model*)


(* ::Text:: *)
(*displaced length d mm*)
(*corner angle \[Theta] radians*)
(*original line width w mm*)
(*ink % TEGDMA*)


(* ::Text:: *)
(*final width*)
(*OUTPUT: number*)
(*CALLED BY: dp*)
(*CALLED BY: (timeseries.nb)*)


wf[d_, \[Theta]_, w_]:=(-(Pi-\[Theta])*d*Tan[\[Theta]/2]+Sqrt[(Pi-\[Theta])^2*d^2*Tan[\[Theta]/2]^2 - 2(Pi-\[Theta])(-2*w*d-w^2Cot[\[Theta]/2])])/(Pi-\[Theta])


(* ::Text:: *)
(*laplace pressure differential*)
(*OUTPUT: number*)
(*CALLS: wf*)
(*CALLED BY: dinit, drelax, dshear*)


dp[d_, \[Theta]_, w_, ink_]:=\[Gamma]list[ink](1/(d*Tan[\[Theta]/2])+1/(d*Tan[\[Theta]/2]+wf[d, \[Theta],w]) )


(* ::Text:: *)
(*displaced length*)
(*OUTPUT: number*)
(*CALLS: viscosity, dp*)
(*CALLED BY: (timeseries.nb)*)


dinit[d_, \[Theta]_, ink_, vs_, \[Lambda]_]:=(dp[d, \[Theta], 0.3, ink]*\[Lambda]*2*0.3/(viscosity[ink]*vs))*(Cos[\[Theta]/2]/(1-Sin[\[Theta]/2]));


drelax[d_, \[Theta]_, ink_, vs_, \[Lambda]_]:=dp[d, \[Theta], 0.3, ink]*\[Lambda]*(10*Switch[\[Theta]/Degree, 60, 3, 90, 4, 108, 5, 120, 6, 135, 8]
								-2*0.3)/(viscosity[ink]*vs)*(Cos[\[Theta]/2]/(1-Sin[\[Theta]/2]));


dshear[d_, \[Theta]_, ink_, vs_, \[Lambda]_]:=dp[d, \[Theta], 0.3, ink]*\[Lambda]*(10*Switch[\[Theta]/Degree, 60, 3, 90, 4, 108, 5, 120, 6, 135, 8]
								+2*0.3)/(viscosity[ink]*vs)*(Cos[\[Theta]/2]/(1-Sin[\[Theta]/2]));


(* ::Text:: *)
(*change in width*)
(*OUTPUT: number*)
(*CALLS: wf*)
(*CALLED BY: (timeseries.nb)*)


changewidth[d_, \[Theta]_]:=wf[d, \[Theta], 0.3] - 0.3;


(* ::Text:: *)
(*change in position*)
(*OUTPUT: number*)
(*CALLS: wf*)
(*CALLED BY: (timeseries.nb)*)


changepos[d_, \[Theta]_]:=d*Tan[\[Theta]/2] + 0.3/2 - (d*Tan[\[Theta]/2]+wf[d, \[Theta], 0.3]/2)*Sin[\[Theta]/2]


(* ::Text:: *)
(*LAPTABLE is constructed in (timeseries.nb)*)


LAPTABLE = Import["Tables\\laptable.csv"];


(* ::Section:: *)
(*plots as a function of distance from the corner*)


(* ::Subsection:: *)
(*averaging*)


(* ::Subsubsection:: *)
(*mbounds*)


(* ::Text:: *)
(*mbounds finds the mean and standard error of the data set and outputs three points, {mean-ster, mean, mean+ster}*)
(*l = list of data*)
(*OUTPUT: 3 points*)
(*CALLS: ster*)
(*CALLED BY: bounds*)


mbounds[l_]:=Module[{m, er}, m = Mean[l]; er = ster[l]; {m-er, m, m+er}]


(* ::Subsubsection:: *)
(*bounds*)


(* ::Text:: *)
(*bounds sorts a list and finds three lines, {mean-ster, mean, mean+ster}*)
(*tp = list of data to plot*)
(*OUTPUT: list of 3 lines*)
(*CALLS: mbounds*)
(*CALLED BY: distplotinitrow2, dpir3tps*)


bounds[tp_]:=Module[{s, sr, r, ds, t1}, 
s =Sort[tp];
t1 = Partition[s, Round[Length[s]/25]];
Transpose[mbounds/@t1]
]


(* ::Subsection:: *)
(*plot grids*)


(* ::Subsubsection:: *)
(*fticks*)


(* ::Text:: *)
(*define ticks*)
(*pos = bool true to plot position*)
(*OUTPUT: list of ticks*)
(*CALLED BY: tplot*)


fticks[pos_]:=Module[{ftleft, ftright, ftbot, fttop, ts},
If[pos, ts = 0.05, ts = 0.005];
ftleft =  Table[{i, If[i==0, "0.00", SetAccuracy[i, If[pos,3,4]]]},{i, -1, 1, ts}];
ftright =Table[{i, ""},{i, -1, 1, ts}];
ftbot =  Table[{-i, i}, {i, 0, 12, 2}];
fttop = Table[{-i, ""}, {i, 0, 12, 2}];
{ftleft, ftright, ftbot, fttop}
]


(* ::Subsubsection:: *)
(*tplot*)


(* ::Text:: *)
(*plot as a function of distance from the corner*)
(*tp = list of data to plot*)
(*colorsi = initial color list*)
(*bottom = true to include bottom labels*)
(*left = true to include left labels*)
(*yvar = y variable number to plot (column of HEADER2)*)
(*top = bool true to include top labels*)
(*label = plot label*)
(*labellist = list of legend labels*)
(*legend = bool true to plot legend*)
(*mm = {min, max} in y for plot range*)
(*edge = edge length in mm*)
(*legendlabel = legend label string*)
(*OUTPUT: one plot*)
(*CALLS: fticks, getdarkcolors*)
(*CALLED BY: distplotinitrow2, distplotinitrow3*)


tplot[tp_, colorsi_, bottom_, left_, yvar_, top_, label_, labellist_, legend_, mm_, edge_, legendlabel_]:=Module[{lp, is, ftleft, ftright, ftbot, fttop, ticks, fs, ylabel, colors},
lp = {{50, 2}, {40, 2}};
is = 2.25*72;
fs = 10; (*font size*)
If[!left,is = is-45; lp[[1,1]] =5; ];
If[!bottom, lp[[2,1]] = 2;];
{ftleft, ftright, ftbot, fttop} = fticks[yvar==45 || yvar==54 || yvar==48 || yvar==-1];
ticks = {{If[left, ftleft, ftright], ftright}, {If[bottom,ftbot,fttop], fttop}};
ylabel = Switch[yvar, 45, "Init position (mm)",54, "Relaxed position (mm)", 48, "Sheared position (mm)", 46, "Init width", 55, "Relaxed width", 49, "Sheared width", -1, "Position (mm)", -2, "Width (mm)",  _, HEADER2[[yvar]]];
If[Length[colorsi]==0
	, colors = getdarkcolors[Length[labellist]];
	, colors = colorsi
];
ListPlot[tp
	, AspectRatio->1
	, Axes->None
	, Filling->Table[1+3*(i-1)->{{3+3*(i-1)},Directive[Opacity[0.25], colors[[i]]]}, {i, Length[colors]}]
	, FrameLabel->{If[bottom, "Distance (mm)", None],If[left,  ylabel, None]}
	, FrameTicks-> ticks
	, GridLines->{None, If[yvar==45 || yvar==48 || yvar==54 || yvar==-1, {-0.15, 0, 0.15}, {0}]}
	, ImageSize->is
	, ImagePadding->lp
	, LabelStyle->Directive[fs, Black]
	, PlotLabel->If[top, Style[label, fs], None] 
	, PlotLegends->If[legend, Labeled[LineLegend[colors, labellist, LabelStyle->fs], Style[legendlabel, fs, FontFamily->"Arial"], Top], None]
	, PlotMarkers->None
	, PlotRange->{{-edge, 0},mm}
	, PlotRangePadding->{None, Scaled[0.03]}
	, PlotStyle->Flatten[Table[{None, colors[[i]], None}, {i, Length[colors]}],1]]
]


(* ::Subsubsection:: *)
(*distplotinitrow2*)


(* ::Text:: *)
(*distplotinitrow2 outputs two plots in a row, one for layer-by-layer and one for bath, splitting on the line number*)
(*cflist = 2 big lists {cflayer, cfbath}*)
(*top = bool true to label top*)
(*bottom = bool true to label bottom axis*)
(*yvar = column in header2 to plot *)
(*edge = edge length (6, 8, 10, or 12)*)
(*OUTPUT: row of plots*)
(*CALLS: bounds, getColors, tplot*)
(*CALLED BY: (timeseries.nb)*)


distplotinitrow2[cflist_,top_, bottom_, yvar_, edge_]:=Module[{xvar, fs, cf2, tp1, colors, tp, lp, is, labellist, g1, ylabel, tps, mm, left, legend, ftleft, ftright, ftbot, fttop, linei},
xvar = 14; (*distance from corner*)
linei =  If[yvar==45 || yvar==47, 1, 2];
tps = Table[
		Flatten[
			Table[
				cf2 = Select[cf, #[[4]]==edge&&#[[9]]==line&&#[[yvar]]!=0 &];
				tp1 = ({-1, 1}*#+{If[yvar<54,0.6,-0.6], If[yvar==54 || yvar==48, -0.3, 0]})&/@cf2[[;;, {xvar, yvar}]]; (*reverse x axis and shift to adjust for ahead or behind*)
				(*bounds outputs a list of 3 lists, for the mean-error, mean, and mean+error*)
				tp = Sort/@bounds[tp1]
			, {line,linei,3}]
		,1]
	,{cf, cflist}];
colors = getColors[3];
colors[[2]] = Darker[colors[[2]]];
colors = colors[[linei;;3]];
labellist = Range[linei, 3];
mm = MinMax[Flatten[tps,2][[;;,2]]];
Row[
	Table[
		If[i==1, left=True; legend=False;, left = False; legend = True];
		tplot[tps[[i]], colors, bottom, left, yvar, top, Switch[i,1,  "Layer-by-layer",2, "Bath"], labellist, legend, mm, edge, "Pass"]
	,{i,2}]
]
]


(* ::Subsubsection:: *)
(*dpir3tps*)


(* ::Text:: *)
(*dpir3tps gets a list of data to plot*)
(*pos = bool true to plot position*)
(*cf2df = list of all points in layer-by-layer support with ending corners removed*)
(*cf3df = list of all points in bath support with ending corners removed*)
(*cf2db = list of all points in layer-by-layer support with starting corners removed*)
(*cf3db = list of all points in bath support with starting corners removed*)
(*edge = edge length in mm*)
(*OUTPUT: list of points to plot*)
(*CALLS: bounds*)
(*CALLED BY: distplotinitrow3, output is used in wholepoly, (timeseries.nb)*)


dpir3tps[pos_, cf2df_, cf3df_, cf2db_, cf3db_, edge_]:=Module[{line, yvar, cf, cf2, tp1, xvar, tp, tps},
xvar = 14; (*distance from corner*)
tps = Table[
		Flatten[
			Table[
				If[i==1, line = 1, line = 2];
				yvar = Switch[i, 1, 45, 2, 54, 3, 48] + If[pos, 0, 1];
				If[j==1
					, If[i==2
						, cf = cf2df
						, cf = cf2db
					]
					, If[i==2
						, cf=cf3df
						, cf = cf3db
					]
				];
				cf2 = Select[cf, #[[4]]==edge && #[[9]]==line && #[[yvar]]!=0 &];
				tp1 = ({-1, 1}*#+{If[yvar<54,0.6,-0.6], If[yvar==54 || yvar==48, -0.3, 0]})&/@cf2[[;;, {xvar, yvar}]]; (*reverse x axis and shift to adjust for ahead or behind*)
				(*bounds outputs a list of 3 lists, for the mean-error, mean, and mean+error*)
				tp = Sort/@bounds[tp1]
			, {i, 3}]
		,1]
	,{j, 2}];
tps
]


(* ::Subsubsection:: *)
(*distplotinitrow3*)


(* ::Text:: *)
(*distplotinitrow3 outputs two plots in a row, one for layer-by-layer and one for bath, for line 1 over the course of the print, split into initial, relaxed, and sheared*)
(*cf2df = list of all points in layer-by-layer support with ending corners removed*)
(*cf3df = list of all points in bath support with ending corners removed*)
(*cf2db = list of all points in layer-by-layer support with starting corners removed*)
(*cf3db = list of all points in bath support with starting corners removed*)
(*top = bool true to label top*)
(*bottom = bool true to label bottom axis*)
(*edge = edge length (6, 8, 10, or 12)*)
(*pos = True to plot position, false to plot width*)
(*OUTPUT: row of 2 plots for layer-by-layer and bath*)
(*CALLS: tplot, dpir3tps*)
(*CALLED BY: (timeseries.nb)*)


distplotinitrow3[{cf2db_, cf3db_, cf2df_, cf3df_}, top_, bottom_, edge_, pos_]:=Module[{fs, cf2, tp1, colors, tp, lp, is, labellist, g1, ylabel, tps, mm, left, legend, ftleft, ftright, ftbot, fttop,line, yvar, cf},
tps = dpir3tps[pos, cf2df, cf3df, cf2db, cf3db, edge];
labellist ={"Init", "Relaxed", "Sheared"};
mm = MinMax[Flatten[tps,2][[;;,2]]];
Row[
	Table[
		If[i==1, left=True; legend=False;, left = False; legend = True];
		tplot[tps[[i]], {}, bottom, left, If[pos, -1, -2], top, Switch[i,1,  "Layer-by-layer",2, "Bath"]
					, labellist, legend, mm, edge, ""]
	,{i,2}]
]
]


(* ::Subsection:: *)
(*hexagon cartoons*)


(* ::Subsubsection:: *)
(*hexpoints*)


(* ::Text:: *)
(*hexpoints produces a list of points on a hexagon, doubling the first point*)
(*OUTPUT: list of points*)
(*CALLED BY: onedge, wholepoly*)


hexpoints:=Module[{cp},
cp = Reverse[CirclePoints[6]*10];
cp = Append[cp, cp[[1]]];
cp = RotationMatrix[Pi/6].#&/@cp
]


(* ::Subsubsection:: *)
(*onedge*)


(* ::Text:: *)
(*onedge plots the exaggerated particle distribution on one edge of the hexagon*)
(*plist = list of positions*)
(*wlist = list of widths*)
(*pscale = arbitrary scaling factor for amplifying position*)
(*wscale = arbitrary scaling factor for amplifying width*)
(*edgenum = edge number 1-6*)
(*color = color to plot distribution in*)
(*OUTPUT: graphics object of the distribution*)
(*CALLS: hexpoints*)
(*CALLED BY: wholepoly*)


onedge[plist_, wlist_, pscale_, wscale_, edgenum_, color_]:=Module[{l1, poly, mid, w1, w2, g1},
l1 = plist;
l1[[;;, 2]] = l1[[;;, 2]]*pscale;
l1[[;;,1]] = l1[[;;,1]]+5;
w1 = w2 = l1;
w1[[;;, 2]] = w1[[;;,2]] + (wlist[[;;,2]] - 0.065)*wscale;
w2[[;;, 2]] = w2[[;;,2]] - (wlist[[;;,2]] - 0.065)*wscale;
poly = hexpoints;
mid = Mean[poly[[edgenum;;edgenum+1]]];
g1 = Graphics[Translate[Rotate[{color, Opacity[0.5], Polygon[Join[w1, Reverse[w2]]], Opacity[1], Thickness[0.02],Line[l1]}, -edgenum*Pi/3 + Pi/6, {0,0}], mid]];
g1
]


(* ::Subsubsection:: *)
(*wholepoly*)


(* ::Text:: *)
(*plot an exaggerated particle distribution on a whole polygon*)
(*mode = 1 for layer-by-layer, 2 for bath*)
(*step = 1 for initial, 2 for relaxed, 3 for sheared*)
(*tpspos = list of positions from dpir3tps*)
(*tpsw = list of widths from dpir3tps*)
(*OUTPUT: graphics object of whole polygon*)
(*CALLS: onedge, hexpoints*)


wholepoly[mode_, step_, tpspos_, tpsw_]:=Module[{g1, colors, cp},
cp = hexpoints;
colors = getdarkcolors[3];
g1 = Show[
	Graphics[{EdgeForm[Gray], FaceForm[], Gray, Arrowheads[0.1], Table[Arrow[cp[[i;;i+1]]], {i, 6}]}]
	,
	Table[
		onedge[tpspos[[mode,3*step-1]], tpsw[[mode,3*step-1]], 25, 100, i, colors[[step]]]
	,{i,6}
	]
, ImageSize->2.5*100
];
Export["figures\\timecartoon_"<>Switch[mode, 1, "lbl", 2, "bath"]<> "_"<>Switch[step, 1, "init", 2, "relax", 3, "shear"]<> ".png", g1, ImageResolution->500];
g1
]


(* ::Section:: *)
(*plots as a function of printing parameters*)


(* ::Subsubsection:: *)
(*bme*)


(* ::Text:: *)
(*bme finds the difference between the corners and the center of the line*)
(*s0 = list of data*)
(*corn = 1 to find difference between beginning and mid, 2 to find diff between end and mid*)
(*OUTPUT: number difference btwn corner, mid*)
(*CALLS: ster (master.wl)*)
(*CALLED BY: changetable*)


bme[s0_, corn_]:=Module[{beg, mid, end, dbm, dem, s1},
s1 = s0;
mid= {Mean[#], ster[#]} &@ Select[s1, 4<#[[1]]<6&][[;;,2]];
If[corn==1
	,
	beg =  If[Length[#]>0 , {Mean[#], ster[#]}, {}] &@ Select[s1, #[[1]]<2.5&][[;;,2]];
	dbm = If[Length[beg]>0, {beg[[1]] - mid[[1]], beg[[2]]+mid[[2]]}, {}]
	,
	end= If[Length[#]>0 , {Mean[#], ster[#]}, {}] &@ Select[s1, #[[1]]>7.5&][[;;,2]];
	dem =  If[Length[end]>0, {end[[1]] - mid[[1]], end[[2]]+mid[[2]]}, {}]
]
]


(* ::Subsubsection:: *)
(*changetable*)


(* ::Text:: *)
(*changetable gets a table of the changes in width or position as a function of corner angle for line 1*)
(*{cf2db_, cf3db_, cf2df_, cf3df_} = big lists, for layer-by-layer (2d) and bath (3d) lists and behind filtering (b) and ahead filtering (f)*)
(*metric = 1-6, what we're plotting (init, relaxed, sheared position or width)*)
(*splitvar = independent variable to split up the data (1 = TEG, 2 = print speed, 6 = corner angle)*)
(*OUTPUT: row of differences and metadata*)
(*CALLS: bme*)
(*CALLED BY: used to construct cornerchanges.csv,  (timeseries.nb)*)


changetable[{cf2db_, cf3db_, cf2df_, cf3df_}, metric_, splitvar_]:=Module[{s1, bm, corn, edge, cflist, line, yvar, s1list, btable, corner, clist},
edge = 10;
Switch[metric
	,1, (*init pos*) cflist = {{}, cf2db, cf3db}; line = 1; yvar = 45; corn = 1;
	,2, (*relaxed pos*) cflist = {{}, cf2df, cf3df}; line = 2; yvar = 54; corn = 2;
	,3, (*sheared pos*) cflist = {{}, cf2db, cf3db}; line = 2; yvar = 48; corn = 1;
	,4, (*init width*) cflist = {{}, cf2db, cf3db}; line = 1; yvar = 46; corn = 1;
	,5, (*relaxed width*) cflist = {{}, cf2df, cf3df}; line = 2; yvar = 55; corn = 2;
	,6, (*sheared width*) cflist = {{}, cf2db, cf3db}; line = 2; yvar = 49; corn = 1;
];
clist = Sort[DeleteDuplicates[cflist[[2]][[;;, splitvar]]]];
Table[
	s1list = ConstantArray[{}, 5];
	s1 = Select[cflist[[m]], #[[3]]==m && #[[4]]==edge && #[[9]]==line && #[[yvar]]!=0&];
	btable = Table[
		corner = clist[[i]];
		s1list[[i]] = {#[[1]]+If[corn==1, -0.6, 0.6], #[[2]]}&/@Select[s1, #[[splitvar]]==corner&][[;;, {14, yvar}]];
		bm = bme[s1list[[i]], corn];
		If[Length[bm]>0, {{corner, bm[[1]]}, ErrorBar[bm[[2]]]}, {}]
	, {i,Length[clist]}];
	btable = Select[btable, Length[#]>0&];
(*	Print[Row[ListPlot[Select[#, #[[1]]<3 || 4<#[[1]]<6 || #[[1]]>7&], ImageSize\[Rule]200, Joined\[Rule]False, PlotRange\[Rule]{{0, 10}, Automatic}]&/@s1list]];*)
	btable
, {m, {2,3}}]
]


(* ::Subsubsection:: *)
(*changeplot*)


(* ::Text:: *)
(*changeplot*)
(*t1 = list of data*)
(*pos = true to plot position, false to plot width*)
(*bottom = true to label bottom*)
(*top = true to label top*)
(*left = true to label left*)
(*legend = true to plot legend*)
(*pr = plot range*)
(*labeln = top label number: 1 initial, 2 relaxed, 3, sheared*)
(*xvar = x variable (column number, 6 = corner angle, 1=ink, 2 = speed)*)
(*OUTPUT: plot of independent variable vs. change with experimental and theoretical data*)
(*CALLS: SWELLTABLEV, LAPTABLE*)
(*CALLED BY: plotgrid*)


changeplot[t1_, pos_, bottom_, top_, left_, legend_, pr_, labeln_, xvar_]:=Module[{huangpoints, xtick, xtickempty, ytick, ytickempty, scale
																				, spacing, thescale, swellpoints, xlabel, htscale, stscale
																				, htab, initwidth, abpoints, abscale, htind, label, lapind, lscale, stind, ringpts, ringscale},
stscale = 1;
initwidth = 1/10;
label = Switch[labeln, 1, "Initial", 2, "Relaxed", 3, "Sheared"];
lapind = If[pos, Switch[labeln, 1, 7, 2, 10, 3, 13], Switch[labeln, 1, 6, 2, 9, 3, 12]];
stind = If[pos, 4, 3];
lscale = 1;
ringscale = 1/300;
If[!pos, 
	stscale = stscale*initwidth; 
	lscale = lscale*initwidth;
	ringscale = ringscale*initwidth;
	,
	ringscale = 0;
];
Switch[xvar
	,6, xlabel = "Corner angle (\[Degree])";
		xtick = {60, 90, 108, 120, 135};
		swellpoints = {#[[1, 2]], stscale*Mean[#[[;;, stind]]]}&/@GatherBy[SWELLTABLEV, #[[2]]&];	
		huangpoints = {#[[1, 1]], lscale*Mean[#[[;;, lapind]]]}&/@GatherBy[LAPTABLE[[2;;]], #[[1]]&];
		ringpts = Table[{\[Theta], ringscale*2*Mean[{3,6,9,12}]*Cos[\[Theta]/2*Degree]}, {\[Theta], xtick}];
	,1, xlabel = "w% TEGDMA";
		xtick = Range[20, 35, 5];
		swellpoints = Transpose[{xtick, ConstantArray[stscale*Mean[SWELLTABLEV[[;;, stind]]], 4]}];		
		huangpoints = {#[[1, 2]], lscale*Mean[#[[;;, lapind]]]}&/@GatherBy[LAPTABLE[[2;;]], #[[2]]&];
		ringpts = Transpose[{xtick, ringscale*ConstantArray[Mean[Table[2*Mean[{3,6,9,12}]*Cos[\[Theta]/2*Degree], {\[Theta], xtick}]],4]}];		
	,2, xlabel = "Print speed (mm/s)";
		xtick = Range[3, 12, 3];
		swellpoints = {#[[1, 1]], stscale*Mean[#[[;;, stind]]]}&/@GatherBy[SWELLTABLEV, #[[1]]&];
		huangpoints = {#[[1, 3]], lscale*Mean[#[[;;, lapind]]]}&/@GatherBy[LAPTABLE[[2;;]], #[[3]]&];	
		ringpts = Table[{v, ringscale*2*v*Mean[Table[Cos[\[Theta]/2*Degree], {\[Theta], xtick}]]}, {v, 3, 12, 3}];
]; 
xtickempty = {#, ""}&/@xtick;
If[pos, 
	spacing = If[(pr[[2]]-pr[[1]])/0.05<7, 0.05, 0.1];
	ytick = Range[-1, 1, spacing];
	ytickempty = {#, ""}&/@ytick;
	ytick = {#, If[#==0, If[spacing==0.05, "0.00", "0.0"], SetAccuracy[#, If[spacing==0.05, 3, 2]]]}&/@ytick;
,
	spacing = If[(pr[[2]]-pr[[1]])/0.005<7, 0.005, 0.01];
	ytick = Range[-1, 1, spacing];
	ytickempty = {#, ""}&/@ytick;
	ytick = {#, If[#==0, If[spacing==0.005, "0.000", "0.00"], SetAccuracy[#, If[spacing==0.005, 4, 3]]]}&/@ytick;
];
scale = 2;
Show[
	ErrorListPlot[t1
		, Axes->None
		, FrameLabel->{{If[left, "\[CapitalDelta]"<>If[pos, "position", "width"]<>" (mm)", None], None}, {If[bottom, xlabel, None], None}}
		, FrameTicks->{{If[left, ytick, ytickempty], ytickempty}, {If[bottom, xtick, xtickempty], xtickempty}}
		, FrameTicksStyle->{Black, AbsoluteThickness[scale]}
		, FrameStyle->AbsoluteThickness[scale]
		, GridLines->{None, {-0.15, 0, 0.15}}
		, GridLinesStyle->{Lighter[Gray], AbsoluteThickness[scale/2]}
		, ImageSize->(2*100*scale - If[left, 0, 43*scale])
		, ImagePadding->({{If[left, 45, 2], 2}, {If[bottom, 30, 3], 3}}*scale)
		, Joined->{False, False, True, True}
		, LabelStyle->Directive[10*scale, Black]
		, PlotLabel->If[top, Style[label, 10*scale], None]
		, PlotLegends->If[legend, Placed[{"Layer by layer", "Bath", "Capillary theory", "Swell theory"}, {Top, Right}], None]
		, PlotMarkers->{{\[EmptyCircle], 15*scale}, {\[FilledCircle], 15*scale}}
		, PlotRangePadding->Scaled[0.1]
		, PlotStyle->Black
	]
	,
	ListPlot[If[legend
				, {Labeled[huangpoints, "Smoothing", Below], Labeled[swellpoints, "Swelling", Below], Labeled[ringpts, Style["Ringing", Gray], Below]}
				, {huangpoints, swellpoints, ringpts}
			]
		, Joined-> True
		, LabelStyle-> Directive[10*scale, Black]
		, PlotMarkers-> None
		, PlotRange-> All
		, PlotStyle-> {{Black, Dashed}, Black, Gray}
	]
, PlotRange->pr]
]


(* ::Subsubsection:: *)
(*plotgrid*)


(* ::Text:: *)
(*plotgrid is a grid of plots for change in position and width for initial, relaxed, and sheared distributions*)
(*ctables = table from cornerchanges.csv*)
(*xvar = independent variable: 6 = corner angle, 1 = tegdma conc., 2 = print speed*)
(*OUTPUT: grid of 6 plots*)
(*CALLS: changeplot*)
(*CALLED BY:  (timeseries.nb)*)


plotgrid[ctables_, xvar_]:=Module[{t1, t2, t3, t4, t5, t6, pr1, pr2, grid1},
t1 = {{#[[4]] - If[xvar==1, 800, 0], #[[5]]}, ErrorBar[#[[6]]]}&/@SortBy[#, #[[4]]&]&/@GatherBy[Select[ctables, #[[2]]==1 && #[[1]]==xvar&], #[[3]]&];
t2 = {{#[[4]] - If[xvar==1, 800, 0], #[[5]]}, ErrorBar[#[[6]]]}&/@SortBy[#, #[[4]]&]&/@GatherBy[Select[ctables, #[[2]]==2 && #[[1]]==xvar&], #[[3]]&];
t3 = {{#[[4]] - If[xvar==1, 800, 0], #[[5]]}, ErrorBar[#[[6]]]}&/@SortBy[#, #[[4]]&]&/@GatherBy[Select[ctables, #[[2]]==3 && #[[1]]==xvar&], #[[3]]&];
t4 = {{#[[4]] - If[xvar==1, 800, 0], #[[5]]}, ErrorBar[#[[6]]]}&/@SortBy[#, #[[4]]&]&/@GatherBy[Select[ctables, #[[2]]==4 && #[[1]]==xvar&], #[[3]]&];
t5 = {{#[[4]] - If[xvar==1, 800, 0], #[[5]]}, ErrorBar[#[[6]]]}&/@SortBy[#, #[[4]]&]&/@GatherBy[Select[ctables, #[[2]]==5 && #[[1]]==xvar&], #[[3]]&];
t6 = {{#[[4]] - If[xvar==1, 800, 0], #[[5]]}, ErrorBar[#[[6]]]}&/@SortBy[#, #[[4]]&]&/@GatherBy[Select[ctables, #[[2]]==6 && #[[1]]==xvar&], #[[3]]&];
pr1 = {-0.12, 0.2};
pr2 = {0.0005, 0.012};
grid1 = Grid[{{changeplot[t1, True,False, True,True, False, pr1, 1, xvar], changeplot[t2, True, False, True,False, False, pr1, 2, xvar], changeplot[t3, True,False, True,False, True, pr1,3, xvar]}
			, {changeplot[t4, False, True,False,True, False, pr2, 1, xvar], changeplot[t5, False, True, False,False, False, pr2, 2, xvar], changeplot[t6,False, True, False,False, False, pr2, 3, xvar]}}]
]
