(* ::Package:: *)

(* ::Title:: *)
(*Relax results*)


(* ::Text:: *)
(*Changes in position during relaxation, as a function of relaxation time. Used for "Changes in filament microstructures during direct ink writing with yield stress fluid support"*)
(*Last updated by Leanne Friedrich, November 2019*)


<<"Mathematica\\master.wl";


(* ::Section:: *)
(*utilities*)


(* ::Subsubsection::Closed:: *)
(*nlmtxt*)


(* ::Text:: *)
(*nlmtxt writes an equation in size 7 arial*)


nlmtxt[nlm_]:=s7@SetPrecision[Normal[nlm], 1]


(* ::Section:: *)
(*plots*)


(* ::Subsubsection::Closed:: *)
(*pl*)


(* ::Text:: *)
(*pl is a plot label*)
(*mode = 2 for layer-by-layer, 3 for bath*)
(*p = 0 for position*)
(*lp = left padding in px*)
(*OUTPUT: plot label*)
(*CALLED BY: rpplot, logrelaxplot*)


pl[mode_, p_, lp_]:= Row[{
	Graphics[
		Text[
			Style[Switch[mode
					, 2, If[p==0,"A", "C"]
					, 3, If[p==0,"B","D"]]
				,12*PS
				, FontFamily->"Arial"
			]
			, {0,0}
			, {Left, Bottom}
		]
		, ImageSize->{20*PS, 15*PS}
		, PlotRange->{{0,20*PS},{0,15*PS}}
		, ImagePadding->0
	], Graphics[
		Text[
			Style[Switch[mode,2,"Layer-by-layer",3,"Bath"], 8*PS, FontFamily->"Arial"]
		]
		, ImageSize->{((3.25*72 - 42-25)/2+lp+0)*PS-40*PS, 15*PS}
	], Graphics[
		Text[
			Style["",12*PS, FontFamily->"Arial"]
			, {0,0}
			, {Left, Bottom}
		]
		, ImageSize->{20*PS, 15*PS}
		, PlotRange->{{0,20*PS},{0,15*PS}}
		, ImagePadding->0
	]
}];


(* ::Subsection:: *)
(*linear plots*)


(* ::Subsubsection::Closed:: *)
(*ticksfunc2*)


(* ::Text:: *)
(*list of ticks*)
(*pr = plot range *)
(*dM = major tick spacing*)
(*dm = minor tick spacing*)
(*OUTPUT: tick list*)
(*CALLS: accu (master.wl)*)
(*CALLED BY: rpplot*)


ticksfunc2[pr_, dM_, dm_]:=Module[{majorticks, minorticks, l1, l2, ticksleft, tickstop},
majorticks = Round[Range[pr[[1]], pr[[2]],dM], dM];
minorticks = Complement[Round[Range[pr[[1]], pr[[2]],dm],dm] , majorticks];
l1 = 0.015;
l2 = l1/2;
ticksleft = Join[{#, If[#==0, "0."<>StringJoin[ConstantArray["0", accu[dM]-1]], SetAccuracy[#, accu[dM]]],{l1,0}}
							&/@majorticks, {#, "", {l2,0}}&/@minorticks];
tickstop = Join[{#,"",{l1,0}}&/@majorticks, {#, "",{l2,0}}&/@minorticks];
{ticksleft, tickstop}]


(* ::Subsubsection::Closed:: *)
(*rpplot*)


(* ::Text:: *)
(*a single relax plot*)
(*changetable = changetable.csv imported*)
(*mode = 2 for layer-by-layer, 3 for bath*)
(*p = 0 for position*)
(*colors = color list*)
(*markers = marker list*)
(*used = use d in the fitting formula*)
(*is = image size in in*)
(*OUTPUT: plot of NN and 2NN for time vs. change, fitted*)
(*CALLS: accumulatederror (master.wl), ster (master.wl), nlmtxt (master.wl), s7 (master.wl), ticksfunc2, s8 (master.wl), pl*)
(*CALLED BY: relaxplotgrid*)


rpplot[changetable_, mode_, p_, colors_, markers_, used_, is_]:=Module[{lp, rp, tp2, tp, lbl, tmin, tmax, dt, nlmlist, legend, pr, s1, nn, ticks, ticksleft, ticksright, tickstop, ticksbot},
lp = If[mode==2, 40, 3];
rp = If[mode==2, 2, 15];
tp2 = If[p==0, -0.05, 0.005]*{t^(1/10),t^(1/4)};
nlmlist = {"", ""};
nn = 1;
tp = Table[
	lbl = Flatten[Table[{N[(#[[5]]*#[[4]]-0.6)/#[[2]]], #[[y]], #[[y+1]]}&/@Select[changetable, #[[3]]==mode&&#[[y]]!=0&], {y, ylist}],1];
	If[used
		, nlmlist[[nn]] = Quiet[NonlinearModelFit[lbl[[;;, 1;;2]]
									, {a+b*(t-d)^c, c>0, If[p==2, b>0, b<0], If[p==2, -0.3<a<0, 0.6>a>0], d>0}
									, {a,b,{c, 0.25},d}, t]];
		, nlmlist[[nn]] = Quiet[NonlinearModelFit[lbl[[;;, 1;;2]]
									, {a+b*(t)^c, c>0, If[p==2, b>0, b<0], If[p==2, -0.3<a<0, 0.6>a>0]}
									, {a,b,{c, 0.25}}, t]];
	];
	nn = nn+1;
	tmin = Min[lbl[[;;,1]]];
	tmax = Max[lbl[[;;,1]]];
	dt = (tmax-tmin)/30;
	Select[
		Table[
			s1 = Select[lbl, t<=#[[1]]<t+dt&];
			If[Length[s1]>0
				,{{Mean[s1[[;;,1]]], Mean[s1[[;;,2]]]}
						, ErrorBar[ster[s1[[;;,1]]], accumulatederror[s1[[;;,3]]]+ster[s1[[;;,2]]]]}
				,{}
			]
		,{t, tmin, tmax-dt, dt}]
	, Length[#]>0&]
, {ylist, {{10, 16}+p, {22+p}}}];
(*nlmlist = NonlinearModelFit[#[[;;, 1]], {a+b*t^c, c>0, If[p\[Equal]2, b>0, b<0], If[p\[Equal]2, a<0, a>0]}, {a,b,c}, t]&/@tp;*)
legend  = PointLegend[colors
			, {Row[{s7@"NN, ", nlmtxt[nlmlist[[1]]]}]
					, Style[Row[{s7@"2NN, ", nlmtxt[nlmlist[[2]]]}], colors[[2]]]}
			, LegendMarkers->markers
			, LegendLayout->"Column"];
pr = {{0, 29}, Switch[p, 0,{-0.31, 0.02},2, {-0.05, 0.015}]};
nlmlist = #[t]&/@nlmlist;
ticksbot = If[p==0, ticksfunc2[{-0.3, 0.05}, 0.05, 0.01], ticksfunc2[{-0.05, 0.01}, 0.01, 0.005]];
If[mode==3, ticksbot[[1]]=ticksbot[[2]]];
ticksleft = ticksfunc2[{0, 30}, 5*10^0, 1];
ticksleft[[1,;;,2]] = If[NumberQ[#], Round[#], If[#=="0.", "0", ""]]&/@ticksleft[[1,;;,2]];
ticks = {ticksbot, ticksleft};
Show[
	Graphics[
		Join[
			If[mode==3,
				If[p==0,
					{ Text[s8[Rotate["out", Pi/2]], {pr[[1,2]]+1,pr[[2,1]]}, {Left,Bottom}]
						, Text[s8[Rotate["in", Pi/2]], {pr[[1,2]]+1,pr[[2,2]]}, {Left, Top}]}
					,{ Text[s8[Rotate["narrow", Pi/2]], {pr[[1,2]]+1,pr[[2,1]]}, {Left, Bottom}]
						, Text[s8[Rotate["wide", Pi/2]], {pr[[1,2]]+1,pr[[2,2]]}, {Left,Top}]}
				]
			,{}
			]
		]
	]
	, ErrorListPlot[tp
		, Joined->False
		, PlotRange->pr
		, PlotLegends->Placed[legend, {0.5, 0.16}]
		, PlotMarkers->markers
		, PlotStyle->({Directive[Thin] , #}&/@colors)
		]
	, Plot[nlmlist, {t, 0,pr[[1,2]]}
		, PlotStyle->colors
		,PlotRange->pr
	]
	, AspectRatio->1
	, Axes->None
	, Frame->True
	, FrameTicks->ticks
	, FrameStyle->Black
	, FrameLabel->{"Time (s)", Switch[p, 0, "\[CapitalDelta]position relax (mm)", 2, "\[CapitalDelta]width relax (mm)"]}
	, GridLines->{None, {0}}
	, ImagePadding->(PS*{{lp, rp}, {If[p==0, 1, 30], 2}})
	, ImageSize->((is*72 - 42-25)/2+lp+rp)*PS
	, LabelStyle->Directive[8*PS, Black]
	, PlotLabel->Column[{pl[mode, p, lp](*, legend*)}]
	, PlotRange->pr
	, PlotRangePadding->None
]]


(* ::Subsubsection:: *)
(*relaxplotgrid*)


(* ::Text:: *)
(*grid of relaxplots*)
(*changetable = imported changetable.csv*)
(*ps = plot scale (100/72)*)
(*d = true to include d in formula*)
(*is = image size in in*)
(*OUTPUT: grid of plots*)
(*CALLS: rpplot, markerlist (master.wl)*)
(*CALLED BY: (relaxresults.nb)*)


relaxplotgrid[changetable_, ps_, d_, is_]:=Module[{(*colors, ms, markers, tp2, tp, lbl, legend, epilog, lp, tmin, tmax, dt, s1, rp, pr*)colors, ms, markers},
colors = {Black, Gray};
ms = 0.6;
markers = {#, 10*ps*ms}&/@markerlist[2];
Grid[Table[Table[rpplot[changetable, mode, p, colors, markers, d, is],{mode, 2, 3}], {p, {0, 2}}], Spacings->{0,0}]
];


(* ::Subsection:: *)
(*log plots*)


(* ::Subsubsection:: *)
(*mticks*)


(* ::Text:: *)
(*list of ticks for log plots*)
(*OUTPUT: ticks list*)
(*CALLED BY: logrelax*)


mticks:=Module[{majorticks, minorticks,l1, l2, ticksleft, ticksright, ticksleftw, ticksrightw, ticksbot, tickstop},
majorticks = { 0.05,  0.10, 0.20};
minorticks = Join[Range[0.001, 0.01, 0.001], Range[0.01, 0.1, 0.01], Range[0.1, 1, 0.1]];
l1 = 0.015;
l2 = l1/2;
ticksleft = Join[{-Log[#],SetAccuracy[-#,3],{l1,0}}&/@majorticks, {-Log[#], "", {l2,0}}&/@minorticks];
ticksright = Join[{-Log[#],"",{l1,0}}&/@majorticks, {-Log[#], "",{l2,0}}&/@minorticks];
majorticks = {0.0275,0.03, 0.0325, 0.035, 0.0375};
minorticks = Join[Range[0.001, 0.01, 0.001], Range[0.01, 0.1, 0.01], Range[0.1, 1, 0.1]];
ticksleftw= Join[{Log[#],#,{l1,0}}&/@majorticks, {Log[#], "", {l2,0}}&/@minorticks];
ticksrightw = Join[{Log[#],"",{l1,0}}&/@majorticks, {Log[#], "",{l2,0}}&/@minorticks];
majorticks = {1, 5, 10, 20};
minorticks = Join[Range[1, 10,1], Range[20, 50, 10]];
ticksbot= Join[{Log[#],#,{l1,0}}&/@majorticks, {Log[#], "", {l2,0}}&/@minorticks];
tickstop = Join[{Log[#],"",{l1,0}}&/@majorticks, {Log[#], "",{l2,0}}&/@minorticks];
{ticksleft, ticksright, ticksleftw, ticksrightw, ticksbot, tickstop}
]


(* ::Subsubsection:: *)
(*logrelaxplot*)


(* ::Text:: *)
(*one log plot*)
(*mode = 2 for layer-by-layer*)
(*p = 0 for position*)
(*colors = list of colors*)
(*markers  = list of markers*)
(*ticks = list of ticks*)
(*OUTPUT: one log plot, fitted*)
(*CALLS: ster (master.wl), s8 (master.wl)*)
(*CALLED BY: logrelax*)


logrelaxplot[changetable_, mode_, p_, colors_, markers_, ticks_]:=Module[{lp, rp, pr, tp, lbl, tmin, tmax, dt, s1, lmf, slopes, ticksleft, ticksright, ticksleftw, ticksrightw, ticksbot, tickstop, labelx},
lp = If[mode==2, 40,2];
rp = If[mode==2,2,25];
pr = {{0.1, Log[30]}, If[p==0, (*{-3.5, -1.7}, {-3.6,-3.26}*){1.7, 3.5}, {3.26, 3.6}]};
{ticksleft, ticksright, ticksleftw, ticksrightw, ticksbot, tickstop} = ticks;
tp = Table[
	lbl = Select[
			Flatten[
				Table[
					{Log[N[(#[[5]]*#[[4]]-0.6)/#[[2]]]]
						, If[p==0, -1, 1]*Log[If[p==0,-1,1]*#[[y]]+If[p==0,0,0]]}
					&/@Select[changetable, #[[3]]==mode&&#[[y]]!=0&]
				, {y, ylist}]
			,1]
		, Im[#[[2]]]==0&];
	tmin = Min[lbl[[;;,1]]];
	tmax = Max[lbl[[;;,1]]];
	dt = (tmax-tmin)/15;
	Select[
		Table[
			s1 = Select[lbl, t<=#[[1]]<t+dt&];
			If[Length[s1]>0
				,{{Mean[s1[[;;,1]]],Mean[s1[[;;,2]]]}, ErrorBar[ster[s1[[;;,1]]], ster[s1[[;;,2]]]]}
				,{}
			]
		,{t, tmin, tmax-dt, dt}]
	, Length[#]>0&]
, {ylist, {{10, 16}+p, {22+p}}}];
lmf = {LinearModelFit[tp[[1, ;;, 1]],x,x], LinearModelFit[tp[[2, ;;, 1]],x,x]};
slopes = Superscript["-t", ToString[SetAccuracy[-D[#[x],x], 4]]]&/@lmf;
labelx = pr[[1,2]]*1.1;
Show[
	Graphics[If[mode==3, If[p==0,
					{ Text[s8[Rotate["out", Pi/2]], {labelx,pr[[2,1]]}, {Left,Bottom}]
						, Text[s8[Rotate["in", Pi/2]], {labelx,pr[[2,2]]}, {Left, Top}]}
					,{ Text[s8[Rotate["narrow", Pi/2]], {labelx,pr[[2,1]]}, {Left, Bottom}]
						, Text[s8[Rotate["wide", Pi/2]], {labelx,pr[[2,2]]}, {Left,Top}]}
				]
				,{}]
	]
	,
	ErrorListPlot[tp
		, Joined->False
		, PlotMarkers->markers
		, PlotStyle->({Directive[Thin] , #}&/@colors)

	]
	,
	Plot[
		{Callout[lmf[[1]][x],Style[ slopes[[1]], 12*PS, FontFamily->"Arial", colors[[1]]], {Scaled[0.5], If[p==2,Below, Above]}]
			, Callout[lmf[[2]][x], Style[slopes[[2]], 12*PS, FontFamily->"Arial", colors[[2]]], {Scaled[0.5], If[p==2, Above, Below]}]}
		, {x, pr[[1,1]], pr[[1,2]]}
		, PlotStyle->colors
	]
	, AspectRatio->1
	, Axes->None
	, Frame->True
	, FrameStyle->Black
	, FrameLabel->{"Time between measurements (s)", Switch[p, 0, "\[CapitalDelta]position relax (mm)", 2, "\[CapitalDelta]width relax (mm)"]}
	, FrameTicks->{{If[p==0,ticksleft, ticksleftw],If[p==0, ticksright, ticksrightw]}, {ticksbot, tickstop}}
	, GridLines->{None, {-0.15, 0, 0.15}}
	, ImagePadding->(PS*{{lp, rp}, {35, 2}})
	, ImageSize->((4.5*72 - 42-25)/2+lp+rp)*PS
	, LabelStyle->Directive[8*PS, Black]
	, PlotLabel->pl[mode, p, 40]
	, PlotRange->pr
	, PlotRangePadding->Scaled[0.05]
]
]


(* ::Subsubsection:: *)
(*logrelax*)


(* ::Text:: *)
(*log relax plot grid*)
(*OUTPUT: grid of log plots for positions*)
(*CALLS: markerlist (master.wl), mticks, logrelaxplot*)
(*CALLED BY: (relaxresults.nb)*)


logrelax[changetable_]:=Module[{colors, ms, markers, logrpg, ticksleft, ticks},
colors = {Black, Gray};
ms = 0.6;
markers = {#, 10*PS*ms}&/@markerlist[2];
ticks = mticks;
logrpg = Column[{
			PointLegend[colors, s8/@{"Nearest neighbor", Style["2nd nearest neighbor", colors[[2]]]}
				, LegendMarkers->({#, 20*PS*ms}&/@markerlist[2])
				, LegendLayout->"Row"
			], 
			Grid[Table[
				Table[
					logrelaxplot[changetable, mode, p, colors, markers, ticks]
				, {mode, {2,3}}]
			,{p, {0}}], Spacings->{0,0}]
		}, Alignment->Right]
]
