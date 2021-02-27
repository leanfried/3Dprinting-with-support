(* ::Package:: *)

(* ::Title:: *)
(*Correlations*)


(* ::Text:: *)
(*Functions to make plots and tables of correlations between dependent variables. Used for "Changes in filament microstructures during direct ink writing with yield stress fluid support"*)
(*Last updated November 2019 by Leanne Friedrich*)


<<"Mathematica\\kendall.wl";
<<"Mathematica\\master.wl";


(* ::Section:: *)
(*utilities*)


(* ::Subsubsection:: *)
(*btregion, ctregion*)


(* ::Text:: *)
(*get the region name from the bigtable2 column number n*)
(*OUTPUT: string*)
(*CALLS: REGIONLIST (master.wl)*)
(*CALLED BY: (correlations2.nb)*)


btregion[n_]:=REGIONLIST[[(n-9)/2]]


(* ::Text:: *)
(*get the region name from the changetable column number n*)
(*OUTPUT: string*)
(*CALLS: REGIONLIST (master.wl)*)
(*CALLED BY: (correlations2.nb)*)


ctregion[n_]:=REGIONLIST[[(n-8)/2]]


(* ::Subsubsection:: *)
(*absdist, reldist*)


(* ::Text:: *)
(*n = column number in bigheader2*)
(*short = true to shorten the string*)
(*units = true to show units*)
(*OUTPUT: string*)
(*CALLED BY: (correlations2.nb)*)


absdist[n_, short_, units_]:=Switch[n
			, 31, "Initial "<>If[short, "pos", "position"]<>If[units, " (mm)", ""]
			, 49,  "Relaxed " <>If[short, "pos", "position"]<>" NN"<>If[units, " (mm)", ""]
			, 55, "Relaxed "<>If[short, "pos", "position"]<>" 2NN"<>If[units, " (mm)", ""]
			, 74,  "Sheared "<>If[short, "pos", "position"]<>If[units, " (mm)", ""]
			, 35, "Initial "<>If[short, "w", "width"]<>If[units, " (normalized)", ""]
			, 53, "Relaxed "<>If[short, "w", "width"]<>" NN"<>If[units, " (normalized)", ""]
			, 59, "Relaxed "<>If[short, "w", "width"]<>" 2nd NN"<>If[units, " (normalized)", ""]
			, 76,  "Sheared "<>If[short, "w", "width"]<>If[units, " (normalized)", ""]
			]


(* ::Text:: *)
(*n = column number in combineheader*)
(*short = true to shorten the string*)
(*units = true to show units*)
(*latex = true to print latex symbols, false for mathematica plots*)
(*OUTPUT: string*)
(*CALLED BY: (correlations2.nb)*)


reldist[n_, short_, units_, latex_]:=Switch[n
			, 30, "Initial "<>If[short, "pos", "position"]<>If[units, " (mm)", ""]
			, 36, If[latex, "$\\Delta$", "\[CapitalDelta]"]<>If[short, "pos", "position"]<>" relax NN"<>If[units, " (mm)", ""]
			, 42, If[latex, "$\\Delta$", "\[CapitalDelta]"]<>If[short, "pos", "position"]<>" relax 2nd NN"<>If[units, " (mm)", ""]
			, 48, If[latex, "$\\Delta$", "\[CapitalDelta]"]<>If[short, "pos", "position"]<>" shear"<>If[units, " (mm)", ""]
			, 34, "Initial "<>If[short, "w", "width"]<>If[units, " (normalized)", ""]
			, 40, If[latex, "$\\Delta$", "\[CapitalDelta]"]<>If[short, "w", "width"]<>" relax NN"<>If[units, " (normalized)", ""]
			, 46, If[latex, "$\\Delta$", "\[CapitalDelta]"]<>If[short, "w", "width"]<>" relax 2nd NN"<>If[units, " (normalized)", ""]
			, 52, If[latex, "$\\Delta$", "\[CapitalDelta]"]<>If[short, "w", "width"]<>" shear"<>If[units, " (normalized)", ""]
			]


(* ::Section:: *)
(*tables*)


(* ::Subsubsection:: *)
(*corrtable*)


(* ::Text:: *)
(*latex table of correlations between columns in a table*)
(*ktable = kendall correlation table*)
(*ilist = list of vars: columns in the output table*)
(*jlist = list of vars: rows in the output table*)
(*iheader = header labels for columns*)
(*jheader = header labels for rows*)
(*rotate = rotate column header labels in the latex table*)
(*OUTPUT: string containing latex table*)
(*CALLS: mspace (master.wl), tb2lx (master.wl)*)
(*CALLED BY: (corrrelations2.nb)*)


corrtable[ktable_, ilist_, jlist_, iheader_, jheader_, rotate_]:=Module[{t1, tout, indexlist, j},
Catch[
If[Length[ilist]!=Length[iheader] || Length[jlist!=Length[jheader]],
	Throw["Header lists must be same length as index lists"];
];
indexlist = DeleteDuplicates[Flatten[ktable[[;;,{1,2}]]]];
If[Length[Complement[ilist, indexlist]]>0 || Length[Complement[jlist, indexlist]]>0,
	Throw["A requested index is not in this table."]
];
t1 = Prepend[
		Flatten[
			Table[
				Prepend[
					Table[
						Prepend[
							Table[
								j = jlist[[jj]];
								If[!MemberQ[ilist, j]||i>j
									, mspace[Select[ktable
												, #[[1]]==If[i>j, j, i] && #[[2]]==If[i>j, i, j] && #[[5]]==m &
											][[1,8]]]
									, ""
								]
							, {i, ilist}]
						, jheader[[jj]]]
					,{jj, Length[jlist]}]
				, {"\\multicolumn{"<>ToString[1+Length[ilist]]<>"}{l}{\\textbf{"
							<>Switch[m, 2, "Layer-by-layer", 3, "Bath"]<>"}}"}]
			, {m, 2,3}]
		,1]
	, Prepend[iheader, ""]
];
If[rotate,
	t1[[1, 2;;]] = "\\rotatebox{90}{"<>#<>"}"&/@t1[[1, 2;;]];
];
Do[
	t1[[i,1]] = "\t"<>t1[[i,1]];
,{i, Length[t1]}];
tout = "\\centering\\begin{tabular}{l"
		<>StringJoin[ConstantArray["c", Length[ilist]]]
		<>"}\n"<>tb2lx[t1]<>"\n\\end{tabular}"
]]


(* ::Section:: *)
(*plots*)


(* ::Subsection:: *)
(*Point averaging*)


(* ::Subsubsection:: *)
(*tplist*)


(* ::Text:: *)
(*Average a big list of points. Points will have to be rotated back if rotate=true using rotatedplot2 or rotatedplot3. Points are grouped into 20 equal size groups.*)
(*table = list of data*)
(*ijlist = list of pairs of column numbers*)
(*rotate = bool true to rotate points to x or y axis so that averages will be taken relative to x=y or x=-y*)
(*OUTPUT: {i, j, list of points} for all {i,j} pairs*)
(*CALLS: kall (kendall.wl), avea (master.wl), BTNNINDICES (master.wl)*)
(*CALLED BY: (correlations2.nb)*)


tplist[table_, ijlist_, rotate_]:=Module[{s1, ksign, i, j, lc},
Table[
	i = ij[[1]];
	j = ij[[2]];
	{i,j, Table[
		s1 = Flatten[
				Table[
					Table[
						Select[table, #[[ii]]!=0 && #[[jj]]!=0 && #[[3]]==m &][[;;, {ii,jj}]]
					, {ii, If[i<=72, {i}, BTNNINDICES[[i-72]]]}]
				, {jj, If[j<=72, {j}, BTNNINDICES[[j-72]]]}]
			,2]; 
		If[rotate, 
			ksign = kall[s1,1,2][[1]];
			If[ksign>0
				, s1 = SortBy[(RotationMatrix[Pi/4].#)&/@s1, #[[2]]&];
				, s1 = SortBy[(RotationMatrix[Pi/4].#)&/@s1, #[[1]]&];
			];
			,
			s1 = Sort[s1];
		];
		avea/@Partition[s1, Round[Length[s1]/20]]
	,{m,{3,2}}]}
, {ij, ijlist}]
]


(* ::Subsubsection:: *)
(*tplist2*)


(* ::Text:: *)
(*Average a big list of points. Points will have to be rotated back if rotate = true using rotatedplot2 or rotatedplot3. Points are grouped into 20 equally spaced groups.*)
(*table = list of data*)
(*ijlist = list of pairs of column numbers*)
(*rotate = bool true to rotate points to x or y axis so that averages will be taken relative to x = y or x = -y*)
(*OUTPUT : list of points*)
(*CALLS : kall (kendall.wl), avea (master.wl), BTNNINDICES (master.wl)*)
(*CALLED BY: (correlations2.nb)*)


tplist2[table_, ijlist_, rotate_]:=Module[{s1, ksign, i, j, lc, k, imin, imax, di, s2},
Table[
	i = ij[[1]];
	j = ij[[2]];
	{i,j, Table[
		s1 = Flatten[
				Table[
					Table[
						Select[table, #[[ii]]!=0 && #[[jj]]!=0 && #[[3]]==m &][[;;, {ii,jj}]]
					, {ii, If[i<=72, {i}, BTNNINDICES[[i-72]]]}]
				, {jj, If[j<=72, {j}, BTNNINDICES[[j-72]]]}]
			,2]; 
		If[rotate, 
			ksign = kall[s1,1,2][[1]];
			If[ksign>0
				, k = 2;
				, k = 1;
			];
			s1 = (RotationMatrix[Pi/4].#)&/@s1;
			,
			k = 1;
		];
		imin = Min[s1[[;;, k]]];
		imax = Max[s1[[;;,k]]];
		di = (imax-imin)/20;
		Select[
			Table[
				s2 = Select[s1, i<#[[k]]<i+di&];
				If[Length[s2]>0, avea[s2],{}]
			, {i, imin, imax, di}]
		, Length[#]>0&]
	,{m,{3,2}}]}
, {ij, ijlist}]
]


(* ::Subsection:: *)
(*Plots*)


(* ::Subsubsection:: *)
(*ticksfunc*)


(* ::Text:: *)
(*pr = plot range {{xmin, xmax}, {ymin, ymax}}*)
(*dM = major tick spacing*)
(*dm = minor tick spacing*)
(*OUTPUT: ticks*)
(*CALLS: accu*)
(*CALLED BY: vposgrid, vposgrid2, (correlations2.nb)*)


ticksfunc[pr_, dM_, dm_]:=Module[{majorticks, minorticks, l1, l2, ticksleft, tickstop},
majorticks = Round[Range[Min[pr[[;;,1]]], Max[pr[[;;,2]]],dM], dM];
minorticks = Complement[Round[Range[Min[pr[[;;,1]]], Max[pr[[;;,2]]],dm],dm] , majorticks];
l1 = 0.015;
l2 = l1/2;
ticksleft = Join[{#
					, If[#==0
						, "0."<>StringJoin[ConstantArray["0", accu[dM]-1]]
						, SetAccuracy[#, accu[dM]]
						]
					,{l1,0}
				}&/@majorticks
			, {#, "", {l2,0}}&/@minorticks
			];
tickstop = Join[{#,"",{l1,0}}&/@majorticks
				, {#, "",{l2,0}}&/@minorticks
			];
{{ticksleft, tickstop}, {ticksleft, tickstop}}]


(* ::Subsubsection:: *)
(*fits1, fitsinrange*)


(* ::Text:: *)
(*determine if the point pt fits in the range pr*)
(*OUTPUT: bool*)
(*CALLED BY: fitsinrange*)


fits1[pt_, pr_]:=(pr[[1,1]]<pt[[1]]<pr[[1,2]] && pr[[2,1]]<pt[[2]]<pr[[2,2]])


(* ::Text:: *)
(*given a point pt with error, a plot range pr, and a rotationmatrix r, determine if the point falls in the range*)
(*OUTPUT: bool*)
(*CALLS: fits1*)
(*CALLED BY: rotatedplot4*)


fitsinrange[pt_, pr_, r_]:=Module[{pp, dx, dy, pt1, pt2, pt3, pt4},Catch[
	pp = pt[[1]];
	If[!fits1[r.pp, pr], Throw[False]];
	dx = pt[[2,1]];
	pt1 = r.(pp + {dx, 0});
	If[!fits1[pt1, pr], Throw[False]];
	pt2 = r.(pp - {dx, 0});
	If[!fits1[pt2, pr], Throw[False]];
	dy = pt[[2,2]];
	pt3 = r.(pp + {0, dy});
	If[!fits1[pt3, pr], Throw[False]];	
	pt4 = r.(pp - {0, dy});
	If[!fits1[pt4, pr], Throw[False]];
	True	
]]


(* ::Subsubsection:: *)
(*errorpoint*)


(* ::Text:: *)
(*errorpoint plots a point with error bars that has been rotated*)
(*m = marker (string)*)
(*p = point {{x,y}, ErrorBar[x, y]}*)
(*w = width of error bar ticks*)
(*a = rotation angle (radian)*)
(*OUTPUT: graphics object of one point*)
(*CALLED BY: rotatedplot4*)


errorpoint[m_, p_,w_, a_]:=Module[{r},
r = RotationMatrix[a];
{Text[m, r.p[[1]]]
,Thickness[0.0005]
, Line[{r.{p[[1,1]], p[[1,2]]-p[[2,2]]}, r.{p[[1,1]], p[[1,2]]+p[[2,2]]}}] (*vertical*)
, Line[{r.{p[[1,1]]-p[[2,1]], p[[1,2]]-w}, r.{p[[1,1]]-p[[2,1]], p[[1,2]]+w}}] (*vertical left*)
, Line[{r.{p[[1,1]]+p[[2,1]], p[[1,2]]-w}, r.{p[[1,1]]+p[[2,1]], p[[1,2]]+w}}] (*vertical right*)
, Line[{r.{p[[1,1]]-p[[2,1]], p[[1,2]]}, r.{p[[1,1]]+p[[2,1]], p[[1,2]]}}] (*horizontal*)
, Line[{r.{p[[1,1]]-w, p[[1,2]]-p[[2,2]]}, r.{p[[1,1]]+w, p[[1,2]]-p[[2,2]]}}] (*horizontal bottom*)
, Line[{r.{p[[1,1]]-w, p[[1,2]]+p[[2,2]]}, r.{p[[1,1]]+w, p[[1,2]]+p[[2,2]]}}] (*horizontal bottom*)}
]


(* ::Subsubsection:: *)
(*rotatedplot4*)


(* ::Text:: *)
(*plot correlations*)
(*tp = list of points to plot*)
(*xlabelfunc = x label*)
(*ylabelfunc = y label*)
(*c = correlations row *)
(*pr = plot range*)
(*lines = bool as a function of i and j, true to plot lines*)
(*a =  rotation angle to rotate points*)
(*letter = plot label letter*)
(*is = image size*)
(*annotations = string labels on the outer edge of the frame to add context to what a small and large value means {xmin, xmax, ymin, ymax}*)
(*left = bool to shift kendall circles to the left*)
(*CALLS: fitsinrange, kendallcircles2 (kendall.wl), s8, s12, errorpoint*)
(*CALLED BY: plottplist*)


rotatedplot4[tp_, xlabel_, ylabel_, c_, pr_, lines_, a_, letter_, is_, annotations_, left_]:=Module[{ s1, ps, markers, l1, l2, textleft, arrowend, r
																										, colors, lp, pxscale, letterx, lb, lt, rb, rt},
ps = 1;
markers = {Style[\[FilledCircle], 10*ps],Style[\[EmptyCircle], 10*ps]};
colors = {Gray, Black};
r = RotationMatrix[a];
s1 = If[Length[pr]==2
		, Table[Select[tp[[k]], fitsinrange[#, pr, r]&], {k, Length[tp]}]
		, tp
	];
lp = 50;
If[Length[pr]>0,
	pxscale = (is-lp)/(pr[[1,2]]-pr[[1,1]]);
	letterx = pr[[1,1]] - lp/pxscale;
	lb = Max[pr[[1,1]], pr[[2,1]]];
	lt = Max[pr[[1,1]], -pr[[2,2]]];
	rb = -Min[pr[[1,2]], pr[[2,1]]];
	rt = -Max[-pr[[1,2]], -pr[[2,2]]];
];
Show[
	 Graphics[{
		Text[kendallcircles2[c]
			, {If[left
					, pr[[1,1]]
					, (pr[[1,2]]+pr[[1,1]])/2
				]
				, pr[[2,1]]
				}
			, If[left, {Left, Bottom}, Bottom]]
		, Text[s12[letter], {letterx, pr[[2,2]]}, {Left, Bottom}]
		, If[lines, 
			{Gray
				, Thin
				, Line[{{lb, lb}, {rt, rt}}]
				, Line[{{lt, -lt}, {rb, -rb}}]
			}
			, {}
		  ]
		, Text[s8[annotations[[1]]], {pr[[1,1]], pr[[2,2]]}, {Left, Bottom}]
		, Text[s8[annotations[[2]]], {pr[[1,2]], pr[[2,2]]}, {Right, Bottom}]
		, Text[Rotate[s8[annotations[[3]]], Pi/2], {pr[[1,2]]+5/pxscale, pr[[2,1]]}, {Left, Bottom}]
		, Text[Rotate[s8[annotations[[4]]], Pi/2], {pr[[1,2]]+5/pxscale, pr[[2,2]]}, {Left, Top}]
	}]
	, Graphics[
		Table[
			{
				colors[[k]]
				, errorpoint[markers[[k]], #, 0.005, a]&/@s1[[k]]
			}
		, {k,Length[s1]}]
		]
		, AspectRatio->1
		, Frame->True
		, FrameStyle->Black
		, FrameLabel->{xlabel, ylabel}
		, GridLines-> {{0},{0}}
		, ImageSize->is
		, ImagePadding->{{lp,15},{40, 20}}
		, LabelStyle->Directive[8*PS, Black]
		, PlotLabel->None
		, PlotRange->pr
		, PlotRangePadding->None
]
];


(* ::Subsubsection:: *)
(*plottplist*)


(* ::Text:: *)
(*plot correlations*)
(*r = {x index, y index}*)
(*xlabelfunc = x label as a function of column number*)
(*ylabelfunc = y label as a function of column number*)
(*corrtable = full correlation table*)
(*pr = plot range*)
(*lines = bool as a function of i and j, true to plot lines*)
(*a =  rotation angle *)
(*letter = plot label letter*)
(*is = image size*)
(*annotations = string labels on the outer edge of the frame to add context to what a small and large value means {xmin, xmax, ymin, ymax}*)
(*left = bool to shift kendall circles to the left*)
(*OUTPUT: one plot*)
(*CALLS: rotatedplot4*)
(*CALLED BY: plotlist, plotijgrid*)


plottplist[r_, xfunc_, yfunc_, corrtable_, pr_, lines_, a_, letter_, is_, annotations_, left_]:=Module[{i, j, tp, c},
i = r[[1]];
j = r[[2]];
tp = r[[3]];
c = SortBy[Select[corrtable, #[[1]]==If[i>j, j, i] && #[[2]]==If[i>j,i,j]&], #[[5]]&][[;;, 6;;]];
rotatedplot4[tp, xfunc[i], yfunc[j], c, pr, lines, a, letter, is, annotations, left]
]


(* ::Subsubsection:: *)
(*plotlist*)


(* ::Text:: *)
(*list of plots*)
(*tplist = {i,j, list of points} for many i, j as a function of i and j*)
(*ilist = list of x indices*)
(*j = list of y indices *)
(*linesfunc = bool as a function of i and j, true to plot lines*)
(*rotatefunc = bool as a function of i and j, true to rotate by -45 deg*)
(*plots = number of plots across on a page*)
(*labels = string labels on the outer edge of the frame to add context to what a small and large value means {xmin, xmax, ymin, ymax}*)
(*xlabelfunc = x label as a function of column number*)
(*ylabelfunc = y label as a function of column number*)
(*klist = list of kendall correlations that contains the correlation of interest*)
(*pr = plotrange as a function of i,j*)
(*ft = frame ticks as a function of plot range*)
(*OUTPUT: table of plots*)
(*CALLS: plottplist*)
(*CALLED BY: (correlations2.nb)*)


plotlist[tplist_, ilist_, jlist_, linesfunc_, rotatefunc_, plots_
				, labels_, xlabelfunc_, ylabelfunc_, klist_, pr_, ft_]:=Module[{s1, letterlist, li},
letterlist = ToUpperCase[Join[Alphabet[], (StringJoin[ConstantArray[#,2]]&/@Alphabet[])]];
li = 0;
Table[
	s1 = Select[tplist[i,j], #[[1]]==i && #[[2]]==j&];
	If[Length[s1]>0
		, li = li+1;
			Show[
				plottplist[s1[[1]]
					, xlabelfunc
					, ylabelfunc
					, klist
					, pr[i,j]
					, linesfunc[i,j]
					, rotatefunc[i,j]
					, letterlist[[li]]
					, 72*6.5/plots*PS
					, labels[i,j]
					, False
				]
			, FrameTicks->ft]
		,0
	]
,{i, ilist}, {j, jlist}]

]


(* ::Subsubsection:: *)
(*pl2grid*)


(* ::Text:: *)
(*plot a grid of plots*)
(*tab = table of plots*)
(*OUTPUT: grid of plots*)
(*CALLED BY: (correlations2.nb)*)


pl2grid[tab_]:=Grid[If[NumberQ[#], "", #]&/@#&/@tab,  Spacings->{0,0}];


(* ::Subsubsection:: *)
(*plotijgrid*)


(* ::Text:: *)
(*grid of plots*)
(*tplist = list of points to plot*)
(*ijlist = list of pairs of indices to plot*)
(*linesfunc = bool as a function of i and j, true to plot lines*)
(*rotatefunc = bool as a function of i and j, true to rotate by -45 deg*)
(*labels = string labels on the outer edge of the frame to add context to what a small and large value means {xmin, xmax, ymin, ymax}*)
(*xlabelfunc = x label as a function of column number*)
(*ylabelfunc = y label as a function of column number*)
(*klist = list of kendall correlations that contains the correlation of interest*)
(*pr = plotrange as a function of i,j*)
(*ft = frame ticks as a function of plot range*)
(*lii = initial number of the letter of the alphabet to start labeling plots (0 to start with A)*)
(*left = bool to shift kendall circles to the left*)
(*CALLS: plottplist*)
(*CALLED BY: (correlations2.nb)*)


plotijgrid[tplist_, ijlist_, linesfunc_, rotatefunc_, labels_, xlabelfunc_, ylabelfunc_
					, klist_, pr_, ft_, lii_, left_]:=Module[{s1, letterlist, li, i, j},
letterlist = ToUpperCase[Join[Alphabet[], (StringJoin[ConstantArray[#,2]]&/@Alphabet[])]];
li = lii;
Grid[{Table[
	i = ij[[1]];
	j = ij[[2]];
	s1 = Select[tplist[i,j], #[[1]]==i && #[[2]]==j&];
	If[Length[s1]>0
		, li = li+1;
			Show[plottplist[s1[[1]], xlabelfunc, ylabelfunc, klist, pr[i,j]
					, linesfunc[i,j], rotatefunc[i,j], letterlist[[li]]
					, 72*6.5/Length[ijlist]*PS, labels[i,j], left], FrameTicks->ft[i,j]]
		,""
	]
,{ij, ijlist}]}, Spacings->{0,0}]
]
