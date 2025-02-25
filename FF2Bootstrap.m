(* ::Package:: *)

(* ::Section:: *)
(*Squared Form Factor Bootstrap*)


(* ::Subsection:: *)
(*Global*)


(* ::Subsubsection:: *)
(*Install IGraph/M*)


(*If[!MemberQ[$Packages,"IGraphM`"],If[ChoiceDialog["\:672c\:7a0b\:5e8f\:7684\:90e8\:5206\:529f\:80fd\:9700\:8981\:5b89\:88c5\:56fe\:8bba\:7a0b\:5e8f\:5305IGraph/M\n\:662f\:5426\:5b89\:88c5?",{"\:5b89\:88c5"->True,"\:53d6\:6d88"->False}],Get["https://raw.githubusercontent.com/szhorvat/IGraphM/master/IGInstaller.m"],Abort[]]]*)


(* ::Subsection:: *)
(*Public*)


BeginPackage["FF2Bootstrap`"(*,{"IGraphM`"}*)]


(* ::Subsubsection:: *)
(*Information*)


(* FF2Bootstrap version 0.0.1 by Xiang Li, 2025.2.25 *)
(* released under GNU General Public License 3 *)


FF2Bootstrap$Version={"0.0.1","2025.2.25"};


Print["Package FF2Bootstrap version ",FF2Bootstrap$Version[[1]],", ",FF2Bootstrap$Version[[2]]];


(* ::Subsubsection:: *)
(*Check definition conflict*)


With[{symbols={{Global`s,"s"},{Global`X,"X"},{Global`p,"p"},{Global`q,"q"},{Global`FF,"FF"}}},Table[Print["Warning: symbol ",sym," previously defined."],{sym,Pick[symbols[[All,2]],ValueQ[#,Method->"SymbolDefinitionsPresent"]&/@symbols[[All,1]]]}]];


(* ::Subsubsection:: *)
(*Usage*)


toList::usage="toList[head][expr]\:8fd4\:56deList@@expr, \:82e5expr\:7684\:5934\:90e8\:662fhead; \:5426\:5219\:8fd4\:56de{expr}."
varToStdRules::usage="varToStdRules[[1]]\:5bf9\:5f62\:5982s[i,j,...]\:7684Mandelstam\:53d8\:91cf\:7684\:5143\:7d20\:6392\:5e8f, varToStdRules[[2]]\:5c06\:5f62\:5982s[i,j,...]\:7684Mandelstam\:53d8\:91cf\:5c55\:5f00\:4e3as[i,j]\:4e4b\:548c."
cyclicalize::usage="cyclicalize[n][expr]\:7ed9\:51fa\:5bf9expr\:4e2d\:7684\:52a8\:529b\:5b66\:53d8\:91cf\:505acyclic\:5f97\:5230\:7684\:5217\:8868, \:9009\:9879\"Variables\"\:53ef\:4ee5\:4fee\:6539\:52a8\:529b\:5b66\:53d8\:91cf\:7684\:5339\:914d\:6a21\:5f0f(\:9ed8\:8ba4\:4e3a_s), \:9009\:9879\"Rules\"\:53ef\:4ee5\:4fee\:6539\:505acyclic\:540e\:5e94\:7528\:7684\:52a8\:529b\:5b66\:53d8\:91cf\:6b63\:89c4\:5316\:89c4\:5219(\:9ed8\:8ba4\:4e3aMost[varToStdRules])."
dihedralize::usage="dihedralize[n][expr]\:7ed9\:51fa\:5bf9expr\:4e2d\:7684\:52a8\:529b\:5b66\:53d8\:91cf\:505adihedral\:5f97\:5230\:7684\:5217\:8868, \:9009\:9879\"Variables\"\:53ef\:4ee5\:4fee\:6539\:52a8\:529b\:5b66\:53d8\:91cf\:7684\:5339\:914d\:6a21\:5f0f(\:9ed8\:8ba4\:4e3a_s), \:9009\:9879\"Rules\"\:53ef\:4ee5\:4fee\:6539\:505adihedral\:540e\:5e94\:7528\:7684\:52a8\:529b\:5b66\:53d8\:91cf\:6b63\:89c4\:5316\:89c4\:5219(\:9ed8\:8ba4\:4e3aMost[varToStdRules])."
stoXRule::usage="stoXRule[n]\:7ed9\:51faMandelstam\:53d8\:91cfs[i,...,j-1]\:5316\:4e3aplanar\:52a8\:529b\:5b66\:53d8\:91cfX[i,j]\:7684\:66ff\:6362\:89c4\:5219, \:8fd9\:91cci,j\:53d6\:503c\:8303\:56f4\:4e3a1~2n, \:89c4\:5219\:53ef\:80fd\:6709\:91cd\:590d."
XtosRule::usage="XtosRule[n]\:5316\:4e3aplanar\:52a8\:529b\:5b66\:53d8\:91cfX[i,j]\:5316\:4e3aMandelstam\:53d8\:91cfs[i,...,j-1]\:7684\:66ff\:6362\:89c4\:5219."
compatPoleQ::usage="compatPoleQ[s1,s2]\:68c0\:67e5pole s1\:4e0es2\:662f\:5426\:76f8\:5bb9, \:82e5\:76f8\:5bb9\:5219\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse."
decomposeFF2Poles::usage="decomposeFF2Poles[poles]\:5c06Form Factor\:6a21\:65b9\:7684poles\:5206\:89e3\:4e3a\:4e24\:4e2asingle copy\:4e2dpoles\:7684\:4e58\:79ef, \:7ed3\:679c\:7b2c{2}\:5c42\:7684\:5143\:7d20\:662f\:5f62\:5982{\:4e00\:4e2acopy,\:53e6\:4e00\:4e2acopy}\:7684\:5217\:8868."
decomposeFF2PolesNice::usage="decomposeFF2PolesNice[poles]\:5728decomposeFF2Poles[poles]\:6709\:552f\:4e00\:5206\:89e3\:65f6\:8fd4\:56de\:8fd9\:4e00\:5206\:89e3, \:4ee5FF[\:4e00\:4e2acopy]FF[\:53e6\:4e00\:4e2acopy]\:7684\:683c\:5f0f\:8f93\:51fa; \:5426\:5219\:7ed9\:51fa\:63d0\:793a."
FF2MSYMcutRule::usage="FF2MSYMcutRule[n][FF[\:4e00\:4e2acopy]\[Times]FF[\:53e6\:4e00\:4e2acopy]]\:68c0\:67e5\:8f93\:5165\:7684\:7ed3\:6784\:4f5c\:4e3aMSYM cut diagram\:662f\:5426\:6ca1\:6709triangle/bubble\:5b50\:56fe, \:82e5\:6ca1\:6709\:5219\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse."
FF2SubPoleRule::usage="FF2SubPoleRule[FF[\:4e00\:4e2acopy]\[Times]FF[\:53e6\:4e00\:4e2acopy]]\:68c0\:67e5\:8f93\:5165\:7684\:7ed3\:6784\:662f\:5426\:6ee1\:8db3(k-1)-pole\:9009\:62e9\:5b9a\:5219, \:5bf9\:6ee1\:8db3\:7684\:7ed3\:6784\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse."
FF2NoTripletRule::usage="FF2NoTripletRule[FF[\:4e00\:4e2acopy]\[Times]FF[\:53e6\:4e00\:4e2acopy]]\:68c0\:67e5\:8f93\:5165\:7684\:7ed3\:6784\:4f5c\:4e3aMSYM cut diagram\:662f\:5426\:6ca1\:67093\:70b9\:5b50\:56fe, \:82e5\:6ca1\:6709\:5219\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse."
drawFeynGraphPeriod::usage="drawFeynGraphPeriod[n,dens,nums]\:5bf9\:8f93\:5165\:7684n\:70b9\:7ed3\:6784\:4f5c\:56fe, \:5206\:5b50nums\:7528\:865a\:7ebf, \:5206\:6bcddens\:7528\:5b9e\:7ebf. \:82e5\:5206\:6bcd\:662f\:5f62\:5982FF[\:4e00\:4e2acopy]\[Times]FF[\:53e6\:4e00\:4e2acopy]\:7684\:7ed3\:6784, \:5219\:4f1a\:7528\:4e0d\:540c\:989c\:8272\:6807\:8bb0\:4e0d\:540ccopy. \:8f93\:5165\:7684\:5206\:5b50\:548c\:5206\:6bcd\:90fd\:5fc5\:987b\:662fplanar\:7684."
drawFeynGraphPeriodNoLabel::usage="drawFeynGraphPeriodNoLabel[n,dens,nums]\:662fdrawFeynGraphPeriod\:7684\:7b80\:5316\:7248\:672c, \:7565\:53bb\:4e86\:6807\:6ce8."
drawCutDiagram::usage="drawCutDiagram[n][FF[\:4e00\:4e2acopy]\[Times]FF[\:53e6\:4e00\:4e2acopy]]\:5bf9\:8f93\:5165\:7684n\:70b9\:7ed3\:6784\:7ed8\:51facut\:56fe, \:9ed8\:8ba4\:6807\:6ce8\:4f7f\:7528s[i,j]\:7684\:683c\:5f0f, \:53ef\:4ee5\:7528Label\:9009\:9879\:5c06\:6807\:6ce8\:6539\:4e3a\:4e0b\:6807."
toEdgeIsomorphism::usage="toEdgeIsomorphism[graph1,graph2][iso]\:5c06graph1\:4e0egraph2\:4e4b\:95f4\:4ee5\:9876\:70b9\:6620\:5c04\:5f62\:5f0f\:7ed9\:51fa\:7684\:56fe\:540c\:6784iso\:6539\:5199\:4e3a\:8fb9\:6620\:5c04\:5f62\:5f0f."
toLabeledEdgeIsomorphism::usage="toLabeledEdgeIsomorphism[graph1,graph2][iso]\:5c06graph1\:4e0egraph2\:4e4b\:95f4\:4ee5\:9876\:70b9\:6620\:5c04\:5f62\:5f0f\:7ed9\:51fa\:7684\:56fe\:540c\:6784iso\:6539\:5199\:4e3a\:5e26\:6807\:6ce8\:7684\:8fb9\:6620\:5c04\:5f62\:5f0f."
findJacobiMove::usage="findJacobiMove[graph]\:8fd4\:56degraph\:6240\:6709\:53ef\:80fd\:7684Jacobi move, \:7ed3\:679c\:7b2c{1}\:5c42\:7684\:5143\:7d20\:662f\:5f62\:5982{s-channel\:56fe->s-channel\:8fb9,t-channel\:56fe->t-channel\:8fb9,u-channel\:56fe->u-channel\:8fb9}\:7684\:5217\:8868."


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection:: *)
(*Symbol*)


With[{symbols={{s,Global`s},{X,Global`X},{p,Global`p},{q,Global`q},{FF,Global`FF}}},Table[SetDelayed@@sym,{sym,symbols}]]


(* ::Subsubsection:: *)
(*General*)


toList[head_][expr_]:=If[Head[expr]=!=head,{expr},List@@expr]


varToStdRules={s[ij__]/;!OrderedQ[{ij}]:>s@@Sort[{ij}],s[ij__]/;Length[{ij}]>2:>Total[s@@@Subsets[{ij},{2}]]};


Options[cyclicalize]={"Variables"->_s,"Rules"->Most[varToStdRules]};
cyclicList[n_Integer]:=Table[Thread[Range[n]->RotateLeft[Range[n],i]],{i,0,n-1}]
cyclicalize[n_Integer,OptionsPattern[]]:=(Table[#/.{x:OptionValue["Variables"]:>(x/.cyc/.OptionValue["Rules"])},{cyc,cyclicList[n]}]&)


Options[dihedralize]={"Variables"->_s,"Rules"->Most[varToStdRules]};
dihedralList[n_Integer]:=Join[cyclicList[n],Table[Thread[Range[n]->RotateLeft[Reverse[Range[n]],i]],{i,0,n-1}]]
dihedralize[n_Integer,OptionsPattern[]]:=(Table[#/.{x:OptionValue["Variables"]:>(x/.dih/.OptionValue["Rules"])},{dih,dihedralList[n]}]&)


stoXRule[n_Integer]:=stoXRule[n]=Select[Flatten@Table[s@@Sort[Mod[Range[i,j-1],n,1]]->X[i,j],{i,n},{j,i+2,2n-1}],DuplicateFreeQ[#[[1]]]&]
XtosRule[n_Integer]:=XtosRule[n]=Reverse/@stoXRule[n]


(* ::Subsubsection:: *)
(*Topology*)


compatPoleQ[s1_s,s2_s]:=Or[SubsetQ[s1,s2],SubsetQ[s2,s1],DisjointQ[s1,s2]]


poleToPlanarForm[n_Integer][pole_s]:=RotateLeft[pole,Position[pole,Complement[pole,Replace[i_Integer:>Mod[i+1,n,1]]/@pole][[1]],{1}][[1,1]]-1]


polesToNestedForm[n_Integer][poles_]:=With[{pre=Gather[SortBy[List@@@poleToPlanarForm[n]/@toList[Times][poles],Minus@*Length],SubsetQ]},Fold[#1/.{a___,Sequence@@#2,b___}:>{a,#2,b}&,RotateLeft[Range[n],pre[[1,1,1]]-1],Flatten[pre,1]]]


takeNestPoles[poles_]:=With[{pole$level=SplitBy[SortBy[toList[Times][poles],Length],Length]},Table[Fold[{nested,next}|->Append[nested,SelectFirst[next,SubsetQ[#,nested[[-1]]]&,Nothing]],{minpole},pole$level[[2;;]]],{minpole,pole$level[[1]]}]]


decomposeFF2Poles[poles_]:=With[{poleList=Sort[DeleteCases[toList[Times][poles],_?NumberQ]],biparts=(set|->({#,Complement[set,#]}&/@Subsets[set]))},With[{decFFs=Select[Subsets[FindClique[Graph[poleList,If[compatPoleQ@@#,#[[1]]\[UndirectedEdge]#[[2]],Nothing]&/@Subsets[poleList,{2}]],\[Infinity],All],2],Union[Flatten[#]]==poleList&]},Table[If[Length[cliques[[1]]]==1,{poles},{#[[1]]Times@@Complement[cliques[[1]],cliques[[2]]],#[[2]]Times@@Complement[cliques[[2]],cliques[[1]]]}&/@Apply[Times,biparts[Intersection[cliques[[1]],cliques[[2]]]],{2}]],{cliques,decFFs}]]]
decomposeFF2PolesNice[poles_]:=With[{decFF=Apply[Times,Map[FF,decomposeFF2Poles[poles],{3}],{2}]},If[Length[decFF]>1,Print["Exception"],If[Length[decFF[[1]]]>1,Print["Subtlety"],decFF[[1,1]]]]]


FF2MSYMcutRule[n_Integer][FF_[poles1_Times]FF_[poles2_Times]]:=With[{nest1=polesToNestedForm[n][poles1],nest2=polesToNestedForm[n][poles2],interCount=({Length[Intersection[#1,#2[[1]]]],Length[Intersection[#1,#2[[2]]]]}&)},
With[{subtree1=Cases[nest1,a_List/;MemberQ[a,_Integer]:>{Cases[a,_Integer],Cases[a,_Integer,{2}]},\[Infinity]],subtree2=Cases[nest2,a_List/;MemberQ[a,_Integer]:>{Cases[a,_Integer],Cases[a,_Integer,{2}]},\[Infinity]]},
AllTrue[Select[subtree1[[All,1]],Length[#]>=2&],node|->AllTrue[subtree2,MatchQ[interCount[node,#],{0,1}|{1,0}|{0,0}]&]]&&AllTrue[Select[subtree2[[All,1]],Length[#]>=2&],node|->AllTrue[subtree1,MatchQ[interCount[node,#],{0,1}|{1,0}|{0,0}]&]]]]
FF2MSYMcutRule[n_Integer][graph_Graph]:=AllTrue[FindCycle[graph,{2,3},All],Length[#]>2&&(!FreeQ[#,"sL"@@Range[n]|"sR"@@Range[n]])&]


FF2SubPoleRule[FF_[poles1_]FF_[poles2_]]:=With[{forbidL=Flatten[Subsets[#,{Length[#]-1}]&/@Select[toList[Times][poles2],Length[#]>2&]],forbidR=Flatten[Subsets[#,{Length[#]-1}]&/@Select[toList[Times][poles1],Length[#]>2&]]},
FreeQ[toList[Times][poles1],Alternatives@@forbidL,{1}]&&FreeQ[toList[Times][poles2],Alternatives@@forbidR,{1}]]


FF2NoTripletRule[FF_[poles1_]FF_[poles2_]]:=With[{list1=toList[Times][poles1],list2=toList[Times][poles2]},
DisjointQ[Join[list1,list2],Flatten[Outer[If[DisjointQ[#1,#2],Union[#1,#2],Nothing]&,list1,list2]]]&&FF2SubPoleRule[FF[poles1]FF[poles2]]]


(* ::Subsubsection:: *)
(*Diagram*)


drawFeynGraphPeriod[n_Integer,poles_,nums_:1]:=With[{nn=2n,circleLayout=({r,m,\[Theta],del}|->Delete[Table[{r Cos[2\[Pi] i/m+\[Theta]],-r Sin[2\[Pi] i/m+\[Theta]]},{i,0,m-1}],del]),$xxs=toList[Times][poles/.stoXRule[n]],$nums=DeleteCases[toList[Times][nums/.stoXRule[n]],_Integer]/.Power[a_,k_]:>Sequence@@Table[a,k]},
Graph[Table[Style[i,Gray],{i,nn}],Join[Table[Style[i\[UndirectedEdge]Mod[i+1,nn,1],Gray],{i,nn}],Table[Labeled[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Blue],Placed[Subscript[X,Sequence@@xx],1/2]],{xx,$xxs}],Table[Labeled[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Red,Dashed],Placed[Style[Subscript[X,Sequence@@xx],Magenta],1/2]],{xx,$nums}]],VertexLabels->Table[i->Placed[If[i<=n,i,SuperPlus[Mod[i,n,1]]],Switch[Floor[4(i-1)/nn+1/2],0,Before,1,Above,2,After,_,Below]],{i,nn}],VertexCoordinates->circleLayout[1,nn,\[Pi],{}]]]/;!MatchQ[poles,FF_[poles1_Times]FF_[poles2_Times]]


drawFeynGraphPeriodNoLabel[n_Integer,poles_,nums_:1]:=With[{nn=2n,circleLayout=({r,m,\[Theta],del}|->Delete[Table[{r Cos[2\[Pi] i/m+\[Theta]],-r Sin[2\[Pi] i/m+\[Theta]]},{i,0,m-1}],del]),$xxs=toList[Times][poles/.stoXRule[n]],$nums=DeleteCases[toList[Times][nums/.stoXRule[n]],_Integer]/.Power[a_,k_]:>Sequence@@Table[a,k]},
Graph[Table[Style[i,Gray],{i,nn}],Join[Table[Style[i\[UndirectedEdge]Mod[i+1,nn,1],Gray],{i,nn}],Table[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Blue],{xx,$xxs}],Table[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Red,Dashed],{xx,$nums}]],VertexLabels->Table[i->Placed[If[i<=n,i,SuperPlus[Mod[i,n,1]]],Switch[Floor[4(i-1)/nn+1/2],0,Before,1,Above,2,After,_,Below]],{i,nn}],VertexCoordinates->circleLayout[1,nn,\[Pi],{}]]]/;!MatchQ[poles,FF_[poles1_Times]FF_[poles2_Times]]


drawFeynGraphPeriod[n_Integer,FF_[poles1_Times]FF_[poles2_Times],nums_:1]:=With[{nn=2n,circleLayout=({r,m,\[Theta],del}|->Delete[Table[{r Cos[2\[Pi] i/m+\[Theta]],-r Sin[2\[Pi] i/m+\[Theta]]},{i,0,m-1}],del]),$xx1s=toList[Times][poles1/.stoXRule[n]],$xx2s=toList[Times][poles2/.stoXRule[n]],$nums=DeleteCases[toList[Times][nums/.stoXRule[n]],_Integer]/.Power[a_,k_]:>Sequence@@Table[a,k]},
Graph[Table[Style[i,Gray],{i,nn}],Join[Table[Style[i\[UndirectedEdge]Mod[i+1,nn,1],Gray],{i,nn}],Table[Labeled[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Blue],Placed[Subscript[X,Sequence@@xx],1/2]],{xx,$xx1s}],Table[Labeled[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Purple],Placed[Subscript[X,Sequence@@xx],1/2]],{xx,$xx2s}],Table[Labeled[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Red,Dashed],Placed[Style[Subscript[X,Sequence@@xx],Magenta],1/2]],{xx,$nums}]],VertexLabels->Table[i->Placed[If[i<=n,i,SuperPlus[Mod[i,n,1]]],Switch[Floor[4(i-1)/nn+1/2],0,Before,1,Above,2,After,_,Below]],{i,nn}],VertexCoordinates->circleLayout[1,nn,\[Pi],{}]]]


drawFeynGraphPeriodNoLabel[n_Integer,FF_[poles1_Times]FF_[poles2_Times],nums_:1]:=With[{nn=2n,circleLayout=({r,m,\[Theta],del}|->Delete[Table[{r Cos[2\[Pi] i/m+\[Theta]],-r Sin[2\[Pi] i/m+\[Theta]]},{i,0,m-1}],del]),$xx1s=toList[Times][poles1/.stoXRule[n]],$xx2s=toList[Times][poles2/.stoXRule[n]],$nums=DeleteCases[toList[Times][nums/.stoXRule[n]],_Integer]/.Power[a_,k_]:>Sequence@@Table[a,k]},
Graph[Table[Style[i,Gray],{i,nn}],Join[Table[Style[i\[UndirectedEdge]Mod[i+1,nn,1],Gray],{i,nn}],Table[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Blue],{xx,$xx1s}],Table[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Purple],{xx,$xx2s}],Table[Style[xx[[1]]\[UndirectedEdge]xx[[2]],Red,Dashed],{xx,$nums}]],VertexLabels->Table[i->Placed[If[i<=n,i,SuperPlus[Mod[i,n,1]]],Switch[Floor[4(i-1)/nn+1/2],0,Before,1,Above,2,After,_,Below]],{i,nn}],VertexCoordinates->circleLayout[1,nn,\[Pi],{}]]]


Options[drawCutDiagram]={"Label"->Automatic,"Format"->Automatic,GraphLayout->"TutteEmbedding",EdgeShapeFunction->{{"Arrow","ArrowSize"->1/30}},ImageSize->300};
drawCutDiagram[n_Integer,opts:OptionsPattern[]][FF_[poles1_]FF_[poles2_]]:=Module[{edges1,edges2,edges,orderV,label$rule,result,sL="sL",sR="sR"},edges1=Table[seq[[$i]]\[UndirectedEdge]seq[[$i+1]],{seq,takeNestPoles[Times@@(s/@Range[n])poles1 s@@Range[n]]},{$i,Length[seq]-1}];edges2=Table[seq[[$i]]\[UndirectedEdge]seq[[$i+1]],{seq,takeNestPoles[Times@@(s/@Range[n])poles2 s@@Range[n]]},{$i,Length[seq]-1}];orderV=ReplaceAll[{ij_sL:>-Length[ij],ij_sR:>Length[ij]}];
label$rule=If[OptionValue["Label"]=!=Automatic,{s[ij__]:>OptionValue["Label"][s,ij],p[i_]:>OptionValue["Label"][p,i]},{}];edges=Union@Flatten[{edges1/.s->sL,edges2/.s->sR}]/.{a_\[UndirectedEdge]b_/;Length[a]>1:>Labeled[a\[UndirectedEdge]b,a/.{sL->s,sR->s}]}//.{a___,sL[i_]\[UndirectedEdge]thisL_,b___,sR[i_]\[UndirectedEdge]thisR_,c___}:>{Labeled[thisL\[UndirectedEdge]thisR,p[i]],a,b,c};
result=EdgeTaggedGraph[SortBy[DeleteDuplicates[Level[edges[[All,1]],{2}]],orderV],edges/.{a_\[UndirectedEdge]b_/;!OrderedQ[orderV[{a,b}]]:>b\[UndirectedEdge]a}/.label$rule,VertexLabels->{sL@@Range[n]->\!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*SuperscriptBox[\\(q\\),\\(2\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\),sR@@Range[n]->\!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*SuperscriptBox[\\(q\\),\\(2\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)},VertexStyle->{sL@@Range[n]->Red,sR@@Range[n]->Red},Sequence@@FilterRules[Join[{opts},Options[drawCutDiagram]],Options[Graph]]];
Switch[OptionValue["Format"],Automatic,result,_,{result,"VertexColors"-><|sL@@Range[n]->1,sR@@Range[n]->1|>}]]


(* ::Subsubsection:: *)
(*Graphical*)


Options[toEdgeIsomorphism]={"Nontrivial"->False};
toEdgeIsomorphism[graph1_Graph,graph2_Graph,OptionsPattern[]][iso_]:=With[{edges1=EdgeList[graph1],edges2=EdgeList[graph2]},
Head[iso]@@If[TrueQ@OptionValue["Nontrivial"],DeleteCases[#,Rule[a_,a_]]&,Identity][Thread[edges1->(edges1/.iso/.Thread[(edges2/.UndirectedEdge[a_,b_,c___]:>UndirectedEdge[b,a,c])->edges2])]]]


Options[toLabeledEdgeIsomorphism]={"Nontrivial"->False,"Direction"->True};
toLabeledEdgeIsomorphism[graph1_Graph,graph2_Graph,OptionsPattern[]][iso_]:=With[{edges1=AnnotationValue[graph1,EdgeLabels],edges2=AnnotationValue[graph2,EdgeLabels]},
Head[iso]@@If[TrueQ@OptionValue["Nontrivial"],DeleteCases[#,Rule[a_,a_]]&,Identity][Table[edge1->SelectFirst[edges2,MatchQ[#[[1]],edge1[[1]]/.iso]&,MapAt[If[TrueQ@OptionValue["Direction"],Minus,Identity],SelectFirst[edges2,MatchQ[#[[1]],SubsetMap[Reverse,edge1[[1]],{1,2}]/.iso]&,Return[Print["EdgeLabel not found."]]],2]],{edge1,edges1}]]]


findJacobiMove[graph_Graph]:=Module[{vertices,edges,schannels,jacobi$move},{vertices,edges}={VertexList[graph],EdgeList[graph]};
If[!AllTrue[Map[Position[vertices,#]&,edges[[All,;;2]],{2}],OrderedQ],Print["Warning: Unsorted Graph."]];
schannels=Select[{#,DeleteCases[IncidenceList[graph,#[[1]]],#],DeleteCases[IncidenceList[graph,#[[2]]],#]}&/@edges,Length[#[[2]]]==Length[#[[3]]]==2&];
schannels=Table[Join[channel,DeleteCases[{Level[channel[[2,All,;;2]],{2}],Level[channel[[3,All,;;2]],{2}]},Alternatives@@channel[[1]],{2}]],{channel,schannels}];
jacobi$move=(channel|->Map[SortBy[(Position[vertices,#,{1}]/.{}->{{\[Infinity]}})&],{Flatten[channel[[;;3]]],Join[{channel[[1]]},Thread[channel[[4;;,1]]\[UndirectedEdge]channel[[1,1]]],Thread[channel[[1,2]]\[UndirectedEdge]channel[[4;;,2]]]],Join[{channel[[1]]},Thread[Extract[channel,{{4,1},{5,2}}]\[UndirectedEdge]channel[[1,1]]],Thread[channel[[1,2]]\[UndirectedEdge]Extract[channel,{{4,2},{5,1}}]]]},{2}]);
Table[Table[With[{result=If[Flatten[channel[[;;3]]]===newedges,graph,EdgeAdd[EdgeDelete[graph,Flatten[channel[[;;3]]]],newedges]]},Fold[Annotate,result,{GraphLayout->If[PlanarGraphQ[result],"TutteEmbedding","SpringElectricalEmbedding"],VertexLabels->"Name"}]]->newedges,{newedges,jacobi$move[channel]}],{channel,schannels}]]


(* ::Subsubsection:: *)
(*End Package*)


End[]
EndPackage[]
