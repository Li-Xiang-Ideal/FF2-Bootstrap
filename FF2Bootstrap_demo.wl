(* ::Package:: *)

(* ::Section:: *)
(*FF2Bootstrap*)


(* ::Subsection:: *)
(*Load Package*)


SetDirectory[NotebookDirectory[]];
<<FF2Bootstrap`


(* ::Subsection:: *)
(*Demo*)


(* ::Text:: *)
(*toList[head][expr]\:8fd4\:56deList@@expr, \:82e5expr\:7684\:5934\:90e8\:662fhead; \:5426\:5219\:8fd4\:56de{expr}.*)


toList[Plus][a+b+c]


toList[Plus][a]


(* ::Text:: *)
(*varToStdRules[[1]]\:5bf9\:5f62\:5982s[i,j,...]\:7684Mandelstam\:53d8\:91cf\:7684\:5143\:7d20\:6392\:5e8f, varToStdRules[[2]]\:5c06\:5f62\:5982s[i,j,...]\:7684Mandelstam\:53d8\:91cf\:5c55\:5f00\:4e3as[i,j]\:4e4b\:548c.*)


s[1,3,2,4,5]/.varToStdRules[[1]]


s[1,2,3,4,5]/.varToStdRules[[2]]


(* ::Text:: *)
(*cyclicalize[n][seed]\:7ed9\:51faseed\:505acyclic\:5f97\:5230\:7684n\:4e2a\:8868\:8fbe\:5f0f\:7684\:5217\:8868, \:5176\:4e2dseed\:4f4d\:4e8e\:5217\:8868\:7684\:7b2c\:4e00\:4f4d.*)


cyclicalize[6][s[1,2]s[1,2,3]]


(* ::Text:: *)
(*dihedralize[n][seed]\:7ed9\:51faseed\:505adihedral\:5f97\:5230\:76842n\:4e2a\:8868\:8fbe\:5f0f\:7684\:5217\:8868, \:5176\:4e2dseed\:4f4d\:4e8e\:5217\:8868\:7684\:7b2c\:4e00\:4f4d.*)


dihedralize[6][s[1,2]s[1,2,3]]


(* ::Text:: *)
(*stoXRule[n]\:7ed9\:51faMandelstam\:53d8\:91cfs[i,...,j-1]\:5316\:4e3aplanar\:52a8\:529b\:5b66\:53d8\:91cfX[i,j]\:7684\:66ff\:6362\:89c4\:5219, \:8fd9\:91cci,j\:53d6\:503c\:8303\:56f4\:4e3a1~2n, \:89c4\:5219\:53ef\:80fd\:6709\:91cd\:590d.*)


stoXRule[5]


(* ::Text:: *)
(*XtosRule[n]\:7ed9\:51faplanar\:52a8\:529b\:5b66\:53d8\:91cfX[i,j]\:5316\:4e3aMandelstam\:53d8\:91cfs[i,...,j-1]\:7684\:66ff\:6362\:89c4\:5219.*)


XtosRule[5]


(* ::Text:: *)
(*compatPoleQ[s1,s2]\:7ed9\:51faTrue\:5f53pole s1\:4e0es2\:76f8\:5bb9, \:53cd\:4e4b\:7ed9\:51faFalse.*)


{compatPoleQ[s[1,2,3,4],s[1,2,3]],compatPoleQ[s[1,2,3,4],s[2,3,4,5]]}


(* ::Text:: *)
(*decomposeFF2Poles[poles]\:5c06FormFactor\:6a21\:65b9\:7684poles\:5206\:89e3\:4e3a\:4e24\:4e2asingle copy\:4e2dpoles\:7684\:4e58\:79ef, \:7ed3\:679c\:7b2c{2}\:5c42\:7684\:5143\:7d20\:662f\:5f62\:5982{\:4e00\:4e2acopy,\:53e6\:4e00\:4e2acopy}\:7684\:5217\:8868.*)


decomposeFF2Poles[s[1,2] s[4,5] s[1,2,3] s[3,4,5] s[1,2,3,4]]


(* ::Text:: *)
(*\:5982\:679cdecomposeFF2Poles[poles]\:6709\:552f\:4e00\:5206\:89e3, decomposeFF2PolesNice\:7ed9\:51fa\:8fd9\:4e00\:5206\:89e3, \:4ee5FF[\:4e00\:4e2acopy]FF[\:53e6\:4e00\:4e2acopy]\:7684\:683c\:5f0f\:8f93\:51fa; \:5426\:5219\:7ed9\:51fa\:63d0\:793a.*)


decomposeFF2PolesNice[s[1,2] s[1,2,3] s[1,2,3,4]s[3,4] s[3,4,5] s[1,3,4,5]]


decomposeFF2PolesNice[s[1,2] s[4,5] s[1,2,3] s[3,4,5] s[1,2,3,4]]


(* ::Text:: *)
(*FF2MSYMcutRule[n][FF[\:4e00\:4e2acopy]*FF[\:53e6\:4e00\:4e2acopy]]\:68c0\:67e5\:8f93\:5165\:7684\:7ed3\:6784\:4f5c\:4e3aMSYM cut diagram\:662f\:5426\:6ca1\:6709triangle/bubble\:5b50\:56fe, \:82e5\:6ca1\:6709\:5219\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse.*)


FF2MSYMcutRule[5][FF[s[1,2] s[1,2,3] s[1,2,3,4]] FF[s[4,5] s[3,4,5] s[1,3,4,5]]]


FF2MSYMcutRule[5][FF[s[1,2] s[1,2,3] s[1,2,3,4]] FF[s[3,4] s[3,4,5] s[1,3,4,5]]]


FF2MSYMcutRule[9][FF[s[1,2]s[1,2,3]s[4,5]s[6,7]s[4,5,6,7]s[4,5,6,7,8]s[4,5,6,7,8,9]]FF[s[3,4]s[2,3,4]s[2,3,4,5]s[1,2,3,4,5]s[7,8]s[7,8,9]s[6,7,8,9]]]


(* ::Text:: *)
(*FF2MSYMcutRule[n][graph]\:68c0\:67e5\:8f93\:5165\:7684\:56fe\:662f\:5426\:6ca1\:6709triangle/bubble\:5b50\:56fe, \:82e5\:6ca1\:6709\:5219\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse.*)
(*\:8fd9\:91ccn\:7684\:610f\:4e49\:5728\:4e8e\:5c06\:540d\:4e3a"sL"[1,...,n]\:548c"sR"[1,...,n]\:7684\:9876\:70b9\:89c6\:4e3aoperator, \:5141\:8bb8\:5176\:51fa\:73b0\:5728triangle\:5b50\:56fe\:4e2d(\:4f46\:4e0d\:5141\:8bb8\:51fa\:73b0\:5728bubble\:5b50\:56fe\:4e2d).*)
(*\:82e5\:8981\:68c0\:67e5\:7684\:56fe\:4e0d\:662fForm Factor\:7684cut diagram, \:5219\:8f93\:5165\:4efb\:610fn\:90fd\:662f\:7b49\:4ef7\:7684.*)


FF2MSYMcutRule[0][Echo[PetersenGraph[5,1,ImageSize->Small]]]


FF2MSYMcutRule[0][Echo[CompleteGraph[5,ImageSize->Small]]]


(* ::Text:: *)
(*FF2SubPoleRule[FF[\:4e00\:4e2acopy]*FF[\:53e6\:4e00\:4e2acopy]]\:68c0\:67e5\:8f93\:5165\:7684\:7ed3\:6784\:662f\:5426\:6ee1\:8db3(k-1)-pole\:9009\:62e9\:5b9a\:5219, \:5bf9\:6ee1\:8db3\:7684\:7ed3\:6784\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse.*)


FF2SubPoleRule[FF[s[1,2] s[1,2,3] s[1,2,3,4]] FF[s[3,4] s[3,4,5] s[1,3,4,5]]]


FF2SubPoleRule[FF[s[1,2] s[4,5] s[1,2,3]] FF[s[3,4] s[2,3,4] s[1,2,3,4]]]


(* ::Text:: *)
(*FF2NoTripletRule[FF[\:4e00\:4e2acopy]*FF[\:53e6\:4e00\:4e2acopy]]\:68c0\:67e5\:8f93\:5165\:7684\:7ed3\:6784\:4f5c\:4e3aMSYM cut diagram\:662f\:5426\:6ca1\:67093\:70b9\:5b50\:56fe, \:82e5\:6ca1\:6709\:5219\:8fd4\:56deTrue, \:5426\:5219\:8fd4\:56deFalse.*)


FF2NoTripletRule[FF[s[1,2] s[1,2,3] s[1,2,3,4]] FF[s[4,5] s[3,4,5] s[1,3,4,5]]]


FF2NoTripletRule[FF[s[1,2]s[1,2,3]s[4,5]s[6,7]s[4,5,6,7]s[4,5,6,7,8]s[4,5,6,7,8,9]]FF[s[3,4]s[2,3,4]s[2,3,4,5]s[1,2,3,4,5]s[7,8]s[7,8,9]s[6,7,8,9]]]


(* ::Text:: *)
(*drawFeynGraphPeriod[n,dens,nums]\:5bf9\:8f93\:5165\:7684n\:70b9\:7ed3\:6784\:4f5c\:56fe, \:5206\:5b50nums\:7528\:865a\:7ebf, \:5206\:6bcddens\:7528\:5b9e\:7ebf\:3002\:82e5\:5206\:6bcd\:662f\:5f62\:5982FF[\:4e00\:4e2acopy]*FF[\:53e6\:4e00\:4e2acopy]\:7684\:7ed3\:6784, \:5219\:7528\:4e0d\:540c\:989c\:8272\:6807\:8bb0\:4e0d\:540ccopy.*)
(*\:6ce8\:610f\:5206\:5b50\:548c\:5206\:6bcd\:90fd\:5fc5\:987b\:662fplanar pole.*)


drawFeynGraphPeriod[5,s[1,2] s[4,5] s[1,2,3] s[3,4,5] s[1,2,3,4]](* \:5206\:5b50\:9ed8\:8ba4\:4e3a1 *)


drawFeynGraphPeriod[5,FF[s[1,2] s[4,5] s[1,2,3]] FF[s[3,4] s[2,3,4] s[1,2,3,4]],s[2,3]s[1,4,5]]


(* ::Text:: *)
(*drawFeynGraphPeriodNoLabel[n,dens,nums]\:662fdrawFeynGraphPeriod\:7684\:7b80\:5316\:7248\:672c, \:7565\:53bb\:4e86\:6807\:6ce8.*)


drawFeynGraphPeriodNoLabel[5,FF[s[1,2] s[1,2,3] s[1,2,3,4]] FF[s[3,4] s[3,4,5] s[1,3,4,5]],s[2,3]s[2,3,4]]


(* ::Text:: *)
(*drawCutDiagram[n][FF[\:4e00\:4e2acopy]*FF[\:53e6\:4e00\:4e2acopy]]\:5bf9\:8f93\:5165\:7684n\:70b9\:7ed3\:6784\:7ed8\:51facut\:56fe, \:9ed8\:8ba4\:6807\:6ce8\:4f7f\:7528s[i,j]\:7684\:683c\:5f0f, \:53ef\:4ee5\:7528Label\:9009\:9879\:5c06\:6807\:6ce8\:6539\:4e3a\:4e0b\:6807.*)
(*drawCutDiagram\:7ed8\:51fa\:7684cut\:56fe\:662f\:65e0\:5411\:56fe, \:5c3d\:7ba1\:56fe\:4e2d\:7684\:8fb9\:4ee5\:7bad\:5934\:5f62\:5f0f\:7ed8\:51fa\:4ee5\:6307\:5b9a\:52a8\:91cf\:7684\:6807\:6ce8\:6d41\:5411.*)


drawCutDiagram[6][FF[s[1,2,3,4,5]s[1,2,3,4]s[2,3,4]s[2,3]]FF[s[1,2]s[1,2,6]s[4,5]s[3,4,5]]]


drawCutDiagram[6,"Label"->Subscript,GraphLayout->"SpringElectricalEmbedding"][FF[s[1,2,3,4,5]s[1,2,3,4]s[2,3,4]s[2,3]]FF[s[1,2]s[1,2,6]s[4,5]s[3,4,5]]]


(* ::Text:: *)
(*toEdgeIsomorphism[graph1,graph2][isomorphism]\:5c06graph1\:4e0egraph2\:4e4b\:95f4\:4ee5\:9876\:70b9\:6620\:5c04\:5f62\:5f0f\:7ed9\:51fa\:7684\:56fe\:540c\:6784isomorphism\:6539\:5199\:4e3a\:8fb9\:6620\:5c04\:5f62\:5f0f.*)


toEdgeIsomorphism[Graph[{1\[UndirectedEdge]2,2\[UndirectedEdge]3,3\[UndirectedEdge]1}],Graph[{1\[UndirectedEdge]2,2\[UndirectedEdge]3,3\[UndirectedEdge]1}]][<|1->2,2->3,3->1|>]


(* ::Text:: *)
(*toLabeledEdgeIsomorphism[graph1,graph2][isomorphism]\:5c06graph1\:4e0egraph2\:4e4b\:95f4\:4ee5\:9876\:70b9\:6620\:5c04\:5f62\:5f0f\:7ed9\:51fa\:7684\:56fe\:540c\:6784isomorphism\:6539\:5199\:4e3a\:5e26\:6807\:6ce8\:7684\:8fb9\:6620\:5c04\:5f62\:5f0f.*)


toLabeledEdgeIsomorphism[Graph[{Labeled[1\[UndirectedEdge]2,a],Labeled[2\[UndirectedEdge]3,b],Labeled[3\[UndirectedEdge]1,a]}],Graph[{Labeled[1\[UndirectedEdge]2,a],Labeled[2\[UndirectedEdge]3,b],Labeled[3\[UndirectedEdge]1,a]}]][<|1->2,2->3,3->1|>]


(* ::Text:: *)
(*findJacobiMove[graph]\:8fd4\:56degraph\:6240\:6709\:53ef\:80fd\:7684Jacobi move, \:7ed3\:679c\:7b2c{1}\:5c42\:7684\:5143\:7d20\:662f\:5f62\:5982{s-channel\:56fe->s-channel\:8fb9,t-channel\:56fe->t-channel\:8fb9,u-channel\:56fe->u-channel\:8fb9}\:7684\:5217\:8868.*)
(*graph\:4e2d\:6240\:6709\:7684\:8fb9\:90fd\:5e94\:5f53\:662f\:6709\:5e8f\:7684, \:5373, \:82e5\:5b58\:5728\:8fb9a\[UndirectedEdge]b, \:5219a\:5728VertexList[graph]\:4e2d\:7684\:4f4d\:7f6e\:5e94\:5f53\:5728b\:4e4b\:524d.*)


EchoFunction[First][findJacobiMove[Echo[PetersenGraph[5,1,GraphLayout->"TutteEmbedding",ImageSize->Small]]]];
%/.Rule->HighlightGraph


(* ::Subsection:: *)
(*Applications*)


(* ::Text:: *)
(*\:4ee5\:4e0b\:4ee3\:7801\:5c55\:793a\:4e86\:5bf95\:70b9Form Factor\:6a21\:65b9\:7684\:4e00\:4e2a\:7b80\:5355\:5e94\:7528*)


(* ::Text:: *)
(*\:751f\:62105\:70b9Form Factor\:7684single copy\:7684\:62d3\:6251\:7684seed*)


FF2$ToposSeed[n_Integer]:=With[{k=n-2,poles=Sort[DeleteCases[Flatten@Table[s@@Range[i,j],{i,n-1},{j,i+1,n}],s@@Range[n]]]},DeleteDuplicatesBy[Nest[Union@*Map[term|->Sequence@@Table[term pole,
{pole,Select[Complement[poles,toList[Times][term]],this|->AllTrue[toList[Times][term],compatPoleQ[#,this]&]]}]],poles,k-1],Union@*dihedralize[n]]]


EchoFunction[Length][FF2$ToposSeed[5]]


(* ::Text:: *)
(*\:751f\:62105\:70b9Form Factor\:6a21\:65b9\:7684\:62d3\:6251, \:7ed3\:679c\:7b2c{2}\:5c42\:4e2d\:7684\:5143\:7d20\:4e92\:76f8dihedral\:7b49\:4ef7*)


FF2$ToposDih[n_Integer]:=With[{seed=FF2$ToposSeed[n],replaceFF=ReplaceAll[FF->Identity]},With[{unionFF=(SortBy[replaceFF]@*DeleteDuplicatesBy[replaceFF]),
raw=Select[Flatten@KroneckerProduct[FF/@seed,FF/@Flatten[dihedralize[n][seed]]],FreeQ[_Power]@*replaceFF]},unionFF[(unionFF@*dihedralize[n])/@(1/raw)]]]


EchoFunction[Length][FF2$ToposDih[5]]


(* ::Text:: *)
(*\:7ed9\:51fa5\:70b9Form Factor\:6a21\:65b9\:6ee1\:8db3\:9009\:62e9\:5b9a\:5219\:7684\:62d3\:6251*)


FF2$ToposSelected[n_Integer]:=FF2$ToposSelected[n]=Select[FF2$ToposDih[5],FF2SubPoleRule[1/#[[1]]]&&FF2MSYMcutRule[5][1/#[[1]]]&]


EchoFunction[Length][FF2$ToposSelected[5]]


(* ::Text:: *)
(*\:5bf95\:70b9Form Factor\:6a21\:65b9\:6ee1\:8db3\:9009\:62e9\:5b9a\:5219\:7684\:3001dihedral\:4e0d\:7b49\:4ef7\:7684\:62d3\:6251\:7ed8\:51facut\:56fe; \:5e76\:57fa\:4e8e\:6b64\:5217\:51famaster\:56fe.*)


{drawCutDiagram[5][1/#],#}&/@FF2$ToposSelected[5][[All,1]];
FF2$ToposFromCutData[5]=ReverseSortBy[Gather[%,IsomorphicGraphQ[#1[[1]],#2[[1]]]&],First@*First];
EchoFunction[Length][FF2$ToposFromCutData[5][[All,1,1]]]


(* ::Text:: *)
(*IsomorphicGraphQ\:53ef\:4ee5\:5224\:65ad\:4e24\:4e2a\:56fe\:662f\:5426\:540c\:6784:*)


FF2$ToposFromCutData[5][[1,{1,2},1]]
IsomorphicGraphQ@@%


FF2$ToposFromCutData[5][[{1,2},1,1]]
IsomorphicGraphQ@@%


(* ::Text:: *)
(*FindGraphIsomorphism\:53ef\:4ee5\:7ed9\:51fa\:4e24\:4e2a\:7b49\:4ef7\:56fe\:4e4b\:95f4\:7684\:5177\:4f53\:540c\:6784, \:7528\:9876\:70b9\:95f4\:6620\:5c04\:7684\:5f62\:5f0f\:7ed9\:51fa:*)


FF2$ToposFromCutData[5][[1,{1,2},1]]
EchoFunction[Length][FindGraphIsomorphism[Sequence@@%,All]](* All\:8868\:793a\:7ed9\:51fa\:6240\:6709\:540c\:6784 *)


(* ::Text:: *)
(*\:5229\:7528FF2Bootstrap\:63d0\:4f9b\:7684\:51fd\:6570, \:53ef\:4ee5\:5c06\:4e0a\:8ff0\:540c\:6784\:6539\:5199\:4e3a\:66f4\:6709\:7528\:7684\:5f62\:5f0f:*)


toEdgeIsomorphism[Sequence@@FF2$ToposFromCutData[5][[1,{1,2},1]]]/@%
toLabeledEdgeIsomorphism[Sequence@@FF2$ToposFromCutData[5][[1,{1,2},1]]]/@%%


(* ::Text:: *)
(*\:4ece\:6539\:5199\:540e\:7684\:540c\:6784\:4e2d\:63d0\:53d6\:52a8\:91cf\:4e4b\:95f4\:7684\:5bf9\:5e94\:5173\:7cfb, \:5e76\:6c42\:89e3\:4e4b:*)


Normal[%][[All,All,All,2]]/.s[ij__]:>Total[p/@{ij}]
Table[Solve[MapAt[ReplaceAll[p->pNEW],rules,{All,2}]/.Rule->Equal][[1]],{rules,%}]


(* ::Text:: *)
(*\:9a8c\:8bc1\:56fe\:540c\:6784\:5c06q\:53d8\:6362\:4e3a\[PlusMinus]q(\:53d6\:51b3\:4e8e\:540c\:6784\:662f\:5426\:4ea4\:6362\:4e24\:4e2aoperator):*)


Expand[Total[pNEW/@Range[5]]/.%]
