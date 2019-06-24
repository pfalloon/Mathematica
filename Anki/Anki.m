(* Wolfram Language package *)

BeginPackage["Anki`"]

ClearAll["Anki`*", "Anki`Private`*"];

Anki::usage = "Anki[] opens an Anki session"
CreateAnkiDb::usage = "CreateAnkiDb[collection, lists] initializes a set of csv files representing the Anki dataset."

Begin["`Private`"];

$ankiDir = FileNameJoin[{$HomeDirectory, "MyStuff", "Mathematica", "Data", "Anki"}];

$framedOptions = {
	Background -> LightGray,
	FrameMargins -> 30,
	RoundingRadius -> 10,
	FrameStyle-> {Thickness[3]},
	ImageSize -> {1000, 600},
	Alignment -> {Center, Center}
};

$columnOptions = {
	Spacings -> 5,
	Alignment -> Center
};

$styleOptions = {
	FontFamily -> "ComicSansMS"
};

$buttonStyle = {
	FontSize -> 16,
	FontFamily -> "Helvetica"
};

$bigSize = 100;
$smallSize = 50;
$buttonSize = 16;

$textPaneSize = {600, 200};
$textPaneOptions = {Alignment -> {Center, Center}};

$ankiDoublingCount = 4;

CreateAnkiDb[collection_, lists_] := Module[
	{collectionDir = FileNameJoin[{$ankiDir, collection}]},
	If[!DirectoryQ[collectionDir], CreateDirectory[collectionDir]];
	Do[
		Export[
			FileNameJoin[{collectionDir, First[list] <> ".csv"}],
			Table[
				{
					word,
					0, (* streak (how many times in a row correctly answered); level increases by 1 when ankiDoublingCount reached *)
					0, (* level: interval between re-tests is 2^level *)
					0  (* time-to-next: interval (how many tests) before this word appears again *)
				},
				{word, Last[list]}
			]
		],
		{list, lists}
	]
]

(* load Anki data from csv files *)
loadAnkiData[baseDir_:$ankiDir] := 
	Table[
		FileBaseName[collectionDir] -> Table[
			With[{list = Import[listFile]}, If[Length[list] > 0, FileBaseName[listFile] -> list, Nothing]],
			{listFile, FileNames["*.csv", collectionDir]}
		],
		{collectionDir, Select[FileNames["*", baseDir], DirectoryQ]}
	]

Anki[] := CreateDialog[
	DynamicModule[
		{db = loadAnkiData[], collection, list, started = False, score, testRows, nTest, currentPos, row, wrongUns},
		Row[{
			(* buttons and choosers *)
			Pane[
				Grid[
					{
						(*Button[
							Style["Add\[Ellipsis]", Sequence @@ $buttonStyle],
							None
						],*)
						{
							Style["collection: ", Sequence @@ $buttonStyle],
							If[Length[db] == 0,
								collection = -1;
								PopupMenu[0, {0 -> "(none available)"}, Enabled -> False, MenuStyle -> Prepend[$buttonStyle, Gray]],
								collection = 1;
								PopupMenu[Dynamic[collection], Table[k -> db[[k, 1]], {k, Length[db]}], Enabled -> !started, MenuStyle -> $buttonStyle]
							]
						},
						{
							Style["list: ", Sequence @@ $buttonStyle],
							Dynamic[
								If[Length[db] == 0 || Length[db[[collection, 2]]] == 0,
									list = -1;
									PopupMenu[0, {0 -> "(none available)"}, Enabled -> False, MenuStyle -> Prepend[$buttonStyle, Gray]],
									list = 1;
									PopupMenu[Dynamic[list], Table[k -> db[[collection, 2, k, 1]], {k, Length[db[[collection, 2]]]}], Enabled -> !started, MenuStyle -> $buttonStyle]
								],
								TrackedSymbols :> {collection}
							]
						}
					},
					Alignment -> {{Right, Left}, Baseline},
					Spacings -> {0, 2}
				],
				{300, 100}
			],
			(* main panel *)
			Dynamic[
				Which[
					collection == -1 || list == -1,
					Framed[
						Column[
							{
								Pane[
									Style["Add some word lists to get started!", $smallSize, Sequence @@ $styleOptions],
									$textPaneSize,
									Sequence @@ $textPaneOptions
								]
							},
							Sequence @@ $columnOptions
						],
						Sequence @@ $framedOptions
					],
					!started,
					(* initialization *)
					With[{min = Min[db[[collection, 2, list, 2, All, 2]]]},
						(* if necessary, bring forward so there are some rows to do *)
						If[min > 0, db[[collection, 2, list, 2, All, 2]] -= min]
					];
					testRows = RandomSample[Flatten[Position[db[[collection, 2, list, 2, All, 2]], 0]]];
					nTest = Length[testRows];
					currentPos = 1;
					score = 0;
					wrongUns = {};
					Framed[
						Column[
							{
								Pane[
									Style[
										ToString[nTest] <> " " <> If[nTest == 1, "item", "items"] <> " to review\[Ellipsis]",
										$smallSize,
										Sequence @@ $styleOptions
									],
									$textPaneSize,
									Sequence @@ $textPaneOptions
								],
								Column[{
									If[nTest > 30,
										Button[
											Style["Test First 20", $smallSize, Sequence @@ $styleOptions],
											testRows = testRows[[;;20]];
											nTest = 20;
											started = True
										],
										Nothing
									],
									Button[Style[If[nTest > 30, "Test Them All!", "Start Test!"], $smallSize, Sequence @@ $styleOptions], started = True]
								}]
							},
							Sequence @@ $columnOptions
						],
						Sequence @@ $framedOptions
					],
					currentPos > Length[testRows],
					Framed[
						Column[
							{
								Pane[
									Style["Done!", $bigSize, Sequence @@ $styleOptions],
									$textPaneSize,
									Sequence @@ $textPaneOptions
								],
								Style["Score: " <> ToString[score] <> " out of " <> ToString[nTest], $smallSize, Sequence @@ $styleOptions],
								Column[{
									Button[
										Style["Save Results", $smallSize, Sequence @@ $styleOptions],
										Export[FileNameJoin[{$ankiDir, db[[collection, 1]], db[[collection, 2, list, 1]] <> ".csv"}], db[[collection, 2, list, 2]]]
									],
									Button[Style["Do Another Test", $smallSize, Sequence @@ $styleOptions], started = False]
								}]
							},
							Sequence @@ $columnOptions
						],
						Sequence @@ $framedOptions
					],
					True,
					row = db[[collection, 2, list, 2, testRows[[currentPos]]]];
					Framed[
						Column[
							{
								Pane[
									Style[row[[1]], $bigSize, Sequence @@ $styleOptions],
									$textPaneSize,
									Sequence @@ $textPaneOptions
								],
								Column[{
									Button[
										Style["Nailed it \[LongDash] well done!", $smallSize, Sequence @@ $styleOptions],
										If[!MemberQ[wrongUns, row[[1]]],
											row[[2]] += 1; (* increment streak *)
											If[row[[2]] == $ankiDoublingCount, row[[2]] = 0; row[[3]] += 1]; (* increment level *)
											row[[4]] = 2^row[[3]]; (* increment time-to-next *)
											db[[collection, 2, list, 2, testRows[[currentPos]]]] = row;
											score += 1
										];
										currentPos += 1
									],
									Button[
										Style["Let's come back to that one!", $smallSize, Sequence @@ $styleOptions],
										AppendTo[testRows, testRows[[currentPos]]];
										row[[2]] = 0; (* reset streak *)
										row[[3]] = 0; (* reset level *)
										row[[4]] = 1; (* reset time-to-next *)
										db[[collection, 2, list, 2, testRows[[currentPos]]]] = row;
										AppendTo[wrongUns, row[[1]]];
										currentPos += 1
									]
								}]
							},
							Sequence @@ $columnOptions
						],
						Sequence @@ $framedOptions
					]
				],
				TrackedSymbols :> {collection, list, started, currentPos}
			]
		}]
	],
	WindowTitle -> "Papa's amazing Anki Program",
	WindowOpacity -> 0.9
]

End[]

EndPackage[]

Anki[]