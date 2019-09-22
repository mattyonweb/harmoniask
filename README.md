# harmoniask

Exploring jazz music theory with Haskell.

Harmoniask is a little project to embed elements of Jazz music into Haskell, such as chords, substitutions, standard harmonic sequences and so on. 

As a standalone application, it can read a text file containing sequences of chords and output it in a MIDI file, possibly using alternative tunings (eg. Pythagoric, Werckmeister...). Note however that not every MIDI player support the MTS (Midi Tuning Standard) for alternative tunings.

### How to install

	git clone
	stack build
	
### Some examples

A simple chord, both in its abstract and concrete representations: 

	λ> majTriad
	Chord [Unison,ThirdMaj,Fifth]
	
	λ> realizeChord majTriad 60 -- 60 = Middle C
	[60,64,67]

Constructing chords via combinators:

	λ> addOnTop SeventhMin majTriad 
	Chord [Unison,ThirdMaj,Fifth,SeventhMin]
	
Constructing more complex chords:

	λ> x_9b = 
		addOnTop SeventhMin |> 
		addOnTop NinthMaj   |> 
		alter NinthMaj Flat |> 
		remove Fifth
	
	λ> x_9b majTriad 
	Chord [Unison,ThirdMaj,SeventhMin,NinthMin]
	
	λ> realizeChord (x_9b majTriad) 60 
	[60,64,70,72]

Some simple `Sequence` (sequence of chords that stays inside a tonality):

	λ> v_i = Sequence [Fifth, Unison] [x_7 majTriad, x_maj7 majTriad]
	Sequence [Fifth,Unison] 
	         [Chord [Unison,ThirdMaj,Fifth,SeventhMin]
			 ,Chord [Unison,ThirdMaj,Fifth,SeventhMaj]]	
	
	λ> realizeSeq 60 v_i
	[[67,71,74,77],[60,64,67,71]] -- G7 | Cmaj7

Some `Sequence` combinators:

	λ> ii_v_i = Sequence 
	            [SecondMaj, Fifth, Unison]
                [x_7 minTriad, x_7 majTriad, x_maj7 majTriad]
	
	λ> ii_v_tritone = (removeLastSeq |>>| tritoneSub)
	λ> ii_v_tritone ii_v_i
	Sequence [SecondMaj,SecondMin] 
		     [Chord [Unison,ThirdMin,Fifth,SeventhMin], 
			  Chord [Unison,ThirdMaj,Fifth,SeventhMin]]
			  
	λ> realizeSeq 60 $ ii_v_tritone ii_v_i
	[[62,65,69,72],[61,65,68,71]] -- D-7 | Db7

Realizing a `Sequence` while trying to reduce the distance between the notes of subsequent chords: 

	λ> realizeSeqCompact 60 ii_v_i
	[[62,65,69,72], -- D-7
	 [62,65,67,71], -- G7, 2nd inversion
	 [60,64,67,71]] -- Cmaj7
	
Creating `Harmony`s, a collection of sequences in different tonalities:

	circleFifths :: Harmony
	circleFifths = Harmony intervals sequences
		where intervals = concat $ replicate 6 [Up Fourth, Down Fifth]
		sequences       = replicate 12 ii_v_i

	λ> circleFifths
	Harmony 
		[Up Fourth, Down Fifth, Up Fourth, Down Fifth...] 
		[Sequence [SecondMaj,Fifth,Unison] 
		          [Chord [Unison,ThirdMin,Fifth,SeventhMin],
				   Chord [Unison,ThirdMaj,Fifth,SeventhMin],
				   Chord [Unison,ThirdMaj,Fifth,SeventhMaj]],
	     Sequence [SecondMaj,Fifth,Unison] 
		          [Chord [Unison,ThirdMin,Fifth,SeventhMin],
				   Chord [Unison,ThirdMaj,Fifth,SeventhMin],
				   Chord [Unison,ThirdMaj,Fifth,SeventhMaj]],
	     Sequence ... ... ... ]
		 
Exporting to a Midi file:

	λ> examplePitches = realizeHarmonyWith (noteToMidi C) realizeSeqCompact circleFifths
	λ> exampleTrack = makeTrack (MidiOptions 120 Nothing Nothing) examplePitches
	λ> writeMidiFile "examples/out.mid" $ createMidiFile $ exampleTrack

[Here](examples/out.mid) is the result.
