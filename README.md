# harmoniask

Exploring jazz music theory with Haskell.

### Some examples

     λ> scale C Major
     [C,D,E,F,G,A,B]
     λ> scale Fs Minor
     [Fs,Gs,A,B,Cs,Ds,F]
     λ> triad (scale D Major)
     [D,Fs,A]
     λ> triad (scale B Diminished)
     [B,D,F]
     λ> quadriad (scale C Major)
     [C,E,G,B]
     λ> scaleChords (scale C Major)
     [[C,E,G,B],[D,F,A,C],[E,G,B,D],[F,A,C,E],[G,B,D,F],[A,C,E,G],[B,D,F,A]]
     λ> ii_v_i (scale C Major)
     [[D,F,A,C],[G,B,D,F],[C,E,G,B]]
     λ> take 14 $ coltraneChanges C
     [[G,B,D,F],[C,E,G,B],[B,Ds,Fs,A],[E,Gs,B,Ds],[Ds,G,As,Cs],[Gs,C,Ds,G],[G,B,D,F],[C,E,G,B],[B,Ds,Fs,A],[E,Gs,B,Ds],[Ds,G,As,Cs],[Gs,C,Ds,G],[G,B,D,F],[C,E,G,B]]
     λ> tritoneChange $ ii_v_i (scale C Major)
     [[D,F,A,C],[Cs,F,Gs,B],[C,E,G,B]]
     λ> inversions $ triad (scale C Major)
     [[C,E,G],[E,G,C],[G,C,E]]
     λ> inversions $ quadriad (scale F Augmented)
     [[F,A,C,E],[A,C,E,F],[C,E,F,A],[E,F,A,C]]
     λ> let cs@[ii,v,i] = ii_v_i (scale C Major)
     λ> cs
     [[D,F,A,C],[G,B,D,F],[C,E,G,B]]
     λ> shortestPath ii v
     [D,F,G,B]
     λ> shortifyChords cs
     [[D,F,A,C],[D,F,G,B],[C,E,G,B]]
     λ> shortifyChords $ take 14 $ coltraneChanges C
     [[G,B,D,F],[G,B,C,E],[Fs,A,B,Ds],[E,Gs,B,Ds],[Ds,G,As,Cs],[Ds,G,Gs,C],[D,F,G,B],[C,E,G,B],[B,Ds,Fs,A],[B,Ds,E,Gs],[Cs,Ds,G,As],[C,Ds,G,Gs],[B,D,F,G],[B,C,E,G]]
