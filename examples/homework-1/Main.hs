import Prelude.Reflected

import Proofs

import SequentCalc
import SequentCalc.Latex

import Text.LaTeX
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Texy

proofs :: [(Proof, FilePath)]
proofs = [ (qImpliesPOrNotP, "q-implies-p-or-not-p.tex")
         , (qImpliesPOrQ, "q-implies-p-or-q.tex")
         , (notPImpliesPImpliesQ, "not-p-implies-p-implies-q.tex")
         ]

main :: IO ()
main = mapM_ writeProofToFile proofs
  where writeProofToFile (proof, file) =
          renderFile file (texy proof :: LaTeX)

