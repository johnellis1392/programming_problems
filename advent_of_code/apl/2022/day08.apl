input_filename←'input.test.txt'
input←⍎¨⊃⎕NGET input_filename 1

⍝ Check visibility for each row of matrix
visible←{↑((⊢≥⌈\)∨(⊢≥⌽∘⌈\∘⌽))¨∘↓⍵}

⍝ Check visibility for rows & columns
visible2←{(visible ⍵)∨(⍉∘visible∘⍉⍵)}input

⍝ Refactor
visible3←{↑((⊢≥⌈\)∨(⊢≥⌽∘⌈\∘⌽))¨∘↓⍵}{(⍺⍺⍵)∨(⍉∘⍺⍺∘⍉⍵)}input

⍝ Found this link to another solution that's super clever:
⍝ https://github.com/jayfoad/aoc2022apl/blob/main/p8.dyalog
⍝
⍝ NOTE: if f is a function, you can pass it as a left-hand-side
⍝ argument by doing this:
⍝ f {⍺⍺ ⍵} input

data←⍎¨↑⊃⎕NGET input_filename 1
f←{⍵>¯1⍪¯1↓⌈\⍵}
part1←+/,{(f⍵)∨⊖f⊖⍵}{(⍺⍺⍵)∨⍉⍺⍺⍉⍵}
⍝ Part1 Answer = 1796