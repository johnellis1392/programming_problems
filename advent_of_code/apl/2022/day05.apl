input←⊃⌷NGET 'input.test.txt' 1
r←,{×⍴⍵=0}¨input
x←r⊆input
headers←1⊃x
body←2⊃x
digits←'0123456789'

parsed_headers←⌽((⍴headers)-1)↑{((⍴⍵)⍴(1 1 1 0))⊆⍵}¨headers

⍝ TODO: Finish this...
y←{⍎¨(⍵∊digits)⊆⍵}¨body
n fc tc←⊃y
