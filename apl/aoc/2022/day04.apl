⍝ filename←'input.test.txt'
filename←'input.txt'
input←⊃⎕NGET filename 1
input2←(↑((⍎¨('-'∘≠⊆⊢))¨(','∘≠⊆⊢)))¨input
range←{((1⌷⍵)-1)↓⍳(2⌷⍵)}

⍝ Part 1
issubset←{a←range 1⌷⍵ ⋄ b←range 2⌷⍵ ⋄ (∧/b∊c)∨(∧/c∊b)}
result←+/issubset¨input2

⍝ Part 2
issubset2←{a←range 1⌷⍵ ⋄ b←range 2⌷⍵ ⋄ ∨/×(a∩b)}
result2←+/issubset2¨input2
