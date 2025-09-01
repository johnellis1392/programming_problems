n←4
⍝ n←14
t0←'mjqjpqmgbljsphdztnvjfqwrcgsmlb'
⍝ f←n++/~n{⍺∘=⍤⍴⍤∪¨⍺,/⍵}t0
f←{⊃⍺++/~⍺∘=⍤⍴⍤∪¨⍺,/⍵}
n f t0
