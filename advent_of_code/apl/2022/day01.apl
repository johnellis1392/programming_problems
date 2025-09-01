⍝ Initial attempt
input←⊃⎕NGET 'input.txt' 1
result←{+/(⍎¨⍵)}¨((×≢¨)⊆⊢)input

⍝ Input from code_report video on this topic
input←⍎¨¨((×≢¨)⊆⊢)⊃⎕NGET 'input.txt' 1

⍝ Calculate Sums and take max
⌈/+/¨input

⍝ Sort input, take top 3, sum
+/3↑(⊂⍤⍒⌷⊢)+/¨input