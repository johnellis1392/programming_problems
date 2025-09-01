NB. Learning some Basic J functions
a =: 1+i.10
+/a
*/a
+/*:a
%/a
2|a
^/a
#a
(2|a)#a

b =: 2 5 $ a NB. Reshape array
$ b NB. Get dimensions of list
0, a NB. Concatenate lists
1 { a
2 { a NB. Get element at index
'park' i. 'k' NB. indexOf
b -: b NB. Match; compares shape and values
(1 - ' ' = (s =:'this is a sentence')) # s NB. Remove spaces

a2 =: 'The answer is' ; 42 NB. Link together two elements; creates a list
": 42 NB. toString

p =: 4 1 $ 1 2 3 4
q =: 4 1 $ 3 0 1 1

2 3 $ ' p ' ; ' q ' ; ' p+q ' ; p ; q ; p+q

beans =: < 'baked beans' NB. Box a value
> beans NB. Unbox a value

filename =. < 'input.txt'
readfile =: 1!:1
data =: readfile filename
data =: <;._1 LF,data


NB. test_input =: '1abc2
NB. pqr3stu8vwx
NB. a1b2c3d4e5f
NB. treb7uchet'
