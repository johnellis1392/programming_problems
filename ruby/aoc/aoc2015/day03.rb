require 'set'
f=File.read('./input/2015/day03.txt')
d={?^=>[0,1],?v=>[0,-1],?<=>[-1,0],?>=>[1,0]}

# Part 1
v=Set[]
f.chars.map{|c|d[c]}.reduce([0,0]){|a,b|c=a.zip(b).map(&:sum);v.add(c);c}
p v.count

# Part 2
v=Set[]
a,b=[],[]
t=->c{c.map{|i|d[i]}.reduce([0,0]){|a,b|c=a.zip(b).map(&:sum);v.add(c);c}}
f.chars.zip(0..).map{|(c,i)|(i%2==0?a:b).push(c)}
t.(a);t.(b)
p v.count