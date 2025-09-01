require 'set'
f=File.read('./input/2016/day01.txt')
d={n:[0,1],s:[0,-1],e:[1,0],w:[-1,0]}
l={n: :w,w: :s,s: :e,e: :n}
r={n: :e,e: :s,s: :w,w: :n}
t={R:r,L:l}

# Part 1
z=:n
a,b=f.split(', ').map{|s|c,n=s[0].to_sym,s[1..].to_i;z=t[c][z];x,y=*d[z];[x*n,y*n]}.reduce([0,0]){|a,b|[a[0]+b[0],a[1]+b[1]]}
p a.abs+b.abs

# Part 2
z=:n;v=Set[];res=nil
f.split(', ')
.map{|s|c,n=s[0].to_sym,s[1..].to_i;z=t[c][z];[d[z]]*n}
.reduce([]){|a,b|a+b}
.reduce([0,0]){|a,b|v.include?(a)?(res=a;break):(c=[a[0]+b[0],a[1]+b[1]];v.add(a);c)}
p res[0].abs+res[1].abs