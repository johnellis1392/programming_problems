f=File.read('./input/2016/day02.txt')
d={U:[-1,0],D:[1,0],L:[0,-1],R:[0,1]}

# Part 1
h=->(a,b){[(a[0]+b[0]).clamp(0,2),(a[1]+b[1]).clamp(0,2)]}
n=%w(123 456 789)
z=[1,1]
p f.lines.map{|l|z=l.strip.chars.reduce(z){|c,e|h.(c,d[e.to_sym])};r,c=*z;n[r][c]}.join

# Part 2
h=->(a,b){[(a[0]+b[0]).clamp(0,4),(a[1]+b[1]).clamp(0,4)]}
n=[
  [nil,nil,'1',nil,nil],
  [nil,'2','3','4',nil],
  ['5','6','7','8','9'],
  [nil,'A','B','C',nil],
  [nil,nil,'D',nil,nil],
]
z=[2,0]
p f.lines.map{|l|z=l.strip.chars.reduce(z){|a,e|r,c=*h.(a,d[e.to_sym]);n[r][c]?[r,c]:a};r,c=*z;n[r][c]}.join