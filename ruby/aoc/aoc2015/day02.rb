f=File.read('./input/2015/day02.txt')
p f.lines.map{|l|a=l.strip.split('x').map(&:to_i);s=a.combination(2).map{|b|b[0]*b[1]};2*s.sum+s.min}.sum
p f.lines.map{|l|a=l.strip.split('x').map(&:to_i).sort;a[0]*a[1]*a[2]+2*(a[0]+a[1])}.sum