f=File.read('./input/2015/day06.txt')
B=1000;g=Array.new(B){Array.new(B){false}}
F=->(x1,y1,x2,y2,&h){(y1..y2).each{|r|(x1..x2).each{|c|g[r][c]=h.call(g[r][c])}}}

# Part 1
f.lines.map{|l|_,c,*a=l.strip.match(/^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$/).to_a;[c,a.map(&:to_i)]}.each do|c,a|
  case c
  when /^turn on$/; F.(*a){|_|true}
  when /^turn off$/; F.(*a){|_|false}
  when /^toggle$/; F.(*a){|v|!v}
  end
end
p g.sum{|r|r.count{|c|c}}

# Part 2
g=Array.new(B){Array.new(B){0}}
f.lines.map{|l|_,c,*a=l.strip.match(/^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$/).to_a;[c,a.map(&:to_i)]}.each do|c,a|
  case c
  when /turn on/; F.(*a){|v|v+1}
  when /turn off/; F.(*a){|v|[v-1,0].max}
  when /toggle/; F.(*a){|v|v+2}
  end
end
p g.sum{|r|r.sum}