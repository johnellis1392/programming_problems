f=File.read('./input/2016/day03.txt')
p f.lines.map{|l|l.strip.split(/\s+/).map(&:to_i)}.reject{|a|a.permutation.any?{|b|b[0]+b[1]<=b[2]}}.size
p f.lines.map{|l|l.strip.split(/\s+/).map(&:to_i)}.each_slice(3).map(&:transpose).flat_map{|a|a}.reject{|a|a.permutation.any?{|b|b[0]+b[1]<=b[2]}}.size