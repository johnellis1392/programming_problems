f=File.read('./input/2015/day05.txt')
p f.lines.reject{|s|s.gsub(/[^aeiou]/,'').size<3||!s.match(/([a-z])\1/)||s.match(/(ab|cd|pq|xy)/)}.size
p f.lines.reject{|s|!s.match(/(\w).\1/)||!s.match(/(\w\w).*\1/)}.size