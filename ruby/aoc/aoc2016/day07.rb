f=File.read('./input/2016/day07.txt')
# f='abba[mnop]qrst
# abcd[bddb]xyyx
# aaaa[qwer]tyui
# ioxxoj[asdfgh]zxcvbn'
# 193 too high
p f.lines.map(&:strip)
  .reject{|b|b.match?(/\[[^\[\]]*(\w)(?!\1{3})(\w)\2\1[^\[\]]*\]/)}
  .select{|b|b.match?(/(\w)(?!\1{3})(\w)\2\1/)}
  .size

# f='aba[bab]xyz
# xyx[xyx]xyx
# aaa[kek]eke
# zazbz[bzb]cdb'

# 446 too high
# 272 wrong
# p f.lines.map(&:strip)
#   .select{|a|a.match?(/\[[^\[\]]*(\w)(?!\1)(\w)\1[^\[\]]*\].*\2\1\2/)||
#              a.match?(/(\w)(?!\1)(\w)\1.*\[[^\[\]]*\2\1\2[^\[\]]*\]/)}
#   .size
  # .each{|a|p a}
def process(l)
  outs,ins=[],[];b=false
  l.scan(/((\w)(?!\1)\w\2|\[|\])/).map{|a|a.first}.each{|i|
    i=='['?(b=true;next):i==']'?(b=false;next):((b)?ins:outs).push(i)}
  [outs,ins]
end
# f.lines.map(&:strip)
# p process('aba[bab]xyz')
p process('aba[bcbbab]xyx')
