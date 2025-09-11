require 'set'
def validBraces(braces)
  c={?]=>?[,?}=>?{,?)=>?(}
  o=Set.new(c.keys);i=Set.new(c.values)
  res=braces.chars.reduce([]) do |s,a|
    if i===a then s.push(a)
    elsif !i===c[a] then return false
    else s.pop
    end
    s
  end
  res.empty?
end

p validBraces('[[[]]]')
p validBraces('[[[]]')