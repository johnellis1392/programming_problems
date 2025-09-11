#!/usr/bin/ruby
f=File.read('./input/2015/day01.txt')
p f.chars.sum{|c|c=='('?1:-1}
res=-1
f.chars.zip(0..).reduce(0){|s,(c,i)|s<0?(res=i;break):s+(c=='('?1:-1)}
p res