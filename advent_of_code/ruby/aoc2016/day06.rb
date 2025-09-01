f=File.read('./input/2016/day06.txt')
# f='eedadn
# drvtee
# eandsr
# raavrd
# atevrs
# tsrnev
# sdttsa
# rasrtv
# nssdts
# ntnada
# svetve
# tesnvt
# vntsnd
# vrdear
# dvrsen
# enarar'
tally=->a{a.group_by{|b|b}.transform_values{|b|b.size}}
V=f.lines.map{|l|l.strip.chars}.transpose.map{|r|tally.(r).entries.sort_by{|(_,b)|b}}
P1=V.map{|b|b[-1].first}.join
p P1

P2=V.map{|b|b[0].first}.join
p P2