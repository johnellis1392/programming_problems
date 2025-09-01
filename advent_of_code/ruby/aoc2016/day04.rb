f=File.read('./input/2016/day04.txt')
# f='aaaaa-bbb-z-y-x-123[abxyz]
# a-b-c-d-e-f-g-h-987[abcde]
# not-a-real-room-404[oarel]
# totally-real-room-200[decoy]'
F=->i{i.gsub('-','').chars.group_by{|a|a}.transform_values{|a|a.size}.to_a.group_by{|a,b|b}.transform_values{|a|a.map{|b|b[0]}}.to_a.sort_by{|(a,b)|a}.reverse.map{|a|a[1].sort}.flatten.take(5).join}

# Valid Hashes
Z=f.lines.map{|l|l.strip.match(/^([a-z-]+)-(\d+)\[([a-z]+)\]$/).to_a[1..]}.reject{|(n,s,h)|F.(n)==h}

# Part 1
p Z.sum{|(_,s,_)|s.to_i}

# Part 2
A='a'.ord
D=->(s,n){s.chars.map{|c|c.match?(/[a-z]/)?(((c.ord-A+n)%26+A).chr):c}.join.gsub('-',' ')}
Y=f.lines.map{|l|l.strip.match(/^([a-z-]+)-(\d+)\[([a-z]+)\]$/).to_a[1..]}
Y.map{|(n,s,h)|[s,D.(n,s.to_i)]}
.each{|s|p s}
# .reject{|s,n|n.match?(/radioactive|hazardous|rampaging|weaponized|projectile|unstable|military|corrosive|cryogenic|egg|rabbit|scavenger|basket|grass|flower|bunny|jellybean|chocolate|user testing/)}