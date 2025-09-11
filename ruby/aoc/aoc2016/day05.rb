require 'digest'
require 'set'
I='ojvtpuvg'
# I='abc'

# Part 1
i=0;v=I+i.to_s
P1=(1..8).map{|_|loop do i+=1;v=Digest::MD5.hexdigest(I+i.to_s);break if v.start_with?('0'*5)end;p v;v[5]}.join
p P1

# Part 2
i=0;v=I+i.to_s;a=Set[]
V=(1..8).map{|_|loop do i+=1;v=Digest::MD5.hexdigest(I+i.to_s);break if v.start_with?('0'*5)&&(?0..?7)===v[5]&&!a.include?(v[5])end;p v;a.add(v[5]);v}
P2=V.map{|v|[v[5],v[6]]}.sort_by{|(a,_)|a}.map{|(_,b)|b}.join
p P2