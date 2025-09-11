n=23456790
takewhile=lambda f,l:f(l[0])and [l[0]]+takewhile(f,l[1:])or[]
m,r=map,range
# P=[2,3,5,7]
# isprime=lambda n:all(m(lambda i:n%i!=0, takewhile(lambda x: x<n//2+1, P))) and not P.append(n)
def isprime(x):
  pass
isascending=lambda x:x==0 or x%10>(x//10)%10 and isascending(x//10)
list(m(print,filter(lambda x:isprime(x)and isascending(x),r(11,n,2))))