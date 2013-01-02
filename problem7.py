from time import time
from math import sqrt

primes = [2, 3]

def sieve(n):
  x = primes[-1]
  while len(primes) < n:
    x += 2
    while not prime(x):
      x += 2
    primes.append(x)
#    print "#%d: %d " % (len(primes), x) 
     
def prime(x):
  n = 0
  while n<len(primes) and primes[n] < 2*sqrt(x):
    if x % primes[n] == 0:
      return False
    n += 1
  return True


timer = time()
sieve(10001)
print "Calculated %d in %f seconds." % (primes[-1], time()-timer)
