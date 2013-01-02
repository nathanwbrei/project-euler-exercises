from time import time
from math import sqrt

primes = [2, 3]

def sieve(n):
  x = primes[-1]
  while primes[-1] < n:
    x += 2
    while not prime(x):
      x += 2
    primes.append(x)
#    print "#%d: %d " % (len(primes), x) 
     
def prime(x):
  n = 0
  while n<len(primes) and primes[n] <= sqrt(x):
    if x % primes[n] == 0:
      return False
    n += 1
  return True

timer = time()
sieve(1000000)
print "Calculated %d in %f seconds." % (primes[-1], time()-timer)
primes.pop()    # Remove additional prime
print "Sum of primes is %d" % (sum(primes))


# Calculated 1000003 in 33.552063 seconds.
# Sum of primes is 37550402023

#Calculated 2000003 in 45.901819 seconds.
#Sum of primes is 142913828922

tot = 0
for i in range(1000000):
    if prime()
