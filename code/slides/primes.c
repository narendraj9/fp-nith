/* Find first 2 primes in the range [1000, 1000000] */

int count = 0;
int primes[2] = {0};

for (int i = 1000; i <= 1000000 && count < 2; ++i) {
     if (is_prime(i))
          primes[count++] = i;
}
