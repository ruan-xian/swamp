/* The GCD algorithm in MicroC */
int a;
int b;

int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int main() {
  int elser;
  int y;
  a = 18;
  b = 9;
  elser = 2;
  y = 14;
  print(gcd(elser,y));
  print(gcd(3,15));
  print(gcd(99,121));
  print(gcd(a,b));
  return 0;
}
