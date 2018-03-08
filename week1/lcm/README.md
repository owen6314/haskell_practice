# lcm
A little more difficult than a-plus-b.

The first problem I encountered was the use of gcd, the embedded function, at first I use it as gcd(x,y) in C way. In fact in Haskell it should be gcd x y.

'Div' and '/' also bother me a little, I don't know why it doesn't work by using x * y / gcd x y. But it works fine by using div (x*y) (gcd x y)

