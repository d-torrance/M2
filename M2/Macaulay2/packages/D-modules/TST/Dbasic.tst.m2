clearAll()
path = join(path, {"../"})
load "Dloadfile.m2";

-- Boundary cases
W = QQ[x,Dx,WeylAlgebra => {x=>Dx}];
I0 = ideal (0_W);
I1 = ideal (1_W);
assert (Ddim I0 == 2);
assert (Ddim I1 == -1);
assert (Drank I0 == infinity);
assert (Drank I1 ==  0);
assert (singLocus I0 == 0);
assert (singLocus I1 == ideal(1_W));
assert (charIdeal I0 == 0);
assert (chI = charIdeal I1; chI == ideal(1_(ring chI)) );

-- Boundary cases for module scripts
M = directSum(cokernel gens I0, cokernel gens I1);
N = directSum(cokernel gens I1, cokernel gens I1);
assert (Ddim M == 2);
assert (Ddim N == -1);
assert (Drank M == infinity);
assert (Drank N == 0);
assert (singLocus M == 0);
assert (singLocus N == ideal 1_W);
assert (charIdeal M == 0);
assert (chN = charIdeal N; chN == ideal(1_(ring chN)) );

-- Properties of AppellF1
I = AppellF1 ({2,4,-1,3/2});
J = substitute (AppellF1 ({3,-1,7/3,-5}), ring I);
K = directSum(cokernel gens I, cokernel gens J);
assert (Ddim I == Ddim J);
assert (Drank I == Drank J);
assert (singLocus I == singLocus J);
assert (charIdeal I == charIdeal J);
assert (isHolonomic K);
assert (Drank K == Drank I + Drank J);
assert (singLocus K == singLocus I);

w' = {0,0,1,1}
assert (inw(I,w') == inw(J,w'));

-- Ranks of gkz systems
A = matrix{{1,1,1,1},{0,1,3,4}};
assert (Drank(gkz(A, {1,3})) == 4);
assert (Drank(gkz(A, {1,2})) == 5);
assert (isHolonomic gkz(A,{-1/2, 5/3}));

-- Polynomial and Rational annihilators
W = QQ[u,v,Du,Dv, WeylAlgebra => {u => Du, v => Dv}];
f = u^5 - v^2;
I = PolyAnn f;
J = RatAnn f;
K = RatAnn (u-v^2, f);
L = directSum (W^1/I, W^1/J);
assert ( isHolonomic I );
assert ( isHolonomic J );
assert ( isHolonomic K );
assert ( isHolonomic L );
assert ( Drank I == 1 );
assert ( Drank J == 1 );
assert ( Drank K == 1 );
assert ( Drank L == 2 );
assert ( singLocus I == ideal(1_W) );
assert ( singLocus J == ideal(f) );
assert ( singLocus K == ideal(f) );
assert ( singLocus L == ideal(f) );

-- Initial ideals and gb's in the same Grobner cone
A = matrix{{1,1,1},{0,2,7}};
b = {1,5};
I = gkz(A,b,Vars=>Local);

-- weight vector of the form (-u,u)
w1 = {-1,-10,-30,1,10,30};
w2 = {-1,-10,-31,1,10,31};
I1 = inw(I, w1);
G1 = gbw(I, w1);
assert(I1 == inw(I, w2));
assert(G1 == gbw(I, w2));
setHomSwitch false;
I1' = inw(I, w1);
G1' = gbw(I, w1);
assert(I1' == I1);
assert(G1' == G1);
assert(I1' == inw(I, w2));
assert(G1' == gbw(I, w2));
setHomSwitch true;

-- weight vector (u,v) with u+v > 0
w1 = {0,1,2,3,4,100};
w2 = {0,1,2,3,4,101};
assert(inw(I,w1) == inw(I, w2));
assert(gbw(I,w1) == gbw(I, w2));

-- weight vector (u,v) with some comp's of u+v > 0, others equal to 0.
w1 = {1,-3,107,-1,4,-5};
w2 = {1,-3,108,-1,4,-5};
I1 = inw(I, w1);
assert(I1 == substitute(inw(I, w2), ring I1));
assert(gbw(I, w1) == gbw(I, w2));
