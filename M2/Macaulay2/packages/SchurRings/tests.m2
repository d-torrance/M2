--------------------
-- test Jacobi-Trudi
--------------------
TEST ///
E = symmetricRing(QQ,5)
f = jacobiTrudi({4,1},E)
g = toS f
G = ring g
assert (g == G_{4,1})
///

TEST ///
E = symmetricRing(QQ,5)
f = jacobiTrudi({2,1},E)
g = toS f
G = ring g
assert (g == G_{2,1})
///

TEST ///
E = symmetricRing(QQ,13)
f = jacobiTrudi({7,4,2},E)
g = toS f
G = ring g
assert (toS f == G_{7,4,2})
///

TEST ///
P = symmetricRing(QQ,6)
f = toS plethysm(jacobiTrudi({2},P), jacobiTrudi({2},P))
G = ring f
assert(f == G_{4}+G_{2,2})
///

TEST ///
Q = symmetricRing(QQ,5)
--S = schurRing(QQ,q,4)
S = schurRing Q
f = toS(plethysm(jacobiTrudi({3},Q), jacobiTrudi({2},Q)))
--assert(dim f == 220)
assert(dim(4,f) == 220)
///
------------------------
-- end test Jacobi-Trudi
------------------------

-----------
-- test dim
-----------

TEST ///
R = schurRing(r,3,GroupActing => "Sn")
S = schurRing(R,s,2)
T = schurRing(S,t,4,GroupActing => "Sn")
assert( (dim(r_1)) == 1 )
assert( (dim(s_2)) == 3 )
assert( (dim(s_3)) == 4 )
assert( (dim(t_4)) == 1 )
assert( (dim(r_1 * s_2 * t_3)) == 3 )
assert( (dim(r_1 * s_3 + t_4)) == 5 )
///

---------------
-- end test dim
---------------

---------------------
-- test plethysm, toS
---------------------
TEST ///
R = symmetricRing(QQ,4)
pl = plethysm({1,1},jacobiTrudi({2},R))
G = schurRing ring pl
assert(toS pl == G_{3,1})
///

TEST ///
R = symmetricRing(QQ,12)
pl = plethysm({1,1,1},jacobiTrudi({4},R))
assert(#listForm(toS pl) == 7)
///

TEST ///
R = symmetricRing(QQ, 9)
S = schurRing(QQ,q,3)
pl = plethysm(h_3,q_{2,1})
assert (dim(pl) == 120)
///

TEST ///
R = symmetricRing(QQ,3)
S = schurRing R
assert(toS(h_3 @ e_3) == S_{3,3,3})
///

TEST ///
S = schurRing(QQ,q,4)
assert(plethysm(q_{2,1},q_{1,1,1}) == q_{3,3,2,1})
///

TEST ///
R = symmetricRing(QQ, 12)
f = e_4
lambda = new Partition from {3}
assert(plethysm(lambda,f) == h_3 @ e_4)
///

TEST ///
schurRing(QQ,s,2)
assert(dim(plethysm(s_{2,1}+s_{3},s_{3})) == 40)
///

TEST ///
R = symmetricRing(QQ,20)
assert(#listForm(toS plethysm(h_5,h_4)) == 95)
///


TEST ///
R = symmetricRing(QQ, 10)
S = schurRing R
sch = toS(plethysm({2,1},h_3))
assert(dim(5,sch) == 14280)
///

TEST ///
R = symmetricRing 5
S = schurRing(s,3)
assert( ((h_2 + p_2) @ s_{2,1}) == 2*s_(4,2)-s_(4,1,1)-s_(3,3)+s_(3,2,1)+2*s_(2,2,2) )
///

TEST ///
S = schurRing(s,3)
T = schurRing(S,t,4,GroupActing => "Sn")
Q = schurRing(T,q,2)
assert( (s_2 @ q_{2,1}) == q_(4,2) )
assert( (s_{2,1} - s_{1,1,1} @ (t_3 + t_(2,1))) == -s_()*t_(1,1,1)+s_(2,1)*t_() )
assert( (plethysm({3},s_1 * t_1 * q_1)) == s_3*t_1*q_3+s_(2,1)*t_1*q_(2,1) )
assert( (plethysm({2,1},s_1 + t_1 + q_1)) == q_(2,1)+(t_1+s_1*t_())*q_2+(t_1+s_1*t_())*q_(1,1)+((2*s_1+s_())*t_1+(s_2+s_(1,1))*t_())*q_1+((s_2+s_(1,1)+s_1)*t_1+s_(2,1)*t_())*q_() )
assert( toH plethysm({2,1},s_1 + t_1 + q_1) == toH plethysm({2,1},toE(s_1 + t_1 + q_1)) )
///

----------------------------
-- end test of plethysm, toS
----------------------------

------------------------------------
----- symmetricPower & exteriorPower
------------------------------------

TEST ///
S = schurRing(s,5)
T = schurRing(S,t,3,GroupActing => "Sn")
assert( (symmetricPower(2,s_3)) == s_6+s_(4,2) )
assert( (symmetricPower(2,t_2)) == t_2 )
assert( (symmetricPower(2,s_3+t_2)) == (s_3+s_())*t_2+(s_6+s_(4,2))*t_() )
assert( (exteriorPower(3,s_3 * t_{2,1} - t_3)) == (s_(7,1,1)+s_(6,3)+s_(5,3,1)-s_(5,1)+s_(3,3,3)-s_(3,3)-s_())*t_3+(s_(8,1)+s_(7,2)+s_(7,1,1)+2*s_(6,3)+s_(6,2,1)+s_(5,4)+2*s_(5,3,1)-s_(5,1)+s_(4,3,2)+s_(3,3,3)-s_(3,3)+s_3)*t_(2,1)+(s_(7,1,1)+s_(6,3)-s_6+s_(5,3,1)-s_(4,2)+s_(3,3,3))*t_(1,1,1) )
assert( (exteriorPower(5,s_1 * t_1)) == s_(1,1,1,1,1)*t_1 )
///

TEST ///
R = symmetricRing(3,GroupActing => "Sn")
S = symmetricRing(R,4)
T = symmetricRing(S,2,GroupActing => "Sn")

a = R.hVariable 3
b = S.eVariable 4
c = T.pVariable 2

assert (toS(symmetricPower(3,a*b*c)) == symmetricPower(3,toS(a*b*c)))
assert (toS(exteriorPower(3,a*b*c)) == exteriorPower(3,toS(a*b*c)))
assert (toS(symmetricPower(3,a+b-c)) == symmetricPower(3,toS(a+b-c)))
assert (toS(exteriorPower(3,a*b-c)) == exteriorPower(3,toS(a*b-c)))
///

----------------------------------------
----- end symmetricPower & exteriorPower
----------------------------------------

-------------------------------------------------------------------
----- test characters of symmetric groups, scalarProd, internalProd
-------------------------------------------------------------------
TEST ///
R = symmetricRing(QQ,20)
S = schurRing(QQ,o,20)
assert(scalarProduct(o_{6,4,3,2,1},jacobiTrudi({3,3,3},symmetricRing S)*toP(o_{4,2,1})) == 2)
assert(scalarProduct(jacobiTrudi({6,4,3,2,1},R),jacobiTrudi({4,3,3,3,2,1},R)) == 0)
assert(scalarProduct(jacobiTrudi({6,4,3,2,1},R),o_{4,3,3,3,2,1}) == 0)
///

TEST ///
R = symmetricRing(QQ,5)
A = schurRing(QQ,a,4)
assert(internalProduct(e_2+h_2,a_{2}) == a_{2}+a_{1,1})
assert(toE internalProduct(a_{2},e_2+h_2) == toE p_1^2)
assert(dim internalProduct(a_{2,1}*a_{1},a_{2,2}) == 176)
///


TEST ///
R = symmetricRing(QQ,10)
ch1 = new ClassFunction from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
ch2 = new ClassFunction from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
assert(toP symmetricFunction(internalProduct(ch1,ch2),R) == 1/8*p_1*p_2^2*p_3+1/5*p_1*p_2*p_5)
assert(toP symmetricFunction(ch1*ch2,R) == 1/8*p_1*p_2^2*p_3+1/5*p_1*p_2*p_5)
///

TEST ///
R = symmetricRing(QQ,4)
f = p_2^2
g = (e_2+h_2)^2
ch1 = classFunction(f)
ch2 = classFunction(g)
assert(symmetricFunction(internalProduct(ch1,ch2),R) == 0)
assert(internalProduct(f,g) == 0)
///
---------------------------------------------------------------------
--- end test characters of symmetric groups, scalarProd, internalProd
---------------------------------------------------------------------

---------------------------
--- test toS, toP, toE, toH
---------------------------
TEST ///
R = symmetricRing(QQ,6)
assert(toE(toS(e_1*e_2*e_3)) == e_1*e_2*e_3)
///

TEST ///
R = symmetricRing(QQ,5)
S = schurRing(QQ,q,3)
assert(toE(q_{2}) + e_2 == e_1^2)
///

TEST///
R = symmetricRing(QQ, 4)
assert(toP toE toH toE toH toP toE toE toP toH (p_1+p_2+p_3) == p_1+p_2+p_3)
///

TEST ///
R = symmetricRing(QQ,6)
S = schurRing R
toSf = map(S, R, apply(gens R, x -> toS(x)))
assert(toSf(e_1*e_2*e_3) == S_(3,2,1)+S_(3,1,1,1)+S_(2,2,2)+2*S_(2,2,1,1)+2*S_(2,1,1,1,1)+S_(1,1,1,1,1,1))
assert(toSf(h_1*h_2*h_3) == S_{1}*S_{2}*S_{3})
///

TEST ///
R = symmetricRing(QQ,7)
assert(toH toP toE (toS (jacobiTrudi({2,1},R))^2) == (h_1*h_2-h_3)^2)
///

TEST ///
S = schurRing(s,5,GroupActing => "Sn")
R = symmetricRing S
T = schurRing(S,t,3,EHPVariables => (getSymbol "eT", getSymbol "hT", getSymbol "pT"))
Q = symmetricRing T
a = toS((R.pVariable 2) * (R.eVariable 3))
b = a * (t_3 - t_{2,1})
c = toH(a + b)
d = toE(a - b)
f = toP b
assert( (a) == s_(3,1,1)-s_(2,2,1)-s_(1,1,1,1,1) )
assert( (b) == (s_(3,1,1)-s_(2,2,1)-s_(1,1,1,1,1))*t_3+(-s_(3,1,1)+s_(2,2,1)+s_(1,1,1,1,1))*t_(2,1) )
assert( (c) == (h_1^5-4*h_1^3*h_2+4*h_1*h_2^2+h_1^2*h_3-2*h_2*h_3)*hT_1*hT_2+(-2*h_1^5+8*h_1^3*h_2-8*h_1*h_2^2-2*h_1^2*h_3+4*h_2*h_3)*hT_3-h_1^5+4*h_1^3*h_2-4*h_1*h_2^2-h_1^2*h_3+2*h_2*h_3 )
assert( (d) == (-e_1^2*e_3+2*e_2*e_3)*eT_1^3+(3*e_1^2*e_3-6*e_2*e_3)*eT_1*eT_2+(-2*e_1^2*e_3+4*e_2*e_3)*eT_3+e_1^2*e_3-2*e_2*e_3 )
assert( (f) == (-(1/36)*p_1^3*p_2+(1/12)*p_1*p_2^2-(1/18)*p_2*p_3)*pT_1^3+((1/12)*p_1^3*p_2-(1/4)*p_1*p_2^2+(1/6)*p_2*p_3)*pT_1*pT_2+((1/9)*p_1^3*p_2-(1/3)*p_1*p_2^2+(2/9)*p_2*p_3)*pT_3 )
assert( (toH(c - d - 2*f)) == 0 )
assert( (toE(c - d - 2*f)) == 0 )
assert( (toP(c - d - 2*f)) == 0 )
///
-------------------------------
--- end test toS, toP, toE, toH
-------------------------------

-------------------------------------------------
--- test schurLevel, symmetricRing, schurRing
-------------------------------------------------

TEST ///
R = symmetricRing 5
S = schurRing R
S1 = schurRing(R,s1,3,GroupActing => "Sn")
R1 = symmetricRing S1
R2 = symmetricRing(R1,3,GroupActing => "Sn")
S2 = schurRing R2
assert( (schurLevel QQ) == 0 )
assert( schurLevel (ZZ/5) == 0 )
assert( (schurLevel R) == 1 )
assert( (schurLevel R1) == 2 )
assert( (schurLevel R2) == 3 )
assert( (schurLevel S) == 1 )
assert( (schurLevel S1) == 2 )
assert( (schurLevel S2) == 3 )
///

-----------------------------------------------------
--- end test schurLevel, symmetricRing, schurRing
-----------------------------------------------------

-----------------------
-- test centralizerSize
-----------------------

TEST ///
assert( (centralizerSize{3,2,1}) === 144 )
assert( (centralizerSize{1,1,1}) === 6 )
assert( (centralizerSize{3}) === 6 )
assert( (centralizerSize{5,2,1,1,1}) === 57600 )
assert( (centralizerSize{5,5,5}) === 13436928000 )
assert( (centralizerSize{4,4,2,2}) === 5308416 )
assert( (centralizerSize{1}) === 1 )
///

---------------------------
-- end test centralizerSize
---------------------------

-----------------------
-- test schurResolution
-----------------------

TEST ///
S = schurRing(QQ,s,3)
rep = s_{2}
M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}}
sR = schurResolution(rep,M)
assert( (#sR) == 4 )
assert( (sR#2#0#1) == s_(3,2,1) )
assert( (last last sR) == (4,s_(3,3,2)) )

rep = s_{3}
M = {1_S,s_{3},s_{6},s_{9},s_{12},s_{15},s_{18},s_{21},s_{24},s_{27}}
d = 7
sR = schurResolution(rep,M,DegreeLimit => d)
assert( (#sR) == 7 )
assert( (sR#2#0#0) == 3 )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1}, {27}, {105}, {189}, {189}, {105}, {27}} )

T = schurRing(S,t,4)
rep = s_1 * t_1
M = {1_T} | apply(splice{1..8},i -> s_i * t_i)
sR = schurResolution(rep,M)
assert( (last last sR) == (8,s_(3,3,2)*t_(2,2,2,2)) )
assert( (first first sR) == (0,t_()) )
assert( (sR#1#0) == (2,s_(1,1)*t_(1,1)) )
assert( (sR#4#1) == (6,s_(2,2,2)*t_(2,2,2)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{18},{52},{60},{24,10},{12},{3}} )

n = 5;
S = schurRing(QQ,s,n,GroupActing => "Sn");
rep = s_n + s_{n-1,1};
M = {s_n}
sR = schurResolution(rep,M,DegreeLimit => n)
assert( (last sR#2) == (2,s_(4,1)+s_(3,1,1)) )
assert( (first sR#3) == (3,s_(3,1,1)+s_(2,1,1,1)) )
assert( (last last sR) == (5,s_(1,1,1,1,1)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{5},{10},{10},{5},{1}} )

M = {s_n} | splice{n:rep};
sR = schurResolution(rep,M)    
assert( (last sR#2) == (3,s_(4,1)+s_(3,2)+s_(3,1,1)+s_(2,2,1)) )
assert( (first sR#3) == (4,s_(3,1,1)+s_(2,2,1)+s_(2,1,1,1)) )
assert( (last last sR) == (5,s_(2,1,1,1)) )

l = apply(sR,i -> i / (j -> dim last j))
assert( (l) == {{1},{10},{20},{15},{4}} )
///

---------------------------
-- end test schurResolution
---------------------------

------------------------------------
-- tests for kostkaNumber
------------------------------------
TEST ///
-- Standard known Kostka numbers
assert(kostkaNumber({2,1},{2,1}) == 1)
assert(kostkaNumber({2,1},{1,1,1}) == 2)
assert(kostkaNumber({3},{2,1}) == 1)
assert(kostkaNumber({3},{3}) == 1)
assert(kostkaNumber({1,1,1},{1,1,1}) == 1)
assert(kostkaNumber({2,2},{2,1,1}) == 1)
-- Dominance order: K = 0 if mu does not dominate lambda
assert(kostkaNumber({2,1},{3}) == 0)
-- Size mismatch
assert(kostkaNumber({2,1},{2,2}) == 0)
///

------------------------------------
-- tests for toM
------------------------------------
TEST ///
S = schurRing(QQ,s,4);

-- Output is a RingElement in a monomial-basis SchurRing
r1 = toM s_{2,1};
assert(class ring r1 === SchurRing);
assert((ring r1).Basis == "Monomial");

-- s_{2,1} = m_{2,1} + 2 m_{1,1,1}
lf1 = new HashTable from listForm r1;
assert(lf1#{2,1} == 1);
assert(lf1#{1,1,1} == 2);
assert(#(keys lf1) == 2);

-- s_{3} = m_3 + m_{2,1} + m_{1,1,1}
r2 = toM s_{3};
lf2 = new HashTable from listForm r2;
assert(lf2#{3} == 1);
assert(lf2#{2,1} == 1);
assert(lf2#{1,1,1} == 1);

-- s_{1,1,1} = m_{1,1,1}
r3 = toM s_{1,1,1};
lf3 = new HashTable from listForm r3;
assert(lf3#{1,1,1} == 1);
assert(#(keys lf3) == 1);

-- Linearity
r4 = toM(s_{3} + 2*s_{2,1});
lf4 = new HashTable from listForm r4;
assert(lf4#{3} == 1);
assert(lf4#{2,1} == 3);
assert(lf4#{1,1,1} == 5);

-- Zero
r5 = toM(0_S);
assert(r5 == 0);

-- The associated monomial ring is cached across calls
assert(ring r1 === ring r2);

-- Two-argument form with user-supplied target ring
MM = schurRing(QQ,mm,4,Basis => "Monomial");
r6 = toM(s_{2,1},MM);
assert(ring r6 === MM);
lf6 = new HashTable from listForm r6;
assert(lf6#{2,1} == 1);
assert(lf6#{1,1,1} == 2);

-- Monomial-basis input: identity
r7 = toM MM_{2,1};
lf7 = new HashTable from listForm r7;
assert(lf7#{2,1} == 1);
assert(#(keys lf7) == 1);

-- Error if second argument is not a monomial-basis ring
caught := false;
try toM(s_{2,1},S) else caught = true;
assert(caught);

-- Works through a Symmetric ring by first going to Schur
R = symmetricRing(QQ,4);
r8 = toM(h_2);
lf8 = new HashTable from listForm r8;
assert(lf8#{2} == 1);
assert(lf8#{1,1} == 1);
///

------------------------------------
-- tests for Monomial-basis ring
------------------------------------
TEST ///
M = schurRing(QQ,m,4,Basis => "Monomial");
assert(M.Basis == "Monomial");

-- m_1 * m_1 = m_2 + 2 m_{1,1}
p1 = m_{1} * m_{1};
lf1 = new HashTable from listForm p1;
assert(lf1#{2} == 1);
assert(lf1#{1,1} == 2);

-- m_2 * m_1 = m_3 + m_{2,1}
p2 = m_{2} * m_{1};
lf2 = new HashTable from listForm p2;
assert(lf2#{3} == 1);
assert(lf2#{2,1} == 1);

-- m_{1,1} * m_1 = m_{2,1} + 3 m_{1,1,1}
p3 = m_{1,1} * m_{1};
lf3 = new HashTable from listForm p3;
assert(lf3#{2,1} == 1);
assert(lf3#{1,1,1} == 3);

-- Consistency with Schur-basis product: m_1 = s_1, so m_1*m_1 in M
-- should match the m-expansion of s_1*s_1 computed via toM into M
S = schurRing(QQ,s,4);
assert(toM(s_{1} * s_{1}, M) == m_{1} * m_{1});
///

--------------------
-- tests for Sp stable ring (GroupActing "Sp")
--------------------
TEST ///
Sp = schurRing(QQ,sp,GroupActing => "Sp");
S = schurRing(QQ,s);
assert(Sp.GroupActing == "Sp");
-- Koike branching: sp_1,1 = s_1,1 - 1; use two-arg toS to target S explicitly.
assert(toS(sp_{1,1}, S) == S_{1,1} - 1);
-- sp_2 = s_2 (Sym^2 is irreducible for Sp)
assert(toS(sp_{2}, S) == S_{2});
-- sp_{1,1,1} = s_{1,1,1} - s_1
assert(toS(sp_{1,1,1}, S) == S_{1,1,1} - S_{1});
-- sp_{2,1,1} = s_{2,1,1} - s_2 - s_{1,1} + 1.
-- Verify: at Sp(6), dim sp_{2,1,1} = 105 - 21 - 15 + 1 = 70, matches
-- Littlewood branching s_{2,1,1}|_Sp(6) = sp_{2,1,1} + sp_{2} + sp_{1,1}.
assert(toS(sp_{2,1,1}, S) == S_{2,1,1} - S_{2} - S_{1,1} + 1);
///

TEST ///
S = schurRing(QQ,s);
Sp = schurRing(QQ,sp,GroupActing => "Sp");
-- Inverse Koike: s_{1,1} = sp_{1,1} + 1
assert(toSp(s_{1,1}, Sp) == Sp_{1,1} + 1_Sp);
-- s_2 = sp_2
assert(toSp(s_2, Sp) == Sp_{2});
-- Round trip: toSp o toS is identity on sp-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toSp(toS(Sp_lam, S), Sp) == Sp_lam);
-- Round trip: toS o toSp is identity on s-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toS(toSp(S_lam, Sp), S) == S_lam);
///

TEST ///
-- Sp multiplication is the Newell-Littlewood product
Sp = schurRing(QQ,sp,GroupActing => "Sp");
-- sp_1 * sp_1 = sp_2 + sp_{1,1} + 1 (since V otimes V for Sp = Sym^2 + Lambda^2,
-- and Lambda^2 contains a 1-dim invariant form)
assert(sp_{1} * sp_{1} == sp_{2} + sp_{1,1} + 1_Sp);
-- sp_2 * sp_1 = sp_3 + sp_{2,1} + sp_1
assert(sp_{2} * sp_{1} == sp_{3} + sp_{2,1} + sp_{1});
-- sp_{1,1} * sp_1 = sp_{2,1} + sp_{1,1,1} + sp_1
assert(sp_{1,1} * sp_{1} == sp_{2,1} + sp_{1,1,1} + sp_{1});
-- Associativity: (sp_1 * sp_1) * sp_1 == sp_1 * (sp_1 * sp_1)
assert((sp_{1} * sp_{1}) * sp_{1} == sp_{1} * (sp_{1} * sp_{1}));
///

--------------------
-- tests for O stable ring (GroupActing "O")
--------------------
TEST ///
O = schurRing(QQ,o,GroupActing => "O");
S = schurRing(QQ,s);
assert(O.GroupActing == "O");
-- Koike branching: o_2 = s_2 - 1 (Sym^2 loses the invariant bilinear form)
assert(toS(o_{2}, S) == S_{2} - 1);
-- o_{1,1} = s_{1,1}  (Lambda^2 V is irreducible for O(n), n >= 3)
assert(toS(o_{1,1}, S) == S_{1,1});
-- o_3 = s_3 - s_1
assert(toS(o_{3}, S) == S_{3} - S_{1});
-- o_{2,1} = s_{2,1} - s_1
assert(toS(o_{2,1}, S) == S_{2,1} - S_{1});
///

TEST ///
S = schurRing(QQ,s);
O = schurRing(QQ,o,GroupActing => "O");
-- Inverse Koike: s_2 = o_2 + 1
assert(toO(s_{2}, O) == O_{2} + 1_O);
-- s_{1,1} = o_{1,1}
assert(toO(s_{1,1}, O) == O_{1,1});
-- Round trip: toO o toS is identity on o-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toO(toS(O_lam, S), O) == O_lam);
-- Round trip: toS o toO is identity on s-basis
for lam in {{3,2,1}, {4,2}, {2,1,1}, {5}, {1,1,1,1}} do
    assert(toS(toO(S_lam, O), S) == S_lam);
///

TEST ///
-- O multiplication is the Newell-Littlewood product for O
O = schurRing(QQ,o,GroupActing => "O");
-- o_1 * o_1 = o_2 + o_{1,1} + 1
assert(o_{1} * o_{1} == o_{2} + o_{1,1} + 1_O);
-- o_2 * o_1 = o_3 + o_{2,1} + o_1
assert(o_{2} * o_{1} == o_{3} + o_{2,1} + o_{1});
-- Associativity
assert((o_{1} * o_{1}) * o_{1} == o_{1} * (o_{1} * o_{1}));
///

TEST ///
-- Cross-conversion between Sp and O: route via a shared Schur ring.
Sp = schurRing(QQ,sp,GroupActing => "Sp");
O = schurRing(QQ,o,GroupActing => "O");
S = schurRing(QQ,s);
assert(toSp(o_{2}, Sp) == toSp(toS(o_{2}, S), Sp));
assert(toO(sp_{2}, O) == toO(toS(sp_{2}, S), O));
-- Cached associated rings reused
assert(ring(toSp o_{2}) === ring(toSp o_{1,1}));
///

--------------------
-- tests for O odd/even (B_n vs D_n) distinction
--------------------
TEST ///
-- Direct construction of finite Odd (B_n) and Even (D_n) O rings.
Oodd  = schurRing(QQ, ob, 2, GroupActing => "O", OddOrEven => "Odd");
Oeven = schurRing(QQ, od, 2, GroupActing => "O", OddOrEven => "Even");
assert(Oodd.OddOrEven  == "Odd");
assert(Oeven.OddOrEven == "Even");

-- O(5) = B_2 Weyl dimensions
assert(dim ob_{1}     == 5);   -- defining rep
assert(dim ob_{1,1}   == 10);  -- Lambda^2 V, adjoint of SO(5)
assert(dim ob_{2}     == 14);  -- Sym^2 V minus trace
assert(dim ob_{2,1}   == 35);

-- O(4) = D_2 Weyl dimensions (SO(4) = SU(2) x SU(2) / Z_2)
assert(dim od_{1}     == 4);   -- defining rep
assert(dim od_{2}     == 9);   -- Sym^2 V minus trace (dim 10-1)
-- Lambda^2 V splits under SO(4) into 3+3; partition (1,1) picks one component.
assert(dim od_{1,1}   == 3);
-- SO(4) = SU(2) x SU(2) / Z_2.  Partition (a,b) with a>=b>=0 corresponds to
-- (j_1,j_2) = ((a+b)/2,(a-b)/2), dim (2j_1+1)(2j_2+1); (2,1) -> (3/2,1/2).
assert(dim od_{2,1}   == 8);

-- Default "O" ring (no OddOrEven) falls back to Odd (B_n): backward compat.
Odefault = schurRing(QQ, odef, 2, GroupActing => "O");
assert(Odefault.OddOrEven == "Odd");
assert(dim odef_{1} == 5);
///

TEST ///
-- specialize with OddOrEven option.
Ost = schurRing(QQ, o2, GroupActing => "O");
-- Default: Odd (B_n).
g = specialize(o2_{1}, 2);
assert((ring g).OddOrEven == "Odd");
assert(dim g == 5);
-- Explicit OddOrEven => "Even" selects D_n.
g2 = specialize(o2_{1}, 2, OddOrEven => "Even");
assert((ring g2).OddOrEven == "Even");
assert(dim g2 == 4);
-- Odd and Even at the same n get distinct cached rings.
assert(ring g =!= ring g2);
///

TEST ///
-- OddOrEven is rejected for non-O rings.
ok := true;
try (schurRing(QQ, xx, GroupActing => "Sp", OddOrEven => "Odd"); ok = false) else null;
assert ok;
-- Invalid value rejected.
ok = true;
try (schurRing(QQ, yy, 3, GroupActing => "O", OddOrEven => "Bogus"); ok = false) else null;
assert ok;
///

--------------------
-- tests for SL(n) Schur ring
--------------------
TEST ///
-- Basic SL(n) canonicalization: partitions with n nonzero rows collapse
-- by subtracting the last row from every part.
SL3 = schurRing(QQ, sl, 3, GroupActing => "SL");
-- sl_{1,1,1} = det = trivial rep in SL(3)
assert(sl_{1,1,1} == 1_SL3);
-- sl_{2,1,1} = sl_{1} after subtracting 1 from each row
assert(sl_{2,1,1} == sl_{1});
-- sl_{3,3,3} = trivial
assert(sl_{3,3,3} == 1_SL3);
-- sl_{4,2,1} = sl_{3,1}
assert(sl_{4,2,1} == sl_{3,1});
-- Partitions with fewer than n parts are unchanged
assert(sl_{2,1} == sl_{2,1});
assert(sl_{3} == sl_{3});
///

TEST ///
-- SL multiplication agrees with GL mult followed by det-collapse.
-- In SL(3), sl_{1} * sl_{1,1} = s_{2,1} + s_{1,1,1} = s_{2,1} + 1.
SL3 = schurRing(QQ, sl, 3, GroupActing => "SL");
assert(sl_{1} * sl_{1,1} == sl_{2,1} + 1_SL3);
-- sl_{1} * sl_{1} = sl_{2} + sl_{1,1}
assert(sl_{1} * sl_{1} == sl_{2} + sl_{1,1});
-- (sl_{1})^3 = sl_{3} + 2 sl_{2,1} + sl_{1,1,1}
--           = sl_{3} + 2 sl_{2,1} + 1     (in SL(3))
assert(sl_{1}^3 == sl_{3} + 2*sl_{2,1} + 1_SL3);
-- Associativity
assert((sl_{2} * sl_{1}) * sl_{1} == sl_{2} * (sl_{1} * sl_{1}));
///

TEST ///
-- SL(2): every SL(2) rep is indexed by a single integer (highest weight).
SL2 = schurRing(QQ, sl2, 2, GroupActing => "SL");
-- sl2_{1,1} = det = trivial
assert(sl2_{1,1} == 1_SL2);
-- sl2_{2,1} = sl2_{1}
assert(sl2_{2,1} == sl2_{1});
-- Clebsch-Gordan for SU(2): V_a * V_b = sum_{k} V_{a+b-2k}, k=0..min(a,b).
-- In partition language: sl2_{a} * sl2_{b} = sum sl2_{a+b-2k}.
-- sl2_{2} * sl2_{2} = sl2_{4} + sl2_{2} + 1  (spins 2,1,0)
assert(sl2_{2} * sl2_{2} == sl2_{4} + sl2_{2} + 1_SL2);
-- sl2_{3} * sl2_{1} = sl2_{4} + sl2_{2}
assert(sl2_{3} * sl2_{1} == sl2_{4} + sl2_{2});
///

TEST ///
-- Stable SL (numgens = infinity) coincides with stable GL.
SLstable = schurRing(QQ, slinf, GroupActing => "SL");
-- No det-collapse happens; partitions are preserved.
assert(slinf_{1,1,1,1} != 1_SLstable);
-- Multiplication matches plain GL.
assert(slinf_{1} * slinf_{1} == slinf_{2} + slinf_{1,1});
///

TEST ///
-- toS on an SL element lifts to the associated plain GL ring.
SL3 = schurRing(QQ, sl, 3, GroupActing => "SL");
f = sl_{2,1};
g = toS f;
T = ring g;
assert(class T === SchurRing);
assert(T.GroupActing == "GL");
assert(numgens T == 3);
assert(g == T_{2,1});
-- sl_{1,1,1} = 1 lifts to 1 in GL, not to s_{1,1,1}.
assert(toS (sl_{1,1,1}) == 1_T);
///

--------------------
-- tests for SSW modification rules
--------------------
TEST ///
-- Paper's worked example ([SSW] Example 3.20):
-- lambda = (6,5,4,4,3,3,2), n=2, gives tau=(6,5), i=8, sign=+1.
assert(modificationRule({6,5,4,4,3,3,2}, 2, "C") == ({6,5}, 1));

-- Admissible partitions pass through unchanged with sign +1.
assert(modificationRule({3,2,1}, 3, "C") == ({3,2,1}, 1));
assert(modificationRule({2,1}, 3, "C") == ({2,1}, 1));
assert(modificationRule({}, 2, "C") == ({}, 1));

-- sp_{1,1} in Sp(2): SSW-C returns null (strip size = 0), so the
-- formal character is zero in Sp(2).  Corresponds to Koike: s_{1,1} =
-- sp_{1,1} + 1, and in Sp(2), s_{1,1} = Lambda^2 V = det = trivial, so
-- sp_{1,1} must be 0.
assert(modificationRule({1,1}, 1, "C") === null);

-- sp_{1,1,1} in Sp(2): SSW-C returns ((1), -1).  Corresponds to
-- s_{1,1,1} = Lambda^3 V = 0 for dim V = 2 via Koike
-- s_{1,1,1} = sp_{1,1,1} + sp_{1} = -sp_{1} + sp_{1} = 0.
assert(modificationRule({1,1,1}, 1, "C") == ({1}, -1));

-- Longer test: lambda = (2,2,1), n=1.  r=3, L=2. k with lam_k+3-k=2:
-- k=1: 2, no.  k=2: 3, no.  k=3: 3, no.  No k works -> null.
assert(modificationRule({2,2,1}, 1, "C") === null);

-- Testing type D (m = 2n, even-dim orthogonal O(2n)).
-- lambda = (1,1) in O(2): admissible (lam^T_1 + lam^T_2 = 2+0 = 2 = m).
-- so we should get ((1,1), 1).
assert(modificationRule({1,1}, 1, "D") == ({1,1}, 1));

-- Testing type B (m = 2n+1, odd-dim orthogonal O(2n+1)).
-- lambda = (1) in O(3): admissible (lam^T = (1); 1+0 = 1 <= 3).
assert(modificationRule({1}, 1, "B") == ({1}, 1));
///

-- Regression: Weyl dimensions of Sp(2n) and O(2n+1), O(2n) via dim
TEST ///
-- Sp(4) = C_2 Weyl dims
Sp4 = schurRing(QQ, getSymbol "sp4", 2, GroupActing => "Sp");
assert(dim Sp4_{1} == 4);
assert(dim Sp4_{2} == 10);
assert(dim Sp4_{1,1} == 5);
assert(dim Sp4_{2,1} == 16);
assert(dim Sp4_{2,2} == 14);
-- Sp(4) multiplication
assert(Sp4_{1,1}*Sp4_{1,1} == Sp4_{2,2} + Sp4_{2} + 1_Sp4);
-- dim of a product equals product of dims
assert(dim(Sp4_{1}*Sp4_{1}) == 16);

-- Sp(6) = C_3 Weyl dims
Sp6 = schurRing(QQ, getSymbol "sp6", 3, GroupActing => "Sp");
assert(dim Sp6_{1} == 6);
assert(dim Sp6_{1,1} == 14);
assert(dim Sp6_{2} == 21);
assert(dim Sp6_{1,1,1} == 14);

-- O(5) = B_2 Weyl dims (numgens 2, Odd)
O5 = schurRing(QQ, getSymbol "o5", 2, GroupActing => "O", OddOrEven => "Odd");
assert(dim O5_{1} == 5);
assert(dim O5_{2} == 14);
assert(dim O5_{1,1} == 10);
assert(dim O5_{2,1} == 35);
assert(O5_{1}*O5_{1} == O5_{2} + O5_{1,1} + 1_O5);
assert(dim(O5_{1}*O5_{1}) == 25);

-- O(6) = D_3 Weyl dims (numgens 3, Even)
O6 = schurRing(QQ, getSymbol "o6", 3, GroupActing => "O", OddOrEven => "Even");
assert(dim O6_{1} == 6);
assert(dim O6_{1,1} == 15);
assert(dim O6_{2} == 20);
///

-- Regression: plethysm in Sp and O rings routes via GL Schur
TEST ///
-- Stable GL helper
Sp = schurRing(QQ, getSymbol "sp", GroupActing => "Sp");
O  = schurRing(QQ, getSymbol "oo", GroupActing => "O");

-- plethysm of identity (s_1) is identity
assert(plethysm({1}, Sp_{2})   == Sp_{2});
assert(plethysm({1}, O_{1,1}) == O_{1,1});

-- Sym^2 and Lambda^2 of the defining rep for Sp and O (stable)
assert(plethysm({2},   Sp_{1}) == Sp_{2});                 -- adjoint of Sp
assert(plethysm({1,1}, Sp_{1}) == Sp_{1,1} + 1_Sp);
assert(plethysm({2},   O_{1})  == O_{2} + 1_O);
assert(plethysm({1,1}, O_{1})  == O_{1,1});                 -- adjoint of O

-- Finite-rank plethysm works and matches detour via Schur
Sp4 = schurRing(QQ, getSymbol "sp4", 2, GroupActing => "Sp");
O5  = schurRing(QQ, getSymbol "o5",  2, GroupActing => "O", OddOrEven => "Odd");
assert(plethysm({2}, Sp4_{1}) == Sp4_{2});
assert(plethysm({2}, O5_{1})  == O5_{2} + 1_O5);
///

--------------------------------------------------------------------
-- Branching formulas (King, J. Phys. A 8 (1975), 429-449)
--------------------------------------------------------------------
TEST ///
-- GL branching (stable): Delta(s_lambda) = sum c^lambda_{mu,nu} s_mu (x) s_nu
SGL = schurRing(QQ, getSymbol "sGL", infinity, GroupActing => "GL");
A   = schurRing(QQ, getSymbol "aGL", infinity, GroupActing => "GL");
B   = schurRing(QQ, getSymbol "bGL", infinity, GroupActing => "GL");
h = branch(SGL_{2}, A, B);
assert(h#({2}, {})   == 1);
assert(h#({1}, {1}) == 1);
assert(h#({}, {2})  == 1);
h = branch(SGL_{1,1}, A, B);
assert(h#({1,1}, {})  == 1);
assert(h#({1}, {1})   == 1);
assert(h#({}, {1,1})  == 1);

-- Finite-rank GL(6) -> GL(3) x GL(3): dimensions must match
SGL6 = schurRing(QQ, getSymbol "sGL6", 6, GroupActing => "GL");
A3   = schurRing(QQ, getSymbol "aGL3", 3, GroupActing => "GL");
B3   = schurRing(QQ, getSymbol "bGL3", 3, GroupActing => "GL");
h = branch(SGL6_{2}, A3, B3);
d := sum for k in keys h list lift(h#k, ZZ) * dim(A3_(k#0)) * dim(B3_(k#1));
assert(d == 21);    -- dim Sym^2(C^6) = 21
h = branch(SGL6_{1,1}, A3, B3);
d = sum for k in keys h list lift(h#k, ZZ) * dim(A3_(k#0)) * dim(B3_(k#1));
assert(d == 15);    -- dim Lambda^2(C^6) = 15
///

TEST ///
-- Sp branching (stable): extra terms from delta with parts in pairs.
Sp  = schurRing(QQ, getSymbol "spS", infinity, GroupActing => "Sp");
Asp = schurRing(QQ, getSymbol "asp", infinity, GroupActing => "Sp");
Bsp = schurRing(QQ, getSymbol "bsp", infinity, GroupActing => "Sp");
h = branch(Sp_{1,1}, Asp, Bsp);
-- (mu, nu) from delta = {} (standard LR) plus (,) from delta = {1,1} (cols-even)
assert(h#({}, {})     == 1);
assert(h#({1,1}, {})  == 1);
assert(h#({}, {1,1})  == 1);
assert(h#({1}, {1})   == 1);

-- Finite Sp(4) -> Sp(2) x Sp(2): dimensions match after modification.
RSp4 = schurRing(QQ, getSymbol "sprSp4", 2, GroupActing => "Sp");
R1   = schurRing(QQ, getSymbol "sprA",   1, GroupActing => "Sp");
R2   = schurRing(QQ, getSymbol "sprB",   1, GroupActing => "Sp");
h = branch(RSp4_{1,1}, R1, R2);
d := sum for k in keys h list lift(h#k, ZZ) * dim(R1_(k#0)) * dim(R2_(k#1));
assert(d == 5);
h = branch(RSp4_{2}, R1, R2);
d  = sum for k in keys h list lift(h#k, ZZ) * dim(R1_(k#0)) * dim(R2_(k#1));
assert(d == 10);
h = branch(RSp4_{2,1}, R1, R2);
d  = sum for k in keys h list lift(h#k, ZZ) * dim(R1_(k#0)) * dim(R2_(k#1));
assert(d == 16);
///

TEST ///
-- O branching (stable): extra terms from delta with all parts even.
OS = schurRing(QQ, getSymbol "oS", infinity, GroupActing => "O");
OA = schurRing(QQ, getSymbol "oA", infinity, GroupActing => "O");
OB = schurRing(QQ, getSymbol "oB", infinity, GroupActing => "O");
h = branch(OS_{2}, OA, OB);
assert(h#({}, {})     == 1);  -- from delta = {2}
assert(h#({2}, {})    == 1);
assert(h#({}, {2})    == 1);
assert(h#({1}, {1})   == 1);

-- Finite O(7) -> O(3) x O(4):
RO7 = schurRing(QQ, getSymbol "or7", 3, GroupActing => "O", OddOrEven => "Odd");
RO3 = schurRing(QQ, getSymbol "or3", 1, GroupActing => "O", OddOrEven => "Odd");
RO4 = schurRing(QQ, getSymbol "or4", 2, GroupActing => "O", OddOrEven => "Even");
h = branch(RO7_{1}, RO3, RO4);
d := sum for k in keys h list lift(h#k, ZZ) * dim(RO3_(k#0)) * dim(RO4_(k#1));
assert(d == 7);
h = branch(RO7_{2}, RO3, RO4);
d  = sum for k in keys h list lift(h#k, ZZ) * dim(RO3_(k#0)) * dim(RO4_(k#1));
assert(d == 27);  -- dim(Sym^2 C^7 - trace)

-- ZZ shortcut form:  branch(f, m, n) builds factor rings automatically.
h2 = branch(RO7_{1}, 1, 2);
assert(#keys h2 >= 1);
///

--------------------------------------------------------------------
-- specialize on GL/SL rings and on multi-level towers
--------------------------------------------------------------------
TEST ///
-- Stable GL -> finite GL(n) truncates partitions of length > n.
GLst = schurRing(QQ, getSymbol "glstT", infinity, GroupActing => "GL");
f1 = specialize(GLst_{2,1}, 2);
assert(f1 != 0);
assert((ring f1).GroupActing == "GL");
assert(numgens ring f1 == 2);
-- partition with too many rows vanishes
assert(specialize(GLst_{1,1,1}, 2) == 0);

-- SL specialize collapses determinant row.
SLst = schurRing(QQ, getSymbol "slstT", infinity, GroupActing => "SL");
f2 = specialize(SLst_{2,1}, 2);
assert((ring f2).GroupActing == "SL");
-- In SL(2), s_{2,1} == det * s_{1} -> sl_{1}.
assert(f2 == (ring f2)_{1});
///

TEST ///
-- Multi-level towers: GL over GL.
A = schurRing(QQ, getSymbol "aTowr", 3, GroupActing => "GL");
B = schurRing(A, getSymbol "bTowr", 3, GroupActing => "GL");
-- Multiplication works (previously tested) and specialize works on topmost.
g = specialize(B_{2,1}, 2);
assert(g != 0);
assert((ring g).GroupActing == "GL");
assert(numgens ring g == 2);

-- Multi-level list specialize: outer rank then inner rank.
r = specialize(B_{2,1}, {2, 2});
assert(schurLevel ring r == 2);
assert(numgens ring r == 2);
assert(numgens coefficientRing ring r == 2);
-- infinity placeholder leaves a layer alone.
r2 = specialize(B_{2,1}, {2, infinity});
assert(numgens ring r2 == 2);
assert(numgens coefficientRing ring r2 == 3);
///

TEST ///
-- Stable Sp and O towers over a GL coefficient ring: multiplication.
AG = schurRing(QQ, getSymbol "agTT", 3, GroupActing => "GL");
SpG = schurRing(AG, getSymbol "spgTT", 2, GroupActing => "Sp");
-- Sp(4) over AG: V tensor V = sp_2 + sp_{1,1} + 1.
assert(SpG_{1} * SpG_{1} == SpG_{2} + SpG_{1,1} + 1_SpG);

OG = schurRing(AG, getSymbol "ogTT", 2, GroupActing => "O", OddOrEven => "Odd");
-- V tensor V in O(5) = B_2: o_2 + o_{1,1} + 1.
assert(OG_{1} * OG_{1} == OG_{2} + OG_{1,1} + 1_OG);

-- Specialize Sp-over-GL stays a level-2 ring with same GL coefficient ring.
SpSt = schurRing(AG, getSymbol "spstTT", infinity, GroupActing => "Sp");
f = (AG_{1}) * SpSt_{2,1};
sf = specialize(f, 2);
assert(schurLevel ring sf == 2);
assert(coefficientRing ring sf === AG);
-- Coefficient preserved through specialize.
assert(not zero sf);
///

TEST ///
-- RatGL stable ring: construction, bipartition indexing, Koike product.
S = schurRing(QQ, getSymbol "sRat", infinity, GroupActing => "RatGL");
-- Bipartition indexing uses the IVT symbol: sRat_{{alpha},{beta}}.
assert(sRat_{{1},{}} != 0);
assert(sRat_{1} == sRat_{{1},{}});       -- flat partition syntax lifts with empty second weight
assert(sRat_{2,1} == sRat_{{2,1},{}});
assert(sRat_{{},{}} == 1_S);             -- trivial character

-- Koike product: chi_{(1),()} * chi_{(),(1)} = chi_{(1),(1)} + chi_{(),()}
v    = sRat_{{1},{}};
vdul = sRat_{{},{1}};
assert(v * vdul == sRat_{{1},{1}} + sRat_{{},{}});

-- chi_{(1),(1)}^2 = sum over 5 bipartitions + trivial + 2*adjoint.
prod = (sRat_{{1},{1}})^2;
expectedProd = (sRat_{{2},{2}} + sRat_{{2},{1,1}} + sRat_{{1,1},{2}}
     + sRat_{{1,1},{1,1}} + 2*sRat_{{1},{1}} + sRat_{{},{}});
assert(prod == expectedProd);

-- Koike-Terada boundary case: when ell(alpha) + ell(beta) = n+1 the
-- border-strip length L = ell(alpha)+ell(beta)-n-1 is zero, so the
-- character specializes to 0.
assert(specialize(sRat_{{1,1},{1,1}}, 3) == 0);
assert(specialize(sRat_{{2},{1}},     1) == 0);
assert(specialize(sRat_{{1},{1}},     1) == 0);

-- Non-trivial modification: at GL(3), alpha=(4,3,2,2), beta=(5,2,2,1,1)
-- has ell+ell=9 > n+1=4, and the Koike-Terada rule reduces it by two
-- border-strip passes to the admissible pair ((4,1),(5)) with sign -1.
Tfin3 = schurRing(QQ, getSymbol "tFin3", 3, GroupActing => "RatGL");
assert(toRatGL(sRat_{{4,3,2,2},{5,2,2,1,1}}, Tfin3) == -tFin3_{{4,1},{5}});
-- Another non-trivial pass: at GL(4) the column bipartition (1^3,1^3)
-- drops to (1^2,1^2) with sign -1 (one border-strip step of length 1).
Tfin4 = schurRing(QQ, getSymbol "tFin4", 4, GroupActing => "RatGL");
assert(toRatGL(sRat_{{1,1,1},{1,1,1}}, Tfin4) == -tFin4_{{1,1},{1,1}});

-- Ring-homomorphism property of specialize.
a = sRat_{{2,1},{1}};
b = sRat_{{1},{2}};
assert(specialize(a*b, 3) == specialize(a,3) * specialize(b,3));

-- toRatGL: lift a plain GL character by adjoining trivial second weight.
G = schurRing(QQ, getSymbol "gRG", infinity, GroupActing => "GL");
R = schurRing(QQ, getSymbol "tRat", infinity, GroupActing => "RatGL");
assert(toRatGL(gRG_{2,1} + 3*gRG_{1}, R) == tRat_{{2,1},{}} + 3*tRat_{{1},{}});
///

TEST ///
-- RatGL finite ranks: dimension formula and modification on products.
T = schurRing(QQ, getSymbol "uRat", 3, GroupActing => "RatGL");
-- Fundamental dims at GL(3).
assert(dim(1_T)             == 1);
assert(dim(uRat_{{1},{}})   == 3);  -- standard V
assert(dim(uRat_{{},{1}})   == 3);  -- dual V*
assert(dim(uRat_{{1},{1}})  == 8);  -- adjoint (trace-free part of V tensor V*)
assert(dim(uRat_{{2},{}})   == 6);  -- Sym^2 V
assert(dim(uRat_{{1,1},{}}) == 3);  -- Alt^2 V

-- Adjoint squared at GL(3): dimensions total 64 = 8^2.  chi_{(1,1),(1,1)}
-- hits the Koike-Terada boundary (ell sum 4 = n+1 = 4, so L = 0) and
-- vanishes in the expansion.
adj = uRat_{{1},{1}};
adj2 = adj^2;
assert(dim adj2 == 64);
expectedAdj2 = (uRat_{{2},{2}} + uRat_{{2},{1,1}} + uRat_{{1,1},{2}}
     + 2*uRat_{{1},{1}} + uRat_{{},{}});
assert(adj2 == expectedAdj2);

-- Alt^2 V tensor Alt^2 V* at GL(3) isomorphic to V tensor V* (dim 9).
-- Stable decomposition has chi_{(1,1),(1,1)} + chi_{(1),(1)} + chi_{(),()};
-- the first term is dropped at GL(3).
prodAltAlt = uRat_{{1,1},{}} * uRat_{{},{1,1}};
assert(dim prodAltAlt == 9);
assert(prodAltAlt == uRat_{{1},{1}} + uRat_{{},{}});
///


-------------------------------------------------------------------------
-- Tier B API (toSymm exported, toGL, toSn, convert) + Tier A guards  --
-------------------------------------------------------------------------

TEST ///
-- toSymm round-trip: s_{2,1} -> Jacobi-Trudi -> back to s_{2,1}
S = schurRing(QQ, getSymbol "s", 4);
f = s_{2,1};
g = toSymm f;
assert(ring g =!= S);           -- lands in the associated symmetric ring
assert(toS g == f);              -- round-trips
-- scalar passes through
assert(toSymm(3_S) == toSymm(3));
-- toSymm on a plain number is the identity
assert(toSymm(5) == 5);
///

TEST ///
-- toGL synonym and target-ring promotion.
R = symmetricRing(QQ, 4);
-- h_3 -> Schur is s_3.  (R_{0} ... R_{11} lays out e/p/h; avoid depending
-- on the exact index order by using the named generators.)
debug SchurRings
use R;
assert(toGL(h_3) == toS(h_3));

T = schurRing(QQ, getSymbol "t", 3);
assert(toGL(h_3, T) == t_{3});
assert(ring toGL(h_3, T) === T);

-- toGL rejects a non-GL target.
Sn = schurRing(QQ, getSymbol "nT", 4, GroupActing => "Sn");
assert(try (toGL(h_3, Sn); false) else true);
///

TEST ///
-- toSn: re-label into an Sn ring; respect finite-rank drop.
S  = schurRing(QQ, getSymbol "s", 4);
Sn = schurRing(QQ, getSymbol "n", 4, GroupActing => "Sn");

x = s_{2,1} + 3 * s_{1,1,1};
y = toSn(x, Sn);
assert(ring y === Sn);
assert(y == n_{2,1} + 3 * n_{1,1,1});

-- internalProduct via the re-labeled element: s_{3} corresponds to the
-- trivial S_3-irrep (dim 1) whose internal product with itself is
-- itself.  At schurRing level this exercises the Sn multiplication path.
t = toSn(s_{3}, Sn);
assert(t * t == n_{3});

-- From a symmetric ring: toSn(h_2) should equal toS(h_2) relabeled.
R = symmetricRing(QQ, 4);
use R;
assert(toSn(h_2, Sn) == n_{2});

-- Rejects wrong target.
assert(try (toSn(s_{2,1}, S); false) else true);

-- Finite-rank drop: a partition with more parts than numgens Sn is dropped.
Sn2 = schurRing(QQ, getSymbol "nT2", 2, GroupActing => "Sn");
assert(toSn(s_{1,1,1}, Sn2) == 0_Sn2);
assert(toSn(s_{2} + s_{1,1,1}, Sn2) == nT2_{2});
///

TEST ///
-- convert dispatcher: route by target's GroupActing / ring kind.
SGL = schurRing(QQ, getSymbol "sC", 4);
SSn = schurRing(QQ, getSymbol "nC", 4, GroupActing => "Sn");
SSp = schurRing(QQ, getSymbol "pC", 2, GroupActing => "Sp");
SOrt = schurRing(QQ, getSymbol "oC", 4, GroupActing => "O");
SRat = schurRing(QQ, getSymbol "rC", infinity, GroupActing => "RatGL");

x = sC_{2,1};
assert(convert(x, SSp) == toSp(x, SSp));
assert(convert(x, SOrt) == toO (x, SOrt));
assert(convert(x, SSn) == toSn(x, SSn));
assert(convert(x, SRat) == toRatGL(x, SRat));
assert(convert(x, SGL)  == x);                  -- identity

-- symmetric-ring target
R = symmetricRing(QQ, 4);
cR = convert(x, R);
assert(ring cR === R);
-- Value check: jacobiTrudi({2,1}) in R is the symmetric function of s_{2,1}.
assert(cR == jacobiTrudi({2,1}, R));
///

TEST ///
-- Tier A#2: stable (rank-infinite) SchurRings reject toE/toH/toP with
-- a clear, actionable error.
SinfGL = schurRing(QQ, getSymbol "sInf", infinity);
assert(try (toE(sInf_{2,1}); false) else true);
assert(try (toH(sInf_{2,1}); false) else true);
assert(try (toP(sInf_{2,1}); false) else true);

-- Same for a stable Sn ring.
SinfSn = schurRing(QQ, getSymbol "nInf", infinity, GroupActing => "Sn");
assert(try (toE(nInf_{2,1}); false) else true);

-- Finite-rank Schur ring still works.
Sfin = schurRing(QQ, getSymbol "sFin", 4);
use Sfin;
assert(toE(sFin_{2,1}) != 0);
assert(toH(sFin_{2,1}) != 0);
assert(toP(sFin_{2,1}) != 0);
///

TEST ///
-- Tier A#3 documentation: verify Sp/O -> toE/toH/toP behavior (treated as
-- plain Schur labels).  sp_{2,1} labeled as a Schur partition goes to
-- Jacobi-Trudi of {2,1} = h_1 h_2 - h_3.  (CHARACTER semantics instead
-- would require going through toS first, which lands in a different
-- GL Schur ring whose symmetricRing is distinct -- we document that
-- elsewhere; here we only verify the "label" behavior.)
Sp3 = schurRing(QQ, getSymbol "spA", 3, GroupActing => "Sp");
use Sp3;
hLabel = toH(spA_{2,1});
RH = ring hLabel;
assert(hLabel == RH.hVariable(1) * RH.hVariable(2) - RH.hVariable(3));

-- And toS of sp_{2,1} at Sp(6) is the known Koike inverse s_{2,1} - s_1.
tsElt = toS(spA_{2,1});
glRing = ring tsElt;
assert(tsElt == glRing_{2,1} - glRing_{1});
///

TEST ///
-- Tier C#9: skewSchurExpansion returns the same result on the second
-- call (memoization), and the values are correct.
debug SchurRings;
a = skewSchurExpansion({3,2,1}, {2,1});
b = skewSchurExpansion({3,2,1}, {2,1});
assert(a == b);
-- Known LR content: s_{3,2,1 / 2,1} = s_3 + 2 s_{2,1} + s_{1,1,1}.
-- (The two LR tableaux for s_{2,1} come from the two valid column-strict
-- fillings of the disconnected skew shape with content (2,1).)
-- Sort the partition list so the assertion is order-independent.
expected = sort {({1,1,1}, 1), ({2,1}, 2), ({3}, 1)};
assert(sort a == expected);

-- Trivial corners.
assert(skewSchurExpansion({}, {})     == {({}, 1)});
assert(skewSchurExpansion({3}, {3})   == {({}, 1)});
assert(skewSchurExpansion({3}, {4})   == {});
assert(skewSchurExpansion({2,1}, {2,2}) == {});  -- mu not contained in lam

-- Another known case: s_{2,1 / 1} = s_2 + s_{1,1}.
assert(sort skewSchurExpansion({2,1}, {1}) == sort {({2}, 1), ({1,1}, 1)});
///

TEST ///
-- Tier C#10: the shared cached workSymRing used by plethysm,
-- skewSchurExpansion, and the Kostka routines must NOT rebind the
-- user's global e/h/p/s symbols when it is created or grown.  Before
-- this guard was added, calling plethysm would leak the cache ring into
-- the top-level environment and subsequent expressions like `e_4` or
-- `h_3` would evaluate into the wrong ring, breaking == comparisons.
R = symmetricRing(QQ, 12);
use R;
beforeRing = ring (e_4);
assert(beforeRing === R);
-- Force creation of the cached workSymRing (skewSchurExpansion uses it,
-- as do plethysm(BasicList,RingElement) and kostkaNumber).
lambda = new Partition from {3};
p1 = plethysm(lambda, e_4);
-- After plethysm, user's e/h/p must still resolve inside R.
assert(ring (e_4) === R);
assert(ring (h_3) === R);
assert(ring (p_2) === R);
-- And the plethysm result must live in the user's ring, not the cache.
assert(ring p1 === R);
-- Sanity: equality with the `@` form (h_3 @ e_4) still holds -- both
-- sides must live in R.
p2 = h_3 @ e_4;
assert(ring p2 === R);
assert(p1 == p2);
///

TEST ///
-- kostkaNumber: values against known references (hook-length formula,
-- trivial content, dominance-order zeros), and cross-check against
-- the independent h_mu -> Schur polynomial expansion.
assert(kostkaNumber({3,1},   {1,1,1,1})   == 3);
assert(kostkaNumber({2,2},   {1,1,1,1})   == 2);
assert(kostkaNumber({5},     {2,1,1,1})   == 1);
assert(kostkaNumber({3,2,1}, {3,2,1})     == 1);
assert(kostkaNumber({2,2,1}, {3,1,1})     == 0);  -- mu not dominated
assert(kostkaNumber({},      {})          == 1);
assert(kostkaNumber({3,2},   {2,2,1})     == 2);

-- Cross-check against h_mu -> Schur expansion on every pair in degree 6.
R = symmetricRing(QQ, 6);
use R;
for mu in partitions 6 do (
     muL := toList mu;
     hProd := product for i from 0 to #muL - 1 list (R.hVariable(muL#i));
     refMap := new MutableHashTable;
     for t in listForm toS hProd do refMap#(t#0) = lift(t#1, ZZ);
     for la in partitions 6 do (
	  lamL := toList la;
	  vRef := if refMap#?lamL then refMap#lamL else 0;
	  assert(kostkaNumber(lamL, muL) == vRef);
	  );
     );
///

TEST ///
-- Sn specialize: stable Sn -> finite Sn via truncation by #parts.
-- Also verify the finite -> finite form.
Sn = schurRing(QQ, getSymbol "snA", infinity, GroupActing => "Sn");
use Sn;
f = snA_{3,2} + snA_{4,1,1};  -- two partitions: 2 parts and 3 parts
g = specialize(f, 2);
-- Target ring snfin2 has at most 2 parts, so the 3-part term drops.
assert(ring g =!= Sn);
assert((ring g).GroupActing == "Sn");
assert(numgens ring g == 2);

-- Finite Sn -> smaller finite Sn (the (3,2) term survives; drops nothing yet).
Sn5 = schurRing(QQ, getSymbol "snB", 5, GroupActing => "Sn");
use Sn5;
h = snB_{3,2} + snB_{1,1,1,1,1};  -- both fit in 5 parts
g2 = specialize(h, 2);
assert(numgens ring g2 == 2);
-- Only the 2-part partition survives (the 5-parts one drops).
assert(size g2 == 1);
///

TEST ///
-- toRatGL from a symmetric ring: should route through toS.
-- And toSymm on a RatGL "polynomial" element (no negative components)
-- should recover the symmetric-ring representation.
R = symmetricRing(QQ, 4);
use R;
Rt = schurRing(QQ, getSymbol "rtR", infinity, GroupActing => "RatGL");
x = toRatGL(h_2, Rt);
assert(ring x === Rt);
-- h_2 = s_2, so x should equal rt_{{2},{}}.
assert(x == rtR_{{2},{}});

-- toSymm round-trip on a polynomial RatGL element: rt_{{2,1},{}}
-- should map back to the symmetric function for s_{2,1}.
use Rt;
y = rtR_{{2,1},{}};
ys = toSymm y;
-- Must live in some symmetricRing, and toS(ys) should recover s_{2,1}.
assert((ring ys).?EHPVariables);  -- it's a symmetric ring
-- Going through the associated SchurRing, confirm the partition.
zs = toS ys;
assert((listForm zs)#0#0 == {2,1});
assert((listForm zs)#0#1 == 1);

-- toSymm on a non-polynomial (beta nonempty) element must error cleanly.
z = rtR_{{2,1},{1}};
assert(try (toSymm z; false) else true);
///
