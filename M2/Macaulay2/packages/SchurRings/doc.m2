undocumented {Schur}

doc ///
Key
  SchurRings
Headline
  Rings representing irreducible representations of general linear or symmetric groups
Description
  Text
    This package makes computations in the representation rings of general linear groups 
    and symmetric groups possible.
    
    Given a positive integer {\tt n} we may define a polynomial ring in {\tt n}
    variables over an arbitrary base ring , whose monomials correspond to the irreducible 
    representations of {\tt GL(n)}, and where multiplication is given by the decomposition of 
    the tensor product of representations. We create such a ring in Macaulay2 using the 
    @TO schurRing@ function.
  
  Example
    S = schurRing(QQ,s,4)
    R = schurRing(r,infinity)
    
  Text    
    Note that in the above, {\tt n} is allowed to be equal to {\tt \infty}. However, in this
    version of the package, many of the features from the case {\tt n} finite are missing
    from the infinite case, so the user is advised to use large values for {\tt n} as a
    substitute, whenever necessary.

    We determine the relative dimension of a SchurRing over its base using the @TO numgens@ function:

  Example 
    numgens S
    numgens R
   
  Text
    
    For {\tt k\leq n}, one may interpret the degree
    {\tt k} homogeneous component of a @TO SchurRing@ as the representation ring of the symmetric
    group {\tt S_k}. In this ring, the multiplication is different than the one in 
    the representation ring of {\tt GL(n)}. By default, the elements of a @TO SchurRing@ are 
    interpreted as (virtual) characters of 
    a general linear group. This interpretation is controlled by the option @TO GroupActing@,
    whose default value is "GL". To indicate that the elements of a Schur ring should
    be interpreted as characters of the symmetric group, one has to set the option @TO GroupActing@
    to "Sn".
  
  Example
    Q = schurRing(q,4,GroupActing => "Sn")
    
  Text
        
    A monomial in {\tt S} represents the irreducible representation with a given highest weight. 
    The standard {\tt GL(4)}-representation is
   
  Example
    V = s_1

  Text
    
    We may see the dimension of the corresponding irreducible representation using @TO dim@:

  Example
    dim V

  Text
    Multiplication of elements corresponds to tensor product of representations. The 
    value is computed using a variant of the Littlewood-Richardson rule.
  
  Example
    V * V
    V^3

  Text 
    
    The third symmetric power of {\tt V} is obtained by
     
  Example
    W = s_{3}
    dim W
   
  Text
    
    and the third exterior power of {\tt V} can be obtained using

  Example
    U = s_{1,1,1}
    dim U
    
  Text
  
    Alternatively, one can use the functions @TO symmetricPower@ and @TO exteriorPower@:
    
  Example
    W = symmetricPower(3,V)
    U = exteriorPower(3,V)
    
  Text
  
    We can in fact take symmetric powers and exterior powers of any representation:
    
  Example
    exteriorPower(2,W)
    symmetricPower(2,U)

  Text
  
    and compute even more general forms of @TO plethysm@:
    
  Example
     plethysm(W+U,W+U)
   
  Text
    
    Alternatively, we can use the binary operator @TO symbol \@ @ to compute plethysm:
  
  Example
    s_2 @ s_3
    (W+U) @ (W+U)

  Text
  
    All the above calculations assume that we're dealing with representations of {\tt GL(4)}.
    But as symmetric functions of degree three, {\tt W} and {\tt U}, can be thought of as characters of the
    symmetric group {\tt S_3}. Let us first ``move'' these symmetric functions into a Schur ring
    designed to deal with characters of symmetric groups (like the ring {\tt Q} defined
    above):
    
  Example
    W' = toS(W,Q)
    U' = toS(U,Q)
    
  Text
    
    Now {\tt W'} corresponds to the trivial representation of {\tt S_3},
    and {\tt U'} to the sign representation. As such, we can tensor them together using the
    function @TO internalProduct@, or the binary operator @TO symbol *@.
    
  Example
    W' * U'

  Text
  
    We can generate the class function corresponding to an {\tt S_n}-representation, using
    the function @TO classFunction@:
    
  Example
    cfW = classFunction(W')
    cfU = classFunction(U')
    
  Text
    
    We can multiply class functions together, and transform class functions into symmetric
    functions using the function @TO symmetricFunction@:
    
  Example
    cfWU = cfW * cfU
    symmetricFunction(cfWU,Q)
    
  Text
  
    The result of the previous computation is of course the same as that of taking the product
    of {\tt W'} and {\tt U'}.
    
    We can take exterior and symmetric powers of {\tt S_n}-representations, just as for
    {\tt GL}-modules (compare to {\tt o16} and {\tt o17}):
    
  Example
    exteriorPower(2,W')
    symmetricPower(2,U')

  Text
      
    We can write any symmetric function in terms of the standard {\tt e}- (elementary
    symmetric), {\tt h}- (complete) and {\tt p}- (power sum) bases, using the functions 
    @TO toE@, @TO toH@, @TO toP@ respectively:
    
  Example
    toE U
    toH U
    toP W
    
  Text
    
    These expressions live in the Symmetric ring associated to {\tt S}, which can be obtained
    using the function @TO symmetricRing@:
    
  Example
    A = symmetricRing S
    
  Text
  
    Similarly, any Symmetric ring has a Schur ring attached to it, which can be obtained using
    the function @TO schurRing@:
    
  Example
    schurRing A === S
  
  Text  
  
    We construct tensor products of Schur rings iteratively by allowing Schur rings over
    base rings that are also Schur rings:
    
  Example
    T = schurRing(S,t,3)
    
  Text
    
    The Schur ring {\tt T} can thus be thought of as the representation ring of 
    {\tt GL(V)\times GL(V')}, where {\tt V} is as before a vector space of dimension 
    {\tt 4}, and {\tt V'} is a vector space of dimension {\tt 3}. The representation 
    corresponding to {\tt V'} is
  
  Example
    V' = t_1
  
  Text
   
    The function @TO schurLevel@ indicates the number of Schur rings that have been
    tensored together to obtain any given ring:
    
  Example
    schurLevel T
    schurLevel S
    schurLevel QQ
    
  Text
    
    We can now check Cauchy's formula for decomposing symmetric/exterior powers of a
    tensor product:
    
  Example
    symmetricPower(3,V*V')
    exteriorPower(3,V*V')
  
  Text
  
    We end with the computation of the {\tt GL(n)}- and {\tt S_n}-equivariant resolutions
    of the residue field of a polynomial ring in {\tt n} variables. The function that does
    this calculation, @TO schurResolution@, is based on an empirical method, which gives
    the correct answer in surprisingly many situations.
    
    In the {\tt GL(n)} situation, we are resolving the residue field which as a representation
    has character {\tt 1_S}. The space of linear forms in the polynomial ring 
    considered as a {\tt GL}-representation has character {\tt V = s_1}.
    
  Example
    n = 4
    M = {1_S}
    schurResolution(V,M,DegreeLimit => n)
    
  Text
  
    Not surprisingly, the syzygy modules are generated by the exterior powers of {\tt V}.

    The residue field as a representation of the symmetric group {\tt S_n}
    has character {\tt s_n}. The space of linear forms in the polynomial ring 
    considered as an {\tt S_n}-representation coincides with the permutation representation
    of {\tt S_n}, thus has character {\tt s_n + s_{n-1,1}}.

  Example
    rep = q_n + q_(n-1,1)
    M = {q_n}
    sR = schurResolution(rep,M,DegreeLimit => n)
 
  Text
    
    We can check that the second syzygy module is generated by the second exterior power of the permutation
    representation.
    
  Example
    eP2rep = exteriorPower(2,rep)
    eP2rep == last sR#2#0

  Text

    {\bf Variant character rings.}  Beyond the general linear and
    symmetric groups, the package supports several other families of
    representation rings.  All of them are realized as @TO SchurRing@s,
    with behavior controlled by the option @TO GroupActing@ (and, for
    basis conversions, @TO Basis@).  The current values are:

    $\bullet$ {\tt "GL"} (default): polynomial representations of
    $GL_n$, with basis the Schur functions $s_\lambda$ and the
    Littlewood-Richardson product.

    $\bullet$ {\tt "Sn"}: the representation ring of the symmetric
    group $S_n$, with basis indexed by partitions and product the
    internal product (Kronecker product of $S_n$-characters).  See
    @TO internalProduct@.

    $\bullet$ {\tt "Sp"}: the (universal or finite-rank) character
    ring of the symplectic groups, with basis $sp_\lambda$ and product
    the Newell-Littlewood product.  See @TO toSp@.

    $\bullet$ {\tt "O"}: the (universal or finite-rank) character ring
    of the orthogonal groups, with basis $o_\lambda$.  The finite-rank
    ring distinguishes type $B$ and type $D$ via the option
    @TO OddOrEven@.  See @TO toO@.

    $\bullet$ {\tt "RatGL"}: the ring of rational (i.e. finite-dim'l)
    representations of $GL_n$, whose irreducibles are indexed by
    bipartitions $(\alpha,\beta)$.  See @TO toRatGL@.

    $\bullet$ {\tt "SL"}: the ring of polynomial representations of
    $SL_n$ (rows of length $n$ are killed).

    Orthogonally, the option @TO Basis@ $=>$ {\tt "Monomial"} replaces
    the Schur basis with the monomial symmetric functions $m_\lambda$;
    multiplication is implemented by round-tripping through the Schur
    basis using Kostka numbers (@TO kostkaNumber@, @TO toM@).

  Example
    Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
    sp_{1} * sp_{1}
    O = schurRing(QQ, o, 4, GroupActing => "O");
    o_{1} * o_{2}
    Rat = schurRing(QQ, r, 3, GroupActing => "RatGL");
    r_{{1},{1}} * r_{{1},{}}
    Mon = schurRing(QQ, m, 4, Basis => "Monomial");
    m_{1} * m_{1}

  Text

    {\bf Stable vs.\ finite-rank rings.}  Each of the above flavors
    comes in two sizes.  Passing a positive integer {\tt n} to
    @TO schurRing@ constructs the {\em finite-rank} ring, where
    partitions are restricted in length (to $n$ for {\tt GL}/{\tt Sn}
    and to the appropriate rank for {\tt Sp}/{\tt O}/{\tt RatGL}) and
    the Sam-Snowden-Weyman modification rules are applied
    automatically.  Passing {\tt infinity} instead constructs the
    {\em stable} (universal) ring: a polynomial ring with countably
    many generators, one for each partition, on which all operations
    are performed without modification.  This is the ring of universal
    characters of @TO2 {GroupActing,"Koike--Terada"}@ --- a single
    computation that specializes to every finite rank.

  Example
    StabGL = schurRing(QQ, sg, infinity);
    StabSp = schurRing(QQ, tp, infinity, GroupActing => "Sp");
    tp_{1,1} * tp_{1,1}

  Text

    Having computed in the stable ring, one can @TO specialize@ to a
    finite rank at the end.  The modification rule drops or re-signs
    any partition that exceeds the target rank.

  Example
    f = tp_{1,1} * tp_{1,1};
    specialize(f, 2)     -- inside Sp(4)
    specialize(f, 3)     -- inside Sp(6)

  Text

    {\bf Conversions between variants.}  Every basis conversion is
    reversible and functorial.  The table below summarizes the
    user-level commands for moving between them:

    $\bullet$ @TO toS@, @TO toGL@: to the plain Schur (GL) basis.

    $\bullet$ @TO toE@, @TO toH@, @TO toP@: to the $e/h/p$-basis (lives
    in the associated @TO symmetricRing@).

    $\bullet$ @TO toSymm@: inverse of @TO toS@; pushes a Schur-basis
    element into the symmetric ring.

    $\bullet$ @TO toM@: to the monomial basis.

    $\bullet$ @TO toSp@, @TO toO@: to the symplectic or orthogonal
    character basis, via the inverse Koike branching formulas.

    $\bullet$ @TO toRatGL@: embeds a (polynomial) GL character into
    the ring of rational GL characters.

    $\bullet$ @TO toSn@: carries coefficient data from a GL-style ring
    into an Sn-style ring (same partition labels, different product).

    $\bullet$ @TO convert@: a universal dispatcher --- given any source
    and any target, it picks the right converter.

    $\bullet$ @TO specialize@: stable ring $\to$ finite-rank ring, for
    any variant (applies the appropriate modification rule).

    $\bullet$ @TO branch@: restricts a Schur, Sp, or O character along
    the diagonal of a product of two classical groups.

  Example
    S = schurRing(QQ, s, 5);
    Sp = schurRing(QQ, sp, 2, GroupActing => "Sp");
    convert(s_{2,1}, Sp)
    convert(oo, S)

  Text

    The compatibility of these conversions can be verified against the
    Newell-Littlewood product directly.  For instance, in the stable
    symplectic ring, $sp_{(1)}^2 = sp_{(2)} + sp_{(1,1)} + 1$; equivalently,
    $(s_{(1)})^2 = s_{(2)} + s_{(1,1)}$, and expanding each Schur function
    in the Sp basis via @TO toSp@ and adding yields the same element.

  Text

    {\bf Mathematical background.}  We collect some pointers for
    readers who wish to consult the original sources.

    $\bullet$ {\it Schur-Weyl duality} states that the group algebras
    of $GL_n$ and $S_d$ act on $V^{\otimes d}$ with mutually centralizing
    images, so the decomposition of $V^{\otimes d}$ as a $GL_n \times
    S_d$-module provides a bijection between polynomial representations
    of $GL_n$ of degree $d$ and representations of $S_d$.  The
    @TO2 {"toSn","toSn"}@ / @TO toGL@ pair is the computational
    realization of this bijection.

    $\bullet$ The {\it Frobenius characteristic} identifies the
    representation ring of $S_n$ with the degree-$n$ component of the
    ring of symmetric functions.  Under this isomorphism, the
    character of a virtual representation corresponds to a symmetric
    function, and the internal product of characters to a product
    expressible in the power-sum basis.  See @TO classFunction@ and
    @TO symmetricFunction@.

    $\bullet$ The {\it Koike-Terada universal characters} provide a
    single family of symmetric functions whose specializations at
    $n$ variables recover the irreducible characters of $Sp(2n)$ (or
    $O(n)$) for every $n$.  Equivalently, the stable Sp/O character
    rings form a purely combinatorial object, and the Sam-Snowden-Weyman
    modification rule tells us how to specialize to finite rank.

    $\bullet$ The {\it Newell-Littlewood product} is the multiplication
    in the stable Sp/O ring, expressed as a Littlewood-Richardson-like
    sum:
    $$sp_\mu \cdot sp_\nu = \sum_{\alpha,\beta,\gamma}
      c^\mu_{\alpha,\beta}\, c^\nu_{\alpha,\gamma}\, sp_{\beta\gamma},$$
    where $c^\lambda_{\mu\nu}$ are the Littlewood-Richardson
    coefficients.  The analogous formula holds for $o$.  Our
    implementation replaces this with the equivalent route
    (Schur basis, multiply, back to Sp/O via @TO toSp@/@TO toO@),
    which is both faster and easier to certify.

    $\bullet$ {\it King branching formulas} describe the restriction
    of a character of a classical group to a two-factor subgroup.
    See @TO branch@.

    $\bullet$ {\it Plethysm} $f[g]$ encodes composition of Schur
    functors: $f[g]$ is the character of the $GL(V)$-representation
    obtained by applying the functor with character $f$ to the
    representation with character $g$.  Our implementation agrees with
    the classical definition on power-sum generators:
    $p_n[g(x_1,x_2,\dots)] = g(x_1^n, x_2^n, \dots)$.  See
    @TO plethysm@.

  Text

    {\bf References.}

    $\bullet$ I.\ G.\ Macdonald, {\it Symmetric Functions and Hall
    Polynomials}, 2nd ed., Oxford University Press, 1995.

    $\bullet$ W.\ Fulton, {\it Young Tableaux}, LMSST {\bf 35},
    Cambridge University Press, 1997.

    $\bullet$ W.\ Fulton and J.\ Harris, {\it Representation Theory.
    A First Course}, GTM {\bf 129}, Springer, 1991.

    $\bullet$ R.\ P.\ Stanley, {\it Enumerative Combinatorics, Vol.\ 2},
    Cambridge University Press, 1999.

    $\bullet$ B.\ E.\ Sagan, {\it The Symmetric Group.  Representations,
    Combinatorial Algorithms, and Symmetric Functions}, 2nd ed., GTM
    {\bf 203}, Springer, 2001.

    $\bullet$ J.\ Weyman, {\it Cohomology of Vector Bundles and
    Syzygies}, Cambridge Tracts in Math.\ {\bf 149}, 2003.

    $\bullet$ K.\ Koike, {\it On the decomposition of tensor products
    of the representations of classical groups: by means of universal
    characters}, Adv.\ Math.\ {\bf 74} (1989), 57--86.

    $\bullet$ K.\ Koike and I.\ Terada, {\it Young-diagrammatic methods
    for the representation theory of the classical groups of type
    $B_n$, $C_n$, $D_n$}, J.\ Algebra {\bf 107} (1987), 466--511.

    $\bullet$ R.\ C.\ King, {\it Branching rules for classical Lie
    groups using tensor and spinor methods}, J.\ Phys.\ A {\bf 8}
    (1975), 429--449.

    $\bullet$ S.\ V.\ Sam and A.\ Snowden, {\it Introduction to twisted
    commutative algebras}, arXiv:1209.5122 (2012).

    $\bullet$ S.\ V.\ Sam and A.\ Snowden, {\it Stability patterns in
    representation theory}, Forum Math.\ Sigma {\bf 3} (2015), e11.

    $\bullet$ S.\ V.\ Sam, A.\ Snowden, and J.\ Weyman, {\it Homology of
    Littlewood complexes}, Selecta Math.\ (N.S.) {\bf 19} (2013),
    655--698.
///

doc ///
Key
  SchurRing
  (symbol _,SchurRing,List)
  (symbol _,SchurRing,Sequence)
  (symbol _,SchurRing,ZZ)
Headline
  The class of all Schur rings
Description
  Text
    A Schur ring is the representation ring for the general linear group of {\tt n\times n}
    matrices, and one can be constructed with @TO schurRing@.

  Example
    S = schurRing(QQ,s,4)

  Text

    Alternatively, its elements can be interpreted as virtual characters of symmetric groups,
    by setting the value of the option @TO GroupActing@ to {\tt "Sn"}.

  Example
    Q = schurRing(QQ,q,4,GroupActing => "Sn")
  Text

    The element corresponding to the Young diagram {\tt \{3,2,1\}}, is obtained as follows.

  Example
    s_{3,2,1}

  Text

    Alternatively, we can use a @TO Sequence@ instead of a @TO List@ as the index of a Schur
    function.

  Example
    s_(3,2,1)

  Text

    For Young diagrams with only one row one can use positive integers as subscripts.

  Example
    q_4

  Text

    The name of the Schur ring can be used with a subscript to describe a symmetric
    function.

  Example
    Q_{2,2}
    S_5

  Text

    The dimension of the underlying virtual {\tt GL}-representation can be obtained
    with @TO dim@.

  Example
    dim s_{3,2,1}

  Text

    Multiplication in the ring comes from tensor product of representations.

  Example
    s_{3,2,1} * s_{1,1}
    q_{2,1} * q_{2,1}

  Text

    To extract data in an element in a SchurRing, use @TO "listForm"@:

  Example
    listForm (s_{3})^2
    q_{2,1} * q_{2,1}
    listForm oo

  Text

    By varying the option @TO GroupActing@ one obtains Schur rings for a wide range
    of classical groups.  The {\tt "Sp"} flavor is the representation ring of the
    symplectic group {\tt Sp(2n)}; multiplication in this ring is governed by the
    Newell--Littlewood rule rather than the Littlewood--Richardson rule.

  Example
    Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
    sp_{2,1} * sp_{1,1}
    dim sp_{2,1}
    exteriorPower(2,sp_{1})

  Text

    For the orthogonal flavor {\tt "O"} one must further specify the parity
    of the ambient vector space via @TO OddOrEven@.  For instance,
    {\tt O(5)} (type {\tt B_2}) is obtained as follows.

  Example
    O5 = schurRing(QQ,oo5,5,GroupActing => "O",OddOrEven => "Odd");
    dim oo5_{2,2,1}

  Text

    The rational-{\tt GL} flavor {\tt "RatGL"} implements virtual {\tt GL}-modules
    whose weights may be negative, using the {\tt (\alpha,\beta)} pair convention.

  Example
    Rg = schurRing(QQ,r,3,GroupActing => "RatGL");
    r_({2,1},{1})
    r_({1},{}) * r_({},{1})

  Text

    One can also iterate the construction to form tensor products of Schur rings,
    which is useful for bivariate characters.

  Example
    S = schurRing(QQ,s,4);
    T = schurRing(S,t,3);
    (s_{2,1} + t_{1,1})^2

SeeAlso
  schurRing
  symmetricRing
  GroupActing
  Basis
  OddOrEven
///

doc ///
Key
  schurRing
  (schurRing,Ring,Symbol,ZZ)
  (schurRing,Ring,Thing,ZZ)
  (schurRing,Ring,Symbol)
  (schurRing,Ring,Thing)
  (schurRing,Thing,ZZ)
  (schurRing,Thing)
Headline
  Make a SchurRing
Description
  Text
    {\tt S = schurRing(A,s,n)} creates a Schur ring of degree {\tt n} over the base ring
    {\tt A}, with variables based on the symbol {\tt s}. This is the representation ring
    for the general linear group of {\tt n} by {\tt n} matrices, tensored with the ring
    {\tt A}. If {\tt s} is already assigned a value as a variable in a ring, its base
    symbol will be used, if it is possible to determine.

  Example
    S = schurRing(QQ[x],s,3);
    (x*s_{2,1}+s_3)^2

  Text
    Alternatively, the elements of a Schur ring may be interpreted as characters of
    symmetric groups. To indicate this interpretation, one has to set the value of the option
    @TO GroupActing@ to "Sn".

  Example
    S = schurRing(s,4,GroupActing => "Sn");
    exteriorPower(2,s_(3,1))

  Text
    If the dimension {\tt n} is not specified, then one should think of {\tt S} as the
    full ring of symmetric functions over the base {\tt A}, i.e. there is no restriction
    on the number of parts of the partitions indexing the generators of {\tt S}.

  Example
    S = schurRing(ZZ/5,t)
    (t_(2,1)-t_3)^2

  Text
    If the base ring {\tt A} is not specified, then @TO QQ@ is used instead.

  Example
    S = schurRing(r,2,EHPVariables => (re,rh,rp))
    toH r_(2,1)

  Text

    Beyond the default {\tt GL} flavor, the options @TO GroupActing@
    and @TO Basis@ choose between the symmetric-group ({\tt "Sn"}),
    symplectic ({\tt "Sp"}), orthogonal ({\tt "O"}), special-linear
    ({\tt "SL"}), rational-GL ({\tt "RatGL"}), and monomial-basis
    variants.  See the main @TO SchurRings@ page for the full landscape.

    For example, the symplectic representation ring lives in its own
    @TO SchurRing@, and the orthogonal flavor takes an additional
    @TO OddOrEven@ option to distinguish {\tt O(2n+1)} from {\tt O(2n)}.

  Example
    Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
    sp_{2,1}*sp_{1,1}
    O4 = schurRing(QQ,o,4,GroupActing => "O",OddOrEven => "Even");
    dim o_{3,2,1}

  Text

    The monomial-symmetric-function basis is activated with
    {\tt Basis => "Monomial"}, and multiplication switches to the
    corresponding convolution on weak compositions.

  Example
    M = schurRing(QQ,m,4,Basis => "Monomial");
    m_{2,1} * m_{1}
    toS m_{2,1}

  Text

    One can iterate @TO schurRing@ to produce a tower of Schur rings,
    a convenient setting for bi-graded or bivariate character calculations.

  Example
    S = schurRing(QQ,s,4);
    T = schurRing(S,t,3);
    coefficientRing T
    s_{1,1} * t_{2,1}

  Text

    Passing {\tt n => infinity} (or omitting the dimension) builds the stable
    ring of symmetric functions, with no bound on the number of parts.

  Example
    Sinf = schurRing(ZZ/7,u);
    numgens Sinf
    (u_{2,1})^2

SeeAlso
  SchurRing
  symmetricRing
  GroupActing
  Basis
  OddOrEven
///

doc ///
Key
  (coefficientRing, SchurRing)
Headline
  Coefficient ring of a Schur ring
Usage
  coefficientRing S
Inputs
  S:SchurRing
Description
  Text
    Given a Schur ring {\tt S}, the function returns its coefficient ring.
    The coefficient ring may be any commutative ring that Macaulay2 supports,
    including other Schur rings obtained by iterating @TO schurRing@.

  Example
    S = schurRing(ZZ[x],s,4);
    coefficientRing S
    A = schurRing(QQ,a,3);
    B = schurRing(A,b,2);
    coefficientRing B

  Text

    For a tower of Schur rings, {\tt coefficientRing} peels off one layer at a
    time, allowing the user to navigate the entire construction.

  Example
    T = schurRing(B,t,2);
    coefficientRing T
    coefficientRing coefficientRing T

  Text

    One can build Schur rings over finite-field coefficients as well.

  Example
    P = schurRing(ZZ/5,p,4);
    coefficientRing P
    (p_{2,1} + p_{1})^2
SeeAlso
  schurRing
  SchurRing
///

document {
     Key => {SchurRingIndexedVariableTable,(symbol _,SchurRingIndexedVariableTable,Thing)},
     "This class is used as part of the implementation of a type of indexed variable used just for Schur rings.",
     PARA{"It is what makes the partition-indexed notation ", TT "s_{3,2,1}", " or ",
	  TT "s_(3,2,1)", " work: when you write ", TT "s", " in a Schur ring, ", TT "s",
	  " is bound to a ", TO "SchurRingIndexedVariableTable", " whose ", TT "_",
	  " method accepts a list, a sequence, or an integer and returns the corresponding",
	  " Schur-basis element.  The same mechanism drives the partition-indexed notation
	  in every flavor of ", TO "SchurRing", " -- monomial, symplectic, orthogonal,
	  symmetric-group, and rational-GL variants all share this indexing interface."},
     EXAMPLE {
	  "S = schurRing(QQ, s, 4);",
	  "class s",
	  "s_{2,1}",
	  "s_(2,1)",
	  "s_2"
	  },
     PARA{"Each Schur ring comes with its own indexed variable table, and partitions with
	  many parts print in the same compact way."},
     EXAMPLE {
	  "T = schurRing(QQ, t, 5);",
	  "t_{4,3,2,1}",
	  "t_{1,1,1,1}",
	  "dim t_{4,3,2,1}"
	  },
     PARA{"For rings with ", TT "Basis => \"Monomial\"", ", the same table selects the
	  monomial-symmetric-function basis elements instead."},
     EXAMPLE {
	  "M = schurRing(QQ, m, 4, Basis => \"Monomial\");",
	  "m_{2,1}",
	  "m_{2,1} * m_{1}"
	  },
     PARA{"In the rational-GL flavor, subscripts are pairs of lists encoding positive
	  and negative weights."},
     EXAMPLE {
	  "R = schurRing(QQ, r, 3, GroupActing => \"RatGL\");",
	  "r_({2,1},{1})"
	  },
     SeeAlso => { IndexedVariableTable, SchurRing, schurRing }
     }

doc ///
Key
  symmetricRing
  (symmetricRing,Ring,ZZ)
  (symmetricRing,ZZ)
Headline
  Make a Symmetric ring
Usage
  symmetricRing(A,n)
  symmetricRing n
Inputs
  A:Ring
  n:ZZ
Description
  Text

    The method {\tt symmetricRing} creates a Symmetric ring of dimension {\tt n} over a base ring
    {\tt A}. This is the subring of the ring of symmetric functions over the base {\tt A}
    consisting of polynomials in the first {\tt n} elementary (or complete, or power sum)
    symmetric functions. If {\tt A} is not specified, then it is assumed to be @TO QQ@.

  Example
    R = symmetricRing(QQ[x,y,z],4)
    e_2*x+y*p_3+h_2
    toS oo

  Text

    The elements of a Symmetric ring can be interpreted as characters of either symmetric or
    general linear groups. This is controlled by the value of the option @TO GroupActing@, whose
    default value is "GL" (general linear group). The other possibility for its value is
    "Sn" (symmetric group).

  Example
    R = symmetricRing(QQ,3,GroupActing => "Sn")
    toE symmetricPower(2,e_2)

  Text

    The three symmetric-function generators -- elementary, complete, and power
    sum -- are all accessible in the same ring, and conversions between them
    are handled by @TO toE@, @TO toH@, @TO toP@, and @TO toS@.

  Example
    R = symmetricRing(QQ,5);
    toS ((R.pVariable 2)^3)
    toH (R.pVariable 2)
    toS (R.eVariable 2 * R.hVariable 2)

  Text

    A Symmetric ring can be built over any commutative base; coefficients can
    be polynomial rings, modular rings, or even other Schur rings.

  Example
    R7 = symmetricRing(ZZ/7,4);
    (R7.eVariable 2)^3
    Rt = symmetricRing(QQ[t],3,GroupActing => "Sn");
    toS(Rt.eVariable 2 * t)

SeeAlso
  SchurRing
  schurRing
  eVariable
  hVariable
  pVariable
///

doc ///
Key
  eVariable
Headline
  Elementary symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.eVariable} is a function
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th elementary symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.

  Example
    R = symmetricRing(QQ,5,EHPVariables => (a,b,c));
    R.eVariable 3

  Text

    The elementary generators are related to the Schur basis via {\tt e_k =
    s_{1^k}}; @TO toS@ realizes this on any product of {\tt e}-variables.

  Example
    R = symmetricRing(QQ,4);
    toS (R.eVariable 2)
    toS ((R.eVariable 2)^2)

  Text

    One may also use {\tt eVariable} from a Symmetric ring built with a
    non-standard coefficient ring or group acting on it.

  Example
    Rsn = symmetricRing(QQ,4,GroupActing => "Sn");
    toS symmetricPower(2, Rsn.eVariable 2)
    Rmod = symmetricRing(ZZ/5,3);
    (Rmod.eVariable 1)^5

SeeAlso
  hVariable
  pVariable
  symmetricRing
  toS
///

doc ///
Key
  hVariable
Headline
  Complete symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.hVariable} is a function
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th complete symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.

  Example
    R = symmetricRing(QQ,2,EHPVariables => (x,y,z));
    R.hVariable 2

  Text

    Complete symmetric functions translate to one-row Schur functions via
    {\tt h_k = s_k}; this is the other half of the {\tt e/h}-duality.

  Example
    R = symmetricRing(QQ,4);
    toS (R.hVariable 3)
    toS (R.eVariable 2 * R.hVariable 2)

  Text

    They interact cleanly with power sums: Newton's identities are realized by
    @TO toE@ and @TO toP@, and here we convert a cube of {\tt h_2} into the
    power-sum basis.

  Example
    R = symmetricRing(QQ,3);
    toP ((R.hVariable 2)^2)

  Text

    Complete symmetric functions are available over any coefficient ring.

  Example
    Rt = symmetricRing(QQ[t],3);
    toS (Rt.hVariable 2 + t * Rt.eVariable 2)

SeeAlso
  eVariable
  pVariable
  symmetricRing
  toS
///

doc ///
Key
  pVariable
Headline
  Power-sum symmetric functions in a Symmetric ring
Description
  Text
    For a Symmetric ring {\tt R} of dimension {\tt n}, {\tt R.pVariable} is a function
    which assigns to each index {\tt 1\leq i\leq n} the {\tt i}-th power-sum symmetric
    function. If {\tt i} is outside the given bounds, an error is returned.

  Example
    R = symmetricRing(QQ,4);
    R.pVariable 2

  Text

    Power sums are an algebraically independent generating set for the ring of
    symmetric functions over @TO QQ@, and products of {\tt p_i}'s expand into
    the Schur basis using the character table of the symmetric group.

  Example
    R = symmetricRing(QQ,5);
    toS ((R.pVariable 2)^3)

  Text

    The same variable in a Symmetric ring with {\tt GroupActing => "Sn"}
    represents a virtual character of a symmetric group, and conversion to
    elementary and complete generators is handled by @TO toE@ and @TO toH@.

  Example
    Rsn = symmetricRing(QQ,4,GroupActing => "Sn");
    toE (Rsn.pVariable 2)
    toH (Rsn.pVariable 3)

  Text

    Power sums are also useful as the natural input to @TO plethysm@.

  Example
    R = symmetricRing(QQ,4);
    toS plethysm(R.pVariable 2, R.hVariable 2)

SeeAlso
  eVariable
  hVariable
  symmetricRing
  toS
  plethysm
///

doc ///
   Key
     (numgens,SchurRing)
   Headline
     Number of generators of Schur ring.
   Description
      Text

     	  Given a Schur ring {\tt S}, the function {\tt numgens} outputs the number
	  of generators of {\tt S}. This is equal to the relative dimension of {\tt S}
	  over its base ring, and also to the maximal number of parts of a partition
	  allowed as an index for the elements of {\tt S}.

      Example
      	  R = schurRing(QQ,r,6);
	  numgens R
	  S = schurRing(s);
	  numgens S

      Text

	  When a Schur ring is built on top of another Schur ring as its coefficient
	  ring, {\tt numgens} measures only the outermost (relative) layer.  Nested
	  Schur rings thus model tensor products of representation rings, each layer
	  tracked by its own @TO numgens@.

      Example
	  A = schurRing(QQ,a,3);
	  B = schurRing(A,b,2);
	  numgens B
	  numgens coefficientRing B

      Text

	  The {\tt numgens} value for a stable Schur ring (built with {\tt n => infinity}
	  or without an explicit rank) is @TO infinity@, reflecting the fact that no
	  partition length is excluded.

      Example
	  Sinf = schurRing(QQ,u);
	  numgens Sinf
	  u_{4,3,2,1}

      Text

	  The same rule applies to the symplectic and orthogonal flavors.

      Example
	  Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
	  numgens Sp
	  O = schurRing(QQ,o,5,GroupActing => "O",OddOrEven => "Odd");
	  numgens O
   SeeAlso
     schurRing
     SchurRing
///

doc ///
   Key
     (schurRing,Ring)
   Headline
     The Schur ring corresponding to a given Symmetric ring.
   Usage
     S = schurRing R
   Inputs
     R:Ring
   Outputs
     S:SchurRing
   Description
      Text

     	  Given a ring {\tt R}, the function {\tt schurRing} attempts to return a
	  Schur ring {\tt S} that is associated to {\tt R} in a natural way. Namely, if
	  the attribute {\tt R.Schur} points to a Schur ring, then the function returns
	  that ring. If {\tt R} is already a Schur ring, then the ring {\tt R} is returned.
	  Otherwise, if the Schur level of {\tt R} is at least one, then the function
	  constructs a Schur ring over the base ring {\tt A} of {\tt R}, having the same
	  relative dimension over {\tt A} as {\tt R}. If the Schur level of {\tt R} is zero, then
	  an error is returned.

      Example
      	  R = schurRing(QQ,r,6);
	  schurRing R
	  Q = symmetricRing(QQ,3);
	  A = schurRing Q;
	  schurRing Q

      Text

	  Passing an existing Schur ring to {\tt schurRing} simply returns it, which is
	  convenient as a guard when a function wants to accept either a Symmetric ring
	  or a Schur ring as input.

      Example
	  S = schurRing(QQ,s,4);
	  schurRing S === S

      Text

	  For a Symmetric ring with the {\tt "Sn"} interpretation, the associated Schur
	  ring inherits this flavor and is cached on the ring.

      Example
	  Rsn = symmetricRing(QQ,4,GroupActing => "Sn");
	  Ssn = schurRing Rsn;
	  numgens Ssn

      Text

	  The construction also works over polynomial coefficient rings, producing a
	  Schur ring with the same parameters as coefficients.

      Example
	  Rx = symmetricRing(QQ[x],3);
	  Sx = schurRing Rx;
	  coefficientRing Sx
   SeeAlso
     symmetricRing
     SchurRing
     schurRing
///

doc ///
   Key
     (symmetricRing,Ring)
   Headline
     The Symmetric ring corresponding to a given (Schur) ring.
   Usage
     R = symmetricRing S
   Inputs
     S:Ring
   Outputs
     R:Ring
   Description
      Text

     	  Given a (Schur) ring {\tt S}, the function {\tt symmetricRing} returns a
	  (Symmetric) ring {\tt R} that is associated to {\tt S} in a natural way. Namely, if
	  the attribute {\tt S.symmetricRing} points to a ring, then the function returns
	  that ring. If {\tt S} is not a Schur ring, then the function returns {\tt S}.
	  Otherwise, if {\tt S} is a Schur ring, then the function
	  constructs a polynomial ring over the Symmetric ring {\tt R_A} of the base ring {\tt A} of
	  {\tt R}, having the same relative dimension over {\tt R_A} as {\tt S} over {\tt A}.

      Example
      	  A = schurRing(QQ,a,6);
	  B = schurRing(A,b,3);
	  symmetricRing B
	  symmetricRing ZZ

      Text

	  For a plain Schur ring, the associated Symmetric ring is a polynomial ring
	  in the elementary, complete, and power-sum generators.

      Example
	  S = schurRing(QQ,s,4);
	  R = symmetricRing S;
	  (R.eVariable 2)^2
	  toS ((R.eVariable 2)^2)

      Text

	  The construction plays well with coefficient rings of different flavors, and
	  with the symmetric-group interpretation via {\tt GroupActing => "Sn"}.

      Example
	  Ssn = schurRing(QQ,c,4,GroupActing => "Sn");
	  Rsn = symmetricRing Ssn;
	  numgens Rsn

      Text

	  On a tower of Schur rings, {\tt symmetricRing} produces a tower of Symmetric
	  rings mirroring the coefficient-ring structure.

      Example
	  A = schurRing(QQ,a,3);
	  B = schurRing(A,b,2);
	  RB = symmetricRing B;
	  coefficientRing RB
   SeeAlso
     schurRing
     SchurRing
     symmetricRing
///

doc ///
   Key
     toS
     (toS,RingElement)
     (toS,RingElement,SchurRing)
   Headline
     Schur (s-) basis representation
   Usage
     fs = toS f
     fs = toS(f,S)
   Description
      Text

    	Given a symmetric function {\tt f}, the function 
        {\tt toS} yields a representation of {\tt f} as a linear
	combination of Schur functions. 

     	If {\tt f} is an element of a Symmetric ring {\tt R} and the output Schur ring {\tt S}
	is not specified, then the output {\tt fs} is an element of the Schur ring 
	associated to {\tt R} (see @TO schurRing@).
        
      Example
        R = symmetricRing(QQ,4);
        fs = toS(e_1*h_2+p_3)
        S = schurRing(s,2);
	toS(fs,S)
	
      Text
      
        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = symmetricRing(4, EHPVariables => (a,b,c), SVariable => r);
	S = symmetricRing(R, 2, EHPVariables => (x,y,z), SVariable => s);
	T = symmetricRing(S, 3, SVariable => t);
	A = schurRing T;
	f = a_3*x_2*e_1 - b_1*z_2*p_3
	toS f

      Text

        The Jacobi-Trudi determinant $s_\lambda = \det(h_{\lambda_i - i + j})$
	is inverted by {\tt toS}: feeding a Jacobi-Trudi expression back
	through {\tt toS} recovers a single Schur label.

      Example
        R = symmetricRing(QQ,5);
        toS jacobiTrudi({3,2,1},R)

      Text

        {\bf GL to Sn.}  The GL Schur basis and the Frobenius-characteristic
	(Sn) basis share the same partition index set.  Combining {\tt toS}
	with @TO toSn@ carries coefficient data between the two flavors.

      Example
        G  = schurRing(QQ, g, 4);
        Sn = schurRing(QQ, n, 4, GroupActing => "Sn");
        a  = toSn(g_{2,1}, Sn)
        toS(a, G)

      Text

        {\bf Sp {\rm to} S {\rm to} Sp.}  Elements of a variant-basis
	ring (e.g.\ {\tt "Sp"}, {\tt "O"}) can be expanded into the plain
	GL Schur basis with {\tt toS}, and the resulting combination
	re-expressed in a variant-basis ring via @TO convert@.

      Example
        Sp = schurRing(QQ, sp, 4, GroupActing => "Sp");
        f  = toS sp_{2,1}
        Sp' = schurRing(QQ, sq, 4, GroupActing => "Sp");
        convert(f, Sp')

   SeeAlso
     toH
     toE
     toP
     toSn
     toSymm
     jacobiTrudi
///

doc ///
  Key
    toE
    (toE,RingElement)
  Headline
     Elementary symmetric (e-) basis representation
  Usage
     fe = toE f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fe:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toE} yields a representation of {\tt f} as a polynomial
	  in the elementary symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fe} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toE(h_3*e_3)
	  S = schurRing(s,4)
	  toE S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.

      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toE f

      Text

        {\bf Variant bases.}  When {\tt f} lives in a @TO SchurRing@
	with {\tt GroupActing} in $\{${\tt "Sp"}, {\tt "O"},
	{\tt "RatGL"}, {\tt "SL"}$\}$ or {\tt Basis => "Monomial"},
	the output is obtained by treating the partition labels of
	{\tt f} as plain Schur labels and applying the Jacobi-Trudi
	determinant.  This is {\it not} the same as the symmetric
	function representing the character of the underlying irrep.
	To obtain the character expansion, first call @TO toS@ to get
	the plain GL Schur expansion, and then compose with {\tt toE}:
	{\tt toE(toS f)}.

      Example
        Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
        toE sp_{2,1}
        toE toS sp_{2,1}

      Text

        Composing {\tt toE} with @TO toS@ lets you convert an arbitrary
	$e$/$h$/$p$-expression into the $e$-basis via the Schur basis,
	which is sometimes numerically cleaner than Newton's identities.

      Example
        R = symmetricRing(QQ,4);
        toE toS (e_1 * h_2 + p_3)
        toE toS (h_2^2)

      Text

        The names of the output variables are controlled by
	@TO EHPVariables@; {\tt toE} writes its result in the first
	slot of that triple.

      Example
        Rxyz = symmetricRing(QQ, 4, EHPVariables => (x,y,z));
        toE(y_3)
        toE(z_2)

      Text

        {\bf Stable rings.}  If {\tt f} lives in a rank-infinite
	SchurRing (created with {\tt numgens => infinity}), {\tt toE}
	raises an error: the associated symmetric ring can only be
	constructed at finite rank.  Use @TO specialize@ to fix a rank
	first.

      Example
        Sinf = schurRing(QQ, u, infinity);
        try toE u_{2,1} else "error: stable ring has no symmetricRing"
        toE specialize(u_{2,1}, 3)

  SeeAlso
    toH
    toS
    toP
    toSymm
    specialize
    EHPVariables
///

doc ///
  Key
    toH
    (toH,RingElement)
  Headline
     Complete symmetric (h-) basis representation
  Usage
     fh = toH f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fh:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toH} yields a representation of {\tt f} as a polynomial
	  in the complete symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fh} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toH(h_3*e_3)
	  S = schurRing(s,4)
	  toH S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toH f

      Text

        {\bf Variant bases.}  When {\tt f} lives in a @TO SchurRing@
	with {\tt GroupActing} in $\{${\tt "Sp"}, {\tt "O"},
	{\tt "RatGL"}, {\tt "SL"}$\}$ or {\tt Basis => "Monomial"},
	partition labels are treated as plain Schur labels (Jacobi-Trudi
	is applied directly); the result is not the same as the
	character expansion.  Use {\tt toH(toS f)} for the latter.

      Example
        O = schurRing(QQ, o, 4, GroupActing => "O");
        toH o_{2,1}
        toH toS o_{2,1}

      Text

        Roundtripping through the Schur basis recovers the original
	$h$-expression (up to the usual commutative-polynomial rewriting).

      Example
        R = symmetricRing(QQ,5);
        toH toS (h_2 * h_3)
        toH toE toS (h_2^2)

      Text

        The names of the output variables follow @TO EHPVariables@;
	{\tt toH} writes its result using the second name in that triple.

      Example
        Rxyz = symmetricRing(QQ, 4, EHPVariables => (x,y,z));
        toH(x_1 * x_2)
        toH(z_3)

      Text

        {\bf Stable rings.}  Rank-infinite SchurRings raise an error;
	use @TO specialize@ to fix a rank first.

      Example
        Sinf = schurRing(QQ, v, infinity);
        try toH v_{3,1} else "error: stable ring has no symmetricRing"
        toH specialize(v_{3,1}, 4)

  SeeAlso
    toE
    toS
    toP
    toSymm
    specialize
    EHPVariables
///

doc ///
  Key
    toP
    (toP,RingElement)
  Headline
     Power-sum (p-) basis representation
  Usage
     fp = toP f
  Inputs
     f:RingElement
       element of a Symmetric or Schur ring
  Outputs
     fp:RingElement
        element of a Symmetric ring
  Description
      Text

          Given a symmetric function {\tt f}, the function 
          {\tt toP} yields a representation of {\tt f} as a polynomial
	  in the power-sum symmetric functions.

  	  If {\tt f} is an element of a Schur ring {\tt S} then the output {\tt fp} is an 
	  element of the Symmetric ring associated to {\tt S} (see @TO symmetricRing@).

      Example
      	  R = symmetricRing 7;
	  toP(h_3*e_3)
	  S = schurRing(s,4)
	  toP S_{3,2,1}

      Text

        This also works over tensor products of Symmetric/Schur rings.
	
      Example
        R = schurRing(r, 4, EHPVariables => (a,b,c));
	S = schurRing(R, s, 2, EHPVariables => (x,y,z));
	T = schurRing(S, t, 3);
	A = symmetricRing T;
	f = (r_1+s_1+t_1)^2
	toP f

      Text

        {\bf Variant bases.}  Same caveat as @TO toE@ and @TO toH@: on
	elements of a variant-basis Schur ring, labels are treated as
	plain Schur labels; use {\tt toP(toS f)} to get the character
	expansion in the power-sum basis.

      Example
        Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
        toP sp_{2,1}
        toP toS sp_{2,1}

      Text

        Composing {\tt toP} with @TO toE@ or @TO toH@ exercises
	Newton's identities: {\tt toP(toE f)} returns {\tt f} in the
	$p$-basis, and {\tt toH(toP f)} returns it back in the $h$-basis.

      Example
        R = symmetricRing(QQ,5);
        toP toE (p_2 * p_3)
        toH toP (h_2 * h_3)

      Text

        The output variables for {\tt toP} use the third slot of
	@TO EHPVariables@.

      Example
        Rxyz = symmetricRing(QQ, 4, EHPVariables => (x,y,z));
        toP(y_3)
        toP(x_1 * x_2)

      Text

        {\bf Stable rings.}  Rank-infinite SchurRings raise an error;
	use @TO specialize@ to fix a rank first.

      Example
        Sinf = schurRing(QQ, w, infinity);
        try toP w_{2,2} else "error: stable ring has no symmetricRing"
        toP specialize(w_{2,2}, 4)

  SeeAlso
    toH
    toS
    toE
    toSymm
    specialize
    EHPVariables
///

doc ///
Key
  jacobiTrudi
  (jacobiTrudi,BasicList,Ring)
Headline
  Jacobi-Trudi determinant
Usage
  f = jacobiTrudi(lambda,R)
Inputs
  lambda:BasicList
         a nonincreasing list of integers, or a partition
  R:Ring
    a Symmetric ring
Outputs
  f:RingElement
    an element of a Symmetric ring
Description
  Text

    Given a partition {\tt lambda} and Symmetric ring {\tt R},
    the method evaluates the Jacobi-Trudi determinant corresponding
    to the partition {\tt lambda}, yielding a representation of
    the Schur function {\tt s_{lambda}} as a symmetric function
    in {\tt R}. The default option is to represent this symmetric
    function in terms of {\tt e-}polynomials.

  Example
    R = symmetricRing(QQ,10);
    jacobiTrudi({3,2,2,1},R)
    jacobiTrudi(new Partition from {4,4,1},R,EorH => "H")
    toS oo

  Text

    Selecting {\tt EorH => "H"} uses the conjugate determinant
    formula $s_\lambda = \det(h_{\lambda_i - i + j})$.  The two
    branches produce different {\tt e}- vs {\tt h}-polynomials
    but always represent the same Schur function:

  Example
    R = symmetricRing(QQ,8);
    lam = {4,3,2,1};
    fe = jacobiTrudi(lam,R,EorH => "E");
    fh = jacobiTrudi(lam,R,EorH => "H");
    toS fe
    toS fh
    toS fe == toS fh

  Text

    The routine caches intermediate subdeterminants on the ring
    via @TO [jacobiTrudi,Memoize]@, so a second call on a large
    partition returns almost instantly:

  Example
    R = symmetricRing(QQ,6);
    elapsedTime jacobiTrudi({4,3,2,1},R);
    elapsedTime jacobiTrudi({4,3,2,1},R);

  Text

    Passing a partition through @TO toSymm@ applied to the
    corresponding Schur label reproduces the Jacobi-Trudi output:

  Example
    R = symmetricRing(QQ,5);
    S = schurRing R;
    jacobiTrudi({3,2,1},R) == toSymm(S_{3,2,1})

  Text

    {\tt jacobiTrudi} works over tensor products of Symmetric
    rings, producing a determinant in the outermost set of
    generators:

  Example
    R = symmetricRing(QQ, 4, EHPVariables => (a,b,c));
    T = symmetricRing(R, 3, EHPVariables => (x,y,z));
    jacobiTrudi({2,1},T)
    jacobiTrudi({3,2},T, EorH => "H")
///

doc///
   Key
     EorH
     [jacobiTrudi,EorH]
   Headline
     e- or h- representation of Jacobi-Trudi determinant
   Usage
     EorH => s
   Inputs
     s:String
       either "E" or "H"
   Description
     Text
       This option allows one to choose between evaluating the
       Jacobi-Trudi determinant in the {\tt e}- or {\tt h}- basis.
       If the length of the conjugate partition {\tt lambda'} is
       larger than the length of {\tt lambda}, then it is
       computationally less expensive to set the option {\tt EorH}
       to {\tt "H"}. Otherwise, the default value {\tt "E"} is more
       efficient.

     Example
       R = symmetricRing(QQ,8);
       fe = jacobiTrudi({2,2,2,2,2},R,EorH => "E");
       fh = jacobiTrudi({2,2,2,2,2},R,EorH => "H");
       fe
       fh

     Text

       Although the two polynomials are superficially different,
       they are equal as symmetric functions, as seen after
       applying @TO toS@:

     Example
       toS fe == toS fh

     Text

       When the conjugate partition is much longer than
       {\tt lambda} itself, the {\tt "H"}-branch requires a
       smaller determinant and runs measurably faster.  For
       example on {\tt lambda = (10)} the conjugate is
       $(1^{10})$, so {\tt "H"} only sets up a 1x1 determinant:

     Example
       R = symmetricRing(QQ,12);
       elapsedTime jacobiTrudi({10},R,EorH => "E",Memoize => false);
       elapsedTime jacobiTrudi({10},R,EorH => "H",Memoize => false);
///

doc///
   Key
     [jacobiTrudi,Memoize]
   Headline
     Store values of the jacobiTrudi function.
   Usage
     Memoize => b
   Inputs
     b:Boolean
   Description
     Text

       If the option is set to {\tt true} then all the values of the jacobiTrudi
       function that are computed are recorded into a special hash table attached
       to the symmetric ring inside which the computations are done.  This makes
       repeated evaluations on related partitions substantially faster, at the
       cost of some extra memory in the ring.

     Example
       R = symmetricRing(QQ,6);
       jacobiTrudi({4,3,2,1},R,Memoize => true) == jacobiTrudi({4,3,2,1},R,Memoize => false)
       elapsedTime jacobiTrudi({5,4,3,2,1},R,Memoize => true);
       elapsedTime jacobiTrudi({5,4,3,2,1},R,Memoize => true);
       elapsedTime jacobiTrudi({5,4,3,2,1},R,Memoize => false);
///

doc ///
Key
  plethysm
  (plethysm,RingElement,RingElement)
Headline
  Plethystic operations on representations
Usage
  pl = plethysm(f,g)
  pl = f @ g
Inputs
  f:RingElement
    element of Symmetric ring or Schur ring
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of the ring of {\tt g}
Description
  Text
    Given a symmetric function {\tt f} and the character {\tt g} of a virtual representation of a product
    of general linear and symmetric groups, the method computes the character of the
    plethystic composition of {\tt f} and {\tt g}. The result of this operation will be an element of
    the ring of {\tt g}. We use the binary operator @TO symbol \@ @ as a synonym for the plethysm function.

  Example
    R = symmetricRing(QQ,5);
    pl = plethysm(h_2,h_3)
    toS pl
    S = schurRing(QQ,q,3);
    h_2 @ q_{2,1}
    plethysm(q_{2,1},q_{2,1})
    T = schurRing(S,t,2,GroupActing => "Sn");
    plethysm(q_{1,1,1}-q_{2,1}+q_{3},q_{2,1}*t_2-t_{1,1})
    p_3 @ (q_{2,1}*t_2-t_{1,1})

  Text

    Since the power-sum basis behaves multiplicatively under
    plethysm, one has the identity
    {\tt plethysm(p_m, p_n) = p_{mn}}, and more generally any
    complete/power-sum pair commutes under plethysm, as shown
    below:

  Example
    R = symmetricRing(QQ,8);
    toS plethysm(p_2,p_3) == toS p_6
    toS plethysm(h_3,p_2) == toS plethysm(p_2,h_3)

  Text

    The symmetric and antisymmetric squares of an irreducible
    representation decompose via {\tt Sym^2 = plethysm(\{2\},-)}
    and {\tt \Lambda^2 = plethysm(\{1,1\},-)}, and their sum
    recovers the ordinary tensor square:

  Example
    S = schurRing(QQ,s,4);
    sym2 = plethysm({2},s_{2,1})
    wedge2 = plethysm({1,1},s_{2,1})
    s_{2,1}*s_{2,1} - sym2 - wedge2

  Text

    Plethysm makes sense for representations of symmetric groups
    as well.  In an {\tt Sn}-flavored ring, {\tt plethysm(lambda, chi)}
    applies the Schur functor {\tt S_lambda} to the
    {\tt Sn}-representation {\tt chi}:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    plethysm({2,1},c_{2,1,1})

  Text

    Plethysm also works over tensor products of Schur rings,
    mixing a GL factor and an {\tt Sn} factor:

  Example
    G = schurRing(QQ,g,3);
    N = schurRing(G,n,3,GroupActing => "Sn");
    plethysm({2},g_1*n_{2,1})
///

doc ///
Key
  (plethysm,BasicList,RingElement)
Headline
  Plethystic operations on representations
Usage
  pl = plethysm(lambda,g)
Inputs
  lambda:BasicList
         nonincreasing sequence of positive integers, or partition
  g:RingElement
    element of Symmetric ring or Schur ring
Outputs
  pl:RingElement
     element of the ring of {\tt g}
Description
  Text

    The method computes the character of the representation obtained by applying the Schur functor
    {\tt S_{\lambda}} to the representation with character {\tt g}, where
    {\tt \lambda} is a partition.

  Example
    R = symmetricRing(QQ,3);
    S = schurRing(QQ,q,3);
    toE plethysm({2,1},e_1*e_2-e_3)
    plethysm({2,1,1},q_{1,1})
    T = schurRing(S,t,4,GroupActing => "Sn");
    plethysm({1,1},q_1*t_{3,1})

  Text

    Even simple plethysms of Schur functions are not obvious a priori.
    For example, {\tt Sym^2} of the antisymmetric square
    $\Lambda^2 V = S_{1,1}V$ breaks up as:

  Example
    S = schurRing(QQ,s,4);
    plethysm({2},s_{1,1})

  Text

    Applying a partition directly lets one extract isotypic
    summands, e.g.\ the two pieces of {\tt V^{\otimes 2}} for
    an {\tt Sn}-representation:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    sym2chi = plethysm({2}, c_{3,1})
    wed2chi = plethysm({1,1}, c_{3,1})
    c_{3,1}*c_{3,1} - sym2chi - wed2chi

  Text

    For representations of products of groups, the plethysm is
    applied diagonally; here on a GL x GL tensor product:

  Example
    A = schurRing(QQ,a,3);
    B = schurRing(A,b,2);
    plethysm({2},a_1*b_1)
///

doc ///
Key
  (plethysm,RingElement,ClassFunction)
  (plethysm,BasicList,ClassFunction)
Headline
  Plethystic operations on class functions
Usage
  pl = plethysm(f,cF)
  pl = plethysm(lambda,cF)
Description
  Text

    These methods describe the result of applying plethystic operations to a virtual
    character of a symmetric group. These operations are described either via a symmetric
    function {\tt f}, or a partition {\tt lambda}. Since {\tt cF} corresponds to an {\tt S_n}-
    representation, the option @TO GroupActing@ is irrelevant in this case.

  Example
    cF = new ClassFunction from {{2} => 1, {1,1} => -1};
    pl1 = plethysm({1,1},cF)
    R = symmetricRing 5;
    pl2 = plethysm(e_1+e_2,cF)
    S = schurRing R;
    symmetricFunction(cF,S)
    symmetricFunction(pl1,S)
    symmetricFunction(pl2,S)

  Text

    Applying the partition {\tt \{2\}} to the sign character of
    {\tt S_2} gives {\tt Sym^2} of the sign, which is the trivial
    representation of {\tt S_4}:

  Example
    sgn = new ClassFunction from {{2} => -1, {1,1} => 1};
    pl = plethysm({2},sgn)
    symmetricFunction(pl, schurRing(QQ, s, 4, GroupActing => "Sn"))

  Text

    Plethysm by a power-sum class function is Adams-type:
    {\tt p_k} sends a representation to its {\tt k}-th Adams
    operation (on class functions).

  Example
    cF2 = new ClassFunction from {{3} => 2, {2,1} => 0, {1,1,1} => -1};
    R2 = symmetricRing 4;
    plethysm(p_2, cF2)
///
-*
doc ///
Key
  (exteriorPower,ZZ,RingElement)
Headline
  Exterior power of a representation
Usage
  ep = exteriorPower(n,rep)
Inputs
  n:ZZ
  rep:RingElement
      an element of a SchurRing
Outputs
  ep:RingElement
Description
  Text
  
     Given a representation {\tt rep} of a product of general linear
     groups, and a positive integer {\tt n}, the function returns the
     {\tt n}-th exterior power of this representation.
     
  Example
     S = schurRing(QQ,s,2)
     T = schurRing(S,t,3)
     exteriorPower(4,s_{1}+t_{1})
///

doc ///
Key
  (symmetricPower,ZZ,RingElement)
Headline
  Symmetric power of a representation
Usage
  ep = symmetricPower(n,rep)
Inputs
  n:ZZ
  rep:RingElement
      an element of a SchurRing
Outputs
  ep:RingElement
Description
  Text
  
     Given a representation {\tt rep} of a product of general linear
     groups, and a positive integer {\tt n}, the function returns the
     {\tt n}-th symmetric power of this representation.
     
  Example
     S = schurRing(QQ,s,2)
     T = schurRing(S,t,3)
     symmetricPower(4,s_{1}+t_{1})
///
*-

doc ///
Key
  schurResolution
  (schurResolution,RingElement,List,List)
  (schurResolution,RingElement,List)
Headline
  Compute an ``approximate'' equivariant resolution of a module.
Usage
  resol = schurResolution(rep,M,lS)
  resol = schurResolution(rep,M)
Inputs
  rep:RingElement
      element of a SchurRing
  M:List
    list of representations, corresponding to the homogeneous components of a module {\tt M}.
  lS:List
    list of representations, corresponding to the homogeneous components of a polynomial ring {\tt S}.
Outputs
  resol:List
Description
  Text
  
     Given a representation {\tt rep} of a (product of) general linear
     or symmetric group(s) {\tt G}, we consider the symmetric algebra {\tt S = Sym(rep)}
     and an {\tt S}-module {\tt M} which is also a {\tt G}-module in such
     a way that the {\tt S}-module structure on {\tt M} respects the 
     {\tt G}-action. More generally, {\tt S} can be any graded ring, of which one inputs only
     finitely many homogeneous components as a list {\tt lS} of characters of {\tt G}. The main reason
     why we allow this generality is because most of the time it is computationally expensive to calculate
     the symmetric powers of the representation {\tt rep}, so we give the user the option to compute these
     symmetric powers by different methods and use the results as input for the schurResolution routine.
     
     We are interested in computing an equivariant 
     resolution of {\tt M}. This depends on both the {\tt G}- and {\tt S}-module structure 
     of {\tt M} in general, but in many examples that occur in practice, it turns out that
     the differentials in the resolution have maximal rank among all 
     {\tt G}-module homomorphisms between the free modules in the resolution.
     We will therefore assume that this is the case for the module {\tt M} that we are
     trying to resolve, and thus disregard its {\tt S}-module structure.
     
     More precisely, the assumptions that we make about {\tt M} are as follows: {\tt M} is 
     a graded {\tt S}-module, with {\tt M_i = 0} for {\tt i<0}, where the grading on {\tt S} is standard,
     given by setting the degrees of the elements of {\tt rep} equal to 1. Since we assumed
     that the {\tt G}-structure of {\tt M} determines the syzygies, all the relevant
     information is concentrated in finitely many homogeneous components of {\tt M} (namely
     up to {\tt reg(M)+pd(M)}, the sum of the regularity and the projective dimension of
     {\tt M}). We will thus assume that {\tt M}
     is given as a list of {\tt G}-representations, corresponding to (a subset of) the
     relevant homogeneous components. The function {\tt schurResolution} takes as 
     inputs the representation {\tt rep}, the module {\tt M}, and as optional arguments a {\tt DegreeLimit}
     {\tt d}, and a {\tt SyzygyLimit} {\tt c}. The ring {\tt S} itself can occur as input data, being
     described as a list of {\tt G}-representations, just like {\tt M}.
     The routine outputs the generators of degree at most {\tt d} of the 
     first {\tt c+1} syzygy modules (from {\tt 0} to {\tt c}). They are listed as a 
     sequence of pairs, consisting of the degree of the generators of the syzygy modules 
     together with the characters of the {\tt G}-representations they correspond to. 
     If the syzygy bound {\tt c} is not given,
     then all syzygy modules are computed. If the degree bound {\tt d} is not given, then
     it is assumed to be equal to the largest degree among the homogeneous components of
     {\tt M} in the input, i.e. one less than the length of the @TO List@ {\tt M}.
     
     The example below computes the resolution of the quadratic Veronese 
     surface in {\tt P^5}.
      
  Example
    S = schurRing(QQ,s,3)
    rep = s_{2}
    M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}}
    schurResolution(rep,M)

  Text
  
    Next, we compute the syzygies of degree at most {\tt 7} in the resolution 
    of the cubic Veronese embedding of {\tt P^2}.
    
  Example
    rep = s_{3}
    M = {1_S,s_{3},s_{6},s_{9},s_{12},s_{15},s_{18},s_{21},s_{24},s_{27}}
    d = 7
    schurResolution(rep,M,DegreeLimit => d)

  Text
    
    We can compute the resolution of the ideal of {\tt 2\times 2} minors of a {\tt 3\times 4}
    matrix, which corresponds to the Segre embedding of {\tt P^2\times P^3}:
    
  Example
    T = schurRing(S,t,4)
    rep = s_1 * t_1
    M = {1_T} | apply(splice{1..8},i -> s_i * t_i)
    schurResolution(rep,M)

  Text
  
    The following example computes the equivariant resolution of the residue field of a 
    polynomial ring in {\tt n=5} variables, with respect to the action of the symmetric 
    group {\tt S_n}.
  
  Example
    n = 5;
    S = schurRing(QQ,s,n,GroupActing => "Sn");
    rep = s_n + s_{n-1,1};
    M = {s_n}
    schurResolution(rep,M,DegreeLimit => n)

  Text

    Generalizing this, we can compute the equivariant resolution of the quotient of the
    polynomial ring in {\tt n=5} variables by the ideal of square-free monomials of
    degree two, with respect to the action of the symmetric group {\tt S_n}.

  Example
    M = {s_n} | splice{n:rep};
    schurResolution(rep,M)

  Text

    Ordinary Segre embeddings P^a x P^b can be treated as above by
    taking a tensor product of GL-factors.  For P^1 x P^3, keeping
    only the syzygies in degree at most {\tt 4}:

  Example
    U = schurRing(QQ,u,2);
    V = schurRing(U,v,4);
    rep = u_1 * v_1;
    M = {1_V} | apply(splice{1..5}, i -> u_i * v_i);
    schurResolution(rep,M,DegreeLimit => 4)

  Text

    The cubic Veronese of P^2 has a rich equivariant resolution;
    asking for the first few syzygies of total degree at most {\tt 4}
    gives the classical Koszul-type pattern:

  Example
    S = schurRing(QQ,s,3);
    rep = s_{3};
    M = apply(splice{0..8}, i -> s_{3*i});
    schurResolution(rep,M,DegreeLimit => 4, SyzygyLimit => 3)

  Text

    A larger example: the equivariant resolution of the residue
    field of {\tt Sym(V)} at {\tt rank(V) = 6} under the
    symmetric-group action:

  Example
    n = 6;
    Sb = schurRing(QQ,s,n,GroupActing => "Sn");
    rep = s_n + s_{n-1,1};
    schurResolution(rep, {s_n}, DegreeLimit => n)

///

doc ///
Key
  [schurResolution,DegreeLimit]
Headline
  Specifies the maximal degree of syzygies to be computed
Description
  Text
    This is an optional argument for the @TO schurResolution@ routine. It specifies an upper bound for the
    degrees of the generators of the syzygy modules in the equivariant resolution of an equivariant module {\tt M}
    to be computed by the routine. If a {\tt DegreeLimit} is not specified, then it is assumed to be equal to the
    maximal degree in which the module {\tt M} is specified as a representation.

  Example
    A = schurRing(a,3,GroupActing => "Sn");
    B = schurRing(A,b,2);
    rep = (a_3 + a_{2,1}) * b_1
    d = dim rep
    M = {a_3 * 1_B};
    sR = schurResolution(rep,M,DegreeLimit => d)

  Text

    Decreasing {\tt DegreeLimit} truncates the output to syzygies
    of total degree at most the specified value, while the
    representations themselves are unchanged:

  Example
    S = schurRing(QQ,s,3);
    rep = s_{2};
    M = {1_S,s_{2},s_{4},s_{6},s_{8},s_{10},s_{12}};
    schurResolution(rep,M)
    schurResolution(rep,M,DegreeLimit => 3)

SeeAlso
  [schurResolution,SyzygyLimit]
///
    
doc ///
Key
  [schurResolution,SyzygyLimit]
Headline
  Specifies the number of syzygy modules to be computed
Description
  Text
    This is an optional argument for the @TO schurResolution@ routine. It specifies an upper bound for the
    number of syzygy modules in the equivariant resolution of an equivariant module {\tt M} to be computed
    by the routine. If a {\tt SyzygyLimit} is not specified, then all syzygy modules are computed.

    The example below computes the {\tt 0}-th to {\tt 3}-rd syzygy modules of the {\tt 5}-th Veronese embedding
    of {\tt P^2}.

  Example
    S = schurRing(s,3);
    rep = s_{5};
    M = {1_S,s_{5},s_{10},s_{15},s_{20},s_{25},s_{30}};
    schurResolution(rep,M,SyzygyLimit => 3)

  Text

    Lowering {\tt SyzygyLimit} simply chops the output at the
    requested homological position.  For the quadratic Veronese
    of P^2, asking for only the first syzygy module yields:

  Example
    T = schurRing(QQ,t,3);
    rep2 = t_{2};
    M2 = {1_T,t_{2},t_{4},t_{6},t_{8},t_{10},t_{12}};
    schurResolution(rep2,M2,SyzygyLimit => 1)
    schurResolution(rep2,M2,SyzygyLimit => 2)

SeeAlso
  [schurResolution,DegreeLimit]
///
    
doc ///
  Key
    schurLevel
    (schurLevel,Ring)
  Headline
    Number of SchurRings the ring is a tensor product of.
  Usage
    lev = schurLevel(R)
  Inputs
    R:Ring
  Outputs
    lev:ZZ
  Description
    Text

      For the representation ring {\tt R} of a product of {\tt lev}
      general linear and/or symmetric groups, the function returns
      {\tt lev}.  If {\tt R} is not a representation ring, the
      function returns 0.

    Example
      R = schurRing(QQ,r,3);
      S = schurRing(R,s,5);
      T = schurRing(S,t,2);
      schurLevel R
      schurLevel S
      schurLevel T
      schurLevel QQ

    Text

      A three-level tower mixing {\tt GL}-factors with an {\tt S_n}-factor
      counts each layer, regardless of which group is acting:

    Example
      A = schurRing(QQ,a,3);
      B = schurRing(A,b,4);
      C = schurRing(B,c,2,GroupActing => "Sn");
      schurLevel C

    Text

      A Symmetric ring (produced by @TO symmetricRing@) sits over a
      single tower slot and reports {\tt schurLevel} equal to 1, while
      an ordinary polynomial ring or the base field report 0:

    Example
      schurLevel(symmetricRing(QQ,5))
      schurLevel(QQ[x,y,z])
      schurLevel ZZ
///

doc ///
  Key
    (partitions,Set,BasicList)
  Headline
    Partitions of a set
  Usage
    par = partitions(S,L)
  Inputs
    S:Set
    L:BasicList
      a nonincreasing list of integers, or a partition
  Outputs
    par:List
  Description
    Text

      Given a set {\tt S} and a partition {\tt L=\{l_1\geq l_2\geq\cdots\}},
      the method returns the list of set-partitions of {\tt S} of type
      {\tt L}, i.e. ways of writing {\tt S=S_1\cup S_2\cup\cdots} with the
      {\tt S_i} pairwise disjoint and {\tt |S_i|=l_i}.  The blocks come out
      as an unordered collection of sets.

    Example
      partitions(set{1,2,3,4},{2,1,1})
      partitions(set{a,b,c,d,e},new Partition from {3,2})

    Text

      Changing the shape {\tt L} changes the cycle type.  Two blocks of
      size 2 and one fixed point on five points is the number of
      permutations of cycle type {\tt (2,2,1)} divided by the size of
      the corresponding centralizer:

    Example
      partitions(set{1,2,3,4,5},{2,2,1})
      #partitions(set{1,2,3,4,5},{2,2,1})

    Text

      Passing a @TO Partition@ is equivalent to passing the underlying
      list; counting block-partitions of shape {\tt (3,1)} recovers
      {\tt 4 \choose 1}:

    Example
      partitions(set{1,2,3,4},new Partition from {3,1})
      #partitions(set{1,2,3,4},new Partition from {3,1})

    Text

      Supplying a single-block shape returns a singleton list: the
      only set-partition of type {\tt (n)} is {\tt S} itself.

    Example
      partitions(set{1,2,3},{3})
///

-*
doc ///
 Key
  (chi,BasicList,BasicList)
 Headline
  Irreducible character of symmetric group
Usage
  v = chi(lambda,rho)
Inputs
  lambda:BasicList
   	 a nondecreasing list of positive integers, or a partition
  rho:BasicList
      a nondecreasing list of positive integers, or a partition
Outputs
  v:QQ
Description
  Text

    Given two partitions {\tt lambda} and {\tt rho} of the integer {\tt N}, this method
    computes the value of the irreducible character of the symmetric group
    corresponding to the partition {\tt lambda} evaluated on
    any permutation of cycle-type {\tt rho}.

    The character of the trivial representation takes the value
    1 on any permutation:
  
  Example
    chi({4},{2,1,1})
    
  Text
  
    The character of the sign representation takes the value -1 on
    a cycle of length 4:
  
  Example
    chi({1,1,1,1},{4})
SeeAlso
   symmetricFunction
   classFunction
///
*-

doc ///
Key
  SchurRingElement
Headline
  A type describing elements of a SchurRing
Description
  Text
    Elements of any @TO SchurRing@ -- whether a {\tt GL}-ring, a
    symmetric-group ring, or an orthogonal/symplectic ring -- have
    type {\tt SchurRingElement}.  Products, sums and (in characteristic
    zero) rational scalings of Schur ring elements are again of this
    type.

  Example
    S = schurRing(s,5)
    a = s_{3,2,1}
    instance(a,SchurRingElement)

    T = schurRing(S,t,3,GroupActing => "Sn")
    b = t_{2,1}+t_3
    instance(a*b,SchurRingElement)

  Text
    The same type is used for symplectic characters.  Multiplying two
    {\tt Sp}-characters stays inside the ring:

  Example
    Sp = schurRing(QQ,sp,3,GroupActing => "Sp");
    u = sp_{2,1};
    instance(u,SchurRingElement)
    instance(u*u, SchurRingElement)

  Text
    For an {\tt Sn}-flavored Schur ring the ordinary ring
    multiplication is the tensor product of characters, while
    @TO internalProduct@ gives the pointwise (Kronecker) product:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    internalProduct(c_{3,1}, c_{2,1,1})
    instance(c_{3,1} * c_{2,1,1}, SchurRingElement)

  Text
    In a two-level ring the SchurRingElement type is closed under
    multiplication across levels:

  Example
    G = schurRing(QQ,g,3);
    H = schurRing(G,h,2);
    z = g_{1,1} * h_{2}
    instance(z, SchurRingElement)
///

doc ///
   Key
     (dim,List,SchurRingElement)
     (dim,Thing,SchurRingElement)
     (dim,SchurRingElement)
   Headline
     dimension of representation
   Usage
     d = dim(lis,s)
     d = dim(n,s)
     d = dim s
   Inputs
     lis: List
     	  or @TO Thing@
     s: SchurRingElement
   Outputs
     d: ZZ
        or @TO Expression@
   Description
     Text

       The method returns the dimension of the virtual representation whose
       character is represented by {\tt s}.

     Example
       S = schurRing(s,3)
       dim s_2
       T = schurRing(t,4,GroupActing => "Sn")
       dim t_{2,2}
       U = schurRing(T,u,3)
       dim (t_{2,2}*u_2)

     Text

       Schur characters see the rank of the ambient vector space.
       The representation {\tt s_{2,1}} of {\tt GL(3)} is 8-dimensional,
       while the same shape in {\tt GL(4)} gives a 20-dimensional
       representation:

     Example
       dim ((schurRing(s3,3))_{2,1})
       dim ((schurRing(s4,4))_{2,1})

     Text

       Dimensions are also computed for symplectic and orthogonal
       characters.  For orthogonal groups the option @TO OddOrEven@
       selects {\tt O(2m+1)} versus {\tt O(2m)}:

     Example
       Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
       dim sp_{2,1}
       Oodd = schurRing(QQ, od, 3, GroupActing => "O", OddOrEven => "Odd");
       dim od_{2,1}
       Oeven = schurRing(QQ, oe, 3, GroupActing => "O", OddOrEven => "Even");
       dim oe_{2,1}

     Text

       If {\tt S} is a @TO SchurRing@ of level 1, the ring of polynomial representations of some {\tt GL(V)}, it
       may sometimes be convenient to compute dimensions of {\tt GL(V)}-representations symbolically, without
       specifying the dimension of {\tt V}.  Letting {\tt n} denote the parameter corresponding to {\tt dim(V)}
       we have for example:

     Example
       S = schurRing(s,3)
       dim(n,s_2)
       dim(n,s_{1,1})
       dim(n,s_{2,1})

     Text

       Similar calculations make sense over products of general linear groups. The dimensions of the representations
       can be computed symbolically as functions of a number of parameters
       equal to the @TO schurLevel@ of the ring. The parameters corresponding to levels where the group acting
       is a symmetric group don't have a good interpretation, so they are disregarded in the dimension calculation.
       The order of the input parameters is the descending order of the @TO schurLevel@s: in the example below
       {\tt a} corresponds to {\tt Q}, {\tt b} corresponds to {\tt T} and {\tt c} corresponds to {\tt S}.

     Example
       S = schurRing(s,3)
       T = schurRing(S,t,4)
       Q = schurRing(T,q,5,GroupActing => "Sn")
       dExpr = dim({a,b,c},s_{2}*t_{1,1}*q_{4,1})
       P = QQ[a,b,c]
       value dExpr
       dim({1,2,3},s_{2}*t_{1,1}*q_{4,1})

     Text

       Over a two-level {\tt GL\times GL} tower the formula factors
       as a product of dimensions, one per level, in descending
       {\tt schurLevel} order:

     Example
       A = schurRing(aR,3);
       B = schurRing(A,bR,2);
       dim(aR_{2,1} * bR_{1,1})
       dim({4,5}, aR_{2,1} * bR_{1,1})
///


doc ///
Key
  ClassFunction
  (symbol +,ClassFunction,ClassFunction)
  (symbol -,ClassFunction,ClassFunction)
  (symbol *,ClassFunction,ClassFunction)
  (symbol *,ClassFunction,RingElement)
  (symbol *,RingElement,ClassFunction)
  (symbol *,ClassFunction,Number)
  (symbol *,Number,ClassFunction)
  (symbol ==,ClassFunction,ClassFunction)
Headline
  The class of all Class functions
Description
  Text
    A class function (or virtual character of a symmetric group {\tt S_n})
    is a function that is constant on the conjugacy classes of {\tt S_n}.
    Class functions for {\tt S_n} are in one-to-one correspondence with
    symmetric functions of degree {\tt n}.  The class functions corresponding
    to actual representations of {\tt S_n} are called {\tt characters}.

    The character of the standard representation of {\tt S_3} is

  Example
    S = schurRing(QQ,s,3);
    classFunction(s_{2,1})

  Text
    The character of the sign representation of {\tt S_5} is

  Example
    S = schurRing(QQ,s,5);
    classFunction(s_{1,1,1,1,1})

  Text
    We can go back and forth between class functions and symmetric functions.

  Example
    R = symmetricRing(QQ,3);
    cF = new ClassFunction from {{1,1,1} => 2, {3} => -1};
    sF = symmetricFunction(cF,R)
    toS sF
    classFunction sF

  Text
    We can add, subtract, multiply, scale class functions:

  Example
    S = schurRing(QQ,s,4);
    c1 = classFunction(S_{2,1,1}-S_{4});
    c2 = classFunction(S_{3,1});
    c1 + c2
    c1 * c2
    3*c1 - c2*2

  Text
    The trivial and sign representations of {\tt S_4} are the characters
    of the shapes {\tt (4)} and {\tt (1,1,1,1)}.  Their pointwise product
    (which is {\tt c_1 * c_2} on {\tt ClassFunction}) gives the sign
    representation back:

  Example
    T = schurRing(QQ,t,4);
    triv = classFunction(t_{4})
    sgn = classFunction(t_{1,1,1,1})
    triv * sgn == sgn

  Text
    The regular representation of {\tt S_n} has character {\tt n!} on the
    identity class {\tt (1^n)} and 0 elsewhere.  By Frobenius
    reciprocity, it pairs trivially with the trivial character:

  Example
    reg = new ClassFunction from {{1,1,1,1} => 24}
    scalarProduct(reg, triv)

  Text
    Products of class functions of induced/restricted representations
    recover well-known decompositions: the tensor square of the
    standard representation of {\tt S_4} pairs nontrivially with both
    the trivial and sign characters:

  Example
    std = classFunction(t_{3,1});
    sq = std * std;
    scalarProduct(sq, triv)
    scalarProduct(sq, sgn)
///

doc ///
Key
  (degree,ClassFunction)
Headline
  Degree of virtual character
Description
  Text
    For a virtual character {\tt ch} of a symmetric group on {\tt n}
    letters, the degree of {\tt ch} is {\tt n}.

  Example
    S = schurRing(s,5);
    ch = classFunction s_(3,1,1)
    degree ch

  Text
    The degree of {\tt classFunction(s_\lambda)} always agrees with
    {\tt |\lambda|}:

  Example
    lam = {4,2,1};
    degree classFunction(new Partition from lam) == sum lam

  Text
    A sum of characters of the same {\tt S_n} keeps the same degree --
    mixing distinct partitions of {\tt 5} still yields a class function
    of degree {\tt 5}:

  Example
    R = symmetricRing(QQ,5);
    mix = classFunction(jacobiTrudi({4,1},R)) + 2*classFunction(jacobiTrudi({3,2},R));
    degree mix
///

doc ///
Key
  symmetricFunction
  (symmetricFunction,ClassFunction,Ring)
Headline
  Converts class function to symmetric function
Usage
  f = symmetricFunction(ch,S)
Inputs
  ch:ClassFunction
  S:Ring
    a Symmetric or Schur ring
Outputs
  f:RingElement
    element of a Symmetric or Schur ring
Description
  Text
    Given a virtual character {\tt cF} of a symmetric group, and given a
    Symmetric ring {\tt S}, the method computes the corresponding
    symmetric function as an element of {\tt S}.  The conversion uses
    the Frobenius characteristic map: the regular representation
    ({\tt n!} on the identity, zero elsewhere) maps to {\tt n! e_1^n}
    in the {\tt e}-basis, equivalently to {\tt sum_\lambda f^\lambda s_\lambda}
    in the {\tt s}-basis.

  Example
    S = symmetricRing(QQ,4);
    cF = new ClassFunction from {{1,1,1,1}=>24};
    symmetricFunction(cF,S)
    symmetricFunction(cF,schurRing S)

  Text
    The standard representation of {\tt S_4} has character given by
    {\tt classFunction(s_{3,1})}; passing it through {\tt symmetricFunction}
    recovers {\tt s_{3,1}} on the @TO SchurRing@ side:

  Example
    R = symmetricRing(QQ,4);
    Sch = schurRing R;
    stdCh = classFunction(jacobiTrudi({3,1},R));
    symmetricFunction(stdCh, Sch)

  Text
    Composing {\tt classFunction} with {\tt symmetricFunction} is the
    identity on the {\tt S_n}-side of the Frobenius correspondence --
    roundtripping a class function through the symmetric function
    ring returns it unchanged:

  Example
    sF = symmetricFunction(stdCh, R);
    classFunction sF == stdCh

  Text
    The trivial character of {\tt S_5} has Frobenius image {\tt h_5};
    the sign character maps to {\tt e_5}:

  Example
    R5 = symmetricRing(QQ,5);
    symmetricFunction(classFunction{5}, R5)
    symmetricFunction(classFunction{1,1,1,1,1}, R5)
SeeAlso
  classFunction
--  chi
///

doc ///
Key
  classFunction
  (classFunction,RingElement)
Headline
  Converts symmetric function to class function
Usage
  ch = classFunction(f)
Inputs
  f:RingElement
    element of a Symmetric ring
Outputs
  ch:ClassFunction
Description
  Text
    Given a symmetric function {\tt f}, homogeneous of degree {\tt N},
    the method computes the corresponding virtual character of the
    symmetric group {\tt S_N}.

    The character of the standard representation of {\tt S_5} is

  Example
    R = symmetricRing(QQ,5);
    classFunction(jacobiTrudi({4,1},R))

  Text

    The character of the second exterior power of the standard representation of {\tt S_5} is

  Example
    R = symmetricRing(QQ,5);
    classFunction(jacobiTrudi({3,1,1},R))

  Text
    The sign representation of {\tt S_n} corresponds to the Schur
    polynomial of shape {\tt (1^n)}.  Its class function takes the
    value {\tt sgn(\sigma)} on a permutation of cycle type {\tt \rho}
    -- that is, {\tt -1} raised to the number of even-length cycles:

  Example
    Ssign = schurRing(QQ,s,5);
    classFunction(s_{1,1,1,1,1})

  Text
    Small-{\tt n} character tables are assembled row-by-row from
    {\tt classFunction}.  For {\tt S_3} there are three conjugacy
    classes {\tt (1^3), (2,1), (3)} and three irreducibles, and the
    values are collected below:

  Example
    R3 = symmetricRing(QQ,3);
    for lam in {{3},{2,1},{1,1,1}} list classFunction(jacobiTrudi(lam,R3))

  Text
    Tensor products of {\tt S_n}-representations correspond to
    {\tt internalProduct} of class functions.  For {\tt S_4} the
    tensor square of the standard representation decomposes into
    irreducibles by pairing with each irreducible character via
    @TO scalarProduct@:

  Example
    R4 = symmetricRing(QQ,4);
    std = classFunction(jacobiTrudi({3,1},R4));
    sq = internalProduct(std, std);
    for lam in {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}} list
      scalarProduct(sq, classFunction(jacobiTrudi(lam,R4)))

SeeAlso
  symmetricFunction
--  chi
///

doc ///
Key
  (classFunction,BasicList)
Headline
  Character of irreducible representation of symmetric group
Usage
  ch = classFunction(l)
Inputs
  l:BasicList
    partition
Outputs
  ch:ClassFunction
Description
  Text
    Given a partition {\tt l} of {\tt N}, the method computes the
    character of the irreducible {\tt S_N}-representation corresponding
    to the partition {\tt l}.

  Example
    R = symmetricRing(QQ,7);
    cF = classFunction({3,2,1})
    toS(symmetricFunction(cF,R))

  Text
    Enumerating the irreducible characters of {\tt S_4} and pairing
    them with @TO scalarProduct@ recovers the orthonormality relations
    from representation theory -- the diagonal entries are 1 and the
    off-diagonal entries are 0:

  Example
    chars = for lam in partitions 4 list classFunction toList lam;
    matrix for a in chars list for b in chars list scalarProduct(a,b)

  Text
    Irreducible characters of different shapes are orthogonal.  Here
    we verify orthogonality for three partitions of 6:

  Example
    c1 = classFunction({3,2,1});
    c2 = classFunction({4,1,1});
    c3 = classFunction({2,2,2});
    scalarProduct(c1,c2)
    scalarProduct(c2,c3)

  Text
    Self-pairings return 1 for each irreducible, independent of the
    shape, and the trivial (row shape) and sign (column shape)
    characters are the 1-dimensional irreducibles:

  Example
    scalarProduct(c1,c1)
    classFunction({5})
    classFunction({1,1,1,1,1})
SeeAlso
  symmetricFunction

///

doc ///
Key
  scalarProduct
Headline
  Standard pairing on symmetric functions/class functions
Description
  Text

    This method computes the standard Hall scalar product on the ring
    {\tt \Lambda} of symmetric functions. One way to define this product
    is by imposing that the collection of Schur functions {\tt s_{\lambda}}
    form an orthonormal basis.

    Alternatively, by the correspondence between symmetric functions
    and virtual characters of symmetric groups, this scalar product
    coincides with the standard scalar product on class functions.

    The number of standard tableaux of shape {\tt \{4,3,2,1\}} is:

  Example
    R = symmetricRing(QQ,10);
    S = schurRing(QQ,s,10);
    scalarProduct(h_1^10,s_{4,3,2,1})

  Text

    The Schur basis is orthonormal: {\tt <s_\lambda, s_\mu>} equals
    {\tt 1} if {\tt \lambda = \mu} and {\tt 0} otherwise.

  Example
    T = schurRing(QQ,t,5);
    scalarProduct(t_{3,1,1}, t_{3,1,1})
    scalarProduct(t_{3,1,1}, t_{2,2,1})
    scalarProduct(t_{4,1}, t_{3,2})

  Text

    The power-sum basis is orthogonal with norms given by the
    centralizer sizes: {\tt <p_\rho, p_\rho> = z_\rho}.  Here we
    verify this for the three partitions of {\tt 3}.

  Example
    U = symmetricRing(QQ,4);
    scalarProduct(p_3, p_3) == centralizerSize{0,0,1}
    scalarProduct(p_2*p_1, p_2*p_1) == centralizerSize{1,1}
    scalarProduct(p_1^3, p_1^3) == centralizerSize{3}

  Text

    By the Cauchy identity, {\tt <p_1^n, p_1^n>} counts the
    elements of {\tt S_n}, i.e.\ it equals {\tt n!}:

  Example
    scalarProduct(p_1^4, p_1^4)
SeeAlso
  internalProduct
  centralizerSize
///

doc ///
Key
  (scalarProduct,RingElement,RingElement)
Headline
  Standard scalar product of symmetric functions
Usage
  sp = scalarProduct(f1,f2)
Inputs
  f1:RingElement
     element of a Symmetric Ring
  f2:RingElement
     element of a Symmetric Ring
Outputs
  sp:QQ
Description
  Text

    Given symmetric functions {\tt f1} and {\tt f2}, the method
    computes the standard Hall pairing between {\tt f1} and {\tt f2}.

  Example
    R = symmetricRing(QQ,5);
    S = schurRing R
    scalarProduct(h_5,p_5)
    scalarProduct(S_{4,1},p_5)

  Text

    Indeed, the coefficients of {\tt s_5} and {\tt s_{4,1}} in the
    s-basis expansion of {\tt p_5} are as computed above:

  Example
    toS p_5

  Text

    The pairing {\tt <e_n, p_n>} equals {\tt (-1)^{n-1}}, reflecting
    the sign character of the symmetric group:

  Example
    scalarProduct(e_2, p_2)
    scalarProduct(e_3, p_3)
    scalarProduct(e_4, p_4)

  Text

    The pairing {\tt <s_\lambda, h_\mu>} recovers the Kostka number
    {\tt K_{\lambda,\mu}}, the number of semistandard Young tableaux of
    shape {\tt \lambda} and content {\tt \mu}.  We cross-check this
    against @TO kostkaNumber@:

  Example
    scalarProduct(S_{3,2}, h_2*h_2*h_1) == kostkaNumber({3,2}, {2,2,1})
    scalarProduct(S_{3,2}, h_1^5) == kostkaNumber({3,2}, {1,1,1,1,1})
    scalarProduct(S_{4,1}, h_3*h_2) == kostkaNumber({4,1}, {3,2})
SeeAlso
  kostkaNumber
///

doc ///
Key
  (scalarProduct,ClassFunction,ClassFunction)
Headline
  Standard scalar product of class functions
Usage
  sp = scalarProduct(ch1,ch2)
Inputs
  ch1:ClassFunction
  ch2:ClassFunction
Outputs
  sp:QQ
Description
  Text

    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the standard pairing between {\tt ch1} and {\tt ch2}.

  Example
    ch1 = new ClassFunction from {{3,2} => 2, {2,2,1} => -2, {3,1,1} => 2, {5} => 1};
    ch2 = new ClassFunction from {{2,2,1} => -2, {5} => 1, {1,1,1,1,1} => 5, {3,2} => 3, {4,1} => -2};
    scalarProduct(ch1,ch2)

  Text

    The irreducible characters of {\tt S_n} indexed by distinct
    partitions are orthonormal.  For partitions of {\tt 5}, we can
    verify the full orthogonality relations:

  Example
    S = schurRing(QQ,s,5);
    cF32 = classFunction s_{3,2};
    cF41 = classFunction s_{4,1};
    cF221 = classFunction s_{2,2,1};
    scalarProduct(cF32, cF32)
    scalarProduct(cF32, cF41)
    scalarProduct(cF41, cF221)

  Text

    Decomposing an arbitrary virtual character into irreducibles by
    pairing with each Schur class function recovers the multiplicities:

  Example
    psi = cF32 + 2*cF41 - cF221;
    {scalarProduct(psi, cF32), scalarProduct(psi, cF41), scalarProduct(psi, cF221)}
SeeAlso
  classFunction
///

doc ///
Key
  internalProduct
Headline
  Internal product of symmetric functions/class functions
Description
  Text

    This method computes the internal (Kronecker) product of two homogeneous
    symmetric functions of the same degree.  If we think of these functions
    as virtual characters of some symmetric group, then their internal
    product is just the character of the tensor product of the corresponding
    virtual representations.  We use the binary operator @TO symbol **@ as a
    shorthand for @TO internalProduct@.

    The complete symmetric function of degree {\tt n} corresponds
    to the trivial {\tt S_n}-representation and is therefore
    the unit of the representation ring of {\tt S_n}:

  Example
    R = symmetricRing(QQ,5);
    S = schurRing(QQ,s,3);
    internalProduct(h_3,s_{2,1})
    toE(h_3 ** e_3)

  Text

    The square of the sign representation is the trivial representation:

  Example
    toH internalProduct(e_3,e_3)

  Text

    Working in a Schur ring directly, Kronecker products of Schur
    functions give the decomposition of tensor products of irreducible
    {\tt S_n}-representations.  The Kronecker square of {\tt s_{2,1}}
    (the standard representation of {\tt S_3}) decomposes as:

  Example
    T = schurRing(QQ,t,3);
    internalProduct(t_{2,1}, t_{2,1})

  Text

    The exterior square of the sign representation of {\tt S_3} is
    the trivial representation, which on the symmetric-function side
    is the identity {\tt s_{1,1,1} \otimes s_{1,1,1} = s_3}:

  Example
    internalProduct(t_{1,1,1}, t_{1,1,1})
SeeAlso
  scalarProduct
///

doc ///
Key
  (internalProduct,RingElement,RingElement)
Headline
  Kronecker product of symmetric functions
Usage
  ip = internalProduct(f1,f2)
Inputs
  f1:RingElement
     element of a Symmetric ring or a Schur ring
  f2:RingElement
     element of a Symmetric ring or a Schur ring
Outputs
  ip:Ring
     a Symmetric ring or a Schur Ring
Description
  Text

    Given symmetric functions {\tt f1} and {\tt f2}, the method
    computes the Kronecker product {\tt ip} between {\tt f1} and {\tt f2}.
    The output {\tt ip} is an element in the ring of {\tt f2}.

  Example
     R = symmetricRing(QQ,6);
     S = schurRing(QQ,s,6);
     toE(h_3**e_3)
     Q = schurRing(QQ,q,6);
     internalProduct(s_{3,3},q_{4,2})

  Text

    An error is returned if {\tt f1} and {\tt f2} don't have the
    same degree.

    Products of complete homogeneous functions give characters of
    permutation representations of {\tt S_n}; their Kronecker product
    decomposes accordingly.  For instance, in degree {\tt 4}:

  Example
    internalProduct(h_3*h_1, h_2*h_2)

  Text

    The same computation can be carried out directly in a Schur ring
    with option {\tt GroupActing => "Sn"}, where multiplication
    {\tt *} is the internal product.  The Kronecker square of the
    standard representation {\tt s_{3,1}} of {\tt S_4} decomposes as
    trivial + sign + standard + {\tt s_{2,2}}:

  Example
    Sn = schurRing(QQ,c,4,GroupActing => "Sn");
    c_{3,1} * c_{3,1}
    internalProduct(c_{3,1}, c_{3,1})

  Text

    The method is compatible with @TO toSn@: we can first convert a
    product of {\tt h}'s into the Schur basis of an {\tt S_n} ring,
    then take Kronecker products there.

  Example
    toSn(h_2*h_1*h_1, Sn) * c_{3,1}
///

doc ///
Key
  (internalProduct,ClassFunction,ClassFunction)
Headline
  Tensor product of virtual representations
Usage
  ip = internalProduct(ch1,ch2)
Inputs
  ch1:ClassFunction
  ch2:ClassFunction
Outputs
  ip:ClassFunction
Description
  Text

    Given virtual characters {\tt ch1} and {\tt ch2}, the method
    computes the character of the tensor product of corresponding
    virtual representations of the symmetric group.

  Example
    ch1 = new ClassFunction from {{4,4} => 2, {8} => -1, {5,2,1} => 2, {3,2,2,1} => 1};
    ch2 = new ClassFunction from {{2,2,2,2} => -4, {5,2,1} => 1, {3,2,2,1} => 3};
    internalProduct(ch1,ch2)
    ch1 * ch2

  Text

    A classical example: the tensor square of the standard
    representation of {\tt S_4} has character values obtained by
    squaring the standard character.  Pairing with itself recovers
    the multiplicities of the irreducibles in the tensor square:

  Example
    S = schurRing(QQ,s,4);
    std = classFunction s_{3,1};
    sq = internalProduct(std, std)
    scalarProduct(sq, classFunction s_{4})
    scalarProduct(sq, classFunction s_{3,1})
    scalarProduct(sq, classFunction s_{2,2})
    scalarProduct(sq, classFunction s_{2,1,1})
SeeAlso
  classFunction
///

doc ///
Key
  centralizerSize
  (centralizerSize,List)
Headline
  Size of the centralizer of a permutation
Usage
  n = centralizerSize(rho)
Inputs
  rho:List
Outputs
  n:ZZ
Description
  Text

    {\tt rho} is a list representing the cycle type of some permutation:
    the {\tt i}-th entry in {\tt rho} is the number of cycles of length
    {\tt i} of the permutation.
    The output of the function {\tt centralizerSize} is the size of the
    centralizer in the symmetric group of any permutation of cycle type
    {\tt rho}.  If the cycle type {\tt rho} corresponds to a partition
    {\tt \lambda}, then {\tt centralizerSize(rho)} is also the value of
    the square norm {\tt <p_\lambda, p_\lambda>}.

  Example
    centralizerSize{1,1,1}
    R = symmetricRing(QQ,6);
    u = p_1 * p_2 * p_3;
    scalarProduct(u,u)

  Text

    A few values of {\tt z_\rho} for small cycle types: the identity
    of {\tt S_n} has centralizer size {\tt n!}, while an {\tt n}-cycle
    has centralizer of size {\tt n} (generated by itself).

  Example
    centralizerSize{4}           -- cycle type (1,1,1,1), all of S_4
    centralizerSize{0,0,0,1}     -- a single 4-cycle
    centralizerSize{2,1}         -- cycle type (2,1,1)

  Text

    Burnside's classical identity {\tt \sum_{\lambda \vdash n} 1/z_\lambda = 1}
    expresses that the uniform measure on {\tt S_n} sums to {\tt 1}.
    We verify this for {\tt n = 5}:

  Example
    parts5 = {{5}, {4,1}, {3,2}, {3,1,1}, {2,2,1}, {2,1,1,1}, {1,1,1,1,1}};
    cycMult = la -> apply(toList(1..max la), i -> #positions(la, j -> j == i));
    sum(parts5, la -> 1/centralizerSize cycMult la)
SeeAlso
  scalarProduct
///

doc ///
  Key
    Memoize
  Headline
    Option to record values of the jacobiTrudi function
  Description
    Text

      This is an optional argument for the @TO jacobiTrudi@
      function, allowing one to store its values
      in order to speed up computations.  When {\tt Memoize => true},
      every computed value is cached in a hash table on the symmetric
      ring, so repeated calls on the same partition return the cached
      value in constant time.

    Example
      R = symmetricRing(QQ, 10);
      elapsedTime jacobiTrudi({4,3,2,1}, R, Memoize => true);
      elapsedTime jacobiTrudi({4,3,2,1}, R, Memoize => true);

    Text

      The cache is attached to the ring {\tt R}.  After one partition
      is memoized, subsequent calls with a different partition perform
      the full Jacobi-Trudi determinant expansion, then cache it as
      well:

    Example
      elapsedTime jacobiTrudi({5,3,2}, R, Memoize => true);
      elapsedTime jacobiTrudi({5,3,2}, R, Memoize => true);

    Text

      Without {\tt Memoize => true}, each call recomputes the
      determinant from scratch; for large partitions this can be
      substantially more expensive than a single cached lookup.

    Example
      elapsedTime jacobiTrudi({4,3,2,1}, R);
      elapsedTime jacobiTrudi({4,3,2,1}, R);
  SeeAlso
    jacobiTrudi
///

doc ///
Key
  SVariable
  [schurRing,SVariable]
  [symmetricRing,SVariable]
Headline
  Specifies symbol representing s-functions
Description
  Text
    This is an optional argument for the constructor of a Symmetric ring. It indicates the
    symbol to be used to denote s-functions in the associated Schur ring. The default value
    is {\tt s}.

  Example
    R = symmetricRing(QQ,5,SVariable => getSymbol"s");
    S = schurRing R
    s_2^2

  Text

    The chosen symbol becomes the actual indexing name for the
    basis elements of the Schur ring.  Any symbol may be used; for
    instance, {\tt sigma} yields Schur elements {\tt sigma_\lambda}
    which multiply by Littlewood-Richardson:

  Example
    T = schurRing(QQ,sigma,4)
    sigma_{1,1}^2

  Text

    An {\tt Sn}-flavored ring using a custom {\tt SVariable} stores
    characters of the symmetric group on that symbol:

  Example
    Sn = schurRing(QQ,sig,4,GroupActing => "Sn");
    sig_{3,1} * sig_{2,2}

  Text

    Distinct {\tt SVariable} choices allow a tensor product of two
    Schur rings to carry unambiguous variable names for each
    factor, so a bi-representation has a clean display:

  Example
    S = schurRing(QQ,s,3);
    T = schurRing(S,tau,2);
    s_{2,1} * tau_{1,1}

SeeAlso
  EHPVariables
///

doc ///
Key
  EHPVariables
  [schurRing,EHPVariables]
  [symmetricRing,EHPVariables]
Headline
  Specifies sequence of symbols representing e-, h-, and p-functions
Description
  Text
    This is an optional argument for the constructor of a Symmetric or Schur ring. It indicates the
    symbols to be used to denote e-, h-, and p-functions in the associated Symmetric ring. The
    default values are {\tt (e,h,p)}.

  Example
    S = schurRing(QQ,s,4,EHPVariables => (getSymbol"a",getSymbol"b",getSymbol"c"));
    R = symmetricRing S
    epol = toE s_{2,2,2}
    toS epol

  Text

    The three symbols become the polynomial-ring generators of
    the associated @TO symmetricRing@.  With the default
    {\tt (e,h,p)}, inspecting {\tt gens} shows all three families
    indexed by {\tt 1..n}:

  Example
    R = symmetricRing(QQ,3);
    gens R

  Text

    A custom triple works the same way.  Below, {\tt a_i, c_i, b_i}
    correspond respectively to the elementary, power-sum and
    complete homogeneous symmetric functions in the associated
    Symmetric ring:

  Example
    S2 = schurRing(QQ,s,4,EHPVariables => (getSymbol"a",getSymbol"b",getSymbol"c"));
    R2 = symmetricRing S2;
    gens R2

  Text

    The option propagates across tensor products of Schur rings,
    so each factor may carry its own e/h/p alphabet without name
    clashes:

  Example
    A = schurRing(QQ,sa,3,EHPVariables => (getSymbol"ea",getSymbol"ha",getSymbol"pa"));
    B = schurRing(A,sb,2,EHPVariables => (getSymbol"eb",getSymbol"hb",getSymbol"pb"));
    schurLevel B

SeeAlso
  SVariable
///

doc ///
Key
  GroupActing
  [schurRing,GroupActing]
  [symmetricRing,GroupActing]
Headline
  Specifies the group that is acting
Description
  Text
    This is an optional argument for the @TO schurRing@ and @TO symmetricRing@ functions.
    When the exterior or symmetric powers of a symmetric function {\tt g} are computed, the result
    depends on whether {\tt g} is interpreted as a virtual representation of a general
    linear, symmetric, symplectic, or orthogonal group. The option {\tt GroupActing} specifies the
    interpretation to be considered. Its possible values are {\tt "GL"} (the default),
    {\tt "Sn"}, {\tt "SL"}, {\tt "Sp"}, {\tt "O"}, and {\tt "RatGL"}.

  Example
    S = schurRing(s,2);
    exteriorPower(3,s_2)
    T = schurRing(t,2,GroupActing => "Sn");
    symmetricPower(2,t_{1,1})

  Text

    The first example computes the decomposition of {\tt \Lambda^3(Sym^2(V))} into irreducible
    {\tt GL(V)}-representations, while the second one computes the
    second symmetric power of the sign representation of the symmetric group {\tt S_2}.

    Multiplication differs sharply between the {\tt "GL"} and
    {\tt "Sn"} interpretations.  Under {\tt "GL"}, the product is
    the Littlewood-Richardson tensor product of polynomial
    representations, which is degree-additive in the partitions.
    Under {\tt "Sn"}, multiplication is the ordinary tensor
    product of characters of a single symmetric group, so only
    partitions of the {\it same} size may be multiplied, and the
    product is expanded in the Kronecker coefficients:

  Example
    Sgl = schurRing(QQ,s,4);
    s_{2,1} * s_{2,1}
    Ssn = schurRing(QQ,t,4,GroupActing => "Sn");
    t_{2,1,1} * t_{2,1,1}

  Text

    The values {\tt "Sp"} and {\tt "O"} select the stable (universal) character ring of
    the symplectic and orthogonal groups, respectively. The basis elements are indexed by
    partitions, with {\tt sp_\lambda} (respectively {\tt o_\lambda}) standing for the
    irreducible symplectic (respectively orthogonal) character associated to {\tt \lambda}.
    Multiplication in these rings is the Newell-Littlewood product, implemented by
    conversion to the Schur basis via Koike's branching formulas and back:

  Example
    Sp = schurRing(QQ,sp,GroupActing => "Sp");
    sp_{1} * sp_{1}
    sp_{1,1}
    toS sp_{1,1}
    O = schurRing(QQ,o,GroupActing => "O");
    o_{1} * o_{1}
    toS o_{2}

  Text

    Exterior and symmetric powers are likewise reinterpreted.
    In the orthogonal ring, exterior powers of the second
    fundamental character mix several Newell-Littlewood terms:

  Example
    Ofin = schurRing(QQ,obar,5,GroupActing => "O");
    exteriorPower(3, obar_2)

  Text

    Setting {\tt GroupActing => "SL"} forces the top-row
    reduction {\tt s_\lambda = s_{(\lambda_1 - \lambda_n, \ldots, \lambda_{n-1} - \lambda_n)}},
    reflecting the fact that the determinant representation is
    trivial in {\tt SL}.  Long partitions collapse accordingly:

  Example
    SL3 = schurRing(QQ,sl,3,GroupActing => "SL");
    sl_{3,2,1}
    sl_{4,2,2}
    sl_{2,1,1}

  Text

    Finally, {\tt GroupActing => "RatGL"} produces the ring of
    rational (mixed) polynomial representations of {\tt GL(V)},
    whose characters are indexed by pairs of partitions
    (bipartitions), and whose tensor product is again a
    Littlewood-Richardson-style rule:

  Example
    Rg = schurRing(QQ,r,3,GroupActing => "RatGL");
    r_({1},{}) * r_({},{1})

  Text

    Stable rings (no dimension specified) allow arbitrarily many parts in the
    partition labels; a concrete dimension can be supplied to restrict the partitions
    and to model a finite-dimensional group.

SeeAlso
  toSp
  toO
  toS
///

doc ///
Key
  Basis
  [schurRing,Basis]
  [symmetricRing,Basis]
Headline
  Specifies the basis to use for a Schur ring
Description
  Text
    This is an optional argument for the @TO schurRing@ and @TO symmetricRing@
    functions. It selects how the partition-indexed generators of the ring are
    interpreted as symmetric functions. The possible values are {\tt "Schur"}
    (the default) and {\tt "Monomial"}.

    When {\tt Basis => "Schur"}, the ring generator {\tt s_\lambda} represents the
    Schur function indexed by {\tt \lambda}. Multiplication uses the
    Littlewood-Richardson rule supplied by the Macaulay2 engine.

  Example
    S = schurRing(QQ,s,4);
    s_{2,1} * s_{1}

  Text
    When {\tt Basis => "Monomial"}, the ring generator {\tt m_\lambda} instead
    represents the monomial symmetric function indexed by {\tt \lambda}.
    Multiplication is implemented by converting to the Schur basis, multiplying
    there via Littlewood-Richardson, and converting back to the monomial basis
    using Kostka numbers. The resulting ring is abstractly isomorphic to the
    Schur-basis ring but its elements are displayed and stored as linear
    combinations of monomial symmetric functions.

  Example
    M = schurRing(QQ,m,4,Basis => "Monomial");
    m_{1} * m_{1}
    m_{2,1} * m_{1}

  Text
    The consistency of the two bases can be verified against @TO toM@, which
    converts a symmetric function to the monomial basis:

  Example
    S = schurRing(QQ,s,4);
    M = schurRing(QQ,m,4,Basis => "Monomial");
    toM(s_{1} * s_{1},M) == m_{1} * m_{1}

  Text
    A monomial-basis product agrees with the Schur-basis product
    after round-tripping through {\tt toS}, verifying that the two
    rings are isomorphic with isomorphism {\tt toM}/{\tt toS}:

  Example
    x = s_{2,1} * s_{1};
    y = toM(x,M);
    y
    toS(y,S) == x

  Text
    Converting back from the monomial basis to the Schur basis
    recovers the original Schur element:

  Example
    toS(m_{2,1} + m_{1,1,1}, S)

  Text
    The monomial-to-Schur transition is essentially indexed by
    @TO kostkaNumber@: the coefficient of {\tt m_\mu} in
    {\tt s_\lambda} equals {\tt K_{\lambda,\mu}}.  For example,
    the Kostka numbers with {\tt \lambda = (2,1)} reproduce the
    monomial expansion of {\tt s_{(2,1)}}:

  Example
    toM s_{2,1}
    kostkaNumber({2,1},{2,1})
    kostkaNumber({2,1},{1,1,1})

SeeAlso
  schurRing
  symmetricRing
  toM
  kostkaNumber
  GroupActing
///

doc ///
Key
  kostkaNumber
  (kostkaNumber,BasicList,BasicList)
Headline
  Compute a Kostka number
Usage
  k = kostkaNumber(lambda,mu)
Inputs
  lambda:BasicList
    a partition
  mu:BasicList
    a composition (or partition) of the same size as {\tt lambda}
Outputs
  k:ZZ
    the Kostka number {\tt K_{\lambda,\mu}}
Description
  Text

    The Kostka number {\tt K_{\lambda,\mu}} is the number of
    semistandard Young tableaux of shape {\tt \lambda} and content {\tt \mu}.
    Equivalently, it is the coefficient of the Schur function {\tt s_\lambda}
    in the product of complete symmetric functions {\tt h_{\mu_1} h_{\mu_2} \cdots}.

  Example
    kostkaNumber({2,1},{1,1,1})
    kostkaNumber({3},{2,1})
    kostkaNumber({2,1},{2,1})
    kostkaNumber({2,1},{3})

  Text

    Returns {\tt 0} whenever {\tt \lambda} and {\tt \mu} do not have the same
    size, or whenever {\tt \mu} does not dominate {\tt \lambda} (so that no
    tableaux of the requested shape and content exist).

  Text

    The full Kostka matrix {\tt K_{\lambda,\mu}} for partitions of
    {\tt 4}, listed in reverse dominance order
    {\tt (4), (3,1), (2,2), (2,1,1), (1,1,1,1)}, is upper
    triangular with ones on the diagonal (a reflection of the fact
    that Schur functions form an {\tt h}-triangular basis with
    respect to the monomial basis):

  Example
    P = {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}};
    matrix apply(P, la -> apply(P, mu -> kostkaNumber(la,mu)))

  Text

    A useful boundary identity: for any partition {\tt \mu} of
    {\tt n}, the Kostka number with {\tt \lambda = (n)} is {\tt 1},
    counting the unique row tableau:

  Example
    for mu in P list kostkaNumber({4}, mu)

  Text

    Similarly, the Kostka number {\tt K_{\lambda,(1^n)}} counts
    standard Young tableaux of shape {\tt \lambda}, which for
    {\tt \lambda = (2,2)} is {\tt 2} (the hook-length value),
    matching the dimension of the corresponding irreducible
    {\tt S_4}-representation:

  Example
    kostkaNumber({2,2}, {1,1,1,1})
    kostkaNumber({2,1,1}, {1,1,1,1})

SeeAlso
  toM
  toS
  Basis
///

doc ///
   Key
     toM
     (toM,RingElement)
     (toM,RingElement,SchurRing)
   Headline
     Monomial (m-) basis representation
   Usage
     fm = toM f
     fm = toM(f,M)
   Description
      Text

        Given a symmetric function {\tt f}, the function {\tt toM} returns a
        representation of {\tt f} as a linear combination of monomial symmetric
        functions. The output is a {\tt RingElement} in a
        @TO SchurRing@ with @TO Basis@ {\tt => "Monomial"}, so that the
        basis element {\tt m_\mu} represents the monomial symmetric function
        indexed by the partition {\tt \mu}.

        Internally, the conversion uses Kostka numbers: the Schur-to-monomial
        transition is {\tt s_\lambda = \sum_\mu K_{\lambda,\mu} m_\mu}.

      Example
        S = schurRing(QQ,s,4);
        toM s_{2,1}
        toM s_{3}
        toM (s_{2,1} + 2*s_{1,1,1})

      Text

        If the target monomial ring {\tt M} is not specified, the first call to
        {\tt toM} on an element of {\tt S} creates and caches an associated
        monomial-basis ring with the same number of generators and the same
        coefficient ring as {\tt S}; subsequent calls reuse this cached ring.

      Example
        ring(toM s_{2,1}) === ring(toM s_{3})

      Text

        Alternatively one can supply the target monomial ring explicitly.

      Example
        M = schurRing(QQ,m,4,Basis => "Monomial");
        toM(s_{2,1},M)

      Text

        The function also accepts elements of a Symmetric ring, in which case
        the input is first converted to the Schur basis via @TO toS@.

      Example
        R = symmetricRing(QQ,4);
        toM(h_2 * h_1)

      Text

        The identity {\tt h_n = \sum_\mu m_\mu}, summed over all
        partitions of {\tt n}, reflects the fact that the Kostka
        numbers {\tt K_{(n),\mu}} are all {\tt 1}:

      Example
        R5 = symmetricRing(QQ,5);
        toM h_3
        toM h_4

      Text

        Power-sum functions admit a particularly simple monomial
        expansion: {\tt p_n = m_{(n)}} identically, since
        {\tt p_n = \sum_i x_i^n}.  This can be read off for any
        {\tt n}:

      Example
        toM p_3
        toM p_5

      Text

        Longer partitions can still be handled; the coefficients
        are the corresponding Kostka numbers:

      Example
        S6 = schurRing(QQ,s,6);
        toM s_{4,1,1,1}

      Text

        The map {\tt toM} is invertible via @TO toS@, so a Schur
        element roundtrips through the monomial basis and back to
        the same Schur ring:

      Example
        f = s_{3,2,1};
        toS(toM f, S6) == f

   SeeAlso
     toS
     toH
     toE
     toP
     kostkaNumber
     Basis
///

doc ///
   Key
     toSp
     (toSp,RingElement)
     (toSp,RingElement,SchurRing)
   Headline
     Expansion in the basis of symplectic characters
   Usage
     fsp = toSp f
     fsp = toSp(f,Sp)
   Description
      Text

        Given a symmetric function {\tt f}, the function {\tt toSp} returns the
        expression of {\tt f} in the basis of irreducible symplectic characters.
        The output is a {\tt RingElement} in a @TO SchurRing@ with @TO GroupActing@
        {\tt => "Sp"}, so that the basis element {\tt sp_\lambda} stands for the
        irreducible {\tt Sp}-representation associated to the partition {\tt \lambda}.

        The conversion implements the inverse Koike branching formula
        {\tt s_\lambda = \sum_\delta sp_{\lambda/\delta}}, where {\tt \delta} ranges
        over partitions {\tt \delta \subseteq \lambda} all of whose columns have even
        length (equivalently, the parts of {\tt \delta} occur in equal pairs), and
        skew characters are expanded using Littlewood-Richardson coefficients.

      Example
        S = schurRing(QQ,s);
        toSp s_{2}
        toSp s_{1,1}
        toSp s_{3,1}
        toSp (s_{2,1} + 2*s_{1,1})

      Text

        If the target symplectic ring is not specified, the first call to {\tt toSp}
        on an element of {\tt S} creates and caches an associated stable symplectic
        ring with the same coefficient ring as {\tt S}; subsequent calls reuse the
        cached ring.

      Example
        ring(toSp s_{2,1}) === ring(toSp s_{3,1})

      Text

        Alternatively one can supply the target symplectic ring explicitly.

      Example
        Sp = schurRing(QQ,sp,4,GroupActing => "Sp");
        toSp(s_{2,1},Sp)

      Text

        The conversion is inverse to @TO toS@ on the symplectic ring, and
        multiplication of symplectic characters factors through this round-trip.
        At stable rank, {\tt toS(sp_{(1,1)}) = s_{(1,1)} - 1} and
        {\tt toS(sp_{(2,2)}) = s_{(2,2)} - s_{(1,1)}}; the product
        {\tt sp_{(1)}^2} equals the Newell-Littlewood product
        {\tt sp_{(2)} + sp_{(1,1)} + 1}.

      Example
        Sp = schurRing(QQ,sp,GroupActing => "Sp");
        toS sp_{1,1}
        toS sp_{2,2}
        toSp(toS(sp_{2,1}, S), Sp) == sp_{2,1}
        sp_{1}*sp_{1}
        sp_{1,1}*sp_{1,1}

      Text

        At finite rank {\tt n} (the ring parameter {\tt numVariables}), the
        modification rule of Sam-Snowden-Weyman is applied automatically, so
        characters whose partitions exceed {\tt n} rows are folded back to
        admissible Sp basis elements (or zero).  Weyl dimensions are
        available via @TO dim@.  Plethysm is supported and routes through the
        GL Schur ring.

      Example
        Sp4 = schurRing(QQ,sp4,2,GroupActing => "Sp");
        sp4_{1,1}^2
        dim sp4_{2,2}
        plethysm({2}, sp4_{1})

      Text

        An explicit symplectic character decomposition from a
        Schur polynomial can be obtained directly; linearity lets
        the user combine several Schur functions at once:

      Example
        S = schurRing(QQ,s);
        toSp(s_{2,2} + s_{1,1})

      Text

        Plethysm of a symplectic character is well-defined through
        a round-trip to the Schur basis.  Here, the plethysm of
        the trivial {\tt (2)}-power on the defining representation
        {\tt sp_{(1)}} recovers the usual {\tt Sym^2} character,
        and {\tt toS} confirms it is {\tt s_{(2)}}:

      Example
        Sp = schurRing(QQ,sp,GroupActing => "Sp");
        q = plethysm({2}, sp_{1})
        toS q

      Text

        At finite rank, the modification rule folds partitions
        that exceed the rank.  Passing {\tt s_{(2,1,1,1)}} to the
        rank-{\tt 2} symplectic ring {\tt Sp_4} returns the
        modified (and much shorter) character:

      Example
        toSp(s_{2,1,1,1}, Sp4)

   SeeAlso
     toO
     toS
     GroupActing
     dim
     plethysm
///

doc ///
   Key
     toO
     (toO,RingElement)
     (toO,RingElement,SchurRing)
   Headline
     Expansion in the basis of orthogonal characters
   Usage
     fo = toO f
     fo = toO(f,O)
   Description
      Text

        Given a symmetric function {\tt f}, the function {\tt toO} returns the
        expression of {\tt f} in the basis of irreducible orthogonal characters.
        The output is a {\tt RingElement} in a @TO SchurRing@ with @TO GroupActing@
        {\tt => "O"}, so that the basis element {\tt o_\lambda} stands for the
        irreducible {\tt O}-representation associated to the partition {\tt \lambda}.

        The conversion implements the inverse Koike branching formula
        {\tt s_\lambda = \sum_\delta o_{\lambda/\delta}}, where {\tt \delta} ranges
        over partitions {\tt \delta \subseteq \lambda} with all parts even, and skew
        characters are expanded using Littlewood-Richardson coefficients.

      Example
        S = schurRing(QQ,s);
        toO s_{2}
        toO s_{1,1}
        toO s_{3}
        toO (2*s_{2,1} - s_{1,1,1})

      Text

        If the target orthogonal ring is not specified, the first call to {\tt toO}
        on an element of {\tt S} creates and caches an associated stable orthogonal
        ring; alternatively one can supply the target ring explicitly.

      Example
        O = schurRing(QQ,o,4,GroupActing => "O");
        toO(s_{2,1},O)

      Text

        The conversion is inverse to @TO toS@ on the orthogonal ring.  At
        stable rank, {\tt toS(o_{(2)}) = s_{(2)} - 1} and
        {\tt toS(o_{(1,1)}) = s_{(1,1)}}; the product {\tt o_{(1)}^2}
        equals the Newell-Littlewood product
        {\tt o_{(2)} + o_{(1,1)} + 1}.

      Example
        O = schurRing(QQ,o,GroupActing => "O");
        toS o_{2}
        toS o_{1,1}
        toO(toS(o_{2,1}, S), O) == o_{2,1}
        o_{1}*o_{1}

      Text

        At finite rank, the modification rule for types B_n / D_n is applied
        automatically.  The tag @TO OddOrEven@ (default {\tt "Odd"})
        distinguishes {\tt O(2n+1)} (type B_n) from {\tt O(2n)} (type D_n)
        and is used by @TO dim@ to pick the right Weyl dimension formula.
        Plethysm routes through the GL Schur ring.

      Example
        OB2 = schurRing(QQ,oB,2,GroupActing => "O", OddOrEven => "Odd");
        oB_{1}^2
        dim oB_{2,2}
        plethysm({2}, oB_{1})

      Text

        The simplest nontrivial example of the inverse Koike
        branching is the second symmetric power: {\tt s_{(2)}}
        decomposes as {\tt o_{(2)} + 1} (the traceless part plus
        the invariant form):

      Example
        S = schurRing(QQ,s);
        toO s_{2}

      Text

        The conversion composes with the other symmetric-function
        transitions.  One can start from an element of a
        Symmetric ring expressed in {\tt e}- or {\tt h}-variables,
        and let {\tt toO} route it through the Schur basis:

      Example
        R = symmetricRing(QQ,5);
        toO (e_2)
        toO (h_2 * h_1)

      Text

        A rank comparison between odd and even orthogonal groups
        shows that the Weyl dimension truly depends on the value
        of {\tt OddOrEven}: the partition {\tt (2,1)} indexes a
        {\tt 105}-dimensional irreducible of {\tt O(2\cdot 3+1) = O(7)}
        and a {\tt 64}-dimensional irreducible of {\tt O(2\cdot 3) = O(6)}:

      Example
        Oodd  = schurRing(QQ, od, 3, GroupActing => "O", OddOrEven => "Odd");
        Oeven = schurRing(QQ, oe, 3, GroupActing => "O", OddOrEven => "Even");
        dim od_{2,1}
        dim oe_{2,1}

   SeeAlso
     toSp
     toS
     GroupActing
     OddOrEven
     dim
     plethysm
///

doc ///
Key
  branch
  (branch,RingElement,SchurRing,SchurRing)
  (branch,RingElement,ZZ,ZZ)
Headline
  Restrict a Schur, Sp, or O character along a two-factor subgroup
Usage
  h = branch(f, S1, S2)
  h = branch(f, m, n)
Inputs
  f:RingElement
    an element of a SchurRing of type GL, Sp, or O (stable or finite rank)
  S1:SchurRing
    the first factor ring (same GroupActing as the ring of {\tt f})
  S2:SchurRing
    the second factor ring (same GroupActing as the ring of {\tt f})
  m:ZZ
    the rank of the first factor (alternate interface)
  n:ZZ
    the rank of the second factor
Outputs
  h:HashTable
    mapping pairs of partitions {\tt (mu, nu)} to coefficients.  The entry {\tt h#(mu,nu)} is the multiplicity of the irreducible indexed by the pair in the branching of {\tt f}.
Description
  Text

    Implements the classical branching rules of R.\ C.\ King,
    {\it Branching rules for classical Lie groups using tensor and
    spinor methods}, J.\ Phys.\ A {\bf 8} (1975), 429--449.  For any
    partition $\lambda$ and a two-factor restriction of the classical
    group, the character decomposes by a universal formula expressed
    via the triple Littlewood--Richardson coefficient
    $c^\lambda_{\alpha,\beta,\gamma}$ (the coefficient of
    $s_\lambda$ in $s_\alpha s_\beta s_\gamma$).

    $\bullet$ {\tt GL}:  $s_\lambda \mid_{GL(m)\times GL(n)} = \sum c^\lambda_{\mu,\nu}\, s_\mu \otimes s_\nu$
    (the coproduct of Schur functions).

    $\bullet$ {\tt Sp}:  $sp_\lambda \mid_{Sp(2m)\times Sp(2n)} = \sum_{\delta\text{ cols-even}} c^\lambda_{\delta,\mu,\nu}\, sp_\mu \otimes sp_\nu$,
    the sum running over partitions $\delta$ whose conjugate has all parts even (equivalently, the parts of $\delta$ appear with even multiplicity).

    $\bullet$ {\tt O}:  $o_\lambda \mid_{O(a)\times O(b)} = \sum_{\delta\text{ rows-even}} c^\lambda_{\delta,\mu,\nu}\, o_\mu \otimes o_\nu$,
    where $\delta$ ranges over partitions with all parts even.

    For finite-rank factors the output partitions are collapsed via the Sam--Snowden--Weyman modification rules, so any $(\mu,\nu)$ that is killed or re-signed by the rule is handled automatically.

  Example
    S = schurRing(QQ, s, infinity);
    A = schurRing(QQ, a, infinity);
    B = schurRing(QQ, b, infinity);
    pairs branch(S_{2,1}, A, B)
  Text

    A slightly larger GL example: the restriction of $s_{3,2}$ along
    $GL(\cdot) \times GL(\cdot)$ produces the full coproduct of the
    Schur function, one term per ordered pair $(\mu,\nu)$ with
    $c^{(3,2)}_{\mu,\nu}$ nonzero:

  Example
    pairs branch(S_{3,2}, A, B)
  Text

    The Sp branching picks up an extra {\tt (mu, nu) = ({}, {})} term for $sp_{1,1}$ via $\delta = (1,1)$:

  Example
    Sp  = schurRing(QQ, sp,  infinity, GroupActing => "Sp");
    Asp = schurRing(QQ, asp, infinity, GroupActing => "Sp");
    Bsp = schurRing(QQ, bsp, infinity, GroupActing => "Sp");
    pairs branch(Sp_{1,1}, Asp, Bsp)
  Text

    For $sp_{2,1}$ the Sp branching sum runs over the two columns-even
    deltas $\delta = ()$ and $\delta = (1,1)$; the latter gives the
    correction terms supported on partitions of total weight one:

  Example
    pairs branch(Sp_{2,1}, Asp, Bsp)
  Text

    On the orthogonal side, the branching of $o_{2,1}$ runs
    over rows-even deltas $\delta = ()$ and $\delta = (2)$,
    contributing correction terms of total weight one:

  Example
    O  = schurRing(QQ, oGp, infinity, GroupActing => "O");
    AO = schurRing(QQ, aO,  infinity, GroupActing => "O");
    BO = schurRing(QQ, bO,  infinity, GroupActing => "O");
    pairs branch(oGp_{2,1}, AO, BO)
  Text

    The {\tt ZZ, ZZ} form is a convenience that builds anonymous factor rings of the requested ranks (with the same {\tt GroupActing} and, for O, the same {\tt OddOrEven}).

  Example
    Sp4 = schurRing(QQ, sp4, 2, GroupActing => "Sp");
    pairs branch(Sp4_{2,1}, 1, 1)
  Text

    The {\tt (m,n)} shortcut agrees with the result of spelling out
    anonymous factor rings.  For the GL branching of $s_{3,2}$ along
    $GL(2)\times GL(2)$:

  Example
    pairs branch(S_{3,2}, 2, 2)
SeeAlso
  schurRing
  specialize
  GroupActing
  modificationRule
///


doc ///
Key
  toRatGL
  (toRatGL,RingElement,SchurRing)
Headline
  Lift a Schur (GL, SL) character into a rational-GL Schur ring
Usage
  g = toRatGL(f, R)
Inputs
  f:RingElement
    an element of a SchurRing with GroupActing {\tt "GL"} or {\tt "SL"}, or already {\tt "RatGL"}
  R:SchurRing
    a target SchurRing with GroupActing {\tt "RatGL"}
Outputs
  g:RingElement
    the image of {\tt f} in {\tt R}, with each Schur basis element $s_\lambda$ sent to the rational Schur character $s_{\lambda, ()}$
Description
  Text

    A GroupActing {\tt "RatGL"} ring represents rational GL characters
    with bipartition weights $(\alpha, \beta)$, where $\alpha$ is the
    positive part and $\beta$ the negative part of the highest weight.
    See Koike, {\it On the decomposition of tensor products of the
    representations of classical groups}, Adv.\ Math.\ {\bf 74}
    (1989).  Ordinary GL/SL Schur characters $s_\lambda$ embed as
    rational characters with trivial second weight, $s_{\lambda, ()}$;
    {\tt toRatGL} performs this embedding coefficientwise.

    The IndexedVariableTable for a RatGL ring accepts either a bipartition
    ($s_{\{\alpha,\beta\}}$) or a plain partition ($s_{2,1}$, which is
    implicitly $s_{\{2,1\},\{\}}$).

  Example
    G = schurRing(QQ, getSymbol "g", infinity, GroupActing => "GL");
    R = schurRing(QQ, getSymbol "sRat", infinity, GroupActing => "RatGL");
    toRatGL(g_{2,1} + 3 * g_1, R)
  Text

    The simplest embedding sends the standard GL character $s_{1}$
    to the rational character $s_{(1),()}$.  A slightly richer
    input (here an LR product re-expressed by {\tt toRatGL}) goes
    through coefficientwise, so the GL-basis expansion on the right
    matches the one on the left:

  Example
    toRatGL(g_{2,1}, R)
    toRatGL(g_{1} * g_{1}, R)
  Text

    Rational GL characters admit negative powers of the determinant.
    The bipartition $s_{(), (1)}$ is the dual of the standard
    representation, and the tensor product of the standard with its
    dual decomposes as $s_{(1),(1)} \oplus s_{(),()}$ (the adjoint
    representation plus the trivial):

  Example
    Rdual = schurRing(QQ, ratD, infinity, GroupActing => "RatGL");
    ratD_{{1},{}} * ratD_{{},{1}}
  Text

    If {\tt f} already lives in a RatGL ring, it is re-embedded into
    the target {\tt R}.  When {\tt R} has finite rank $n$, a pair
    $(\alpha,\beta)$ with $\ell(\alpha)+\ell(\beta) \leq n$ is
    {\it admissible} and $s_{\alpha,\beta}$ is left unchanged;
    otherwise the Koike--Terada modification rule is applied.  The
    rule iteratively removes a border strip of length
    $L = \ell(\alpha)+\ell(\beta)-n-1$ starting at the first box of
    the last row, from both $\alpha$ and $\beta$, contributing a sign
    $(-1)^{c(R_\alpha)+c(R_\beta)-1}$ per step (where $c(R)$ counts
    the columns the strip occupies), until the result is admissible.
    The character vanishes if at some step $L = 0$ or either
    partition admits no border strip of the required length.

    The admissible boundary case $\ell(\alpha)+\ell(\beta) = n+1$
    therefore always vanishes ($L = 0$):

  Example
    S = schurRing(QQ, getSymbol "sStab", infinity, GroupActing => "RatGL");
    T = schurRing(QQ, getSymbol "sFin",  3,        GroupActing => "RatGL");
    toRatGL(sStab_{{1,1},{1,1}} + sStab_{{1},{1}}, T)
  Text

    Embedding the stable bipartitions $((2,1),(\,))$,
    $((1,1,1),(\,))$, and $((1),(1,1))$ into a finite-rank $GL(2)$
    rational ring keeps the first and kills the last two.  The first
    is admissible; the other two hit the boundary
    $\ell(\alpha)+\ell(\beta) = n+1 = 3$ so $L = 0$:

  Example
    Tfin2 = schurRing(QQ, getSymbol "sFin2", 2, GroupActing => "RatGL");
    toRatGL(sStab_{{2,1},{}}, Tfin2)
    toRatGL(sStab_{{1,1,1},{}}, Tfin2)
    toRatGL(sStab_{{1},{1,1}}, Tfin2)
  Text

    When $\ell(\alpha)+\ell(\beta) > n+1$ the rule is genuinely
    non-trivial and can produce a non-zero modified bipartition
    with a sign.  For instance, at $GL(3)$ with
    $\alpha = (4,3,2,2)$ and $\beta = (5,2,2,1,1)$ the rule removes
    a border strip of length $5$ from each partition, leaving
    $(4,1,1)$ and $(5,1)$; a further pass (now with $L=1$) strips
    one box from each, giving $(4,1)$ and $(5)$ with an overall
    sign of $-1$:

  Example
    Tfin3 = schurRing(QQ, getSymbol "sFin3", 3, GroupActing => "RatGL");
    toRatGL(sStab_{{4,3,2,2},{5,2,2,1,1}}, Tfin3)

  Text

    Another non-trivial example at $GL(4)$: the columns
    $(1^3,1^3)$ satisfy $\ell(\alpha)+\ell(\beta) = 6 > n+1 = 5$,
    so one border strip of length $L = 1$ is removed from each,
    producing $(1^2,1^2)$ with an overall sign of $-1$:

  Example
    Tfin4 = schurRing(QQ, getSymbol "sFin4", 4, GroupActing => "RatGL");
    toRatGL(sStab_{{1,1,1},{1,1,1}}, Tfin4)
SeeAlso
  schurRing
  specialize
  GroupActing
///


doc ///
Key
  toSymm
  (toSymm,RingElement)
  (toSymm,Number)
Headline
  Convert a Schur ring element to an element of the associated symmetric ring
Usage
  g = toSymm f
Inputs
  f:RingElement
    typically an element of a @TO SchurRing@ (scalars are returned unchanged)
Outputs
  g:RingElement
    the image of {\tt f} in the @TO symmetricRing@ attached to its parent ring
Description
  Text

    Every @TO SchurRing@ has an associated @TO symmetricRing@ with
    variables $e_i$, $h_i$, $p_i$.  {\tt toSymm} rewrites a Schur-basis
    element in that symmetric ring via the Jacobi-Trudi determinant
    $s_\lambda = \det(h_{\lambda_i - i + j})$.  Scalars are returned
    unchanged.

    This is the dual of @TO toS@, and the two together let you move
    freely between Schur-basis and $e$/$h$/$p$-basis representations.

  Example
    S = schurRing(QQ, s, 4);
    toSymm s_{2,1}
  Example
    toS oo
  Text

    A larger partition produces a bigger Jacobi-Trudi expansion in
    the $e$-basis; here is $s_{3,2,1}$ written in the
    symmetric ring:

  Example
    toSymm s_{3,2,1}
  Text

    {\tt toSymm} is additive and is a left-inverse of @TO toS@, so
    {\tt toS toSymm} is the identity on Schur-basis elements.  It
    distributes over sums and scalars just like any ring map:

  Example
    toSymm(s_{2,1} + 3 * s_{1,1})
    toS toSymm s_{2,1} == s_{2,1}
  Text

    In a tensor-product (two-layer) Schur ring, {\tt toSymm} is
    applied to the outermost layer only, so a product like
    $s_{2,1}\otimes t_{1,1}$ returns an element of the symmetric
    ring of {\tt S} times the original {\tt t} factor:

  Example
    T = schurRing(S, t, 3);
    toSymm(s_{2,1} * t_{1,1})
SeeAlso
  toS
  toE
  toH
  toP
  jacobiTrudi
///


doc ///
Key
  toGL
  (toGL,RingElement)
  (toGL,RingElement,SchurRing)
Headline
  Express an element in the plain GL Schur basis
Usage
  g = toGL f
  g = toGL(f, T)
Inputs
  f:RingElement
    any symmetric-function-like element (in a @TO SchurRing@ of any
    flavor or in a @TO symmetricRing@)
  T:SchurRing
    optional target ring; must have {\tt GroupActing => "GL"}
Outputs
  g:RingElement
    {\tt f} re-expressed in the Schur basis of an associated (or
    user-supplied) GL character ring
Description
  Text

    {\tt toGL} is a readability synonym for @TO toS@.  It is provided
    so that calling code documenting an intent to obtain a {\em GL
    character} reads naturally, in contrast to the more neutral
    {\tt toS}.  Conversions from @TO "Basis"@ {\tt "Monomial"}
    rings and from {\tt GroupActing}-variant rings ({\tt "Sp"},
    {\tt "O"}, {\tt "SL"}, {\tt "RatGL"}) are handled transparently.

  Example
    R = symmetricRing(QQ, 4);
    toGL(R_{0} * R_{5})  -- e_1 * h_2
  Example
    T = schurRing(QQ, t, 3);
    toGL(R_{0} * R_{5}, T)
  Text

    {\tt toGL} and @TO toS@ produce the same output on symmetric-ring
    elements; only the name of the function differs:

  Example
    toGL(e_1 * h_2) == toS(e_1 * h_2)
    toGL(h_3 - p_3) == toS(h_3 - p_3)
  Text

    A monomial-basis Schur ring expands to the plain Schur basis
    in the same way as a Kostka-inverse computation: the monomial
    symmetric function $m_\lambda$ is a signed sum of Schur
    functions.

  Example
    M = schurRing(QQ, m, 3, Basis => "Monomial");
    toGL m_{2,1}
  Text

    Conversion from an {\tt Sp}-ring applies the Koike-Terada
    branching rule that expands a symplectic character in the GL
    Schur basis; the {\tt GL(3)} Schur expansion of the $Sp(6)$
    character $sp_{2,1}$ is:

  Example
    Sp = schurRing(QQ, sp, 3, GroupActing => "Sp");
    toGL sp_{2,1}
  Text

    Finally, {\tt toGL} of a polynomial expression built from
    $e$-, $h$-, or $p$-generators in a {\tt symmetricRing} is an
    efficient way to ask for its Schur decomposition:

  Example
    U = symmetricRing(QQ, 4);
    toGL(e_1 * p_2 + h_3)
SeeAlso
  toS
  toSn
  toSp
  toO
  convert
///


doc ///
Key
  toSn
  (toSn,RingElement,SchurRing)
Headline
  Promote a Schur-basis element into an Sn character ring
Usage
  g = toSn(f, T)
Inputs
  f:RingElement
    element of a @TO SchurRing@ or @TO symmetricRing@
  T:SchurRing
    target ring; must have {\tt GroupActing => "Sn"}
Outputs
  g:RingElement
    the element of {\tt T} with the same partition-indexed
    coefficients as {\tt f} (after converting {\tt f} to the Schur
    basis)
Description
  Text

    The GL and Sn flavors of a @TO SchurRing@ share the same partition
    index set -- the distinction is in the multiplication (LR product
    vs.\ internal product) and in the semantics (polynomial GL
    character vs.\ Frobenius characteristic of an $S_n$-class
    function).  {\tt toSn} simply carries the coefficient data across.

    Finite-rank targets drop partitions with more than {\tt numgens T}
    parts.  Inputs in a variant basis ({\tt "Sp"}, {\tt "O"},
    {\tt "RatGL"}, {\tt "Monomial"}) are first expanded in the plain
    Schur basis via @TO toS@.

  Example
    S  = schurRing(QQ, s,  4);
    Sn = schurRing(QQ, n, 4, GroupActing => "Sn");
    toSn(s_{2,1} + 3 * s_{1,1,1}, Sn)
  Example
    -- internal product: trivial rep at n=3 has Frobenius characteristic s_3
    a = toSn(s_3, Sn);
    a * a
  Text

    Partition labels on the two sides match, but the multiplication
    does not: the GL (LR) product of $s_{2,1}$ with itself lives
    in degree 6, while the $S_3$-Kronecker product of the standard
    representation with itself stays in degree 3 and decomposes as
    $n_{3} + n_{2,1} + n_{1,1,1}$:

  Example
    S3  = schurRing(QQ, s3, 3);
    Sn3 = schurRing(QQ, n3, 3, GroupActing => "Sn");
    s3_{2,1} * s3_{2,1}
    b = toSn(s3_{2,1}, Sn3);
    b * b
  Text

    Inputs in variant bases ({\tt "Sp"}, {\tt "O"}, {\tt "RatGL"},
    {\tt "Monomial"}) are first expanded through @TO toS@, so
    {\tt toSn} also accepts symplectic or orthogonal characters and
    returns the corresponding $S_n$-class function:

  Example
    Sp  = schurRing(QQ, sp,  3, GroupActing => "Sp");
    toSn(sp_{2,1}, Sn3)
    Oo  = schurRing(QQ, ooX, 3, GroupActing => "O");
    toSn(ooX_{2,1}, Sn3)
SeeAlso
  toS
  toGL
  internalProduct
  convert
///


doc ///
Key
  convert
  (convert,RingElement,Ring)
Headline
  Universal dispatcher for converting between Schur ring flavors
Usage
  g = convert(f, T)
Inputs
  f:RingElement
    any symmetric-function-like element
  T:Ring
    target @TO SchurRing@ or @TO symmetricRing@
Outputs
  g:RingElement
    {\tt f} expressed in the natural basis of {\tt T}
Description
  Text

    {\tt convert} is a thin routing layer that inspects the target
    ring's classification and dispatches to the appropriate
    specialized converter:

    \begin{itemize}
      \item @TO SchurRing@ with {\tt GroupActing => "GL"}  $\to$ @TO toS@,
      \item @TO SchurRing@ with {\tt GroupActing => "Sn"}  $\to$ @TO toSn@,
      \item @TO SchurRing@ with {\tt GroupActing => "Sp"}  $\to$ @TO toSp@,
      \item @TO SchurRing@ with {\tt GroupActing => "O"}   $\to$ @TO toO@,
      \item @TO SchurRing@ with {\tt GroupActing => "SL"}  $\to$ @TO toS@,
      \item @TO SchurRing@ with {\tt GroupActing => "RatGL"} $\to$ @TO toRatGL@,
      \item @TO symmetricRing@ $\to$ @TO toSymm@ followed by promotion.
    \end{itemize}

    No new conversion mathematics is performed here; {\tt convert}
    exists purely to simplify user-level code that does not want to
    know which specialized converter to call.

  Example
    S  = schurRing(QQ, s, 4);
    Sp = schurRing(QQ, p, 2, GroupActing => "Sp");
    O  = schurRing(QQ, o, 4, GroupActing => "O");
    Sn = schurRing(QQ, n, 4, GroupActing => "Sn");
    convert(s_{2,1}, Sp)
    convert(s_{2,1}, O)
    convert(s_{2,1}, Sn)
  Text

    The dispatch works from any source basis to any target basis.
    Below we start from a monomial-basis Schur ring and send the
    same element into each of the flavors -- the dispatcher
    selects {\tt toS}, {\tt toSp}, {\tt toO}, and {\tt toSn}
    respectively:

  Example
    M    = schurRing(QQ, m, 4, Basis => "Monomial");
    convert(m_{2,1}, S)
    convert(m_{2,1}, Sp)
    convert(m_{2,1}, O)
    convert(m_{2,1}, Sn)
  Text

    To a rational-GL target, the dispatcher uses @TO toRatGL@,
    which embeds a plain GL Schur character $s_\lambda$ as the
    bipartition $s_{(\lambda, ())}$:

  Example
    RRat = schurRing(QQ, rRat, 4, GroupActing => "RatGL");
    convert(s_{2,1}, RRat)
    convert(s_{3} + 2*s_{1,1}, RRat)
  Text

    A {\tt symmetricRing} source is also handled: here the
    dispatcher routes through {\tt toSymm} and lands in the
    Schur basis of the target ring.

  Example
    U = symmetricRing(QQ, 4);
    convert(e_1 * h_2, S)
    convert(p_3, S)
SeeAlso
  toS
  toSn
  toGL
  toSp
  toO
  toRatGL
  toSymm
///


doc ///
Key
  specialize
  (specialize,RingElement,ZZ)
  (specialize,RingElement,List)
Headline
  Specialize a stable character to a finite rank
Usage
  g = specialize(f, n)
  g = specialize(f, ranks)
Inputs
  f:RingElement
    an element of a @TO SchurRing@ (of any {\tt GroupActing} flavor)
  n:ZZ
    the target rank (a nonnegative integer)
  ranks:List
    a list of target ranks, one per @TO schurLevel@; an entry equal to
    {\tt infinity} leaves that layer unchanged
Outputs
  g:RingElement
    the image of {\tt f} in the corresponding finite-rank ring
Description
  Text

    Every ring produced by @TO schurRing@ comes in two sizes: the
    {\em stable} ring (rank {\tt infinity}), which is a universal
    object admitting arbitrarily many row labels, and the finite-rank
    ring (rank {\tt n}), on which the relevant representation-theoretic
    modification rule is enforced (see @TO modificationRule@).
    The function {\tt specialize} bridges the two: it maps every
    partition-indexed basis element of the stable ring to its image in
    the finite-rank ring, collapsing or re-signing partitions that are
    ``too long'' via the modification rule of
    Sam-Snowden-Weyman (for {\tt Sp}/{\tt O}/{\tt RatGL}) or simply
    truncating (for {\tt GL}/{\tt SL}/{\tt Sn}).

    {\bf GL specialization} drops every Schur label with more than
    {\tt n} parts:

  Example
    S = schurRing(QQ, s, infinity);
    f = s_{3,2,1} + s_{2,1} + s_{1}
    specialize(f, 3)
    specialize(f, 2)

  Text

    {\bf Sp specialization} applies the type-C modification rule:
    characters $sp_\lambda$ with $\ell(\lambda) > n$ are re-expressed
    in the finite-rank ring (possibly with a sign, or as zero).

  Example
    Sp = schurRing(QQ, sp, infinity, GroupActing => "Sp");
    specialize(sp_{2,1} + sp_{1,1,1}, 2)
    specialize(sp_{1,1,1}, 1)

  Text

    Concrete low-rank $Sp$ specializations show the modification in
    action.  At rank 1 ($Sp(2)$), $sp_{1,1,1}$ is modified via
    {\tt "C"} to $-sp_{1}$, and at rank 2 ($Sp(4)$), $sp_{2,1,1}$
    has $\ell(\lambda) = 3 > 2$ and is also modified to a signed
    lower-rank character:

  Example
    specialize(sp_{1,1,1}, 1)
    specialize(sp_{2,1,1}, 1)

  Text

    {\bf O specialization} distinguishes type $B_n$ ({\tt O(2n+1)})
    from type $D_n$ ({\tt O(2n)}) via the @TO OddOrEven@ option.  If
    the stable ring has a stored {\tt OddOrEven} attribute, that value
    is used; otherwise the option must be supplied at the call site.

  Example
    O = schurRing(QQ, o, infinity, GroupActing => "O");
    specialize(o_{2,1}, 3)
    specialize(o_{2,1}, 2, OddOrEven => "Even")

  Text

    The two $O$ flavors give genuinely different images of the same
    partition on the same target rank.  For $\lambda = (2,1)$ at
    rank 3 we compare $O(7)$ with $O(6)$:

  Example
    specialize(o_{2,1}, 3, OddOrEven => "Odd")
    specialize(o_{2,1}, 3, OddOrEven => "Even")

  Text

    {\bf SL specialization} drops rows of length equal to the full
    rank, i.e. "columns" of height $n$, because the determinant
    representation is trivial in $SL(n)$.  Here the stable
    $s_{3,2}$ collapses to $s_1$ in $SL(2)$, and $s_{3,3,1}$
    collapses to $s_{2,2}$ in $SL(3)$:

  Example
    SL = schurRing(QQ, sl, infinity, GroupActing => "SL");
    specialize(sl_{3,2}, 2)
    specialize(sl_{3,3,1}, 3)

  Text

    {\bf Tower specialization}.  For a @TO SchurRing@ obtained by
    iterating the @TO schurRing@ constructor over a coefficient ring
    that is itself a @TO SchurRing@, one can specialize several layers
    at once.  The layers are listed from outermost to innermost, and
    an entry equal to {\tt infinity} leaves that layer stable.

  Example
    A = schurRing(QQ, a, infinity);
    B = schurRing(A, b, infinity, GroupActing => "Sp");
    specialize(b_{1,1} * a_{2}, {2, 3})

  Text

    In a two-layer tower of GL and Sp flavors, the outer and inner
    ranks can be adjusted independently; here is the same element
    specialized to a second choice of ranks:

  Example
    specialize(b_{2,1} + a_{3,1,1}, {3, 2})

SeeAlso
  schurRing
  modificationRule
  OddOrEven
  toSp
  toO
  toRatGL
///

doc ///
Key
  OddOrEven
  [schurRing,OddOrEven]
  [symmetricRing,OddOrEven]
  [specialize,OddOrEven]
Headline
  Select type $B_n$ or type $D_n$ for an orthogonal Schur ring
Description
  Text
    This is an optional argument for the @TO schurRing@ constructor
    (and, by extension, the @TO symmetricRing@ and @TO specialize@
    methods), relevant only when {\tt GroupActing => "O"}.  It
    distinguishes the two flavors of orthogonal group: odd
    ({\tt O(2n+1)}, type $B_n$) and even ({\tt O(2n)}, type $D_n$).
    Its possible values are {\tt "Odd"} (the default) and {\tt "Even"};
    any other value raises an error.

    For stable orthogonal rings (rank {\tt infinity}) the distinction
    is invisible at the level of multiplication, but it affects the
    dimension formula and the modification rule applied when
    @TO specialize@ is called.

  Example
    OB = schurRing(QQ, oB, 2, GroupActing => "O", OddOrEven => "Odd");
    OD = schurRing(QQ, oD, 2, GroupActing => "O", OddOrEven => "Even");
    dim oB_{1}
    dim oD_{1}

  Text

    At rank 3 and $\lambda = (2,1)$, the difference is much more
    pronounced: {\tt O(7)} (type $B_3$) gives a 105-dimensional
    irreducible, whereas {\tt O(6)} (type $D_3$) gives a
    64-dimensional one:

  Example
    Oodd  = schurRing(QQ, od, 3, GroupActing => "O", OddOrEven => "Odd");
    Oeven = schurRing(QQ, oe, 3, GroupActing => "O", OddOrEven => "Even");
    dim od_{2,1}
    dim oe_{2,1}

  Text

    The same partition of larger weight separates the two flavors
    by an even bigger factor.  For $\lambda = (3,2)$ the $O(7)$
    Weyl dimension is 693 while the $O(6)$ Weyl dimension is 300:

  Example
    dim od_{3,2}
    dim oe_{3,2}

  Text

    Supplying {\tt OddOrEven} for a non-orthogonal ring (for example
    {\tt GroupActing => "Sp"} or {\tt "GL"}) is an error.

  Example
    try (schurRing(QQ, bad, 2, GroupActing => "GL", OddOrEven => "Odd")) else print "schurRing rejected OddOrEven on a non-O ring"

SeeAlso
  GroupActing
  schurRing
  specialize
  toO
  dim
///

doc ///
Key
  modificationRule
Headline
  Apply the Sam-Snowden-Weyman modification rule
Usage
  result = modificationRule(lambda, n, type)
Inputs
  lambda:BasicList
    a partition
  n:ZZ
    the target rank
  type:String
    one of {\tt "C"}, {\tt "B"}, or {\tt "D"} -- see below
Outputs
  result:Sequence
    either a pair {\tt (tau, sign)} giving the modified partition
    $\tau$ and a sign $\pm 1$, or @TO null@ if $\lambda$ reduces to
    zero under the rule
Description
  Text

    This is the underlying combinatorial primitive used by
    @TO specialize@ and by the finite-rank multiplication in
    {\tt Sp} and {\tt O} character rings.  It implements the
    modification rules of Sam, Snowden, and Weyman, which describe
    how a universal classical group character becomes a character of
    the finite-rank group (or vanishes).

    The {\tt type} argument selects the classical family:

    $\bullet$ {\tt "C"}: symplectic groups {\tt Sp(2n)}.

    $\bullet$ {\tt "B"}: odd orthogonal groups {\tt O(2n+1)}.

    $\bullet$ {\tt "D"}: even orthogonal groups {\tt O(2n)}.

    Given a partition {\tt lambda}, the rule either returns a pair
    {\tt (tau, sign)}, meaning that the universal character indexed by
    {\tt lambda} equals {\tt sign} times the finite-rank character
    indexed by {\tt tau}, or returns {\tt null}, meaning the
    finite-rank character vanishes.

  Example
    modificationRule({2,1,1}, 1, "C")
    modificationRule({2,1,1}, 2, "C")
    modificationRule({3,1,1}, 1, "B")
    modificationRule({2,2}, 1, "D")

  Text

    A partition can become ``stuck in the bulk'' after modification:
    the rule reduces a long partition to a shorter (but still
    nonempty) partition, possibly with a sign.  For example
    $\lambda = (4,4,2,1)$ at rank 2 in type $B$ reduces to
    $\tau = (4,4,1)$ with sign $-1$:

  Example
    modificationRule({4,4,2,1}, 2, "B")

  Text

    Other partitions cancel to {\tt null}: the rule applies and the
    finite-rank character vanishes outright.  For instance, in type
    $C$ the partition $(3,2,2,2,1)$ at rank 2 is killed, and so is
    $(4,3,2,1)$ at rank 2:

  Example
    modificationRule({3,2,2,2,1}, 2, "C")
    modificationRule({4,3,2,1}, 2, "C")

  Text

    Type $B$ and type $D$ give different answers on the same
    partition even at the same rank.  For $\lambda = (3,2,1)$ at
    rank 2, type $B$ keeps the partition unchanged (with sign
    $+1$) while type $D$ kills it:

  Example
    modificationRule({3,2,1}, 2, "B")
    modificationRule({3,2,1}, 2, "D")

  Text

    Conversely, $\lambda = (4,3,2)$ at rank 2 is killed by type
    $B$ but survives (with a sign) in type $D$:

  Example
    modificationRule({4,3,2}, 2, "B")
    modificationRule({4,3,2}, 2, "D")

  Text

    Most users will not call {\tt modificationRule} directly; it is
    invoked automatically by the finite-rank multiplication and by
    @TO specialize@.  It is exported so that library code that wishes
    to implement custom variants (e.g. twisted character rings, or
    non-standard specialization schemes) can share the same
    combinatorics.

SeeAlso
  specialize
  toSp
  toO
  GroupActing
  OddOrEven
///
