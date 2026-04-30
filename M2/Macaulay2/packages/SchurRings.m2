---*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2007, 2011 Michael Stillman
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

newPackage(
	"SchurRings",
    	Version => "2.0",
    	Date => "April 17, 2026",
    	Authors => {
	     {Name => "Michael Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
	     {Name => "Hal Schenck"},
	     {Name => "Claudiu Raicu", Email => "craicu@nd.edu", HomePage => "http://math.berkeley.edu/~claudiu/"},
	     {Name => "Keller VandeBogert", Email => "keller.v@uky.edu", HomePage => "https://sites.google.com/view/kellervandebogert/home"}
	     },
	Keywords => {"Representation Theory"},
    	Headline => "representation rings of general linear groups and of symmetric groups",
	DebuggingMode => false
    	)

export {"schurRing", "SchurRing", "symmetricRing",
     "toS", "toE", "toP", "toH", "toM", "toSp", "toO", "specialize",
     "kostkaNumber",
     "jacobiTrudi", "plethysm",
     "centralizerSize", "classFunction", "symmetricFunction",
     "scalarProduct", "internalProduct",
     "SchurRingIndexedVariableTable", "EHPVariables", "SVariable",
     "ClassFunction", "schurLevel",
     "schurResolution",
     "SchurRingElement",
     "Memoize", "Schur", "EorH", "GroupActing", "Basis", "OddOrEven",
     "eVariable", "pVariable", "hVariable",
     "modificationRule",
     "branch",
     "toRatGL",
     "toSymm", "toGL", "toSn", "convert"
     }

debug Core

protect symbol symRingForE;
protect symbol mapToE;
protect symbol symRingForP;
protect symbol mapToP;
protect symbol mapFromP;
protect symbol grbE
protect symbol PtoETable
protect symbol HtoETable
protect symbol grbH
protect symbol PtoHTable
protect symbol EtoHTable
protect symbol grbP
protect symbol EtoPTable
protect symbol HtoPTable
--protect symbol plethysmMaps
protect symbol mapFromE
protect symbol sFunction
-- dispatch attributes for variant support (GL/Sn/Sp/O, Monomial, etc.)
protect symbol multiplySchurLevel1
protect symbol highLevelCombine
protect symbol plethysmFcn
protect symbol recTransOp
protect symbol kostkaCache
protect symbol monomialBasisRing
protect symbol symplecticBasisRing
protect symbol orthogonalBasisRing
protect symbol schurBasisRing
protect symbol stableSchurHelper
protect symbol plethysmHelpers
protect symbol specializedSpRings
protect symbol specializedORings
protect symbol specializedGLRings
protect symbol specializedSLRings
protect symbol specializedRatGLRings
protect symbol specializedSnRings
protect symbol associatedRatGLRing
protect symbol ratNegRing
protect symbol ratNegSym
protect symbol ratPosSym
protect symbol ratOuterRing
protect symbol ratRank
protect symbol ratStableHelper


SchurRing = new Type of EngineRing
SchurRing.synonym = "Schur ring"
ClassFunction = new Type of HashTable
ClassFunction.synonym = "Class function"

describe SchurRing := S -> Describe (expression schurRing) (expression last S.baseRings, S.Symbol, S.numgens)
undocumented (describe, SchurRing)

expression SchurRing := S -> (
    if hasAttribute(S, ReverseDictionary)
    then toString getAttribute(S, ReverseDictionary)
    else new FunctionApplication from unhold describe S)
undocumented (expression, SchurRing)

-----------------------------------------------------------------------------
-- Engine boundary: SchurRing plumbing.
-- This section wraps the C++-side `rawSchurRing1` engine (which implements
-- Littlewood-Richardson multiplication on Schur-partition monomials) as an
-- M2 `SchurRing` type.  The engine stores elements as polynomials in
-- monomials-that-are-partitions; the helpers below convert to/from that
-- representation and install element-access sugar (s_lambda, etc.) on top
-- of the raw engine ring.
-----------------------------------------------------------------------------

rawmonom2partition = (m) -> (
     -- Decode an engine Schur monomial (= a raw monomial representing s_lambda)
     -- into its partition.  `rawSparseListFormMonomial m` returns pairs
     -- (x,e) = (row-length, multiplicity); `e:x` builds an e-tuple of x's,
     -- `splice` flattens the list of tuples into the multiset of row lengths,
     -- and `reverse` puts them in weakly-decreasing (partition) order.
     reverse splice apply(rawSparseListFormMonomial m, (x,e) -> e:x)
     )

-- Strip trailing zero parts from a (weakly-decreasing) list representing a
-- partition.  Used to normalize partition keys for memoization and for
-- returning partitions without padding.
stripTrailingZeros = lst -> (
     k := #lst;
     while k > 0 and lst#(k-1) == 0 do k = k - 1;
     take(lst, k)
     )

--various ways of addressing elements of a Schur ring
-- s_{lambda_1, lambda_2, ...} from an explicit partition list
SchurRing _ List := (SR, L) -> new SR from rawSchurFromPartition(raw SR, L)
-- s_(lambda_1, lambda_2, ...) from a partition given as a sequence
SchurRing _ Sequence := (SR, L) -> new SR from rawSchurFromPartition(raw SR, L)
-- s_L for a single-row partition L; `1:L` is M2 syntax for the length-1 tuple (L)
SchurRing _ ZZ := (SR, L) -> new SR from rawSchurFromPartition(raw SR, 1:L)
--
coefficientRing SchurRing := Ring => R -> last R.baseRings
numgens SchurRing := Ring => R -> R.numgens

-- `schurLevel R` = nesting depth: the number of times schurRing/symmetricRing
-- was applied to build R.  Plain rings (e.g. QQ) have level 0; a single
-- schurRing(QQ, s, n) has level 1; schurRing(schurRing(QQ, t, m), s, n) has
-- level 2; each wrap increments the stored value by 1 (see newSchur2 below).
schurLevel = method()
schurLevel (Ring) := R -> if R.?schurLevel then R.schurLevel else 0

--Construction of Schur rings
newSchur2 = method()
newSchur2(Ring,Symbol) := (A,p) -> newSchur2(A,p,-1)

-- Concrete element type returned by `newSchurEngineRing`, used so that M2's
-- method dispatch can distinguish SchurRing elements from generic RingElement.
SchurRingElement = new Type of RingElement
newSchurEngineRing = R -> (
     -- Wrap the raw engine ring R as an M2 SchurRing of SchurRingElement,
     -- bind the RawRing pointer, and cache the identity/zero shortcuts.
     S := new SchurRing of SchurRingElement;
     S.RawRing = R;
     S#1 = 1_S;
     S#0 = 0_S;
     S)

newSchur2(Ring,Symbol,ZZ) := (A,p,n) -> (
     if not (A.?Engine and A.Engine)
     then error "expected coefficient ring handled by the engine";
     -- Build a SchurRing over coefficient ring A with rank n
     -- (n = -1 is the engine's sentinel for "infinite rank / stable GL").
     -- Steps:
     --   (1) build the engine ring via rawSchurRing1 and wrap it;
     --   (2) attach M2-side metadata (Symbol, baseRings, numgens, ...);
     --   (3) install generic engine-backed overloads (+, *, ...);
     --   (4) override expression / listForm to present elements as
     --       partition-indexed s_lambda terms;
     --   (5) propagate .char and bump .schurLevel.
     -- (1) engine ring + M2 wrapper
     SR := newSchurEngineRing rawSchurRing1(raw A,n);
     -- (2) M2-side metadata
     SR.Symbol = p;
     SR.baseRings = append(A.baseRings,A);
     SR.generators = {};
     SR.numgens = if n < 0 then infinity else n;
     SR.degreeLength = 0;
     -- (3) the basic features of SR (arithmetic, etc.) are coded at the engine level
     commonEngineRingInitializations SR;
     ONE := SR#1;
     if A.?char then SR.char = A.char;
     -- (4) pretty-printing: walk (coeff, monomial) pairs from the engine and
     --     render each monomial as s_lambda via rawmonom2partition.
     toExternalString SR := r -> toString expression r;
     expression SR := f -> (
	  (coeffs,monoms) -> sum(
	       coeffs,monoms,
	       (a,m) -> expression (if a == 1 then 1 else new A from a) *
	          new Subscript from {p, (
		    t1 := toSequence rawmonom2partition m;
		    if #t1 === 1 then t1#0 else t1
		    )})
	  ) rawPairs(raw A, raw f);
     -- structured enumeration: return [(partition, coefficient)] pairs
     listForm SR := (f) -> (
     	  n := numgens SR;
     	  (cc,mm) := rawPairs(raw A, raw f);
     	  toList apply(cc, mm, (c,m) -> (rawmonom2partition m, new A from c)));
     -- (5) nesting-depth bookkeeping
     if (A.?schurLevel) then SR.schurLevel = A.schurLevel + 1
     else SR.schurLevel = 1;
     SR
     )

-- SL(n) canonicalization: for finite SL(n), irreducibles are indexed by
-- partitions lambda with lambda_n = 0.  Any partition with n rows of
-- length k at the bottom (i.e. lambda_n = k > 0) equals
--     det^k tensor s_{lambda_1-k, ..., lambda_{n-1}-k}
-- which in SL collapses to s_{lambda_1-k, ..., lambda_{n-1}-k}.
-- Apply this row-by-row: if #lambda == n, strip lambda_n from every part.
-- Rings with numgens = infinity are treated as "stable SL" == GL (no
-- determinant to collapse since there's no top row to pin down).
slCanonicalize = (f, S) -> (
     n := numgens S;
     if n === infinity then return f;
     rawRes := raw(0_S);
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;  -- engine usually prevents this; be safe
	  lamList := toList lam | toList((n - #lam) : 0);
	  k := lamList#(n-1);
	  lamNew := (
	       if k == 0 then toList lam
	       else stripTrailingZeros(for i from 0 to n-1 list lamList#i - k)
	       );
	  rawRes = rawRes + raw promote(c, S) * raw (S_lamNew);
	  );
     new S from rawRes
     );

schurRing = method(Options => {EHPVariables => (getSymbol"e",getSymbol"h",getSymbol"p"), SVariable => getSymbol"s", GroupActing => "GL", Basis => "Schur", OddOrEven => null})
schurRing(Ring,Thing,ZZ) := SchurRing => opts -> (A,p,n) -> (
     try p = baseName p else error "schurRing: can't use provided thing as variable";
     if class p === Symbol then schurRing(A,p,n,opts)
     else error "schurRing: can't use provided thing as variable"
     );
schurRing(Ring,Thing) := SchurRing => opts -> (A,p) -> (
     try p = baseName p else error "schurRing: can't use provided thing as variable";
     if class p === Symbol then schurRing(A,p,opts)
     else error "schurRing: can't use provided thing as variable"
     );

dim SchurRingElement := s -> dimSchur s;
dim(List,SchurRingElement) := (lis,s) -> dimSchur(lis, s);
dim(Thing,SchurRingElement) := (n,s) -> dimSchur(n, s);


---------------------------------------------------------------
---- Rational (type A rational) SchurRings (Koike-Terada) ------
---------------------------------------------------------------
-- A rational irreducible representation of GL(n) is indexed by a pair
-- (alpha, beta) of partitions with ell(alpha) + ell(beta) <= n, giving the
-- dominant integral weight
--     (alpha_1, ..., alpha_p, 0, ..., 0, -beta_q, ..., -beta_1).
-- The stable universal rational character ring (Koike-Terada) has basis
-- {r_{alpha, beta}} and is isomorphic to S (x) S (two disjoint copies of the
-- ring of symmetric functions).  Multiplication is componentwise:
--    r_{alpha, beta} * r_{gamma, delta}
--         = sum_{mu, nu} c^mu_{alpha, gamma} c^nu_{beta, delta} r_{mu, nu}.
-- We therefore implement RatGL as a thin re-tagging of a level-2 GL/GL
-- SchurRing: the outer layer tracks alpha, the inner (auto-generated) layer
-- tracks beta.  All existing level-2 GL/GL multiplication infrastructure
-- applies unchanged.
--
-- Specialization of the stable universal character to a finite rank n is the
-- Koike-Terada modification rule, which is computed here by applying the
-- Weyl reflection formula to the composite weight above.
---------------------------------------------------------------

-- Koike-Terada / Sam-Snowden-Weyman modification rule for rational GL(n).
--
-- Given a stable bipartition label (alpha, beta), specialize
--     chi^{stable}_{alpha, beta} |_{GL(n)}
-- to a finite rational GL(n) character following [SSW] Sec. 5.4 in the
-- border-strip form.  A pair (alpha, beta) is *admissible* when
-- ell(alpha) + ell(beta) <= n; for admissible pairs the specialization is
-- chi^{GL(n)}_{alpha, beta} with sign +1.
--
-- When (alpha, beta) is not admissible, remove a border strip of length
--     L = ell(alpha) + ell(beta) - n - 1
-- from BOTH alpha and beta, starting at the first box of the final row
-- (equivalently the hook at (k,1) with alpha_k + ell(alpha) - k = L and
-- likewise for beta).  The sign contribution of the step is
--     (-1)^(c(R_alpha) + c(R_beta) - 1)
-- where c(R) is the number of columns the strip occupies.  Recurse until
-- admissible, or return 0 if at some step no valid strip of the required
-- length exists.  (L = 0 also forces vanishing: the strip must be non-empty.)
--
-- Returns a list of triples (alpha', beta', coef): either empty (character
-- vanishes) or a singleton [(alpha', beta', sign)] with sign in {+1, -1}.
ratGLModify = (alpha, beta, n) -> (
     a := stripTrailingZeros toList alpha;
     b := stripTrailingZeros toList beta;
     if #a + #b <= n then return {(a, b, 1)};
     sign := 1;
     while #a + #b > n do (
	  L := #a + #b - n - 1;
	  if L == 0 then return {};
	  kA := findBorderStripRow(a, L);
	  if kA === null then return {};
	  kB := findBorderStripRow(b, L);
	  if kB === null then return {};
	  (newA, cA) := removeBorderStripAtFirstColumn(a, kA);
	  (newB, cB) := removeBorderStripAtFirstColumn(b, kB);
	  if odd (cA + cB - 1) then sign = -sign;
	  a = newA;
	  b = newB;
	  );
     {(a, b, sign)}
     )

-- Enumerate all partitions contained componentwise in the given bound.
-- Result includes the empty partition and the bound itself; trailing zeros
-- are stripped.
allSubpartitionsBoundedBy = (bound) -> (
     b := stripTrailingZeros toList bound;
     if #b == 0 then return {{}};
     aux := (idx, prevMax) -> (
	  if idx >= #b then return {{}};
	  upper := min(b#idx, prevMax);
	  flatten for v from 0 to upper list
	       for t in aux(idx+1, v) list prepend(v, t)
	  );
     apply(aux(0, infinity), stripTrailingZeros)
     )

-- Iterate all (alpha, beta, scalar) triples of a RatGL element.
-- Calls f(alpha, beta, scalar) for each triple; scalar lives in the ultimate
-- base (coefficient) ring of the inner layer.
iterateRatGLTerms = (elt, f) -> (
     for outerTerm in listForm elt do (
	  alpha := toList outerTerm#0;
	  bElt := outerTerm#1;
	  for innerTerm in listForm bElt do (
	       beta := toList innerTerm#0;
	       scalar := innerTerm#1;
	       f(alpha, beta, scalar);
	       );
	  );
     )

-- Stable (infinity-rank) RatGL helper ring attached to a finite RatGL ring S.
-- Lazily constructed and cached on the ring.  Used by finite RatGL
-- multiplication (stable componentwise LR, then Koike-Terada modification).
stableRatGLHelperOf = (S) -> (
     if S.?ratStableHelper then S.ratStableHelper
     else (
	  baseR := coefficientRing (S.ratNegRing);
	  helperSym := getSymbol("ratHelper" | toString hash S);
	  H := schurRing(baseR, helperSym, infinity, GroupActing => "RatGL");
	  S.ratStableHelper = H;
	  H
	  )
     )

buildRatGLRing = (R, p, n, opts) -> (
     ------------------------------------------------------------------
     -- (1) Inner / outer ring construction
     --     Build a two-layer GL SchurRing tower: the inner layer B
     --     (beta / "negative" variables) sits over R, and the outer
     --     layer S (alpha / "positive" variables) sits over B.  The
     --     bipartition basis element r_{alpha,beta} will later be
     --     realized as (S_alpha) * (lift of B_beta into S).
     ------------------------------------------------------------------
     negSym := getSymbol(toString p | "Neg");
     innerOpts := opts ++ {GroupActing => "GL", OddOrEven => null};
     B := schurRing(R, negSym, n, innerOpts);
     outerOpts := opts ++ {GroupActing => "GL", OddOrEven => null};
     S := schurRing(B, p, n, outerOpts);

     ------------------------------------------------------------------
     -- (2) Metadata tagging
     --     Re-tag S as the RatGL ring and remember both layers.
     ------------------------------------------------------------------
     S.GroupActing = "RatGL";
     S.ratNegRing = B;
     S.ratNegSym = negSym;
     S.ratPosSym = p;
     S.ratRank = n;
     B.ratOuterRing = S;

     ------------------------------------------------------------------
     -- (3) Multiplication override
     --     Multiplication on universal rational characters is the
     --     Koike product, NOT the componentwise Littlewood-Richardson
     --     product on the auxiliary Schur-polynomial basis.
     --       * Stable rank (n = infinity): install the raw Koike
     --         product directly.
     --       * Finite rank n: lift both factors into the stable
     --         helper ring, multiply there via Koike, then apply the
     --         Koike-Terada (Sam-Snowden-Weyman) modification rule
     --         to specialize back down to rank n.
     ------------------------------------------------------------------
     isFiniteN := not (class n === InfiniteNumber or n < 0);
     if isFiniteN then (
	  S * S := (f1, f2) -> (
	       H := stableRatGLHelperOf S;
	       h1 := toRatGL(f1, H);
	       h2 := toRatGL(f2, H);
	       hProd := h1 * h2;
	       specializeRatGLInto(hProd, n, S)
	       );
	  )
     else (
	  -- Stable RatGL: override componentwise LR with the Koike product.
	  S * S := (f1, f2) -> new S from ratGLKoikeProductRaw(S, f1, f2);
	  );

     ------------------------------------------------------------------
     -- (4) Bipartition-syntax override via IndexedVariableTable
     --     Override so that user-facing bipartition syntax
     --       r_{{a1,a2,...}, {b1,b2,...}}   ->   r_{(alpha, beta)}
     --     builds the basis element directly.  At finite rank, if
     --     #alpha + #beta > n the character reduces via Koike-Terada;
     --     we apply that modification rule here.
     ------------------------------------------------------------------
     t := value p;
     t#symbol _ = a -> (
	  isListish := x -> instance(x, VisibleList);
	  if isListish a and #a == 2 and isListish (a#0) and isListish (a#1) then (
	       alpha := toList a#0;
	       beta := toList a#1;
	       isFin := not (class n === InfiniteNumber or n < 0);
	       if isFin and (#alpha + #beta) > n then (
		    return specializeRatGLInto(
			 new S from ratGLBasisRaw(S, alpha, beta),
			 n, S);
		    );
	       return new S from ratGLBasisRaw(S, alpha, beta);
	       );
	  S _ a
	  );
     S.use = So -> (globalAssign(p, t); So);
     S.use S;

     ------------------------------------------------------------------
     -- (5) Pretty-printing override
     --     Display r_(alpha, beta) instead of the internal product
     --     a_alpha * b_beta that actually represents it.
     ------------------------------------------------------------------
     expression S := f -> (
	  acc := null;
	  (outerCoeffs, outerMonoms) := rawPairs(raw B, raw f);
	  for i from 0 to #outerMonoms - 1 do (
	       alphaLst := toList rawmonom2partition (outerMonoms#i);
	       bElt := new B from (outerCoeffs#i);
	       (innerCoeffs, innerMonoms) := rawPairs(raw R, raw bElt);
	       for j from 0 to #innerMonoms - 1 do (
		    betaLst := toList rawmonom2partition (innerMonoms#j);
		    sc := new R from (innerCoeffs#j);
		    sub := new Subscript from {p, {alphaLst, betaLst}};
		    term := if sc == 1 then expression sub
			    else (expression sc) * (expression sub);
		    acc = if acc === null then term else acc + term;
		    );
	       );
	  if acc === null then expression 0 else acc
	  );
     S
     )

schurRing(Ring,Symbol) := opts -> (R,p) -> schurRing(R,p,infinity,opts)
schurRing(Ring,Symbol,InfiniteNumber) :=
schurRing(Ring,Symbol,ZZ) := SchurRing => opts -> (R,p,n) -> (
     ------------------------------------------------------------------
     -- (1) RatGL short-circuit
     --     Rational GL (Koike-Terada bipartition) rings have their
     --     own builder; dispatch there and return immediately.
     ------------------------------------------------------------------
     if opts.GroupActing == "RatGL" then return buildRatGLRing(R, p, n, opts);

     ------------------------------------------------------------------
     -- (2) newSchur2 construction + field copies
     --     Create the raw engine-level Schur ring S and copy across
     --     option data (EHPVariables, GroupActing, Basis).  Also:
     --       * handle the OddOrEven tag for orthogonal rings,
     --       * initialize the kostkaCache,
     --       * install plethysm-related operator overloads (@, ^,
     --         symmetricPower, exteriorPower) that are common to all
     --         GroupActing variants.
     ------------------------------------------------------------------
     S := local S;
     if n == infinity then S = newSchur2(R,p,-1) else S = newSchur2(R,p,n);
     S.EHPVariables = opts.EHPVariables;
     --S.SVariable = opts.SVariable;
     S.GroupActing = opts.GroupActing;
     S.Basis = opts.Basis;

     -- For GroupActing "O", distinguish O(2n+1) (type B_n, "Odd") from O(2n)
     -- (type D_n, "Even") at finite rank.  Stable rings (n = infinity) ignore
     -- the tag (both limits coincide) but we still store a default for
     -- downstream code that wants to read it.
     if opts.GroupActing == "O" then (
	  if opts.OddOrEven === null then S.OddOrEven = "Odd"
	  else if opts.OddOrEven === "Odd" or opts.OddOrEven === "Even" then
	       S.OddOrEven = opts.OddOrEven
	  else error("schurRing: OddOrEven must be \"Odd\" or \"Even\"; got " | toString opts.OddOrEven);
	  )
     else if opts.OddOrEven =!= null then
	  error "schurRing: OddOrEven is only meaningful with GroupActing => \"O\"";

     S.kostkaCache = new MutableHashTable;
     S @ RingElement := RingElement @ S := (f1,f2) -> plethysm(f1,f2);
     S^ZZ := (f,n) -> product apply(n,i->f);
     symmetricPower(ZZ,S) := (n,s) -> plethysm({n},s);
     exteriorPower(ZZ,S) := opts -> (n,s) -> plethysm(splice{n:1},s);

     --define the multiplication on S
     -- Raw (engine-level) multiplication — available for all variants
     rawMult := (f1,f2) -> new S from raw f1 * raw f2;

     -- For Sn: install ** operator (needed for recTrans and higher-level combine)
     if opts.GroupActing == "Sn" then (
     	  S ** S := (f1,f2) -> rawMult(f1,f2);
	  RingElement ** S := (f,g) -> if member(ring f,S.baseRings | {S}) then promote(f,S) ** g
	       	    	      	   	else if member(S,(ring f).baseRings | {ring f}) then f ** promote(g,S);
	  Number ** S := (f,g) -> if member(ring f,S.baseRings | {S}) then promote(f,S) ** g;
	  S ** Number := (f,g) -> if member(ring g,S.baseRings | {S}) then f ** promote(g,S);
     );

     ------------------------------------------------------------------
     -- (3) Dispatch table
     --     Each GroupActing variant installs three callbacks on S
     --     that are consumed by the unified S*S routine below:
     --       * S.multiplySchurLevel1(f1,f2)  -- product at schurLevel 1
     --       * S.highLevelCombine(c, bProd)  -- how to stitch a
     --                                          coefficient-ring
     --                                          product with a
     --                                          level-1 basis product
     --                                          at higher schurLevel
     --       * S.plethysmFcn                 -- plethysm algorithm
     --       * S.recTransOp                  -- operator used inside
     --                                          recTrans for
     --                                          recursive basis
     --                                          conversion
     ------------------------------------------------------------------
     -- Register dispatch functions based on GroupActing
     if opts.GroupActing == "GL" then (
	  -- -- GL: ordinary general linear group
	  --    Level-1 product is the raw engine Littlewood-Richardson
	  --    product; higher-level combine is ordinary * ; plethysm
	  --    and recTrans both multiplicative.
	  S.multiplySchurLevel1 = (f1,f2) -> rawMult(f1,f2);
	  S.highLevelCombine = (coeff, basisProd) -> rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmGL;
	  S.recTransOp = (a,b) -> a*b;
     ) else if opts.GroupActing == "SL" then (
	  -- -- SL: special linear (collapse determinant)
	  --    Same multiplication as GL, then strip a full column of n
	  --    rows (determinant character is trivial in SL).  Stable
	  --    SL (numgens = infinity) coincides with stable GL.
	  S.multiplySchurLevel1 = (f1,f2) -> slCanonicalize(rawMult(f1,f2), S);
	  S.highLevelCombine = (coeff, basisProd) ->
	       rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmGL;
	  S.recTransOp = (a,b) -> a*b;
     ) else if opts.GroupActing == "Sn" then (
	  -- -- Sn: symmetric group (internal product / Kronecker)
	  --    Level-1 product is the internal (Kronecker) product of
	  --    Sn characters; both high-level combine and recTransOp
	  --    also use **, since the Sn ring is "internal" at every
	  --    schurLevel.  Liftable / zero shortcut avoids engine
	  --    edge cases on scalar factors.
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       cS := coefficientRing S;
	       if liftable(f1,cS) or liftable(f2,cS) then f1 ** f2 else
	       if f1 == 0 or f2 == 0 then 0_S else
	       internalProduct(f1,f2)
	  );
	  S.highLevelCombine = (coeff, basisProd) -> coeff ** basisProd;
	  S.plethysmFcn = plethysmSn;
	  S.recTransOp = (a,b) -> a**b;
     ) else if opts.GroupActing == "Sp" then (
	  -- -- Sp: symplectic (Newell-Littlewood via stable helper)
	  --    Multiplication goes via convert-to-Schur, LR-multiply,
	  --    convert-back (== Newell-Littlewood rule).  We compute
	  --    the GL product in a STABLE Schur helper ring so that
	  --    partitions with more than numgens S rows survive to be
	  --    collapsed by the modification rule in schurToSpRE (the
	  --    finite-n Sp ring's own engine would otherwise truncate
	  --    them too early).
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       H := stableSchurHelperOf S;
	       sf1 := spToSchurRE(f1, H);
	       sf2 := spToSchurRE(f2, H);
	       prod := new H from raw sf1 * raw sf2;
	       schurToSpRE(prod, S)
	  );
	  S.highLevelCombine = (coeff, basisProd) -> rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmSp;
	  S.recTransOp = (a,b) -> a*b;
     ) else if opts.GroupActing == "O" then (
	  -- -- O: orthogonal (Newell-Littlewood via stable helper)
	  --    Same stable-helper trick as Sp, but with the orthogonal
	  --    conversion maps oToSchurRE / schurToORE.
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       H := stableSchurHelperOf S;
	       sf1 := oToSchurRE(f1, H);
	       sf2 := oToSchurRE(f2, H);
	       prod := new H from raw sf1 * raw sf2;
	       schurToORE(prod, S)
	  );
	  S.highLevelCombine = (coeff, basisProd) -> rawMult(promote(coeff,S), basisProd);
	  S.plethysmFcn = plethysmO;
	  S.recTransOp = (a,b) -> a*b;
     ) else error("Unknown GroupActing: " | toString opts.GroupActing);

     ------------------------------------------------------------------
     -- (4) Monomial-basis override
     --     If the user selected the monomial-symmetric basis, we
     --     overwrite multiplySchurLevel1 post-hoc: convert both
     --     factors to the Schur basis, multiply via engine LR, and
     --     convert back.  All other dispatch callbacks are inherited
     --     from the GroupActing branch above.
     ------------------------------------------------------------------
     -- Override multiplication for Monomial basis
     if opts.Basis == "Monomial" then (
	  S.multiplySchurLevel1 = (f1,f2) -> (
	       -- Convert from monomial to Schur
	       sf1 := monomialToSchurRE(f1, S);
	       sf2 := monomialToSchurRE(f2, S);
	       -- Multiply via engine (Littlewood-Richardson)
	       prod := new S from raw sf1 * raw sf2;
	       -- Convert back to monomial
	       schurToMonomialRE(prod, S)
	  );
     );

     ------------------------------------------------------------------
     -- (5) Unified S * S dispatch
     --     At schurLevel 1 we call multiplySchurLevel1 directly.  At
     --     higher schurLevel we expand each factor via listForm into
     --     (partition, coefficient) pairs, multiply bases pairwise
     --     via multiplySchurLevel1, and stitch the coefficient
     --     product back in via highLevelCombine.
     ------------------------------------------------------------------
     -- Unified multiplication dispatch
     S * S := (f1,f2) ->
	  if schurLevel S == 1 then S.multiplySchurLevel1(f1,f2)
	  else (
	       lF1 := listForm f1;
	       lF2 := listForm f2;
	       sum flatten for p1 in lF1 list
		    for p2 in lF2 list
			 S.highLevelCombine(
			      (last p1) * (last p2),
			      S.multiplySchurLevel1(S_(first p1), S_(first p2))
			 )
	  );

     ------------------------------------------------------------------
     -- (6) IndexedVariableTable setup + S.use
     --     Wire up the subscript syntax S_lambda.  For SL rings we
     --     canonicalize the result (strip determinant columns).
     ------------------------------------------------------------------
     t := new SchurRingIndexedVariableTable from p;
     t.SchurRing = S;
     if opts.GroupActing == "SL" then
	  t#symbol _ = a -> slCanonicalize(S _ a, S)
     else
	  t#symbol _ = a -> ( S _ a);
     S.use = S -> (globalAssign(p,t); S);
     S.use S;
     S)

--constructs the Schur ring of a symmetric ring R
--this is a ring with basis consisting of s-polynomials (Schur functions) that
--is abstractly isomorphic to R
--schurRingOf = method()
--schurRingOf (Ring) := R -> (
schurRing (Ring) := opts -> R -> (
     	  if R.?Schur then R.Schur else
	  if schurLevel R > 0 then
	  (
	       if instance(R, SchurRing) then R else
	       (
		    s := R.SVariable;
     	       	    if schurLevel R == 1 then R.Schur = schurRing(coefficientRing R,s,R.dim,EHPVariables => R.EHPVariables, GroupActing => R.GroupActing, Basis => if R.?Basis then R.Basis else "Schur")
		       else R.Schur = schurRing(schurRing coefficientRing R,s,R.dim,EHPVariables => R.EHPVariables, GroupActing => R.GroupActing, Basis => if R.?Basis then R.Basis else "Schur"); --symmetricRing is wrong, right?
     	       	    R.Schur.symmetricRing = R;
	       	    R.Schur
		    )
	       )
	  else error"Expected ring to have a Schur Ring"
     )

schurRing(Thing,ZZ) := opts -> (s,n) -> schurRing(QQ,s,n,opts)
schurRing(Thing,InfiniteNumber) := opts -> (s,n) -> schurRing(QQ,s,n,opts)
schurRing(Thing) := opts -> (s) -> schurRing(QQ,s,-1,opts)

undocumented (schurRing,Ring,Symbol,InfiniteNumber)
undocumented (schurRing,Thing,InfiniteNumber)

--a new type that indexes the elements in the s-basis of a Schur ring
SchurRingIndexedVariableTable = new Type of IndexedVariableTable
SchurRingIndexedVariableTable _ Thing := (x,i) -> x#symbol _ i

--construction of symmetric rings
symmetricRing = method(Options => options schurRing)
symmetricRing (Ring,ZZ) := opts -> (A,n) -> (
	  -- ===================================================================
	  -- Build R = A[e_1..e_n, p_1..p_n, h_1..h_n]: a polynomial ring over A
	  -- with 3n generators in the fixed block order
	  --     R_0       .. R_{n-1}   = e_1..e_n    (elementary)
	  --     R_n       .. R_{2n-1}  = p_1..p_n    (power sum)
	  --     R_{2n}    .. R_{3n-1}  = h_1..h_n    (complete homogeneous)
	  -- deg(e_i) = deg(p_i) = deg(h_i) = i.
	  --
	  -- symRingForE is the SAME ring with the variable order [h | p | e];
	  -- under GRevLex this makes e smallest, so a GB killing
	  --     { h_i - H_i(e),  p_i - P_i(e) }
	  -- reduces any polynomial to e-only. symRingForP is analogous with
	  -- e and p swapped, making p smallest. For H-reduction we just reuse
	  -- R itself (default GRevLex places h last, so h is already smallest).
	  -- ===================================================================

	  -- ==== Construct R and register E/P/H variable accessors ====
     	  (e,h,p) := opts.EHPVariables;
     	  R := A[e_1..e_n,p_1..p_n,h_1..h_n,
	    Degrees => toList(1..n,1..n,1..n), MonomialSize => 8];
     	  R.EHPVariables = opts.EHPVariables;
	  R.SVariable = opts.SVariable;
	  -- Index arithmetic follows the block layout above:
	  --   e_i = R_(i-1),  p_i = R_(n+i-1),  h_i = R_(2n+i-1).
       	  R.eVariable = (i) -> if 1 <= i and i <= n then R_(i-1) else error"Invalid index";
       	  R.pVariable = (i) -> if 1 <= i and i <= n then R_(n+i-1) else error"Invalid index";
       	  R.hVariable = (i) -> if 1 <= i and i <= n then R_(2*n+i-1) else error"Invalid index";
     	  R.GroupActing = opts.GroupActing;
	  R.Basis = opts.Basis;
     	  R.dim = n;

	  -- ==== Propagate dispatch function for plethysm ====
	  if opts.GroupActing == "GL" or opts.GroupActing == "SL" then R.plethysmFcn = plethysmGL
	  else if opts.GroupActing == "Sn" then R.plethysmFcn = plethysmSn;

	  -- ==== Operator overloads on R ====
	  R ** R := (f1,f2) -> internalProduct(f1,f2); --internal product of symmetric functions
     	  R @ RingElement := RingElement @ R := (f1,f2) -> plethysm(f1,f2);
     	  symmetricPower(ZZ,R) := (n,r) -> plethysm({n},r);
     	  exteriorPower(ZZ,R) := opts -> (n,r) -> plethysm(splice{n:1},r);

	  -- ==== Degree sequence and block indices inside R ====
	  -- The degrees of e_i, p_i, h_i are all i, so (1,2,..,n) is repeated
	  -- three times across the concatenated variable list.
	  degSeq := toList(1..n);
	  -- eIdx / pIdx / hIdx are the R-indices of the e / p / h blocks.
     	  blocks := {toList(0..(n-1)),toList(n..(2*n-1)),toList(2*n..(3*n-1))};
	  eIdx := blocks#0;
	  pIdx := blocks#1;
	  hIdx := blocks#2;

	  -- ==== Fresh placeholder symbols for the reordered auxiliary rings ====
	  -- Note: `vrs := symbol vrs` is the idiomatic way to introduce a
	  -- fresh local symbol in M2; the LHS declaration also keeps the
	  -- package's symbol checker happy.
	  vrs := symbol vrs;
	  tempSym := vrs;
	  tempVarsE := apply(eIdx,i->tempSym_i);
	  tempVarsP := apply(pIdx,i->tempSym_i);
	  tempVarsH := apply(hIdx,i->tempSym_i);

	  -- ==== Auxiliary rings with reordered variables ====
	  -- They differ from R only in the order of the variables.
	  -- R itself is used by default for conversion to H-polynomials.

	  -- symRingForE: variable order [h | p | e].  Under GRevLex this makes
	  -- the e-block smallest, so grbE (defined below) reduces any
	  -- polynomial to its e-only representative.
          R.symRingForE = A[tempVarsH | tempVarsP | tempVarsE ,Degrees=>flatten toList(3:degSeq),MonomialOrder=>GRevLex, MonomialSize => 8];
     	  R.mapToE = map(R.symRingForE,R,apply(hIdx|pIdx|eIdx,i->(R.symRingForE)_i));
     	  R.mapFromE = map(R,R.symRingForE,apply(hIdx|pIdx|eIdx,i->R_i));

	  -- symRingForP: variable order [h | e | p].  Under GRevLex this makes
	  -- the p-block smallest, so grbP reduces any polynomial to its
	  -- p-only representative.
     	  R.symRingForP = A[tempVarsH | tempVarsE | tempVarsP,Degrees=>flatten toList(3:degSeq),MonomialOrder=>GRevLex, MonomialSize => 8];
     	  R.mapToP = map(R.symRingForP,R,apply(pIdx|hIdx|eIdx,i->(R.symRingForP)_i));
     	  R.mapFromP = map(R,R.symRingForP,apply(hIdx|eIdx|pIdx,i->R_i));

	  -- ==== Conversion tables between E-, H- and P- polynomials ====
     	  EtoP(n,R);
     	  PtoE(n,R);
     	  HtoE(n,R);
     	  EtoH(n,R);
     	  PtoH(n,R);
     	  HtoP(n,R);

	  -- ==== Groebner bases for conversion between E-, H- and P- polynomials ====
	  -- Each GB lists, for i = 1..n, relations of the form
	  --     p_i - (p_i expressed in the target basis)
	  --     h_i - (h_i expressed in the target basis)
	  -- so that reduction mod the GB kills the non-target blocks.
	  -- grbE / grbP live in symRingForE / symRingForP; grbH lives in R
	  -- itself (default GRevLex already makes h smallest).
     	  R.grbE = forceGB matrix(R.symRingForE, {flatten apply(splice{1..n},i->{R.mapToE(R_(n-1+i))-R.PtoETable#i,R.mapToE(R_(2*n-1+i))-R.HtoETable#i})});
     	  R.grbH = forceGB matrix(R, {flatten apply(splice{1..n},i->{R_(n-1+i)-R.PtoHTable#i,R_(-1+i)-R.EtoHTable#i})});
     	  R.grbP = forceGB matrix(R.symRingForP, {flatten apply(splice{1..n},i->{R.mapToP(R_(-1+i))-R.EtoPTable#i,R.mapToP(R_(2*n-1+i))-R.HtoPTable#i})});
     	  collectGarbage();

	  -- ==== Basis-projection maps: rewrite f using only E-, P- or H-vars ====
     	  R.mapSymToE = (f) -> R.mapFromE(R.mapToE(f)%R.grbE);
     	  R.mapSymToP = (f) -> R.mapFromP(R.mapToP(f)%R.grbP);
     	  R.mapSymToH = (f) -> f%R.grbH;

	  -- ==== Schur level: one more than that of the base ring ====
	  if (A.?schurLevel) then R.schurLevel = A.schurLevel + 1
     	  else R.schurLevel = 1;
     	  R)

--constructs the symmetric ring of a Schur ring 
--if the Schur ring has dimension n, its symmetric ring is the polynomial ring
--in the variables e_1,...,e_n,p_1,...,p_n,h_1,...,h_n, i.e. the other types of
--symmetric functions (besides the Schur functions) that the package implements
symmetricRing (Ring) := opts -> R -> (
     	  if R.?symmetricRing then R.symmetricRing else
	  if class R === SchurRing then
	  (
	       if numgens R === infinity then 
	          error"symmetric ring expects finite schurRings";
     	       if coefficientRing R === ZZ then
	       	  error"base ring has to be QQ";
     	       R.symmetricRing = symmetricRing(symmetricRing coefficientRing R,numgens R,EHPVariables => R.EHPVariables, SVariable => R.Symbol, GroupActing => R.GroupActing, Basis => R.Basis);
     	       R.symmetricRing.Schur = R;
	       R.symmetricRing
	       )
	  else R
     )

symmetricRing(ZZ) := opts -> n -> symmetricRing(QQ,n,opts)

---------------------------------------------------------------
--------------Jacobi-Trudi-------------------------------------
---------------------------------------------------------------
--
-- The Jacobi-Trudi identity expresses a Schur function as a
-- determinant in the complete-homogeneous (h) or elementary (e)
-- symmetric functions:
--
--     s_lambda      = det( h_{lambda_i - i + j} )        (H-variant)
--     s_lambda      = det( e_{lambda'_i - i + j} )       (E-variant,
--                                                        lambda' = conjugate)
--
-- `jacobiTrudi` builds the matrix and either calls det() directly
-- (Memoize => false) or expands cofactor-wise via the recursive
-- helper `jT` (Memoize => true), caching results on R.sFunction.
--
-- File-scope "channel" between jacobiTrudi and jT:
-- `jT` is a plain (non-method) recursive function and needs access to
-- the ambient ring, its dim, and the E/H flag on every recursive call.
-- Rather than thread these through every argument list, we publish them
-- as file-scope locals that `jacobiTrudi` sets just before calling `jT`.
-- These names are a deliberate protocol; do not rename them.
auxR = local auxR;      -- the ambient symmetricRing in use
auxn = local auxn;      -- R.dim (number of variables)
auxEH = local auxEH;    -- 0 for E-variant, 1 for H-variant
----

-- Shared cached symmetricRing workspace.  Multiple internal routines
-- (skewSchurExpansion, plethysm, kostkaNumber, ...) need a transient
-- symmetricRing(QQ, n) just to run jacobiTrudi / toS on a partition
-- before reading off the partition-coefficient list.  Constructing a
-- fresh symmetricRing each time costs ~40-90ms, which dominates small
-- calls; caching a single ring and growing it on demand makes repeat
-- calls essentially free.
--
-- Safety: callers only ever lift partition/coefficient data out of the
-- ring; they never hand a raw element back to the user or compare
-- elements from different calls for identity.  Growing the ring between
-- calls therefore can't invalidate earlier results.
workSymRing := null;
workSymRingSize := 0;

-- Ensure the cached symmetricRing has dim >= need.  Grow (not shrink)
-- in powers-of-two-ish increments to amortize allocation cost.
-- Uses *private* (local) symbols for the e/h/p/s variables so that
-- constructing (or growing) the cached ring does NOT rebind the
-- user's global e, h, p, s at the top level.  Rebinding those would
-- cause expressions like `e_4` or `h_3` entered after a call into
-- plethysm / skewSchurExpansion to evaluate into the wrong ring.
ensureWorkSymRing := (need) -> (
     if workSymRing === null or workSymRingSize < need then (
	  newSize := max(need, 8);
	  if workSymRingSize > 0 then
	       newSize = max(newSize, 2 * workSymRingSize);
	  kv := local kv;
	  ev := local ev;
	  hv := local hv;
	  pv := local pv;
	  workSymRing = symmetricRing(QQ, newSize,
	       SVariable => kv,
	       EHPVariables => (ev, hv, pv));
	  workSymRingSize = newSize;
	  );
     workSymRing
     )

-- Backward-compat alias for readers; same cache.
ensureSkewWorkRing := ensureWorkSymRing

jacobiTrudi = method(Options => {Memoize => true, EorH => "E"})
jacobiTrudi(BasicList,Ring) := opts -> (lambda,R) ->
(
     lam := new Partition from lambda;
     rez := local rez;
     local u;
     if opts.EorH == "H" then u = R.hVariable else (u = R.eVariable;lam = conjugate lam;);
     if opts.Memoize then
     (
	  if not R.?sFunction then R.sFunction = new MutableHashTable;
	  if opts.EorH == "E" then
	  (
     	       -----sFunction#0 records s-polynomials in terms of the e-variables
	       if not R.sFunction#?0 then R.sFunction#0 = new MutableHashTable;
	       auxEH = 0;
	       )
	  else
	  (
     	       -----sFunction#1 records s-polynomials in terms of the h-variables
	       if not R.sFunction#?1 then R.sFunction#1 = new MutableHashTable;
	       auxEH = 1;
	       );
     	  auxR = R;
     	  auxn = R.dim;
     	  rez = jT(lam);
	  )
     else
     (
     	  n := #lam;
     	  rez = det(map(R^n, n, (i,j) -> 
	       (
	       	    aux := lam#i-i+j;
	       	    if aux < 0 or aux>R.dim then 0_R
	       	    else if aux == 0 then 1_R else u aux)
	       ),
	  Strategy => Cofactor);
	  );
     rez
     )

-- Computes the Jacobi-Trudi determinant recursively, via cofactor
-- expansion along the last row of the Jacobi-Trudi matrix.
--
-- The underlying symmetricRing lays its 3n generators out as
--     indices  0 .. n-1      e_1 .. e_n     (elementary)
--     indices  n .. 2n-1     p_1 .. p_n     (power sums)
--     indices  2n .. 3n-1    h_1 .. h_n     (complete homogeneous)
-- so we reach e_k and h_k uniformly by offsetting into the generator
-- list.  With auxEH in {0,1}, the single expression
--
--     auxR_(2*auxEH*auxn - 1 + k)
--
-- selects e_k (auxEH = 0: offset -1, i.e. indices start at 0) or
-- h_k (auxEH = 1: offset 2n-1, i.e. indices start at 2n).  We bind
-- this as `basisOffset` for readability.
jT = (lambda) ->
(
     lambda = toList lambda;
     rez := local rez;
     if auxR.sFunction#auxEH#?lambda then rez = auxR.sFunction#auxEH#lambda
     else
     (
     basisOffset := 2*auxEH*auxn - 1;  -- see comment above
     k := #lambda;
     if k == 0 or lambda#0 == 0 then rez = 1_auxR else
     if k == 1 then rez = auxR_(basisOffset + lambda#0) else
     (
	  -- Cofactor expansion along the last row of the Jacobi-Trudi
	  -- matrix.  At step i we pull out the entry coming from the
	  -- (ll-1-i)-th part of lambda; `leftPart` is the prefix whose
	  -- indices have not yet been consumed, `rightPart` collects
	  -- the shifted tail contributed by the already-consumed
	  -- parts, and `sign` alternates the cofactor sign.
	  leftPart := drop(lambda,-1);
     	  rightPart := {};
	  rez = 0;
	  sign := 1;
	  for i from 0 to k-1 do
	  (
     	       if lambda#(k-1-i)+i <= auxn then --just added, won't work for h-polynomials
	       rez = rez + sign*auxR_(basisOffset + lambda#(k-1-i) + i)*jT(leftPart|rightPart);
	       sign = - sign;
	       leftPart = drop(leftPart,-1);
	       if lambda#(k-1-i) > 1 then
	       rightPart = {lambda#(k-1-i)-1} | rightPart;
	       );
	  );
     auxR.sFunction#auxEH#lambda = rez;
     );
     rez
     )
---------------------------------------------------------------
--------------End Jacobi-Trudi---------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------Plethysm-----------------------------------------
---------------------------------------------------------------

--the cycle type of the k-th power of any permutation of cycle type cyc
powerCycleType := method()
powerCycleType(ZZ,List) := (k,cyc) ->
(
     rsort(flatten (for i in cyc list (g := gcd(i,k);splice{g:i//g})))
     )

-------------------------------------------------------------------------
-- plethysmMap(d, maxg, R)
-------------------------------------------------------------------------
-- The power-sum plethysm operator p_d acting on a symmetricRing R of
-- rank nS.  On the power-sum generators it is the substitution
--
--        p_i  |-->  p_{i*d}.
--
-- When i*d <= nS, p_{i*d} is already a generator of R.  When
-- i*d >  nS, the element p_{i*d} is not directly a variable; instead
-- its expansion in the elementary-symmetric variables lives in
-- R.PtoETable#(i*d), and R.mapFromE re-embeds that expansion into R.
--
-- The returned ring map has the layout expected by symmetricRing
-- generators, namely three blocks of length nS each:
--
--        [ e-slot (nS zeros) | p-slot (images) | h-slot (nS zeros) ].
--
-- Only the p-slot is populated (through index maxg); the e- and
-- h-generators are sent to 0 because callers only feed p-polynomials
-- through this map.
-------------------------------------------------------------------------
-- d is an integer
-- R is symmetricRing n
-- returns the plethysm map p_d : R --> R
--    which sends p_i to p_(i*d).
plethysmMap = (d,maxg,R) -> (
     nS   := R.dim;
     nSd  := nS // d;                            -- largest i with i*d <= nS
     fs   := splice{nS:0_R};                     -- e-block: nS zeros
     topf := min(maxg,nSd);
     -- p-block, part 1: i in 1..topf, image p_{i*d} is an actual variable
     fs = join(fs, apply(1..topf, j -> R.pVariable(d*j)));
     -- p-block, part 2: i in topf+1..maxg, image p_{i*d} is out of range,
     --                  pull it from the cached E-expansion and re-embed
     if maxg > nSd then
        fs = join(fs, apply(topf+1..maxg, j -> R.mapFromE R.PtoETable#(d*j)));
     -- pad the rest of the p-block (up to nS) and then the full h-block;
     -- total remaining length is 2*nS - maxg
     fs = join(fs, 2*nS-maxg:0_R);
     map(R,R,fs)
     )

-------------------------------------------------------------------------
-- plethysmGL(f, g)   --   exterior (GL) plethysm f o g
-------------------------------------------------------------------------
-- Computes the composition of Schur functors applied to
-- GL-representations.  Strategy:
--
--   1. Rewrite f in power-sum variables:  pf = f(p_1,...,p_{nf}).
--   2. Rewrite g in power-sum variables:  pg.
--   3. For each j, the power-sum plethysm p_j o g is obtained from pg
--      by the substitution p_i |-> p_{i*j}; this is exactly
--          (plethysmMap(j, maxg, SRg)) pg.
--   4. Then  f o g  =  pf( p_1 o g, p_2 o g, ..., p_{nf} o g ),
--      realized as a ring map  phi : SRf -> SRg  that sends
--          p_j  |-->  (plethysmMap(j, maxg, SRg)) pg,
--      and sends the e- and h-generators to 0 (pf is pure in p).
--   5. Convert the result back to the Schur basis when the ambient
--      ring of g is a SchurRing.
-------------------------------------------------------------------------
-- exterior plethysm (corresponding to composition
-- of Schur functors of GL-representations)
-- f is a polynomial in symmetricRing / SchurRing SA
-- g is a polynomial in symmetricRing / SchurRing SB
-- result is in symmetricRing / SchurRing SB
plethysmGL = method()
plethysmGL(RingElement,RingElement) := (f,g) -> (
     Rg := ring g;
     Rf := ring f;
     if schurLevel Rf > 1 then error"Undefined plethysm operation";

     issy := not instance(Rg,SchurRing);         -- true => stay in symmetricRing
     pg := toP g;
     pf := toP f;

     SRg := ring pg;                             -- symmetric ring of Rg
     SRf := ring pf;                             -- symmetric ring of Rf

     nf := SRf.dim;
     -- maxf: largest i such that p_i appears in pf
     maxf := max(support(pf)/index//max-nf+1,0);

     auxS := SRg;
     nS   := auxS.dim;
     lev  := schurLevel auxS;
     spg  := support(pg)/index;
     -- maxg: largest i such that p_i appears in pg
     maxg := max(select(spg,i->i<3*nS)//max-nS+1,0);
     -- ensure the E-expansion table reaches index maxf*maxg
     if maxf*maxg >= #auxS.PtoETable then PtoE(maxf*maxg,auxS);

     -- phi : SRf -> SRg sends p_i to the plethystic composition p_i o pg,
     -- so phi(pf) = pf o pg = f o g.
     -- The flattened argument has three blocks matching SRf's generators:
     --   e-block (nf zeros), p-block (plethysmMap images), h-block (nf zeros).
     phi := map(SRg, SRf, flatten splice {
               nf:0_SRg,                         -- e-block: nf zeros
               apply(1..nf, j ->                 -- p-block: p_j o pg for j<=maxf, else 0
                    (if j<=maxf then (plethysmMap(j,maxg,SRg))pg else 0_SRg)),
               nf:0_SRg                          -- h-block: nf zeros
               });
     pl := phi pf;
     if issy then pl else toS pl
)


-- interior plethysm (corresponding to the result of
-- the application of a Schur functor to an S_n-representation)
-- f is a polynomial in symmetricRing N / SchurRing SA
-- g is a polynomial in symmetricRing n / SchurRing SB
-- result is in symmetricRing n / SchurRing SB
plethysmSn = method()
plethysmSn(RingElement,RingElement) := (f,g) ->
(
     symmetricFunction(plethysm(f,classFunction g), ring g)
     )

-- Build / fetch a FINITE GL Schur ring large enough to hold the
-- Schur expansion of a plethysm f \circ g, using degrees of f,g.
-- plethysmGL passes through a symmetricRing, which requires finite numgens;
-- the stable (infinity) helper cannot be used here.
plethysmHelperOf = (S, nWanted) -> (
     key := (coefficientRing S, nWanted);
     if not S.?plethysmHelpers then S.plethysmHelpers = new MutableHashTable;
     if S.plethysmHelpers#?key then S.plethysmHelpers#key
     else (
	  sSym := getSymbol "splhlp";
	  T := schurRing(coefficientRing S, sSym, nWanted);
	  S.plethysmHelpers#key = T;
	  T
	  )
     )

-- Estimate a safe numgens for the finite Schur helper used during
-- plethysm: partitions in the result f \circ g have size deg(f)*deg(g)
-- and thus at most deg(f)*deg(g) rows.
plethysmHelperSize = (f,g) -> (
     lf := listForm f;
     lg := listForm g;
     degF := if #lf == 0 then 0
	     else max apply(lf, t -> sum toList first t);
     degG := if #lg == 0 then 0
	     else max apply(lg, t -> sum toList first t);
     max(degF * degG, 4)
     )

-- Plethysm for the symplectic character ring.
--   f \circ g  where g is a character of Sp(2n).
-- Strategy: convert g to the GL-Schur basis in a sufficiently large finite
-- helper ring H, apply GL plethysm in H, then fold the result back to the
-- Sp-basis via the Littlewood inverse (schurToSpRE), which handles
-- modification rules when the target Sp ring has finite rank.
plethysmSp = method()
plethysmSp(RingElement,RingElement) := (f,g) -> (
     S := ring g;
     Rf := ring f;
     nFin := plethysmHelperSize(f,g);
     H := plethysmHelperOf(S, nFin);
     gS := spToSchurRE(g, H);
     fS := if instance(Rf, SchurRing) and Rf.?GroupActing then (
	       if Rf.GroupActing == "Sp" then
		    spToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else if Rf.GroupActing == "O" then
		    oToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else f
	  ) else f;
     plS := plethysmGL(fS, gS);
     schurToSpRE(plS, S)
     )

-- Plethysm for the orthogonal character ring (analogous to Sp).
plethysmO = method()
plethysmO(RingElement,RingElement) := (f,g) -> (
     S := ring g;
     Rf := ring f;
     nFin := plethysmHelperSize(f,g);
     H := plethysmHelperOf(S, nFin);
     gS := oToSchurRE(g, H);
     fS := if instance(Rf, SchurRing) and Rf.?GroupActing then (
	       if Rf.GroupActing == "Sp" then
		    spToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else if Rf.GroupActing == "O" then
		    oToSchurRE(f, plethysmHelperOf(Rf, nFin))
	       else f
	  ) else f;
     plS := plethysmGL(fS, gS);
     schurToORE(plS, S)
     )

-- plethysm of symmetric functions
plethysm = method()

-- this function is not exported
-- it is used to compute the plethysm of f and g
-- when f is a power-sum symmetric polynomial
auxplet = method()
auxplet(RingElement,RingElement) := (f,g) ->
(
     Rg := ring g;
     pl := local pl;
     if Rg.?plethysmFcn then pl = Rg.plethysmFcn
     else if Rg.GroupActing == "GL" then pl = plethysmGL
     else if Rg.GroupActing == "Sn" then pl = plethysmSn;
     sLg := schurLevel Rg;
     
     if sLg == 1 then return pl(f,g) else
     (
     	  lF := listForm g;
	  return sum for t in lF list auxplet(f,last t) * pl(f,Rg_(first t))
	  );
     )

-- the most general form of plethysm
-- f is an arbitrary symmetric functions
-- g is an element of a representation ring of a product of general linear and/or symmetric groups
plethysm(RingElement,RingElement) := (f,g) ->
(
     pf := toP f;
     Rf := ring pf;
     if schurLevel Rf > 1 then error"Undefined plethysm operation";
     
     pls := new MutableHashTable from {};
     lpf := listForm pf;
     m := (ring pf).dim;
     isSchur := instance(ring g,SchurRing);

     auxg := local auxg;
     if isSchur then auxg = g else auxg = toS g;

     pl := sum for t in lpf list ((last t) * product select(apply(splice{0..m-1}, i -> (ex := (first t)#(m+i);
     	       if ex > 0 then (if pls#?i then (pls#i)^ex else 
	        (pls#i = auxplet(Rf.pVariable(i+1),auxg);(pls#i)^ex)))),j -> j =!= null)); -- this is bad when g is not in a SchurRing

     if isSchur then pl else toSymm pl
     )

-- plethysm of s_lambda and g
plethysm(BasicList,RingElement) := (lambda,g) -> (
     d := sum toList lambda;
     -- Reuse the cached workSymRing instead of constructing a fresh
     -- symmetricRing(QQ,d) per call.  jacobiTrudi only needs dim >= d
     -- (a larger dim is harmless; extra e/h/p variables simply never
     -- appear in the monomials it produces).  See ensureWorkSymRing.
     Rf := ensureWorkSymRing max(d, 1);
     f := jacobiTrudi(lambda,Rf);
     plethysm(f,g)
     )

-- (inner) plethysm of symmetric function f with the class function cF (the character of a certain S_n-representation)
plethysm(RingElement,ClassFunction) := (f,cF) ->
(
     R := ring(cF#(first keys cF));
     if R === ZZ then R = QQ;

     pf := toP f;
     n := degree cF;
     k := (ring pf).dim;
     pvars := (ring pf).pVariable;
     parsn := toList \ partitions(n);  
     newHT := new MutableHashTable;
     for sig in parsn do
     (
	  sublist := for i from 1 to k list
	  (
	       pct := powerCycleType(i,sig);     
	       if cF#?pct then cF#pct else 0
	       );
	  newHT#sig = (map(R,ring pf,splice{k:0} | sublist | splice{k:0})) pf;
	  );
     new ClassFunction from newHT
     )

-- (inner) plethysm of s_lambda with the class function cF (the character of a certain S_n-representation)
plethysm(BasicList,ClassFunction) := (lambda,cF) -> (
     d := sum toList lambda;
     -- Reuse the cached workSymRing; see plethysm(BasicList,RingElement).
     Rf := ensureWorkSymRing max(d, 1);
     f := jacobiTrudi(lambda,Rf);
     plethysm(f,cF))

-*
-- degree of a polynomial in a SchurRing
-- this is no longer used
degSchurPol = method()
degSchurPol(RingElement) := ps -> (
     tms := listForm ps;
     tms/first/sum//max
     )
*-
---------------------------------------------------------------
-----------End plethysm----------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
----Transition between various types of symmetric functions----
---------------------------------------------------------------

-----------------------------------------------------------------
-- Classical basis conversions
-----------------------------------------------------------------
-- At schurLevel 1, symmetricRing(QQ, n) is a free polynomial ring
-- on 3n generators, partitioned as:
--     e_1, ..., e_n  (elementary)    indices   0 .. n-1
--     p_1, ..., p_n  (power-sum)     indices   n .. 2n-1
--     h_1, ..., h_n  (complete)      indices 2n .. 3n-1
-- The ring carries Groebner bases grbE, grbH, grbP (installed by
-- symmetricRing(Ring,ZZ)) which kill all but one of the three
-- families of generators.  The stored maps R.mapSymToE,
-- R.mapSymToH, R.mapSymToP reduce an arbitrary symmetric polynomial
-- against the relevant GB, rewriting it in one family of generators.
--
-- Conversion strategy:
--     toSymm : SchurRing -> symmetricRing    (via jacobiTrudi)
--     toE / toH / toP                        (via GB reduction,
--                                             recursing on schurLevel
--                                             so higher-level
--                                             coefficients are
--                                             rewritten first).
-----------------------------------------------------------------

-- toSymm
toSymm = method()

-- if ps is an element of a schurRing R
-- toSymm returns the symmetric function corresponding to ps, as an element of a symmetricRing, the symmetricRing R;
-- otherwise ps is returned;
toSymm(RingElement) := (ps) ->
(
     S := ring ps;
     if instance(S, SchurRing) then
     (
     -- Special case: RatGL rings have no associated symmetricRing (they
     -- model rational, not polynomial, representations).  An element is
     -- a polynomial character iff every bipartition has empty beta, in
     -- which case we can map it to the symmetricRing via jacobiTrudi on
     -- alpha.  Otherwise error cleanly.
     if S.?GroupActing and S.GroupActing == "RatGL" then (
	  -- Walk the RatGL terms: each is indexed by a bipartition
	  -- (alpha, beta).  #beta > 0  <=>  non-polynomial (dual) part
	  -- is present  =>  set hasNeg and bail out afterwards, since
	  -- there is no canonical map to a symmetricRing in that case.
	  -- For a stable RatGL ring numgens S === infinity, so we size
	  -- the target symmetricRing to the largest alpha that actually
	  -- appears.
	  polyTerms := new MutableList;
	  hasNeg := false;
	  iterateRatGLTerms(ps, (alpha, beta, scalar) -> (
		    -- #beta > 0  <=>  non-polynomial character  ->  hasNeg
		    if #beta > 0 then hasNeg = true
		    else polyTerms#(#polyTerms) = (alpha, scalar);
		    ));
	  if hasNeg then error("toSymm: RatGL element has nonzero negative "
	       | "(beta) components and is not a polynomial character; no "
	       | "canonical map to a symmetricRing exists.  Use "
	       | "specialize(f, n) to evaluate at GL(n) instead.");
	  if #polyTerms == 0 then return 0;
	  -- maximum #alpha determines the minimum symmetric-ring dim.
	  maxLen := max apply(toList polyTerms, t -> #(t#0));
	  nT := numgens S;
	  targetDim := if class nT === InfiniteNumber
	       then max(maxLen, 1) else nT;
	  Rsym := symmetricRing(coefficientRing (S.ratNegRing), targetDim);
	  return sum apply(toList polyTerms, (alpha, c) ->
	       c * jacobiTrudi(alpha, Rsym));
	  );
     R := symmetricRing S;
     tms := listForm ps;
     -- Each (p, a) in tms represents the Schur monomial a * s_p.
     -- jacobiTrudi(p, R) realises s_p as a polynomial in R.
     -- The 'try ... else error ...' catches the case where p has
     -- more rows than R.dim (jacobiTrudi fails): the user needs a
     -- symmetricRing of strictly larger dimension.
     -- The coefficient a is lifted to coefficientRing S and fed
     -- back through toSymm, handling higher-schurLevel coefficients.
     sum apply(tms,(p,a)->(
	       (try b:=jacobiTrudi(p,R) then b else error"Need symmetric ring of higher dimension")*
	       toSymm(lift(a,coefficientRing S))))
     )
     else return ps
)

-- this is the base case of the recursive operation in the general case
-- needed when ps is an element of ZZ or QQ, because ZZ, QQ don't have
-- RingElement as an ancestor
toSymm(Number) := (ps) -> ps

mapSymToE = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the e-polynomials
mapSymToE (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToE then R.mapSymToE f else f
)
mapSymToH = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the h-polynomials
mapSymToH (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToH then R.mapSymToH f else f
)
mapSymToP = method()
-- writes the symmetric functions of maximal schurLevel in f (i.e. those
-- not contained in the coefficient ring of R) in terms of the p-polynomials
mapSymToP (RingElement) := (f) -> (
     R:=ring f; 
     if R.?mapSymToP then R.mapSymToP f else f
)

-- Guard used by toE/toH/toP/toSymm: refuse early (with a helpful
-- message) when a SchurRing input is rank-infinite, because the
-- associated symmetricRing cannot be constructed for stable rings.
-- The underlying engine error ("symmetric ring expects finite
-- schurRings") is internal and not actionable; this guard tells the
-- user what to do instead.
stableRingConversionGuard := (R, opname) -> (
     if class R === SchurRing then (
	  n := numgens R;
	  if class n === InfiniteNumber then
	       error(opname | ": cannot convert an element of a stable "
		    | "(rank-infinite) Schur ring to an e/h/p-basis "
		    | "symmetric function.  Specialize the element to a "
		    | "finite rank first (e.g. via `specialize(f, n)`), "
		    | "or create a finite-rank SchurRing to begin with.");
	  );
     )

toE = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- elementary symmetric polynomials
toE (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then (
	  stableRingConversionGuard(R, "toE");
	  toE toSymm f
	  )
     else
     (
	  if not R.?schurLevel then f else
	  -- schurLevel > 1: split each term t into leadCoefficient (which
	  -- lives in the coefficient ring, one schurLevel down) and
	  -- leadMonomial (a pure level-1 symmetric monomial).  Recurse
	  -- into toE on the coefficient (driving the recursion on
	  -- schurLevel), and apply mapSymToE to the monomial (a GB
	  -- reduction at level 1).  Sum the pieces back up.
	  if R.schurLevel>1 then terms f/(i->(toE leadCoefficient i*(mapSymToE leadMonomial i)))//sum
	  else mapSymToE f
	  )
     )

toP = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- power sums
toP (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then (
	  stableRingConversionGuard(R, "toP");
	  toP toSymm f
	  )
     else
     (
	  if not R.?schurLevel then f else
	  -- Same pattern as toE: leadCoefficient descends one schurLevel
	  -- (recursive toP), leadMonomial is a level-1 symmetric monomial
	  -- reduced to the power-sum family via mapSymToP.
	  if R.schurLevel>1 then terms f/(i->(toP leadCoefficient i*(mapSymToP leadMonomial i)))//sum
	  else mapSymToP f
	  )
     )

toH = method()
-- writes a symmetric function (possibly in a ring
-- with schurLevel larger than one) in terms of
-- complete symmetric polynomials
toH (RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing then (
	  stableRingConversionGuard(R, "toH");
	  toH toSymm f
	  )
     else
     (
	  if not R.?schurLevel then f else
	  -- Same pattern as toE: leadCoefficient recurses one schurLevel
	  -- down via toH, leadMonomial is reduced at level 1 via
	  -- mapSymToH (GB reduction to the complete-homogeneous family).
	  if R.schurLevel>1 then terms f/(i->(toH leadCoefficient i*(mapSymToH leadMonomial i)))//sum
	  else mapSymToH f
	  )
     )

---------------------------------------------------------------
--------------Monomial basis (toM)-----------------------------
---------------------------------------------------------------

-- Kostka number K_{lambda,mu}: number of SSYT of shape lambda, content mu.
--
-- We compute this via the direct SSYT recursion, which is far faster
-- (and uses less memory) than expanding h_mu into the Schur basis and
-- reading off a coefficient.  The recursion, following Stanley (EC2,
-- Prop. 7.10.4 and the horizontal-strip interpretation of h_mu):
--
--   K_{lambda, mu} = sum_{nu} K_{nu, mu'}
--
-- where mu' drops the last (smallest) part mu_r of mu, and nu ranges
-- over partitions obtained from lambda by removing a horizontal strip
-- of size mu_r.  Equivalently, the last symbol "r" is placed in a
-- horizontal strip of size mu_r in the shape lambda, and the rest of
-- the tableau fills the smaller shape nu = lambda \ strip with content
-- mu'.  Base case: K_{{}, {}} = 1.
--
-- Complexity per call: O(|lambda|^{ell(mu)} * ell(mu)) in the worst
-- case, but memoization on (lambda, mu) makes repeat calls O(1) and
-- makes the full Kostka matrix of degree d an O(p(d)^2) computation.
--
-- The memo table kostkaMemo is process-local and keyed on normalized
-- (stripped) partitions, so repeated calls with equivalent inputs hit
-- the cache.

kostkaMemo := new MutableHashTable

-- Enumerate sub-partitions nu of lambda obtained by removing a
-- horizontal strip of size s.  A horizontal strip is a skew shape with
-- at most one box in each column, i.e. the consecutive row differences
-- satisfy lambda_i >= nu_i >= lambda_{i+1} (row i of nu fits between
-- row i and row i+1 of lambda).
--
-- Returns a list of partitions nu (with trailing zeros stripped).
horizontalStripComplements = (lambda, s) -> (
     lam := toList lambda;
     l := #lam;
     if s == 0 then return {lam};
     if s < 0 or s > sum lam then return {};
     -- aux(i, remaining, prevLamBelow) = choose nu_0, nu_1, ..., nu_{i}
     -- given that nu_i <= lam_i and nu_i >= lam_{i+1} (for horizontal
     -- strip: row i of nu must lie between lam_i and lam_{i+1}).
     aux := (idx, remaining) -> (
	  if idx == l then (
	       if remaining == 0 then return {{}} else return {};
	       );
	  lamI  := lam#idx;
	  lowerI := if idx + 1 < l then lam#(idx+1) else 0;
	  -- nu_idx in [lowerI, lamI]; must also not remove more than
	  -- `remaining` boxes across this and all later rows.
	  -- amount removed from this row is lamI - nu_idx
	  -- so nu_idx ranges: max(lowerI, lamI - remaining) ... lamI
	  lo := max(lowerI, lamI - remaining);
	  flatten for v from lo to lamI list
	       for tail in aux(idx+1, remaining - (lamI - v)) list prepend(v, tail)
	  );
     -- Strip trailing zeros from each candidate.
     apply(aux(0, s), stripTrailingZeros)
     )

kostkaNumber = method()
kostkaNumber(BasicList,BasicList) := (lambda,mu) -> (
     lam := stripTrailingZeros toList lambda;
     m   := stripTrailingZeros toList mu;
     if sum lam != sum m then return 0;
     if sum lam == 0 then return 1;
     -- Memoize on normalized (lam, m).
     key := (lam, m);
     if kostkaMemo#?key then return kostkaMemo#key;
     -- Peel off the last (smallest) part of m.
     mLast   := m#(#m - 1);
     mRest   := drop(m, -1);
     -- Sum over all sub-partitions nu of lam obtained by removing a
     -- horizontal strip of size mLast.
     total := sum for nu in horizontalStripComplements(lam, mLast) list
	  kostkaNumber(nu, mRest);
     kostkaMemo#key = total;
     total
     )

-- toM: expand a symmetric function in the monomial basis.
-- Returns a RingElement in a monomial-basis SchurRing (Basis => "Monomial").
-- With no target supplied, an associated monomial ring is cached on the
-- input ring (lazy construction) and used as the output ring.
toM = method()

toM(RingElement) := (f) -> (
     R := ring f;
     if class R =!= SchurRing then return toM(toS f);
     n := numgens R;
     if n === infinity then error "toM requires a SchurRing with finite number of generators";
     -- If R is itself a monomial-basis ring, f is already in monomial form.
     if R.?Basis and R.Basis == "Monomial" then return f;
     M := monomialBasisRingOf R;
     toM(f, M)
     )

toM(RingElement,SchurRing) := (f, M) -> (
     if not (M.?Basis and M.Basis == "Monomial") then
	  error "expected second argument to be a SchurRing with Basis => \"Monomial\"";
     R := ring f;
     if class R =!= SchurRing then return toM(toS f, M);
     dimM := numgens M;
     if dimM === infinity then error "toM requires a target SchurRing with finite number of generators";
     -- Case 1: input is already in monomial basis -- identity map on partition labels
     if R.?Basis and R.Basis == "Monomial" then (
	  rawRes := raw(0_M);
	  for term in listForm f do (
	       if #(term#0) <= dimM then (
		    sc := raw promote(term#1, M);
		    ba := raw M_(term#0);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  new M from rawRes
	  )
     -- Case 2: input is in Schur basis -- apply Kostka conversion
     else schurToMonomialRE(f, M)
     )

-- Compute Kostka matrix and its inverse for degree d in n variables
-- Returns (K, Kinv) where K#lambda#{mu} = K_{lambda,mu}, Kinv#mu#{lambda} = (K^{-1})_{mu,lambda}
computeKostkaMatrices = (d, n) -> (
     parts := select(partitions d, p -> #(toList p) <= n);
     partsL := apply(parts, p -> toList p);
     k := #partsL;
     if k == 0 then return (new HashTable, new HashTable);
     -- Build Kostka matrix using toS(h_mu); reuse the cached workSymRing.
     auxR := ensureWorkSymRing max(n, 1);
     KMat := mutableMatrix(QQ, k, k);
     for j from 0 to k-1 do (
	  mu := partsL#j;
	  hProd := product for i from 0 to #mu - 1 list auxR.hVariable(mu#i);
	  sExp := listForm toS hProd;
	  lookup := new HashTable from apply(sExp, t -> t#0 => t#1);
	  for i from 0 to k-1 do (
	       lam := partsL#i;
	       if lookup#?lam then KMat_(i,j) = promote(lookup#lam, QQ);
	       );
	  );
     -- Invert
     KMatFinal := matrix KMat;
     KInvMat := KMatFinal^(-1);
     -- Build hash tables
     K := new MutableHashTable;
     Kinv := new MutableHashTable;
     for i from 0 to k-1 do (
	  lam := partsL#i;
	  row := new MutableHashTable;
	  for j from 0 to k-1 do (
	       val := KMat_(i,j);
	       if val != 0 then row#(partsL#j) = lift(val, ZZ);
	       );
	  K#lam = new HashTable from row;
	  );
     for j from 0 to k-1 do (
	  mu := partsL#j;
	  row := new MutableHashTable;
	  for i from 0 to k-1 do (
	       val := KInvMat_(j,i);
	       if val != 0 then row#(partsL#i) = lift(val, ZZ);
	       );
	  Kinv#mu = new HashTable from row;
	  );
     (new HashTable from K, new HashTable from Kinv)
     )

-- Convert element from monomial-basis interpretation to Schur-basis interpretation
-- f in SchurRing S where S_(mu) represents m_mu; returns element where S_(lambda) represents s_lambda
-- Uses raw engine operations to avoid triggering overloaded S*S multiplication
monomialToSchurRE = (f, S) -> (
     if f == 0 then return 0_S;
     n := numgens S;
     lf := listForm f;
     rawRes := raw(0_S);
     for term in lf do (
	  mu := term#0;
	  c := term#1;
	  d := sum mu;
	  if d == 0 then rawRes = rawRes + raw promote(c, S)
	  else (
	       if not S.kostkaCache#?d then S.kostkaCache#d = computeKostkaMatrices(d, n);
	       Kinv := (S.kostkaCache#d)#1;
	       muL := toList mu;
	       if Kinv#?muL then
		    for pair in pairs Kinv#muL do (
			 sc := raw promote(c * (pair#1), S);
			 ba := raw S_(pair#0);
			 rawRes = rawRes + sc * ba;
			 );
	       );
	  );
     new S from rawRes
     )

-- Convert element from Schur-basis interpretation to monomial-basis interpretation
-- f in SchurRing S where S_(lambda) represents s_lambda; returns element where S_(mu) represents m_mu
-- Uses raw engine operations to avoid triggering overloaded S*S multiplication
schurToMonomialRE = (f, S) -> (
     if f == 0 then return 0_S;
     n := numgens S;
     lf := listForm f;
     rawRes := raw(0_S);
     for term in lf do (
	  lam := term#0;
	  c := term#1;
	  d := sum lam;
	  if d == 0 then rawRes = rawRes + raw promote(c, S)
	  else (
	       if not S.kostkaCache#?d then S.kostkaCache#d = computeKostkaMatrices(d, n);
	       K := (S.kostkaCache#d)#0;
	       lamL := toList lam;
	       if K#?lamL then
		    for pair in pairs K#lamL do (
			 sc := raw promote(c * (pair#1), S);
			 ba := raw S_(pair#0);
			 rawRes = rawRes + sc * ba;
			 );
	       );
	  );
     new S from rawRes
     )

-- Get or create the default monomial-basis SchurRing associated to S.
-- Used by toM to produce a RingElement output when no target ring is supplied.
monomialBasisRingOf = (S) -> (
     if S.?monomialBasisRing then S.monomialBasisRing
     else (
	  n := numgens S;
	  if n === infinity then error "monomialBasisRingOf requires a SchurRing with finite number of generators";
	  -- use a package-private symbol named "m" to avoid clobbering user globals
	  mSym := getSymbol "m";
	  M := schurRing(coefficientRing S, mSym, n, Basis => "Monomial");
	  S.monomialBasisRing = M;
	  M
	  )
     )

---------------------------------------------------------------
--------------End Monomial basis-------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
--------------Sp / O stable character rings--------------------
---------------------------------------------------------------

-- Koike branching formulas for the stable (universal) character ring:
--    sp_lambda = sum_{delta}  (-1)^{|delta|/2} s_{lambda/delta}
--       where delta runs over partitions contained in lambda with all
--       columns of even length (equivalently, the conjugate delta' has
--       all even parts -- i.e. delta's parts come in equal pairs).
--    o_lambda  = sum_{delta}  (-1)^{|delta|/2} s_{lambda/delta}
--       where delta runs over partitions contained in lambda with all
--       parts even.
-- The inverse formulas are obtained by dropping the sign:
--    s_lambda  = sum_{delta even cols}  sp_{lambda/delta}  (skew Sp character)
--    s_lambda  = sum_{delta even rows}  o_{lambda/delta}   (skew O character)
-- In both directions, skew characters expand as sum_nu c^{lambda}_{delta,nu} X_nu
-- where c is the usual Littlewood-Richardson coefficient and X is the
-- relevant basis; this matches the s-expansion of s_{lambda/delta}.

-- Skew Schur expansion: returns s_{lambda/mu} as a list of (partition, coeff)
-- pairs in the Schur basis.  Uses the Jacobi-Trudi determinant.
--
-- Performance notes.  Two caches make this fast on repeated calls:
--   (1) A single symmetricRing is reused across calls (skewWorkRing),
--       grown on demand when a caller needs h_k with k exceeding the
--       current ring's dim.  This avoids the ~40-90ms symmetricRing
--       construction overhead that would otherwise dominate each call.
--   (2) Results are memoized on the normalized (lambda, mu) key in
--       skewMemo so repeated skew queries (common in Koike product and
--       Sp/O modification rules) cost O(1) after the first.
-- Both caches are process-global and safe because:
--   - skewWorkRing is used only inside this function; growing it never
--     breaks a previous computation (earlier results have already been
--     lifted out of the ring as partition/coefficient pairs).
--   - skewMemo stores only immutable (partition, integer) pairs.
-- Memo table for skewSchurExpansion results.  The cached workSymRing
-- used below is declared earlier in the file (near jacobiTrudi) so that
-- plethysm can share it.
skewMemo := new MutableHashTable;

skewSchurExpansion = method()
skewSchurExpansion(BasicList, BasicList) := (lambda, mu) -> (
     -- Strip trailing zeros (normalize so cache keys collide)
     lam := stripTrailingZeros toList lambda;
     m := stripTrailingZeros toList mu;
     if sum m > sum lam then return {};
     l := #lam;
     if l == 0 then return {({}, 1)};
     mPad := m | toList(l - #m : 0);
     for i from 0 to l-1 do if lam#i < mPad#i then return {};
     if sum lam == sum mPad then return {({}, 1)};
     -- Memo lookup on normalized key (stripped lam, stripped m)
     key := (lam, m);
     if skewMemo#?key then return skewMemo#key;
     -- Jacobi-Trudi: det(h_{lam_i - mPad_j - i + j}) for 1 <= i,j <= l
     nmax := lam#0 + l;
     R := ensureSkewWorkRing nmax;
     M := mutableMatrix(R, l, l);
     for i from 0 to l-1 do for j from 0 to l-1 do (
	  k := lam#i - mPad#j - i + j;
	  if k < 0 then M_(i,j) = 0_R
	  else if k == 0 then M_(i,j) = 1_R
	  else M_(i,j) = R.hVariable(k);
	  );
     dd := det(matrix M, Strategy => Cofactor);
     sExp := toS dd;
     -- listForm of a SchurRingElement is list of (partition, coeff) pairs
     skewMemo#key = apply(listForm sExp, t -> (t#0, lift(t#1, ZZ)))
     )

-- Enumerate partitions mu, contained in lambda componentwise, with all parts even.
-- Each mu is returned as a list of positive parts (trailing zeros stripped).
evenRowsSubpartitionsOf = method()
evenRowsSubpartitionsOf(BasicList) := (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     aux := (idx, prevMax) -> (
	  if idx >= #lam then return {{}};
	  upper := min(lam#idx, prevMax);
	  flatten for v from 0 to upper list (
	       if odd v then continue;
	       for t in aux(idx+1, v) list prepend(v, t)
	       )
	  );
     -- Strip trailing zeros off each candidate
     apply(aux(0, infinity), stripTrailingZeros)
     )

-- Enumerate partitions mu, contained in lambda componentwise, with all columns
-- of even length (equivalently, conjugate mu' has all even parts -- parts of mu
-- come in equal pairs).
evenColsSubpartitionsOf = method()
evenColsSubpartitionsOf(BasicList) := (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     if #lam == 0 then return {{}};
     lamC := toList conjugate new Partition from lam;
     evens := evenRowsSubpartitionsOf(lamC);
     apply(evens, mu -> (
	       if #mu == 0 then {}
	       else toList conjugate new Partition from mu
	       ))
     )

---------------------------------------------------------------
-- Sam-Snowden-Weyman modification rules (border-strip form)
---------------------------------------------------------------
--
-- References:
--   [SSW] Sam-Snowden-Weyman, "Homology of Littlewood complexes"
--         Selecta Math. 19 (2013), 655-698; arXiv:1209.3509
--         Sec. 3.4 (symplectic), Sec. 4.4 (orthogonal).
--   [Kin] King, "Modification rules ... for Bn, Cn, Dn", J. Algebra 107 (1987).
--
-- Given a (possibly invalid) partition lambda, these rules return a pair
--     (tau, sign)
-- such that, formally,
--     {Sp|O}_lambda  ==  sign * {Sp|O}_tau
-- in the corresponding character ring at rank n, OR null if lambda maps to 0.
-- tau is an admissible partition (length <= n for type C; additional first-
-- two-columns condition for types B, D).
--
-- The border-strip description (SSW 3.4 / 4.4) finds a border strip R of a
-- prescribed size starting at the first box of the final row of the current
-- partition, and removes it.  By Remark 3.4 in [SSW], such a border strip R
-- corresponds exactly to a cell b = (k, 1) in the first column whose hook
-- length equals the prescribed size L, i.e. lambda_k + r - k = L.  Removing
-- R then equals removing the hook of (k, 1), and the result is the
-- partition
--     (lambda_1, ..., lambda_{k-1}, lambda_{k+1}-1, ..., lambda_r-1).
-- The number of columns the strip occupies is c(R) = lambda_k.
--
-- findBorderStripRow(lam, L):
--   Given partition lam (with r = #lam rows) and target strip length L,
--   find the unique k in {1,...,r} with lam_k + r - k = L, or null if none.
findBorderStripRow = (lam, L) -> (
     r := #lam;
     k := -1;
     for kk from 1 to r do (
	  if lam#(kk-1) + r - kk == L then (k = kk; break);
	  );
     if k == -1 then null else k
     )

-- removeBorderStripAtFirstColumn(lam, k):
--   Remove the hook of cell (k, 1) from partition lam.
--   Returns (newLam, cR) where cR = # columns occupied by the strip.
removeBorderStripAtFirstColumn = (lam, k) -> (
     r := #lam;
     cR := lam#(k-1);  -- c(R) = lam_k (# columns occupied by the strip)
     newLam := stripTrailingZeros join(
	  take(lam, k-1),
	  for i from k+1 to r list lam#(i-1) - 1
	  );
     (newLam, cR)
     )

-- sigmaInvolution(lam, m):
--   The type B/D involution on partitions: replace lam_1^T (first column
--   length) by (m - lam_1^T) and keep lam_i^T for i >= 2, then transpose back.
--   Returns null if the result is not a valid partition (i.e. if
--   m - lam_1^T < lam_2^T).
sigmaInvolution = (lam, m) -> (
     if #lam == 0 then (
	  if m == 0 then return {}
	  else return toList(m:1)   -- empty^sigma = column of length m
	  );
     lamT := toList conjugate new Partition from lam;
     a := lamT#0;
     newA := m - a;
     if #lamT >= 2 and newA < lamT#1 then return null;
     if newA < 0 then return null;
     if newA == 0 then (
	  trunc := drop(lamT, 1);
	  if #trunc == 0 then return {};
	  return toList conjugate new Partition from trunc;
	  );
     newLamT := prepend(newA, drop(lamT, 1));
     toList conjugate new Partition from newLamT
     )

-- sswTypeC(lambda, n):
--   SSW modification rule for Sp(2n).
--   Returns (tau, sign) or null (meaning zero).
sswTypeC = (lambda, n) -> (
     lam := stripTrailingZeros toList lambda;
     if #lam <= n then return (lam, 1);
     sign := 1;
     current := lam;
     while #current > n do (
	  r := #current;
	  L := 2*(r - n - 1);
	  if L == 0 then return null;  -- strip must be non-empty
	  k := findBorderStripRow(current, L);
	  if k === null then return null;
	  (newLam, cR) := removeBorderStripAtFirstColumn(current, k);
	  sign = sign * (if even cR then 1 else -1);
	  current = newLam;
	  );
     (current, sign)
     )

-- sswTypeBD(lambda, m):
--   SSW modification rule for O(m) (type B if m = 2n+1, type D if m = 2n).
--   Returns (tau, sign) or null.  Here tau must satisfy the O-admissibility
--   condition lam^T_1 + lam^T_2 <= m; the iteration stops whenever this
--   holds.  Differences from type C (see [SSW 4.4]):
--     (D1) strip length L = 2 * ell(lambda) - m,
--     (D2) sign contribution uses c(R) - 1 instead of c(R),
--     (D3) if an odd total number of strips was removed, apply sigma to tau.
sswTypeBD = (lambda, m) -> (
     lam := stripTrailingZeros toList lambda;
     -- Admissibility test: lam^T_1 + lam^T_2 <= m.
     admissible := (p) -> (
	  if #p == 0 then return true;
	  pT := toList conjugate new Partition from p;
	  a := if #pT >= 1 then pT#0 else 0;
	  b := if #pT >= 2 then pT#1 else 0;
	  a + b <= m
	  );
     if admissible lam then return (lam, 1);
     sign := 1;
     numStripsRemoved := 0;
     current := lam;
     while not admissible current do (
	  r := #current;
	  L := 2 * r - m;
	  if L <= 0 then return null;
	  k := findBorderStripRow(current, L);
	  if k === null then return null;
	  (newLam, cR) := removeBorderStripAtFirstColumn(current, k);
	  -- Sign: (-1)^{c(R) - 1}.
	  sign = sign * (if odd cR then 1 else -1);
	  numStripsRemoved = numStripsRemoved + 1;
	  current = newLam;
	  );
     -- (D3) If odd number of strips removed, apply sigma.
     if odd numStripsRemoved then (
	  sigmaResult := sigmaInvolution(current, m);
	  if sigmaResult === null then return null;
	  current = sigmaResult;
	  );
     (current, sign)
     )

-- modificationRule(lambda, n, type):
--   Unified entry point.  type is one of:
--     "C"  (symplectic, Sp(2n))
--     "B"  (odd orthogonal, O(2n+1))
--     "D"  (even orthogonal, O(2n))
--   Returns (tau, sign) or null.
modificationRule = (lambda, n, type) -> (
     if type == "C" then sswTypeC(lambda, n)
     else if type == "B" then sswTypeBD(lambda, 2*n+1)
     else if type == "D" then sswTypeBD(lambda, 2*n)
     else error("modificationRule: unknown type " | toString type)
     )

---------------------------------------------------------------
-- Stable sp_lambda and o_lambda expressed in the Schur basis.
---------------------------------------------------------------
--
-- There is NO clean closed-form Koike-type formula
--     sp_lambda = sum_{delta in C} (-1)^{|delta|/2} s_{lambda/delta}
-- valid for all lambda: such a formula would force every Schur term s_mu
-- appearing in sp_lambda to have |lambda| - |mu| even, and that fails
-- (e.g. sp_{(2,2,2)} contains -s_{(2,1,1)} with |diff|=1 ... is actually
-- the opposite: in our conventions |lambda/mu| *is* always even; the
-- issue is that the naive even-col class over-counts).
--
-- Instead we invert the *forward* (correct) Littlewood branching rule
--     s_lambda = sum_{mu, nu : nu has all even cols} c^{lambda}_{mu,nu} sp_mu
-- recursively:
--     sp_lambda  =  s_lambda  -  sum_{nu even-col, nu != {}} sum_{mu}
--                                       c^{lambda}_{mu,nu} sp_mu.
-- Since nu != {} forces |mu| < |lambda|, recursion terminates.
-- We memoize the resulting s-basis coefficient lists.

-- Cache: partition lambda (list) -> list of (mu, integer) pairs representing
-- sp_lambda = sum_mu (coef) s_mu in the stable universal character ring.
stableSpInSchurCache := new MutableHashTable

-- Return list of (mu, coef) with coef nonzero integer, such that
--   sp_lambda  ==  sum_mu coef * s_mu.
stableSpInSchur = (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     if stableSpInSchurCache#?lam then return stableSpInSchurCache#lam;
     acc := new MutableHashTable;
     acc#lam = 1;
     for nu in evenColsSubpartitionsOf(lam) do (
	  if #nu == 0 then continue;
	  for pair in skewSchurExpansion(lam, nu) do (
	       mu := pair#0;
	       lrCoef := pair#1;
	       for t in stableSpInSchur(mu) do (
		    mu2 := t#0;
		    c := t#1;
		    prev := if acc#?mu2 then acc#mu2 else 0;
		    newVal := prev - lrCoef * c;
		    if newVal == 0 then (
			 if acc#?mu2 then remove(acc, mu2);
			 )
		    else acc#mu2 = newVal;
		    );
	       );
	  );
     ans := for k in keys acc list (k, acc#k);
     stableSpInSchurCache#lam = ans;
     ans
     )

-- Same, but for orthogonal characters (Littlewood uses nu with all even rows).
stableOInSchurCache := new MutableHashTable

stableOInSchur = (lambda) -> (
     lam := stripTrailingZeros toList lambda;
     if stableOInSchurCache#?lam then return stableOInSchurCache#lam;
     acc := new MutableHashTable;
     acc#lam = 1;
     for nu in evenRowsSubpartitionsOf(lam) do (
	  if #nu == 0 then continue;
	  for pair in skewSchurExpansion(lam, nu) do (
	       mu := pair#0;
	       lrCoef := pair#1;
	       for t in stableOInSchur(mu) do (
		    mu2 := t#0;
		    c := t#1;
		    prev := if acc#?mu2 then acc#mu2 else 0;
		    newVal := prev - lrCoef * c;
		    if newVal == 0 then (
			 if acc#?mu2 then remove(acc, mu2);
			 )
		    else acc#mu2 = newVal;
		    );
	       );
	  );
     ans := for k in keys acc list (k, acc#k);
     stableOInSchurCache#lam = ans;
     ans
     )

---------------------------------------------------------------

-----------------------------------------------------------------------------
-- Sp/O <-> Schur basis conversions: "RE" (Raw Engine) family
-----------------------------------------------------------------------------
-- The four functions below (spToSchurRE, schurToSpRE, oToSchurRE,
-- schurToORE) are the low-level engines used to reinterpret a polynomial
-- between the Sp-/O-basis view of a SchurRing and the Schur-basis view of
-- another (possibly the same) SchurRing T.
--
-- "RE" suffix = Raw Engine.  These routines deliberately use raw(f)*raw(g)
-- on the engine side rather than the overloaded S*S product: for Sp/O
-- rings the high-level product is itself defined in terms of these
-- conversions, so calling it here would recurse back into this file.
-- Working at the raw level short-circuits that.
--
-- Forward conversions (*ToSchurRE):
--   spToSchurRE / oToSchurRE expand each basis element sp_lambda (resp.
--   o_lambda) in the stable Schur basis by looking up the precomputed
--   tables stableSpInSchur / stableOInSchur.  Those tables encode the
--   Koike universal-character branching formulas for Sp/O -> GL.  When
--   T has finite rank we simply drop Schur summands s_mu with
--   #mu > rank(T).
--
-- Reverse conversions (schurTo*RE):
--   These invert the forward map via unitriangular peeling (see the loop
--   comments inside each function).  In the finite-rank case we apply
--   the Sam-Snowden-Weyman modification rules (sswTypeC for type C,
--   sswTypeBD for types B/D), and for B/D we further apply
--   sigmaInvolution to keep the output inside the length <= n window
--   the engine can represent.
-----------------------------------------------------------------------------

-- Convert an Sp-basis element f in S to its Schur-basis interpretation in T.
-- T may equal S (in-place reinterpretation) or be a different SchurRing.
-- Uses raw engine arithmetic to avoid triggering overloaded S*S multiplication.
spToSchurRE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     rawRes := raw(0_T);
     -- For each term c * sp_lambda of f, expand sp_lambda in the stable
     -- Schur basis via stableSpInSchur (Koike branching) and accumulate
     -- into rawRes; truncate to length <= dimT when T has finite rank.
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  for pair in stableSpInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if dimT === infinity or #mu <= dimT then (
		    sc := raw promote(c * coef, T);
		    ba := raw T_(mu);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  );
     new T from rawRes
     )

-- Convert a Schur-basis element f to its Sp-basis interpretation in T.
--
-- Unified stable/finite algorithm (unitriangular peeling):
--
-- Stable sp_lambda = s_lambda + (lower terms), so we can compute the
-- sp-basis expansion iteratively: peel off an s_lambda term, record it
-- as the sp_lambda coefficient, and subtract (stable) sp_lambda's full
-- s-basis expansion from the running "working" polynomial.  Repeat until
-- empty.  Termination follows from unitriangularity of sp in s.
--
-- For stable T (numgens infinity), each peeled sp_lambda is already a
-- valid basis element.
--
-- For finite T (Sp(2n)), the peeled sp_lambda is a *stable* sp label that
-- maps to the finite ring via the Sam-Snowden-Weyman type-C modification
-- rule: sp_lambda (stable) = sign * sp_tau (finite) or 0.
schurToSpRE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     -- working: mutable { mu -> coefficient } holding the residual Schur
     -- expansion still to be peeled.  Seeded with the terms of f.
     working := new MutableHashTable;
     for term in listForm f do working#(term#0) = term#1;
     rawRes := raw(0_T);
     -- Peeling loop:
     --   (1) pick any lambda with nonzero coefficient c in `working`;
     --   (2) by unitriangularity, lambda must be a leading sp_lambda --
     --       i.e. sp_lambda = s_lambda + (strictly lower Schur terms);
     --   (3) record c * sp_lambda in rawRes (finite rank: apply the
     --       type-C modification rule sswTypeC);
     --   (4) subtract c * (sp_lambda - s_lambda), i.e. the lower-order
     --       s-basis terms, from `working`, and continue.
     while #working > 0 do (
	  lam := first keys working;
	  c := working#lam;
	  remove(working, lam);
	  if c == 0 then continue;
	  -- Step (3): contribute c * sp_lambda to the output.
	  if dimT === infinity then (
	       rawRes = rawRes + raw promote(c, T) * raw T_lam;
	       )
	  else (
	       -- Finite rank: sswTypeC returns null (term vanishes under
	       -- the type-C modification rule) or a pair (tau, sign)
	       -- giving the admissible partition tau and its overall sign.
	       r := sswTypeC(lam, dimT);
	       if r =!= null then (
		    tau := r#0;
		    sgnMod := r#1;
		    rawRes = rawRes + raw promote(c * sgnMod, T) * raw T_tau;
		    );
	       );
	  -- Step (4): subtract c * sp_lam in the s-basis (minus the s_lam
	  -- term itself, which we already consumed by removing lam from
	  -- working).
	  for pair in stableSpInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if mu === lam then continue;
	       prev := if working#?mu then working#mu else 0;
	       newVal := prev - c * coef;
	       if newVal == 0 then (
		    if working#?mu then remove(working, mu);
		    )
	       else working#mu = newVal;
	       );
	  );
     new T from rawRes
     )

-- Convert an O-basis element f to Schur-basis interpretation.
oToSchurRE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     rawRes := raw(0_T);
     -- Mirrors spToSchurRE, but expands o_lambda using the orthogonal
     -- branching table stableOInSchur.
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  for pair in stableOInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if dimT === infinity or #mu <= dimT then (
		    sc := raw promote(c * coef, T);
		    ba := raw T_(mu);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  );
     new T from rawRes
     )

-- Convert a Schur-basis element f to O-basis interpretation in T.
--
-- Same unified stable/finite algorithm as schurToSpRE (unitriangular
-- peeling), but with even-row Littlewood (GL->O uses nu with all even
-- rows) and with the type B/D SSW rule in the finite case.
-- T.OddOrEven selects:
--   "Odd"  (default) -> O(2n+1) = type B_n, m = 2n+1
--   "Even"           -> O(2n)   = type D_n, m = 2n
schurToORE = (f, T) -> (
     if f == 0 then return 0_T;
     dimT := numgens T;
     kind := if T.?OddOrEven then T.OddOrEven else "Odd";
     m := if dimT === infinity then null else
          (if kind == "Odd" then 2*dimT+1 else 2*dimT);
     -- working: mutable { mu -> coefficient } residual Schur expansion.
     working := new MutableHashTable;
     for term in listForm f do working#(term#0) = term#1;
     rawRes := raw(0_T);
     -- Peeling loop (mirrors schurToSpRE):
     --   (1) pick lambda with nonzero coefficient c in `working`;
     --   (2) unitriangularity: o_lambda = s_lambda + (lower terms);
     --   (3) record c * o_lambda (finite case: apply sswTypeBD, then
     --       sigmaInvolution if the admissible tau has length > n);
     --   (4) subtract c * (o_lambda - s_lambda) from `working` and loop.
     while #working > 0 do (
	  lam := first keys working;
	  c := working#lam;
	  remove(working, lam);
	  if c == 0 then continue;
	  -- Step (3): contribute c * o_lambda to the output.
	  if dimT === infinity then (
	       rawRes = rawRes + raw promote(c, T) * raw T_lam;
	       )
	  else (
	       -- Finite rank: sswTypeBD returns null (term vanishes) or a
	       -- pair (tau, sign) giving the admissible partition under
	       -- the type B/D modification rule.
	       r := sswTypeBD(lam, m);
	       if r =!= null then (
		    tau := r#0;
		    sgnMod := r#1;
		    -- SSW admissibility (col_1(tau)+col_2(tau) <= m) permits
		    -- partitions with length > n.  For the SchurRing engine,
		    -- which only holds length <= n, apply sigmaInvolution:
		    -- for SO(m), o_tau == o_{sigma(tau)} (det is trivial).
		    -- If sigma still doesn't reduce length <= n, the term
		    -- does not correspond to a ring-representable irrep
		    -- and is dropped.
		    if #tau > dimT then (
			 sigmaTau := sigmaInvolution(tau, m);
			 if sigmaTau =!= null and #sigmaTau <= dimT then
			      rawRes = rawRes + raw promote(c * sgnMod, T) * raw T_sigmaTau;
			 )
		    else rawRes = rawRes + raw promote(c * sgnMod, T) * raw T_tau;
		    );
	       );
	  -- Step (4): subtract c * o_lam in the s-basis (minus the s_lam
	  -- term itself, already consumed above).
	  for pair in stableOInSchur(lam) do (
	       mu := pair#0;
	       coef := pair#1;
	       if mu === lam then continue;
	       prev := if working#?mu then working#mu else 0;
	       newVal := prev - c * coef;
	       if newVal == 0 then (
		    if working#?mu then remove(working, mu);
		    )
	       else working#mu = newVal;
	       );
	  );
     new T from rawRes
     )

-- Get or create the associated symplectic-basis SchurRing (stable by default).
symplecticBasisRingOf = (S) -> (
     if S.?symplecticBasisRing then S.symplecticBasisRing
     else (
	  n := numgens S;
	  spSym := getSymbol "sp";
	  T := if n === infinity
	       then schurRing(coefficientRing S, spSym, GroupActing => "Sp")
	       else schurRing(coefficientRing S, spSym, n, GroupActing => "Sp");
	  S.symplecticBasisRing = T;
	  T
	  )
     )

-- Get or create the associated orthogonal-basis SchurRing (stable by default).
orthogonalBasisRingOf = (S) -> (
     if S.?orthogonalBasisRing then S.orthogonalBasisRing
     else (
	  n := numgens S;
	  oSym := getSymbol "o";
	  T := if n === infinity
	       then schurRing(coefficientRing S, oSym, GroupActing => "O")
	       else schurRing(coefficientRing S, oSym, n, GroupActing => "O");
	  S.orthogonalBasisRing = T;
	  T
	  )
     )

-- Get or create the associated plain Schur-basis ring (GroupActing "GL",
-- Basis "Schur") of the same size as S.  Used by toS on Sp/O/Monomial inputs.
schurBasisRingOf = (S) -> (
     if S.?schurBasisRing then S.schurBasisRing
     else (
	  n := numgens S;
	  sSym := getSymbol "s";
	  T := if n === infinity
	       then schurRing(coefficientRing S, sSym)
	       else schurRing(coefficientRing S, sSym, n);
	  S.schurBasisRing = T;
	  T
	  )
     )

-- Get or create a STABLE (infinity-many-generator) plain Schur-basis helper
-- ring for use by Sp/O multiplication.  Finite-n Sp/O multiplication must
-- compute its GL product in an unrestricted Schur ring so partitions with
-- more than numgens S rows (which would be truncated by the engine) are
-- preserved until the modification rule in schurToSpRE/schurToORE can
-- collapse them back into valid Sp/O-basis elements.
stableSchurHelperOf = (S) -> (
     if S.?stableSchurHelper then S.stableSchurHelper
     else (
	  sSym := getSymbol "s";
	  T := schurRing(coefficientRing S, sSym);
	  S.stableSchurHelper = T;
	  T
	  )
     )

---------------------------------------------------------------
--------------End Sp / O stable character rings----------------
---------------------------------------------------------------

-- Module-scope closures read by recTrans.  They are set inside
-- toS(RingElement) (symmetricRing branch) immediately before the call to
-- recTrans, and describe ring-specific logic that recTrans needs at every
-- level of the recursion:
--   leadTermFcn(pl) -- pick the leading h-variable h_i of pl, or null
--                      when pl has no h-variables left (it is a scalar in
--                      the coefficient ring).
--   retFcn(pl)      -- base case: lift pl into the coefficient ring and
--                      continue with toS there (descends one schurLevel).
--   mappingFcn(v)   -- given v = h_i, return the Schur generator s_i of
--                      the corresponding SchurRing.
-- These are kept at module scope (rather than passed as arguments) because
-- recTrans is mutually recursive through toS and needs to see the same
-- closures at every level of the h-polynomial.
leadTermFcn := local leadTermFcn;
retFcn := local retFcn;
mappingFcn := local mappingFcn;

--------------------------------------------------------------------------
-- toS(f): rewrite f in the Schur basis.
--
-- Dispatch tree:
--   1. schurLevel(ring f) == 0  ---->  return f unchanged (base ring).
--   2. ring f is a SchurRing:
--        a. variant bases route through dedicated converters:
--             Basis == "Monomial"    -> monomialToSchurRE
--             GroupActing == "Sp"    -> spToSchurRE
--             GroupActing == "O"     -> oToSchurRE
--             GroupActing == "SL"    -> relabel partitions (sl_lambda
--                                       lifts to s_lambda with
--                                       lambda_n = 0);
--        b. plain GL/Sn/RatGL rings are already Schur-indexed, so toS is
--           a no-op (use toGL/toSn/convert/specialize to move rings).
--   3. ring f is a symmetricRing with schurLevel > 0:
--        convert f to the complete-homogeneous (h) basis via toH, then
--        apply recTrans to rewrite the result in the Schur basis.
--
-- The three closures above are installed in case 3 and consumed by
-- recTrans.  The cryptic mappingFcn index formula
--     (schurRing ring v)_{index v - 2*(ring v).dim + 1}
-- decodes as follows: in a symmetricRing of dimension n the variable
-- layout is e_1..e_n, p_1..p_n, h_1..h_n, so the h-block starts at
-- generator-index 2n and h_i has index 2n + i - 1.  Subtracting 2n - 1
-- recovers i, which then indexes the Schur generator s_i on the
-- single-row partition {i}.
--------------------------------------------------------------------------
toS = method()

toS(RingElement) := (f) -> (
     R := ring f;
     if schurLevel R == 0 then return f;
     if class R === SchurRing then (
	  -- Variant-basis SchurRings: convert to an associated plain Schur ring.
	  local TSch;
	  if R.?Basis and R.Basis == "Monomial" then (
	       TSch = schurBasisRingOf R;
	       return monomialToSchurRE(f, TSch);
	       );
	  if R.?GroupActing and R.GroupActing == "Sp" then (
	       TSch = schurBasisRingOf R;
	       return spToSchurRE(f, TSch);
	       );
	  if R.?GroupActing and R.GroupActing == "O" then (
	       TSch = schurBasisRingOf R;
	       return oToSchurRE(f, TSch);
	       );
	  if R.?GroupActing and R.GroupActing == "SL" then (
	       -- sl_lambda lifts to the GL irrep s_lambda (canonical choice
	       -- with lambda_n = 0).  Just relabel partitions.
	       TSch = schurBasisRingOf R;
	       dimTSch := numgens TSch;
	       rawRes := raw(0_TSch);
	       for term in listForm f do (
		    if dimTSch === infinity or #(term#0) <= dimTSch then (
			 rawRes = rawRes
			      + raw promote(term#1, TSch) * raw (TSch_(term#0));
			 );
		    );
	       return new TSch from rawRes;
	       );
	  -- Fall-through: plain GL, Sn, and RatGL rings are all already
	  -- represented in a Schur-indexed basis, so toS is a no-op --
	  -- there is no finer "plain Schur" basis to convert to.  Users
	  -- wishing to move into a *different* ring (e.g. an Sn element
	  -- into a GL ring, or a RatGL element into a polynomial ring at
	  -- some specific rank) should use toGL(f, T), toSn(f, T),
	  -- convert(f, T), or specialize(f, n) explicitly.
	  return f;
	  );
     -- symmetricRing with schurLevel > 0: go via the h-basis and recTrans.
     (
	  S := schurRing R;
     	  local hf;
     	  n := R.dim;
     	  d := first degree f;
     	  ngS := numgens S;
	  -- mappingFcn: send h-variable v = h_i to the Schur generator s_i
	  -- in the correct SchurRing (see header for the index arithmetic).
     	  mappingFcn = (v) -> (schurRing ring v)_{index v-2*(ring v).dim+1};
	  -- leadTermFcn: return the h-variable of maximal index appearing in
	  -- pl, or null when pl has no h-variable left (it is a scalar).
     	  leadTermFcn = (pl) -> (
     	       R := ring pl;
     	       spl := select(support pl,i->index i<numgens R);
     	       if spl == {} then null else last spl
     	       );
	  -- retFcn: base case of recTrans.  pl is liftable into the
	  -- coefficient ring (it has no h-variables); lift it and recurse
	  -- on toS there, descending one schurLevel.
     	  retFcn = (pl) -> toS lift(pl,(coefficientRing ring pl));
     	  promote(recTrans(toH f),S)
	  )
     )

toS(Thing) := (f) -> f
undocumented(toS,Thing)

toS(Thing,Ring) := (f,T) -> try(lift(f,T)) else f
undocumented(toS,Thing,Ring)

toS(RingElement,SchurRing) := (f, T) ->
(
     R := ring f;
     if schurLevel R == 0 then
     (
	  U := T;
	  while schurLevel U > 0 do U = coefficientRing U;
	  toS(f,U)
	  )
     else
     (
     	  fS := toS f;
     	  dimT := numgens T;
     	  (listForm fS)/(i-> if dimT === infinity or #i#0<=dimT then T_(i#0)*toS(i#1,coefficientRing T) else 0_T)//sum
	  )
     )

-----------------------------------------------------------------------
-- User-facing "change of basis" API for SchurRing variants.
--
-- Five basis-specific conversion methods plus a universal dispatcher:
--
--   toSp  : expand in symplectic    (Sp)   characters
--   toO   : expand in orthogonal    (O)    characters
--   toGL  : expand in general linear(GL)   characters (= plain Schur)
--   toSn  : relabel as symmetric-group (Sn) characters via Frobenius
--   convert : universal entry point; routes to one of the above by
--             inspecting the target ring's GroupActing tag
--
-- Each toX comes in two forms:
--   toX(f)    -- no explicit target; build/fetch a default output ring
--   toX(f,T)  -- target an explicit SchurRing T (must have the
--                appropriate GroupActing tag)
--
-- The common dispatch pattern inside each toX(f,T):
--   1. If f already lives in an X-basis ring, just copy partition
--      labels into T (respecting T's rank) and return.
--   2. If f lives in a different variant basis, route through plain
--      Schur (toS) first, then re-enter toX.
--   3. Otherwise f is in a plain Schur ring: invoke the reverse
--      conversion (schurToSpRE / schurToORE / ...).
-- Thing / Number base cases simply promote or return unchanged.
-----------------------------------------------------------------------

-- ==== toSp ====
-- toSp: expand a symmetric function in the basis of symplectic characters.
-- Returns a RingElement in a SchurRing with GroupActing => "Sp".  With no
-- target supplied, an associated symplectic ring is cached on the input ring
-- (lazy construction) and used as the output ring.
toSp = method()

toSp(RingElement) := (f) -> (
     R := ring f;
     if class R =!= SchurRing then return toSp(toS f);
     if R.?GroupActing and R.GroupActing == "Sp" then return f;
     T := symplecticBasisRingOf R;
     toSp(f, T)
     )

toSp(RingElement, SchurRing) := (f, T) -> (
     if not (T.?GroupActing and T.GroupActing == "Sp") then
	  error "expected second argument to be a SchurRing with GroupActing => \"Sp\"";
     R := ring f;
     if class R =!= SchurRing then return toSp(toS f, T);
     dimT := numgens T;
     -- If f is already in the Sp basis, just re-promote partition labels.
     if R.?GroupActing and R.GroupActing == "Sp" then (
	  rawRes := raw(0_T);
	  for term in listForm f do (
	       if dimT === infinity or #(term#0) <= dimT then (
		    sc := raw promote(term#1, T);
		    ba := raw T_(term#0);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  return new T from rawRes;
	  );
     -- If f is in another variant basis (O, Monomial), route through plain S.
     if (R.?GroupActing and R.GroupActing == "O")
	  or (R.?Basis and R.Basis == "Monomial")
	  then return toSp(toS f, T);
     -- Otherwise f is in a plain Schur ring: apply Koike inverse formula.
     schurToSpRE(f, T)
     )

toSp(Thing) := (f) -> f
undocumented(toSp,Thing)

-- ==== toO ====
-- toO: expand a symmetric function in the basis of orthogonal characters.
-- Returns a RingElement in a SchurRing with GroupActing => "O".  The
-- OddOrEven tag attached to T (default "Odd", i.e. O(2n+1) / type B_n)
-- governs which Littlewood branching rule is used when going from a
-- plain Schur expansion into the O basis.
toO = method()

toO(RingElement) := (f) -> (
     R := ring f;
     if class R =!= SchurRing then return toO(toS f);
     if R.?GroupActing and R.GroupActing == "O" then return f;
     T := orthogonalBasisRingOf R;
     toO(f, T)
     )

toO(RingElement, SchurRing) := (f, T) -> (
     if not (T.?GroupActing and T.GroupActing == "O") then
	  error "expected second argument to be a SchurRing with GroupActing => \"O\"";
     R := ring f;
     if class R =!= SchurRing then return toO(toS f, T);
     dimT := numgens T;
     -- Source is already in the O basis: just re-promote partition labels.
     if R.?GroupActing and R.GroupActing == "O" then (
	  rawRes := raw(0_T);
	  for term in listForm f do (
	       if dimT === infinity or #(term#0) <= dimT then (
		    sc := raw promote(term#1, T);
		    ba := raw T_(term#0);
		    rawRes = rawRes + sc * ba;
		    );
	       );
	  return new T from rawRes;
	  );
     -- Source is in another variant basis (Sp, Monomial): route through plain S.
     if (R.?GroupActing and R.GroupActing == "Sp")
	  or (R.?Basis and R.Basis == "Monomial")
	  then return toO(toS f, T);
     -- Fallback: source is a plain Schur ring; apply Littlewood branching.
     schurToORE(f, T)
     )

toO(Thing) := (f) -> f
undocumented(toO,Thing)

-- ==== toGL ====
-- toGL: re-express an element in the plain GL Schur basis.  This is a
-- thin synonym for toS that reads naturally when the intent is to
-- obtain a GL character.  With a target ring T the element is promoted
-- into T (which must have GroupActing => "GL" and may have finite or
-- infinite rank; finite-rank targets truncate partitions longer than
-- numgens T via specializeGLInto inside toS).
toGL = method()
toGL(RingElement) := (f) -> toS f
toGL(RingElement, SchurRing) := (f, T) -> (
     -- Reject targets whose GroupActing tag is anything other than "GL".
     if T.?GroupActing and T.GroupActing != "GL" then
	  error ("toGL: target ring must have GroupActing => \"GL\", got \""
	       | toString T.GroupActing | "\"");
     toS(f, T)
     )
toGL(Thing) := (f) -> f
undocumented(toGL, Thing)

-- ==== toSn ====
-- toSn: re-express an element as an S_n class function written in the
-- Schur basis (the Frobenius-characteristic convention).  The
-- underlying partition data is carried over verbatim; toSn does NOT
-- apply any restriction formula, because the Sn and GL Schur rings use
-- the same partition index set -- only the multiplication differs
-- (internal product vs. Littlewood-Richardson).  Finite-rank targets
-- drop partitions with more than numgens T parts.
toSn = method()
toSn(RingElement, SchurRing) := (f, T) -> (
     if not (T.?GroupActing and T.GroupActing == "Sn") then
	  error "toSn: target ring must have GroupActing => \"Sn\"";
     R := ring f;
     -- Non-SchurRing inputs: first convert to Schur form.
     if class R =!= SchurRing then return toSn(toS f, T);
     -- From a variant basis other than plain GL/Sn, go through toS.
     if R.?GroupActing and R.GroupActing != "GL" and R.GroupActing != "Sn" then
	  return toSn(toS f, T);
     if R.?Basis and R.Basis == "Monomial" then
	  return toSn(toS f, T);
     -- Copy partition labels into T, respecting the target rank.
     dimT := numgens T;
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := term#0;
	  c   := term#1;
	  if dimT === infinity or (class dimT =!= InfiniteNumber and #lam <= dimT) then (
	       sc := raw promote(c, T);
	       rawRes = rawRes + sc * raw T_lam;
	       );
	  );
     new T from rawRes
     )
toSn(Thing, SchurRing) := (f, T) -> try promote(f, T) else error "toSn: cannot promote input to target ring"
undocumented(toSn, Thing, SchurRing)

-- ==== convert (universal dispatch) ====
-- convert: universal dispatcher.  Given any symmetric-function-like
-- element f and a target ring T, pick the right converter.  The target
-- ring's classification drives the dispatch:
--   * SchurRing with GroupActing "GL"    -> toS(f, T)
--   * SchurRing with GroupActing "Sn"    -> toSn(f, T)
--   * SchurRing with GroupActing "Sp"    -> toSp(f, T)
--   * SchurRing with GroupActing "O"     -> toO(f, T)
--   * SchurRing with GroupActing "SL"    -> toS(f, T)  (GL collapse)
--   * SchurRing with GroupActing "RatGL" -> toRatGL(f, T)
--   * symmetricRing                      -> re-apply Jacobi-Trudi in T
--   * any other ring                     -> try promote(f, T)
-- This is a pure routing layer; no new math is performed here.
convert = method()
convert(RingElement, Ring) := (f, T) -> (
     -- SchurRing target: dispatch on GroupActing
     if instance(T, SchurRing) then (
	  ga := if T.?GroupActing then T.GroupActing else "GL";
	  if ga == "GL" then return toS(f, T);
	  if ga == "Sn" then return toSn(f, T);
	  if ga == "Sp" then return toSp(f, T);
	  if ga == "O"  then return toO(f, T);
	  if ga == "SL" then return toS(f, T);
	  if ga == "RatGL" then return toRatGL(f, T);
	  error ("convert: unknown GroupActing on target: " | toString ga);
	  );
     -- symmetricRing target: re-apply Jacobi-Trudi into T.  toSymm alone
     -- uses the source ring's pre-attached symmetricRing, which is
     -- typically a different ring object from T; computing jacobiTrudi
     -- directly in T produces an element that actually lives in T.
     if T.?schurLevel and schurLevel T > 0 and class T =!= SchurRing then (
	  R := ring f;
	  if R === T then return f;
	  -- From a SchurRing: sum c_lambda * jacobiTrudi(lambda, T)
	  if instance(R, SchurRing) then (
	       tms := listForm f;
	       cR := coefficientRing R;
	       return sum apply(tms, (p, c) ->
		    (try b := jacobiTrudi(toList p, T)
			 else error "convert: target symmetricRing has smaller dim than source partitions; enlarge target")
		    * promote(lift(c, cR), T));
	       );
	  -- Otherwise already in some symmetric ring: try plain promotion.
	  return try promote(f, T) else
	       error "convert: symmetric-ring target is not compatible with source";
	  );
     -- Generic ring: promote if possible
     try promote(f, T) else
	  error ("convert: no route from " | toString ring f | " to " | toString T)
     )
convert(Number, Ring) := (f, T) -> try promote(f, T) else f
convert(Thing, Ring) := (f, T) -> try promote(f, T) else f
undocumented(convert, Thing, Ring)

-----------------------------------------------------------------------
-- Specialization of universal characters to finite-rank groups.
--
-- This section implements the restriction of stable (universal)
-- characters to finite-rank classical groups.  The main entry point is
-- `specialize(RingElement, ZZ)`, which dispatches on `R.GroupActing`.
-- Below it live the per-variant workers `specialize<G>` (which fetch or
-- build a cached finite target) and the `<G>Into` variants (which
-- accept an explicit target ring T).  The `specialized<G>RingOf`
-- helpers memoize one finite ring per (stable source, rank) pair so
-- repeated specializations share a target.
-----------------------------------------------------------------------

-- ==== Cached target rings ====

-- Get or create the cached finite-dimensional Sp(2n) character ring attached
-- to a stable Sp ring S.  Keyed by the half-dimension n.
--
-- The ring is constructed with a rank-tagged symbol (e.g. sp_fin_1, sp_fin_2)
-- so that the globalAssign inside schurRing does NOT clobber the user's
-- original stable symbol (e.g. sp).  Users should reach the finite ring via
-- `ring(specialize(f,n))` rather than via a global variable.
specializedSpRingOf = (S, n) -> (
     if not S.?specializedSpRings then S.specializedSpRings = new MutableHashTable;
     if (S.specializedSpRings)#?n then (S.specializedSpRings)#n
     else (
	  spSym := getSymbol("spfin" | toString n);
	  T := schurRing(coefficientRing S, spSym, n, GroupActing => "Sp");
	  (S.specializedSpRings)#n = T;
	  T
	  )
     )

-- Get or create the cached finite-dimensional O character ring attached to a
-- stable O ring S.  Keyed by the pair (n, kind) where kind is "Odd" for
-- SO(2n+1) / O(2n+1) (type B_n) or "Even" for O(2n) / SO(2n) (type D_n).
specializedORingOf = method(Options => {OddOrEven => "Odd"})
specializedORingOf(SchurRing, ZZ) := opts -> (S, n) -> (
     kind := opts.OddOrEven;
     if kind =!= "Odd" and kind =!= "Even" then
	  error "specializedORingOf: OddOrEven must be \"Odd\" or \"Even\"";
     if not S.?specializedORings then S.specializedORings = new MutableHashTable;
     key := (n, kind);
     if (S.specializedORings)#?key then (S.specializedORings)#key
     else (
	  tag := if kind == "Odd" then "finodd" else "fineven";
	  oSym := getSymbol("o" | tag | toString n);
	  T := schurRing(coefficientRing S, oSym, n, GroupActing => "O", OddOrEven => kind);
	  (S.specializedORings)#key = T;
	  T
	  )
     )

-- ==== Main specialize dispatch ====

-- Specialize a stable Sp (resp. O) universal character to the finite-dim
-- Sp(2n) (resp. O(2n+1)/O(2n)) character ring.
--
-- Algorithm: let f live in a stable Sp ring.  Writing f in the stable Schur
-- basis via the forward Koike formula gives  f = sum_nu c_nu s_nu.  The
-- GL(2n) specialization truncates ell(nu) > 2n to zero.  The surviving
-- s_nu's restrict to Sp(2n) via Littlewood's finite-dimensional branching
-- rule
--   s_nu |_{Sp(2n)} = sum_{delta even-col} sum_{mu : ell(mu) <= n}
--                       c^nu_{delta,mu} sp_mu ,
-- i.e., take the even-column skew expansion and drop partitions of length
-- exceeding n.  The whole specialization is a genuine ring homomorphism:
-- specialize(f*g, n) == specialize(f,n) * specialize(g,n) at the level of
-- GL(2n) characters.  In the sp-basis target ring, the user should be aware
-- that multiplication currently uses the stable Newell-Littlewood product
-- with a length cutoff; a faithful finite-dim product (Phase 5d) is not yet
-- wired in.  For an element of a stable Sp ring whose stable Newell-
-- Littlewood product projects inside ell <= n, the two agree.
specialize = method(Options => {OddOrEven => null})
specialize(RingElement, ZZ) := opts -> (f, n) -> (
     R := ring f;
     if class R =!= SchurRing then error "specialize expects a SchurRing element";
     if n < 0 then error "specialize: dimension n must be >= 0";
     if not R.?GroupActing then error "specialize: missing GroupActing attribute on ring";
     if R.GroupActing == "Sp" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeSp(f, n)
	  )
     else if R.GroupActing == "O" then (
	  kind := if opts.OddOrEven === null then
	       (if R.?OddOrEven then R.OddOrEven else "Odd")
	       else opts.OddOrEven;
	  specializeO(f, n, OddOrEven => kind)
	  )
     else if R.GroupActing == "GL" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeGL(f, n)
	  )
     else if R.GroupActing == "SL" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeSL(f, n)
	  )
     else if R.GroupActing == "RatGL" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeRatGL(f, n)
	  )
     else if R.GroupActing == "Sn" then (
	  if opts.OddOrEven =!= null then
	       error "specialize: OddOrEven is only meaningful for O rings";
	  specializeSn(f, n)
	  )
     else error("specialize is not implemented for GroupActing => \"" | R.GroupActing | "\"")
     )

-- Specialize to an entire tower of ranks at once.  The list describes the
-- target ranks from the topmost layer inward; `infinity` (or any
-- InfiniteNumber) leaves that layer untouched.  Example: for a ring S of
-- schurLevel 2 whose outer layer is a stable Sp ring and inner layer is a
-- stable GL ring, `specialize(f, {4, 3})` produces an element of Sp(8) over
-- GL(3).  A shorter list only specializes the outermost layers.
specialize(RingElement, List) := opts -> (f, ranks) -> (
     R := ring f;
     if #ranks == 0 then return f;
     nTop := ranks#0;
     g := if class nTop === InfiniteNumber then f
	  else specialize(f, nTop, opts);
     if #ranks == 1 then return g;
     -- recurse into coefficient ring
     S := ring g;
     A := coefficientRing S;
     if class A =!= SchurRing then (
	  if #ranks > 1 then error(
	       "specialize: rank list has " | toString (#ranks)
	       | " entries but only "
	       | toString (schurLevel S)
	       | " SchurRing layers are present");
	  return g;
	  );
     -- Rebuild g by specializing each coefficient
     innerRanks := drop(ranks, 1);
     -- Specialize one coefficient (which lives in A)
     specOneCoef := (c) -> specialize(c, innerRanks, opts);
     -- Take inner specialization of an example coefficient to find target A'
     sampleCoef := 1_A;
     Asp := ring specOneCoef(sampleCoef);
     -- Build the specialized outer ring with coefficient ring Asp
     S' := local S';
     spSym := S.Symbol;
     outerRank := numgens S;
     gActing := S.GroupActing;
     oddEven := if S.?OddOrEven then S.OddOrEven else null;
     basis := if S.?Basis then S.Basis else "Schur";
     S' = if oddEven =!= null then
	       schurRing(Asp, spSym, outerRank,
		    GroupActing => gActing, OddOrEven => oddEven, Basis => basis)
	  else
	       schurRing(Asp, spSym, outerRank,
		    GroupActing => gActing, Basis => basis);
     rawRes := raw(0_S');
     for term in listForm g do (
	  lam := term#0;
	  c := term#1;
	  cSpec := specOneCoef(c);
	  sc := raw promote(cSpec, S');
	  ba := raw (S'_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     new S' from rawRes
     )

-- Also allow specializing from a stable Schur (GL) ring directly, with an
-- explicit target kind.  This is occasionally useful when the user already
-- has a Schur-basis representation and wants to restrict along GL -> Sp/O.
specialize(RingElement, ZZ, SchurRing) := opts -> (f, n, T) -> (
     if not (class T === SchurRing and T.?GroupActing) then
	  error "specialize: target ring must be an Sp or O SchurRing";
     if opts.OddOrEven =!= null then (
	  -- allow user to override/verify T's tag
	  if T.?OddOrEven and T.OddOrEven =!= opts.OddOrEven then
	       error("specialize: OddOrEven option " | toString opts.OddOrEven
		    | " does not match target ring's OddOrEven = "
		    | toString T.OddOrEven);
	  );
     if T.GroupActing == "Sp" then specializeSpInto(f, n, T)
     else if T.GroupActing == "O" then specializeOInto(f, n, T)
     else if T.GroupActing == "GL" then specializeGLInto(f, n, T)
     else if T.GroupActing == "SL" then specializeSLInto(f, n, T)
     else if T.GroupActing == "Sn" then specializeSnInto(f, n, T)
     else error("specialize into GroupActing \"" | T.GroupActing | "\" is not supported")
     )

-- ==== Per-variant specialize and *Into workers ====

-- Inject a GL element (possibly coming from an Sp/O/Monomial ring via toS)
-- into an explicit finite GL ring T, truncating partitions of length > n.
-- The `*Into` form accepts an explicit target; the plain `specializeGL`
-- below fetches a cached target via `specializedGLRingOf`.
specializeGLInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing =!= "GL"
	  and R.GroupActing =!= "SL" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = f;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     new T from rawRes
     )

-- Inject a GL/SL element into an explicit finite SL(n) ring T: truncate
-- partitions of length > n, then apply slCanonicalize to normalize
-- representatives modulo the determinant relation.
specializeSLInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing =!= "GL"
	  and R.GroupActing =!= "SL" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = f;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     slCanonicalize(new T from rawRes, T)
     )

-- Restrict a stable GL/Sp character into an explicit finite Sp(2n) ring T
-- via Littlewood's branching rule.  After pushing f to the Schur basis,
-- partitions with ell(nu) > 2n are discarded (GL(2n) truncation), and
-- each surviving s_nu expands as sum over even-column subpartitions
-- delta of nu, then the skew s_{nu/delta} = sum c^nu_{delta,mu} s_mu is
-- projected to the sp-basis by keeping only #mu <= n.
specializeSpInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "Sp" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = toS f;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > 2*n then continue;
	  -- Littlewood Sp branching: s_nu|_Sp(2n) = sum_{delta even-col subpart of nu}
	  -- sum_mu c^nu_{delta,mu} sp_mu, truncated to #mu <= n.
	  for delta in evenColsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Restrict a stable GL/O character into an explicit finite O ring T via
-- Littlewood's branching rule.  nuCutoff is 2n+1 for odd O (type B_n)
-- and 2n for even O (type D_n); partitions longer than this are dropped.
-- Each surviving s_nu restricts via sum over even-row subpartitions
-- delta: sum_mu c^nu_{delta,mu} o_mu, keeping only #mu <= n.
specializeOInto = (f, n, T) -> (
     R := ring f;
     local fs;
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "O" then (
	  sStable := schurBasisRingOf R;
	  fs = toS(f, sStable);
	  ) else fs = toS f;
     kind := if T.?OddOrEven then T.OddOrEven else "Odd";
     nuCutoff := if kind == "Odd" then 2*n+1 else 2*n;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > nuCutoff then continue;
	  -- Littlewood O branching: s_nu|_O = sum_{delta even-row subpart of nu}
	  -- sum_mu c^nu_{delta,mu} o_mu, truncated to #mu <= n.
	  for delta in evenRowsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Restrict a stable Sp character to the cached finite Sp(2n) ring.  See
-- `specializeSpInto` for the Littlewood even-column branching rule.
specializeSp = (f, n) -> (
     R := ring f;
     T := specializedSpRingOf(R, n);
     sStable := schurBasisRingOf R;
     fs := toS(f, sStable);
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > 2*n then continue;
	  -- Littlewood Sp branching (see specializeSpInto): even-column delta,
	  -- skew expansion s_{nu/delta}, keep #mu <= n.
	  for delta in evenColsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Restrict a stable O character to the cached finite O(2n+1) or O(2n)
-- ring, selected by the OddOrEven option.  See `specializeOInto` for
-- the Littlewood even-row branching rule.
specializeO = method(Options => {OddOrEven => "Odd"})
specializeO(RingElement, ZZ) := opts -> (f, n) -> (
     R := ring f;
     T := specializedORingOf(R, n, OddOrEven => opts.OddOrEven);
     sStable := schurBasisRingOf R;
     fs := toS(f, sStable);
     kind := opts.OddOrEven;
     nuCutoff := if kind == "Odd" then 2*n+1 else 2*n;
     rawRes := raw(0_T);
     for term in listForm fs do (
	  nu := term#0;
	  c := term#1;
	  if #nu > nuCutoff then continue;
	  -- Littlewood O branching (see specializeOInto): even-row delta,
	  -- skew expansion s_{nu/delta}, keep #mu <= n.
	  for delta in evenRowsSubpartitionsOf(nu) do (
	       for pair in skewSchurExpansion(nu, delta) do (
		    mu := pair#0;
		    lrCoef := pair#1;
		    if #mu <= n then (
			 sc := raw promote(c * lrCoef, T);
			 ba := raw T_mu;
			 rawRes = rawRes + sc * ba;
			 );
		    );
	       );
	  );
     new T from rawRes
     )

-- Get or create the cached finite-dimensional GL(n) Schur ring attached to a
-- stable GL ring S.  Analogous to specializedSpRingOf.
specializedGLRingOf = (S, n) -> (
     if not S.?specializedGLRings then S.specializedGLRings = new MutableHashTable;
     if (S.specializedGLRings)#?n then (S.specializedGLRings)#n
     else (
	  glSym := getSymbol("glfin" | toString n);
	  T := schurRing(coefficientRing S, glSym, n, GroupActing => "GL");
	  (S.specializedGLRings)#n = T;
	  T
	  )
     )

-- Get or create the cached finite-dimensional SL(n) Schur ring attached to a
-- stable SL (or GL) ring S.
specializedSLRingOf = (S, n) -> (
     if not S.?specializedSLRings then S.specializedSLRings = new MutableHashTable;
     if (S.specializedSLRings)#?n then (S.specializedSLRings)#n
     else (
	  slSym := getSymbol("slfin" | toString n);
	  T := schurRing(coefficientRing S, slSym, n, GroupActing => "SL");
	  (S.specializedSLRings)#n = T;
	  T
	  )
     )

-- Specialize a GL element to GL(n): truncate partitions of length > n.  If
-- the source ring is itself finite with numgens <= n, the engine already
-- handles this and specialize just returns a relabeled element.
specializeGL = (f, n) -> (
     R := ring f;
     T := specializedGLRingOf(R, n);
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     new T from rawRes
     )

-- Specialize a GL element to SL(n): truncate partitions of length > n, then
-- canonicalize via the determinant relation (strip lambda_n from every part).
-- Also accepts an SL input.
specializeSL = (f, n) -> (
     R := ring f;
     T := specializedSLRingOf(R, n);
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_(toList lam));
	  rawRes = rawRes + sc * ba;
	  );
     slCanonicalize(new T from rawRes, T)
     )

---------------------------------------------------------------
-- Specialization for Sn rings (truncation by number of parts)
---------------------------------------------------------------
-- Semantic: an Sn-SchurRing of "rank n" (finite n) allows partitions
-- of arbitrary size but bounded by having at most n parts (matching the
-- constructor schurRing(..., n, GroupActing => "Sn")).  `specialize(f, n)`
-- on an Sn element (stable or finite rank M >= n) simply drops terms
-- whose partition has more than n parts and rebuilds the result in the
-- cached finite target ring.  Unlike GL, there is no modification rule
-- because the finite Sn ring is literally the quotient of the stable
-- ring by the ideal of partitions with > n parts (not a character
-- specialization in the Lie-theory sense, but the user-level analog).

specializedSnRingOf = (S, n) -> (
     if not S.?specializedSnRings then S.specializedSnRings = new MutableHashTable;
     if (S.specializedSnRings)#?n then (S.specializedSnRings)#n
     else (
	  snSym := getSymbol("snfin" | toString n);
	  T := schurRing(coefficientRing S, snSym, n, GroupActing => "Sn");
	  (S.specializedSnRings)#n = T;
	  T
	  )
     )

specializeSn = (f, n) -> (
     R := ring f;
     T := specializedSnRingOf(R, n);
     specializeSnInto(f, n, T)
     )

specializeSnInto = (f, n, T) -> (
     if T.GroupActing =!= "Sn" then
	  error "specializeSn: target ring must have GroupActing => \"Sn\"";
     rawRes := raw(0_T);
     for term in listForm f do (
	  lam := toList term#0;
	  c := term#1;
	  if #lam > n then continue;
	  sc := raw promote(c, T);
	  ba := raw (T_lam);
	  rawRes = rawRes + sc * ba;
	  );
     new T from rawRes
     )

---------------------------------------------------------------
-- Specialization for rational GL (Koike-Terada modification rule)
---------------------------------------------------------------
-- specializedRatGLRingOf(S, n): get or create a finite-rank GL(n) rational
-- Schur ring attached to a stable (or higher-rank) RatGL ring S.  Caching by
-- rank n mirrors specializedGLRingOf / specializedSpRingOf.
specializedRatGLRingOf = (S, n) -> (
     if not S.?specializedRatGLRings then S.specializedRatGLRings = new MutableHashTable;
     if (S.specializedRatGLRings)#?n then (S.specializedRatGLRings)#n
     else (
	  baseR := coefficientRing (S.ratNegRing);
	  ratSym := getSymbol(toString (S.ratPosSym) | "fin" | toString n);
	  T := schurRing(baseR, ratSym, n, GroupActing => "RatGL");
	  (S.specializedRatGLRings)#n = T;
	  T
	  )
     )

-- specializeRatGL(f, n): push f through the Koike-Terada modification rule at
-- rank n and build the result in a cached finite target ring.  The rule is a
-- ring homomorphism: it is a specialization (character evaluation at GL(n)),
-- so it is multiplicative on the universal rational character ring.  The rule
-- lives in ratGLModify (above) and iterateRatGLTerms walks the (alpha, beta,
-- scalar) triples.
specializeRatGL = (f, n) -> (
     R := ring f;
     T := specializedRatGLRingOf(R, n);
     specializeRatGLInto(f, n, T)
     )

-- Construct the RatGL basis element T_{alpha, beta} directly via engine-level
-- raw multiplication.  This bypasses any overridden S*S (which may recurse
-- into Koike-Terada modification) and simply combines outer alpha with inner
-- beta to give the pure bipartition element.
ratGLBasisRaw = (T, alpha, beta) -> (
     B := T.ratNegRing;
     (raw T_alpha) * (raw promote(B_beta, T))
     )

-- Koike universal character product.  For universal rational characters
-- chi_{alpha,beta} the correct product is
--    chi_{alpha,beta} * chi_{gamma,delta}
--      = sum_{epsilon, eta} (s_{alpha/epsilon} s_{gamma/eta})(x) *
--                           (s_{beta/eta}   s_{delta/epsilon})(y)
-- re-expressed in the bipartition basis.  We evaluate the sum by:
--   * iterating over pairs (epsilon, eta), with epsilon contained in both
--     alpha and delta, and eta in both beta and gamma;
--   * lifting the outer skew-Schur expansions to pure-alpha elements of S
--     and the inner ones to pure-beta elements, and multiplying them at the
--     engine level (which performs componentwise LR on the bipartition
--     basis, precisely what is needed for a given (epsilon, eta)).
-- Returns a raw (engine-level) element of S.  Used by RatGL multiplication
-- at both stable and finite rank.
ratGLKoikeProductRaw = (S, f1, f2) -> (
     acc := raw(0_S);
     iterateRatGLTerms(f1, (alpha, beta, sa) -> (
	       iterateRatGLTerms(f2, (gamma, delta, sg) -> (
			 -- Common epsilon bound: componentwise min of alpha and delta.
			 mAD := min(#alpha, #delta);
			 epsBound := for i from 0 to mAD - 1 list min(alpha#i, delta#i);
			 mBG := min(#beta, #gamma);
			 etaBound := for i from 0 to mBG - 1 list min(beta#i, gamma#i);
			 for eps in allSubpartitionsBoundedBy epsBound do (
			      skewAE := skewSchurExpansion(alpha, eps);
			      if #skewAE == 0 then continue;
			      skewDE := skewSchurExpansion(delta, eps);
			      if #skewDE == 0 then continue;
			      for eta in allSubpartitionsBoundedBy etaBound do (
				   skewGH := skewSchurExpansion(gamma, eta);
				   if #skewGH == 0 then continue;
				   skewBH := skewSchurExpansion(beta, eta);
				   if #skewBH == 0 then continue;
				   -- Build A = sum_{mu1} c^alpha_{mu1,eps} S_{mu1,()}.
				   rA := raw(0_S);
				   for pA in skewAE do
					rA = rA + (pA#1) * ratGLBasisRaw(S, toList pA#0, {});
				   rG := raw(0_S);
				   for pG in skewGH do
					rG = rG + (pG#1) * ratGLBasisRaw(S, toList pG#0, {});
				   rB := raw(0_S);
				   for pB in skewBH do
					rB = rB + (pB#1) * ratGLBasisRaw(S, {}, toList pB#0);
				   rD := raw(0_S);
				   for pD in skewDE do
					rD = rD + (pD#1) * ratGLBasisRaw(S, {}, toList pD#0);
				   -- Raw engine multiplication is componentwise LR on
				   -- the bipartition basis, which is what we want.
				   contribution := rA * rG * rB * rD;
				   sc := raw promote(sa * sg, S);
				   acc = acc + sc * contribution;
				   );
			      );
			 ));
	       ));
     acc
     )

-- specializeRatGLInto(f, n, T): same as specializeRatGL but the target ring T
-- is provided explicitly (must have GroupActing => "RatGL" and numgens T == n).
specializeRatGLInto = (f, n, T) -> (
     if T.GroupActing =!= "RatGL" then
	  error "specializeRatGLInto: target ring must have GroupActing => \"RatGL\"";
     if numgens T =!= n then
	  error("specializeRatGLInto: target ring has numgens = " | toString numgens T
	       | " but expected " | toString n);
     rawRes := raw(0_T);
     iterateRatGLTerms(f, (alpha, beta, scalar) -> (
	       for trip in ratGLModify(alpha, beta, n) do (
		    alphaP := trip#0;
		    betaP  := trip#1;
		    coef   := trip#2;
		    sc := raw promote(coef * scalar, T);
		    rawRes = rawRes + sc * ratGLBasisRaw(T, alphaP, betaP);
		    );
	       ));
     new T from rawRes
     )

---------------------------------------------------------------
-- Lifting normal type-A characters to rational characters
---------------------------------------------------------------
-- toRatGL(f, T): given an element f of a plain GL (or SL) SchurRing, or an
-- element of another RatGL ring, lift it into the RatGL ring T by the obvious
-- embedding alpha |-> (alpha, ()) on basis elements.  This realizes the
-- inclusion of polynomial characters into rational characters.
toRatGL = method()
toRatGL(RingElement, SchurRing) := (f, T) -> (
     if T.GroupActing =!= "RatGL" then
	  error "toRatGL: target ring must have GroupActing => \"RatGL\"";
     R := ring f;
     rawRes := raw(0_T);
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "RatGL" then (
	  -- Already rational: re-embed (alpha, beta).  If target rank is finite
	  -- and smaller than the bipartition's "length" ell(alpha)+ell(beta),
	  -- apply the Koike-Terada modification.
	  iterateRatGLTerms(f, (alpha, beta, scalar) -> (
		    nT := numgens T;
		    if class nT === InfiniteNumber then (
			 sc  := raw promote(scalar, T);
			 rawRes = rawRes + sc * ratGLBasisRaw(T, alpha, beta);
			 )
		    else (
			 for trip in ratGLModify(alpha, beta, nT) do (
			      alphaP := trip#0;
			      betaP  := trip#1;
			      coef   := trip#2;
			      sc     := raw promote(coef * scalar, T);
			      rawRes = rawRes + sc * ratGLBasisRaw(T, alphaP, betaP);
			      );
			 );
		    ));
	  )
     else if class R === SchurRing and R.?GroupActing
	  and (R.GroupActing == "GL" or R.GroupActing == "SL") then (
	  -- Lift a plain GL/SL element: alpha |-> (alpha, ()).
	  nT := numgens T;
	  for term in listForm f do (
	       alpha := toList term#0;
	       c     := term#1;
	       if (class nT =!= InfiniteNumber) and #alpha > nT then continue;
	       sc  := raw promote(c, T);
	       rawRes = rawRes + sc * ratGLBasisRaw(T, alpha, {});
	       );
	  )
     else if class R =!= SchurRing and R.?EHPVariables then (
	  -- symmetricRing input: convert to its associated Schur ring (GL) first,
	  -- then recurse into the GL branch above.
	  return toRatGL(toS f, T);
	  )
     else error("toRatGL: source ring must be a SchurRing of type GL/SL/RatGL or a symmetricRing");
     new T from rawRes
     )

-- Convenience: if no target ring is supplied, build a stable RatGL ring
-- attached to the source automatically.  Parallels toSp/toO overloads.
toRatGL(RingElement) := (f) -> (
     R := ring f;
     if class R === SchurRing and R.?GroupActing and R.GroupActing == "RatGL" then
	  return f;
     if not R.?associatedRatGLRing then (
	  rtSym := getSymbol "rt";
	  R.associatedRatGLRing = schurRing(QQ, rtSym, infinity, GroupActing => "RatGL");
	  );
     toRatGL(f, R.associatedRatGLRing)
     )

---------------------------------------------------------------
------------- King branching formulas --------------------------
---------------------------------------------------------------
--
-- For the three families of classical groups we implement the
-- restriction of an irreducible character from a "total" group to a
-- block-diagonal Levi subgroup, following King 1975 ("Branching rules
-- for classical Lie groups using tensor and spinor methods",
-- J. Phys. A 8, 429-449).  In terms of Littlewood-Richardson numbers
--
--   c^lambda_{alpha,beta,gamma}
--     = coefficient of s_lambda in s_alpha * s_beta * s_gamma,
--
-- the three rules are
--
--   GL(m+n) down to GL(m) x GL(n):
--     s_lambda |--> sum_{mu,nu} c^lambda_{(), mu, nu}  s_mu x s_nu
--
--   Sp(2m+2n) down to Sp(2m) x Sp(2n):
--     sp_lambda |--> sum_{delta, mu, nu}
--                      c^lambda_{delta, mu, nu}  sp_mu x sp_nu,
--     where delta ranges over partitions each of whose columns has
--     even length (equivalently, every part of delta has even
--     multiplicity).
--
--   O(m+n) down to O(m) x O(n):
--     o_lambda  |--> sum_{delta, mu, nu}
--                      c^lambda_{delta, mu, nu}  o_mu x o_nu,
--     where delta ranges over partitions all of whose parts are even.
--
-- Partitions mu, nu exceeding the ranks of the respective factor rings
-- are collapsed via the Sam-Snowden-Weyman modification rule
-- (@TO modificationRule@): a single (mu', nu') may appear with a
-- signed integer multiplicity, or (mu, nu) may be suppressed entirely.

-- Partitions of n all of whose parts appear with even multiplicity
-- (conjugate to partitions-of-n-with-even-parts).
partitionsPartsInPairs = (n) -> (
     if n == 0 then return {{}};
     if odd n then return {};
     apply(partitions(n // 2), p -> flatten apply(toList p, e -> {e, e}))
     )

-- Partitions of n all of whose parts are even.
partitionsAllPartsEven = (n) -> (
     if n == 0 then return {{}};
     if odd n then return {};
     apply(partitions(n // 2), p -> apply(toList p, e -> 2*e))
     )

-- Triple Littlewood-Richardson coefficient c^lambda_{a,b,c}
--   = coefficient of s_lambda in s_a * s_b * s_c (in any GL Schur ring).
-- Returns 0 if any partition does not fit the ring's rank.
tripleLRCoeff = (lambda, a, b, c, T) -> (
     nT := numgens T;
     if nT =!= infinity and (#a > nT or #b > nT or #c > nT) then return 0;
     prod := T_(toList a) * T_(toList b) * T_(toList c);
     cf := 0;
     lam := toList lambda;
     for t in listForm prod do
	  if toList first t == lam then cf = last t;
     cf
     )

-- Apply the modification rule to a partition `mu` for a finite-rank
-- target ring S; returns (mu', sign) with sign = 0 meaning "suppressed".
-- For stable targets (numgens infinity) this is the identity.
applyBranchMod = (mu, S) -> (
     n := numgens S;
     if n === infinity then return (toList mu, 1);
     kind := if S.?GroupActing then S.GroupActing else "GL";
     if kind == "GL" then (
	  if #mu > n then return (toList mu, 0) else return (toList mu, 1);
	  );
     if kind == "Sp" then (
	  rSp := modificationRule(toList mu, n, "C");
	  if rSp === null then return (toList mu, 0);
	  return rSp;
	  );
     if kind == "O" then (
	  odd0 := if S.?OddOrEven then S.OddOrEven else "Odd";
	  typ := if odd0 == "Odd" then "B" else "D";
	  rO := modificationRule(toList mu, n, typ);
	  if rO === null then return (toList mu, 0);
	  return rO;
	  );
     error("applyBranchMod: unknown kind " | toString kind)
     )

-- Branch a single basis partition lambda of type `kind` using King's rule,
-- into pairs (mu, nu) of partitions indexing the factor rings S1, S2.
-- Returns a MutableHashTable with entries (mu, nu) -> integer.
branchBasisPartition = (lambda, kind, S1, S2, T) -> (
     lam := toList lambda;
     d := sum lam;
     result := new MutableHashTable;
     addEntry := (mu, nu, c) -> (
	  key := (toList mu, toList nu);
	  if result#?key then result#key = result#key + c
	  else result#key = c;
	  );
     deltas := if kind == "GL" then {{}}
	       else if kind == "Sp" then
		    flatten for de from 0 to d list partitionsPartsInPairs de
	       else if kind == "O" then
		    flatten for de from 0 to d list partitionsAllPartsEven de
	       else error("branch: unsupported kind " | toString kind);
     m := numgens S1;
     n := numgens S2;
     for delta in deltas do (
	  rem := d - sum delta;
	  for k from 0 to rem do (
	       parsL := toList \ partitions k;
	       parsR := toList \ partitions (rem - k);
	       for mu in parsL do for nu in parsR do (
		    c := tripleLRCoeff(lam, delta, mu, nu, T);
		    if c == 0 then continue;
		    -- Apply modification rules for finite ranks
		    muMod := applyBranchMod(mu, S1);
		    if muMod#1 == 0 then continue;
		    nuMod := applyBranchMod(nu, S2);
		    if nuMod#1 == 0 then continue;
		    addEntry(muMod#0, nuMod#0, c * muMod#1 * nuMod#1);
		    );
	       );
	  );
     -- Drop zero entries
     for k in keys result do if result#k == 0 then remove(result, k);
     result
     )

branch = method()

-- Branch an element of a SchurRing with respect to a two-factor restriction.
-- S1 and S2 must have the same GroupActing as the input ring.  Returns a
-- HashTable mapping (mu, nu) -> coefficient.  The coefficients lie in the
-- coefficient ring of the input ring (in particular over QQ for stable
-- rings).
branch(RingElement, SchurRing, SchurRing) := HashTable => (f, S1, S2) -> (
     R := ring f;
     if class R =!= SchurRing then
	  error "branch: input must live in a SchurRing";
     gk := if R.?GroupActing then R.GroupActing else "GL";
     g1 := if S1.?GroupActing then S1.GroupActing else "GL";
     g2 := if S2.?GroupActing then S2.GroupActing else "GL";
     if g1 =!= gk or g2 =!= gk then error(
	  "branch: all three rings must share GroupActing (got "
	       | gk | ", " | g1 | ", " | g2 | ")");
     nR := numgens R;
     n1 := numgens S1;
     n2 := numgens S2;
     -- Sanity check on total rank (skip if any is stable)
     if nR =!= infinity and n1 =!= infinity and n2 =!= infinity then (
	  if n1 + n2 =!= nR then error(
	       "branch: numgens(S1) + numgens(S2) must equal numgens of ring f ("
		    | toString n1 | " + " | toString n2 | " != " | toString nR | ")");
	  );
     lfF := listForm f;
     d := if #lfF == 0 then 0
	  else max apply(lfF, t -> sum toList first t);
     helperSize := max(d, 1);
     T := plethysmHelperOf(R, helperSize);
     result := new MutableHashTable;
     addEntry := (key, v) -> (
	  if result#?key then result#key = result#key + v
	  else result#key = v;
	  );
     for term in lfF do (
	  lambda := toList first term;
	  coef := last term;
	  partResult := branchBasisPartition(lambda, gk, S1, S2, T);
	  for k in keys partResult do addEntry(k, coef * partResult#k);
	  );
     for k in keys result do if result#k == 0 then remove(result, k);
     new HashTable from result
     )

-- Convenience: branch(f, m, n) defaults to anonymous factor rings of ranks
-- m and n with the same GroupActing (and OddOrEven, for O) as ring f.
branch(RingElement, ZZ, ZZ) := HashTable => (f, m, n) -> (
     R := ring f;
     gk := if R.?GroupActing then R.GroupActing else "GL";
     sym1 := getSymbol "f1";
     sym2 := getSymbol "f2";
     oddTag := if R.?OddOrEven then R.OddOrEven else "Odd";
     S1 := if gk == "O"
	   then schurRing(coefficientRing R, sym1, m, GroupActing => "O", OddOrEven => oddTag)
	   else schurRing(coefficientRing R, sym1, m, GroupActing => gk);
     S2 := if gk == "O"
	   then schurRing(coefficientRing R, sym2, n, GroupActing => "O", OddOrEven => oddTag)
	   else schurRing(coefficientRing R, sym2, n, GroupActing => gk);
     branch(f, S1, S2)
     )

---------------------------------------------------------------
----------- End King branching formulas ------------------------
---------------------------------------------------------------

-- recTrans: recursive transform from the h-basis to the Schur basis.
--   Horner-style expansion by the leading h-variable lead = h_i (i maximal
--   appearing in pl).  Writing pl = sum_k coe#k * lead^fdeg#k (with fdeg
--   strictly decreasing), we accumulate
--       rez = ((...((0 * s_i + recTrans(coe#0)) * s_i) + recTrans(coe#1)) ...)
--   where the product "*" is dispatched by auxRecTransOp:
--       * for GL/SL/Sp/O rings (ordinary multiplication in the SchurRing),
--       ** for Sn rings (inner tensor product).
--   Each coe#k is itself recursively rewritten by recTrans (it lives in a
--   SchurRing of lower schurLevel, or ultimately at the base case below).
--   At the base case (lead === null, i.e. pl has no remaining h-variables),
--   retFcn lifts pl into the coefficient ring and calls toS there.
recTrans = method()
recTrans (RingElement) := (pl) ->
(
-- lead = leading h-variable = h_i with i maximal, or null if pl is a scalar
     lead := leadTermFcn pl;
     if lead === null then retFcn pl else
     (
	  -- read the multiplication op off the ring: * for GL/SL/Sp/O, ** for Sn
          auxRecTransOp := (schurRing ring pl).recTransOp;
-- monomials/coefficients of pl viewed as a polynomial in lead
	  (mon,coe) := coefficients(pl,Variables=>{lead});
	  mon = flatten entries mon;
	  coe = flatten entries coe;
     	  rez := 0;
	  cdeg := degree(lead,mon#0)+1;
	  for i from 0 to #mon-1 do
	  (
	       fdeg := degree(lead,mon#i);
	       -- fill in "gaps" in the Horner accumulator: bring cdeg down to
	       -- fdeg+1 by multiplying rez by s_i once per missing power
	       while (cdeg>fdeg+1) do
	       (
		    cdeg = cdeg - 1;
		    rez = auxRecTransOp(rez, mappingFcn(lead));
		    );
	       rez = auxRecTransOp(rez, mappingFcn(lead)) + recTrans(coe#i);
	       cdeg = cdeg - 1;
	       );
	  -- trailing Horner steps for any powers of lead below the smallest fdeg
	  while cdeg>0 do
	       (
		    cdeg = cdeg - 1;
		    rez = auxRecTransOp(rez, mappingFcn(lead));
		    );
	  rez
     	  )
     )

recTrans(Thing) := p -> p

--------
--------
--given a recursive relation for a sequence a_n, given by a convolution of (a_n) with (L_n)
--convolve computes formulas for a_n in terms of L_n
--the main routine is coded in the engine
--the value of conv is used to indicate one of several types of convolution
convolve = method()
convolve(List,ZZ) := (L,conv) -> (
     A := ring L_0;
     toList drop(apply(rawConvolve(L/raw//toSequence, conv), f -> new A from f),1)
     )

--a_n = p_n
--L_n = e_n
PtoE = (m,R) -> (
     n := R.dim;
     A := R.symRingForE;
     p2e := prepend(1_A, for i from 1 to n list ((-1)^(i+1) * A_(2*n+i-1)));
     if m>n then p2e = join(p2e,toList((m-n):0_A));
     R.PtoETable = if n == 0 then {1_A} else {1_A} | (- convolve(p2e,2));
     )

--a_n = h_n
--L_n = e_n
HtoE = (m,R) -> (
     n := R.dim;
     A := R.symRingForE;
     h2e := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.HtoETable = if n == 0 then {1_A} else {1_A} | convolve(h2e,0);
     )

--a_n = h_n
--L_n = p_n
HtoP = (m,R) -> (
     n := R.dim;
     A := R.symRingForP;
     h2p := prepend(1_A, for i from 1 to n list A_(2*n+i-1));
     R.HtoPTable = if n == 0 then {1_A} else {1_A} | convolve(h2p,1);
     )

--a_n = e_n
--L_n = p_n
EtoP = (m,R) -> (
     n := R.dim;
     A := R.symRingForP;
     e2p := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.EtoPTable = if n == 0 then {1_A} else {1_A} | convolve(e2p,1);
     )

--a_n = p_n
--L_n = h_n
PtoH = (m,R) -> (
     n := R.dim;
     A := R;
     p2h := prepend(1_A, for i from 1 to n list (- A_(2*n+i-1)));
     R.PtoHTable = if n == 0 then {1_A} else {1_A} | convolve(p2h,2);
     )

--a_n = e_n
--L_n = h_n
EtoH = (m,R) -> (
     n := R.dim;
     A := R;
     e2h := prepend(1_A, for i from 1 to n list (-1)^(i+1)*A_(2*n+i-1));
     R.EtoHTable = if n == 0 then {1_A} else {1_A} | convolve(e2h,0);
     )


---------------------------------------------------------------
--------------End transition-----------------------------------
---------------------------------------------------------------

---------------------------------------------------------------
-------------Schur Resolutions---------------------------------
---------------------------------------------------------------

--recsyz is a recursive method that takes as input an element el of a SchurRing of positive schurLevel
--and returns the sum of the terms having negative coefficients
--it is used in the routine schurRes to determine representations that are forced to be generators
--of syzygy modules in an equivariant resolution
recsyz = method()
recsyz (Thing) := (el) -> min(el,0)
recsyz (RingElement) := (el) ->
(
     T := ring el;
     listForm el/((u,v)->T_u*recsyz(v))//sum
     )

-----------------------------------------------------------------------------
-- schurResolution
-----------------------------------------------------------------------------
-- Computes the Schur-character data of the minimal free resolution of a
-- GL-equivariant graded module M over a symmetric algebra Sym(V).
--
-- Inputs:
--   rep  : a Schur-ring element giving the GL-character of the representation
--          V (typically s_{(1)}, the defining representation).
--   M    : {M_0, M_1, ..., M_{d-1}} -- the Schur characters of the graded
--          pieces of the module M in internal degrees 0, 1, ..., d-1.
--   plets: (in the (rep,M,plets) variant) the pre-computed list
--          [Sym^0 rep, Sym^1 rep, ..., Sym^degreeBound rep] of symmetric
--          powers of V, i.e. the graded characters of Sym(V). In the
--          (rep,M) variant these are computed on the fly.
--
-- Options:
--   DegreeLimit  : the highest internal degree to resolve to (defaults to
--                  #M - 1 when 0 is passed).
--   SyzygyLimit  : the highest syzygy index to compute (0 means: keep going
--                  until a syzygy step produces no new generators).
--
-- Output:
--   A list of lists {gens_0, gens_1, ...}, where gens_k is a list of pairs
--   (degree, character) giving the generators of the k-th syzygy module of
--   the equivariant minimal free resolution of M. Equivalently, this is the
--   Schur-character refinement of the equivariant Betti table.
-----------------------------------------------------------------------------

schurResolution = method(Options => {DegreeLimit => 0, SyzygyLimit => 0})
schurResolution(RingElement,List) := opts -> (rep,M) ->
(
     degreeBound := opts.DegreeLimit;
     if degreeBound == 0 then degreeBound = #M-1;
     syzygyBound := opts.SyzygyLimit;

     T := ring rep;
     n := schurLevel T;
--symPowersOfRep = [Sym^0 rep, Sym^1 rep, ..., Sym^degreeBound rep],
--i.e. the graded characters of Sym(V); pre-computed once and passed to schurRes.
     symPowersOfRep := new MutableList;
     symPowersOfRep#0 = 1_T;
     for i from 1 to degreeBound do symPowersOfRep#i = symmetricPower(i,rep);

     schurRes(rep,M,new List from symPowersOfRep,DegreeLimit => degreeBound,SyzygyLimit => syzygyBound)
     )

schurResolution(RingElement,List,List) := opts -> (rep,M,plets) ->
(
     degreeBound := opts.DegreeLimit;
     if degreeBound == 0 then degreeBound = #M-1;
     syzygyBound := opts.SyzygyLimit;

     schurRes(rep,M,plets,DegreeLimit => degreeBound,SyzygyLimit => syzygyBound)
     )

schurRes = method(Options => options schurResolution)
schurRes(RingElement,List,List) := opts -> (rep,M,plets) ->
(
-----------------------------------------------------------------------------
-- schurRes -- the actual worker for schurResolution.
--
-- Iterative covering algorithm. Maintains a "residue" moduleResidue#i: the
-- part of the character in internal degree i that has not yet been realized
-- by the resolution so far. At syzygy step k, for each degree i we:
--   (1) Lift the current k-th syzygy generators into degree i by multiplying
--       each generator's character by the appropriate sym power of V:
--         degreeICover = sum_{sy in syzy#k, sy#0 <= i} plets#(i - sy#0) * sy#1
--   (2) Subtract the outstanding residue:   degreeICover -= moduleResidue#i.
--   (3) Extract the negative-coefficient part via recsyz -- these are the
--       irreducibles the current syzygies fail to cover, which must become
--       fresh syzygy generators at this step.
--   (4) Record (i, -newSyzyPart) as a generator of syzy#k if nonzero.
--   (5) Update moduleResidue#i to reflect what has just been absorbed.
-- Stop either after reaching syzygyBound, or (when syzygyBound == 0) as
-- soon as a syzygy step introduces no new generators.
-----------------------------------------------------------------------------
     T := ring rep;
     degreeBound := opts.DegreeLimit;
     syzygyBound := opts.SyzygyLimit;

--moduleResidue#i = the degree-i character still needing to be covered by
--the differential in the equivariant complex; starts as M, padded with
--zeros out to degreeBound.
     moduleResidue := new MutableList from (M | toList((degreeBound+1-#M):0));
     moreToCompute := true;
     syzStep := 0;
--syzygyChars#k = list of (degree, character) pairs for the generators of
--the k-th syzygy module.
     syzygyChars := new MutableList;
     syzygyChars#syzStep = {};
     local degreeICover;
     local newSyzyPart;

--syzygy modules are constructed step by step; stop either on reaching the
--syzygyBound limit, or on finding no new syzygies at a given step.
     while moreToCompute do
     (
	  for i from 0 to degreeBound do
	  (
--(1) Lift the current k-th syzygy generators into degree i.
     	       degreeICover = 0_T;
	       for sy in syzygyChars#syzStep do
	       	    if sy#0 <= i then degreeICover = degreeICover + plets#(i-sy#0) * sy#1
		    else break;
--(2) Subtract the residue. degreeICover must cover moduleResidue#i, i.e.
--there must be a surjection of representations from degreeICover onto
--moduleResidue#i; negative coefficients in the difference signal failure.
	       degreeICover = degreeICover - moduleResidue#i;
--(3) recsyz (defined above) extracts the sum of negative-coefficient
--terms -- exactly the irreducibles that must be covered by fresh syzygies.
	       newSyzyPart = recsyz(degreeICover);
--(4) Record (i, -newSyzyPart) as a new generator of syzy#k if nonzero.
	       if newSyzyPart != 0 then syzygyChars#syzStep = syzygyChars#syzStep | {(i,-newSyzyPart)};
--(5) Update the residue to reflect what has just been absorbed.
	       moduleResidue#i = degreeICover - newSyzyPart;
	       );
--Stopping condition: syzygyBound == 0 means "no user limit" -- keep going
--until a syzygy step produces no new generators; otherwise stop once
--syzStep reaches the requested syzygyBound.
     	  if syzygyBound == 0 then moreToCompute = not (syzygyChars#syzStep == {})
	  else moreToCompute = (syzStep<syzygyBound);
     	  syzStep = syzStep + 1;
	  syzygyChars#syzStep = {};
 	  );
     select(toList syzygyChars,i-> i != {})
     )
					      
---------------------------------------------------------------
-------------end Schur Resolutions-----------------------------
---------------------------------------------------------------


---------------------------------------------------------------
--------------Characters of Symmetric Group--------------------
---------------------------------------------------------------

--given a partition lambda as a nonincreasing sequence of positive integers
--seqToMults returns the representation of this partition as a sequence
--of multiplicities: rez#i is the number of parts of lambda of size (i+1)
seqToMults = method()
seqToMults(List) := (lambda) ->
(
     lam := new Partition from lambda;
     aux := toList(conjugate lam)|{0};
     rez := {};
     for j from 0 to #aux-2 do
     (
     	  dif := aux#j-aux#(j+1);
       	  rez = rez | {dif};
	  );
     rez 
     )

-------------------------------------------------------------------------------
-- Class functions and the Frobenius characteristic map
-------------------------------------------------------------------------------
-- The Frobenius characteristic is the isomorphism
--
--      ch : R(S_n) ----> Lambda^n
--
-- between the ring of virtual characters of the symmetric group S_n and the
-- degree-n component of the ring of symmetric functions. Explicitly,
--
--      ch(chi) = (1/n!) sum_{sigma in S_n} chi(sigma) p_{cycletype(sigma)}
--              = sum_{lambda |- n} (chi(lambda) / z_lambda) p_lambda,
--
-- where z_lambda = centralizerSize(lambda) is the order of the centralizer
-- in S_n of any permutation of cycle type lambda; equivalently, writing
-- m_i for the number of parts of lambda equal to i,
--
--      z_lambda = prod_i i^{m_i} * m_i!.
--
-- A ClassFunction is stored as a HashTable keyed by partitions (encoded as
-- Sequences of parts in weakly-decreasing order); the value chi#lambda is
-- the character value on the conjugacy class of cycle type lambda. If a
-- symmetric function is written in the power-sum basis as
--
--      f = sum_lambda (c_lambda / z_lambda) p_lambda,
--
-- then the matching class function is chi#lambda = c_lambda.
-------------------------------------------------------------------------------

--given a partition lambda represented in as a sequence of multiplicities mults
--where mults#i is the number of parts of lambda of size (i+1)
--multsToSeq represents lambda as a nonincreasing sequence of positive integers
--(inverse of seqToMults, defined elsewhere)
multsToSeq = method()
multsToSeq(List) := (mults) ->
(
     n := #mults;
     par := {};
     for i from 0 to n-1 do
         par = par | splice{mults#i:(i+1)};
     reverse par
     )

--the size z_lambda of the centralizer in S_n of a permutation of cycle type
--lambda (passed here in multiplicity form: lambda#i = m_{i+1}):
--      z_lambda = prod_i i^{m_i} * m_i!
centralizerSize = method()
centralizerSize(List) := lambda ->
(
     product for i from 0 to #lambda-1 list((i+1)^(lambda#i)*(lambda#i)!)
     )

keysCF := method()
keysCF(ClassFunction) := (cF) -> keys cF

degree(ClassFunction) := ch ->
(
     ke := keysCF ch;
     if #ke == 0 then -1 else sum(first ke)
     )

-------------------------------------------------------------------------------
-- Frobenius maps: classFunction (Lambda^n -> R(S_n))
--                 symmetricFunction (R(S_n) -> Lambda^n)
-------------------------------------------------------------------------------

--go from symmetric functions to class functions:
--given f = sum_lambda (c_lambda / z_lambda) p_lambda, return chi with
--chi#lambda = c_lambda. We read f in the p-basis and, for each p-monomial,
--recover lambda from the exponent vector and store coeff * z_lambda.
classFunction = method()
classFunction(RingElement) := (f)->
(
     Rf := ring f;

     R := symmetricRing Rf;
     pf := toP f;
     n := R.dim;

     if (degree pf)#0 > n then error"Can't interpret ring element as a symmetric function";

     (mon,coe) := apply(coefficients pf,i->flatten entries i);
     ch := new MutableHashTable;
     for j from 0 to #mon-1 do
     (
	  -- The symmetricRing has 3n generators laid out as the e-, p-, and
	  -- h-blocks in that order. Indices n..2n-1 pick out the p-block, so
	  -- this slice reads the exponents of p_1, p_2, ..., p_n in mon#j,
	  -- i.e. the multiplicity form of the partition lambda.
     	  degs := (flatten exponents mon#j)_{(n)..(2*n-1)};
     	  par := multsToSeq(degs);
	  ch#par = lift(coe#j,coefficientRing R) * centralizerSize(degs);
	  );
     new ClassFunction from ch
     )

classFunction(BasicList) := (lambda)->
(
     lam := toList(lambda);
     s := symbol s;
     R := schurRing(QQ,s,sum lam);
     classFunction(R_lam)
     )

--go from class functions to symmetric functions:
--apply the Frobenius formula ch(chi) = sum_lambda (chi#lambda / z_lambda) p_lambda.
symmetricFunction = method()
symmetricFunction(ClassFunction,Ring) := (ch,S)->
(
     R := symmetricRing S;
     rez := 0_R;
     n := R.dim;
     for lam in keysCF ch do
     	  rez = rez + ch#lam * (product for i from 0 to #lam-1 list R.pVariable(lam#i)) / centralizerSize(seqToMults lam);
     if instance(S, SchurRing) then toS rez else rez
     )

-------------------------------------------------------------------------------
-- ClassFunction arithmetic (pointwise on conjugacy classes)
-------------------------------------------------------------------------------

ClassFunction + ClassFunction := (ch1,ch2)->
(
     clSum := new MutableHashTable;
     l1 := sum((keysCF ch1)#0);
     l2 := sum((keysCF ch2)#0);
     if l1 != l2 then error("The symmetric functions/characters must have the same degree");
     for i in unique(keysCF(ch1)|keysCF(ch2)) do
     	  (
	       a := b := 0;
	       if ch1#?i then a = ch1#i;
	       if ch2#?i then b = ch2#i;
	       if (a+b != 0) then clSum#i = a + b;
	       );
     new ClassFunction from clSum
     )

RingElement * ClassFunction := Number * ClassFunction := (n,ch) ->
(
     clProd := new MutableHashTable;
     for i in keysCF ch do clProd#i = n*ch#i;
     new ClassFunction from clProd
     )

ClassFunction * RingElement := ClassFunction * Number := (ch,n) -> n*ch;


ClassFunction - ClassFunction := (ch1,ch2)-> ch1 + (-1)*ch2;

ClassFunction == ClassFunction := (ch1,ch2) ->
(
     equ := true;
     for i in unique(keysCF ch1 | keysCF ch2) do
     	  if not ((not ch1#?i and not ch2#?i) or (ch1#?i and ch2#?i and ch1#i == ch2#i)) then
     	  (
	       equ = false;
	       break;
	       );
     equ
     )

-------------------------------------------------------------------------------
-- Scalar (Hall) product and internal (Kronecker) product
-------------------------------------------------------------------------------

--Hall inner product on Lambda: <f,g> = sum_lambda chi_f(lambda) chi_g(lambda) / z_lambda.
--On irreducibles (Schur functions) this counts common constituents: <s_lambda, s_mu> = delta_{lambda,mu}.
scalarProduct = method()
scalarProduct(ClassFunction,ClassFunction) := (ch1,ch2)->
(
     scProd := 0;
     chProd := internalProduct(ch1,ch2);
     for i in keysCF(chProd) do
     	  scProd = scProd + chProd#i / centralizerSize(seqToMults i);
     scProd
     )

scalarProduct(RingElement,RingElement) := (f1,f2)->
(
     ch1 := classFunction f1;
     ch2 := classFunction f2;
     scalarProduct(ch1,ch2)
     )

--Internal (Kronecker) product: pointwise product of class functions on each
--conjugacy class, corresponding under Frobenius to the tensor product of
--S_n-representations. On symmetric functions it is transported through the
--Frobenius isomorphism classFunction / symmetricFunction.
internalProduct = method()
ClassFunction * ClassFunction :=
internalProduct(ClassFunction,ClassFunction) := (ch1,ch2)->
(
     iProd := new MutableHashTable;
     l1 := sum((keysCF ch1)#0);
     l2 := sum((keysCF ch2)#0);
     if l1 == 0 then return(ch1#{} * ch2);
     if l2 == 0 then return(ch2#{} * ch1);
     if l1 != l2 then error("The symmetric functions/characters must have the same degree");
     for i in keysCF(ch1) do
     	  if ch2#?i then iProd#i = ch1#i * ch2#i;
     new ClassFunction from iProd
     )

internalProduct(RingElement,RingElement) := (f1,f2)->
(
     R2 := ring f2;
     R := local R;
     issy := false;
     if (class R2 =!= SchurRing) then issy = true;
     R = symmetricRing ring f2;
     ch1 := classFunction f1;
     ch2 := classFunction f2;
     rez := symmetricFunction(internalProduct(ch1,ch2),R);
     if issy then rez else
     toS rez
     )

-*
chi(BasicList,BasicList) := (lambda, rho) ->
(
     la := toList lambda;
     rh := toList rho;
     ll := sum la;
     if ll != sum(rh) then error"Partitions must have the same size.";
     R := symmetricRing(QQ,ll);
     sl := jacobiTrudi(la,R);
     pr := 1_R;
     for i from 0 to #rh-1 do pr = pr * R_(ll-1+rh#i);
     scalarProduct(sl,pr)
     )
*-
---------------------------------------------------------------
--------------End characters-----------------------------------
---------------------------------------------------------------

--------------------------------
-- Dimension -------------------
--------------------------------
-- Function to compute the dimension of a virtual representation

hooklengths = (lambda) -> (
     mu := conjugate lambda;
     product for i from 0 to #lambda-1 list (
	  product for j from 0 to lambda#i-1 list (
	       lambda#i + mu#j - i - j -1
	       ))
     )

-- Dispatch table for dimension formulas, extensible for Sp/O
dimFormulaTable = new MutableHashTable from {
     "GL" => (n, lambda, powers, base) -> (
     	  if not instance(n,ZZ) then n = hold n;
	  num := product for s from 0 to #powers-1 list (n + (base+s))^(powers#s);
     	  num/hooklengths(new Partition from lambda)
	  ),
     "Sn" => (n, lambda, powers, base) -> (
     	  (sum toList lambda)! / hooklengths(new Partition from lambda)
	  ),
     -- Weyl dimension formula for Sp(2n), type C_n, highest weight lambda
     -- with at most n parts.  Returns 0 if lambda has more than n parts.
     -- Formula:  dim V_lambda
     --   = prod_{1<=i<j<=n} (l_i - l_j)(l_i + l_j) / ((j-i)(2n+2-i-j))
     --   * prod_{1<=i<=n} l_i / (n+1-i)
     -- where l_i = lambda_i + n + 1 - i (pad lambda with zeros to length n).
     "Sp" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "Sp dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  L := for i from 1 to N list ((if i <= r then lambda#(i-1) else 0) + N + 1 - i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       num = num * L#(i-1);
	       den = den * (N + 1 - i);
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (j - i) * (2*N + 2 - i - j);
		    );
	       );
	  num / den
	  ),
     -- Weyl dimension formula for O(2n+1), type B_n.  Positive roots
     -- e_i - e_j, e_i + e_j (1<=i<j<=n) and e_i (1<=i<=n).  rho =
     -- (n-1/2, n-3/2, ..., 1/2) so L_i = 2*lambda_i + 2n+1-2i clears the
     -- half-integers.
     "O_Odd" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "O dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  L := for i from 1 to N list (2*(if i <= r then lambda#(i-1) else 0) + 2*N + 1 - 2*i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       num = num * L#(i-1);
	       den = den * (2*N + 1 - 2*i);
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (2*j - 2*i) * (4*N + 2 - 2*i - 2*j);
		    );
	       );
	  num / den
	  ),
     -- Weyl dimension formula for SO(2n), type D_n.  Positive roots are
     -- e_i +/- e_j (1<=i<j<=n).  rho = (n-1, n-2, ..., 0) is integral, so
     -- l_i = lambda_i + n - i stays integral too.
     --
     -- For partitions with lambda_n > 0 this gives the dimension of a single
     -- SO(2n) irrep; the corresponding O(2n) irrep induced from it has the
     -- same dimension (we parametrize O(2n) irreps by partitions of length
     -- <= n, matching Koike-Terada stable limits).
     "O_Even" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "O dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  if N == 0 then return 1;  -- trivial group
	  L := for i from 1 to N list ((if i <= r then lambda#(i-1) else 0) + N - i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (j - i) * (2*N - i - j);
		    );
	       );
	  num / den
	  ),
     -- Legacy alias: plain "O" defaults to B_n (matches pre-split behavior).
     "O" => (n, lambda, powers, base) -> (
	  if not instance(n,ZZ) then error "O dim formula requires an integer n";
	  r := #lambda;
	  if r > n then return 0;
	  N := n;
	  L := for i from 1 to N list (2*(if i <= r then lambda#(i-1) else 0) + 2*N + 1 - 2*i);
	  num := 1; den := 1;
	  for i from 1 to N do (
	       num = num * L#(i-1);
	       den = den * (2*N + 1 - 2*i);
	       for j from i+1 to N do (
		    num = num * (L#(i-1) - L#(j-1)) * (L#(i-1) + L#(j-1));
		    den = den * (2*j - 2*i) * (4*N + 2 - 2*i - 2*j);
		    );
	       );
	  num / den
	  )
}

dimSchur = method(Options => {GroupActing => "GL"})
dimSchur(Thing,List) := opts -> (n, lambda) -> (
     -- lambda is a list {a0,a1,...,a(r-1)}, a0 >= a1 >= ... >= a(r-1) > 0
     -- n can be a number or a symbol
     powers := new MutableList from toList((if lambda#?0 then lambda#0 else 0) + #lambda - 1 : 0);
     base := 1 - #lambda;
     for i from 0 to #lambda-1 do
       for j from 0 to lambda#i-1 do
       	    powers#(j-i-base) = powers#(j-i-base) + 1;
     ga := opts.GroupActing;
     if not dimFormulaTable#?ga then error("No dimension formula for GroupActing => " | toString ga);
     (dimFormulaTable#ga)(n, lambda, powers, base)
     )
dimSchur(Thing,SchurRingElement) := opts -> (n, F) -> (
     -- assumption: F is an element in a SchurRing of level 1
     if schurLevel(ring F) != 1 then error"Expected a list as input";
     L := listForm F;
     sum apply(L, p -> (
     	       lambda := p#0;
	       p#1 * dimSchur(n,lambda,opts)))
     )

dimSchur(List,SchurRingElement) := opts -> (lis, F) -> (
     -- assumption: F is an element in a SchurRing
     if #lis != schurLevel(ring F) then error"Input list has incorrect size";
     R := ring F;
     gr := R.GroupActing;
     -- Rational GL: the two layers (alpha outer, beta inner) together define
     -- a single GL(n) weight.  We bypass the standard recursion and instead
     -- apply the Weyl dim formula directly to the composite weight.
     if gr == "RatGL" then (
	  nOut := lis#0;
	  nIn  := if #lis >= 2 then lis#1 else nOut;
	  if nOut =!= nIn then
	       error("dimSchur: RatGL requires equal ranks for both layers; got "
		    | toString lis);
	  return dimRatGLList(nOut, F);
	  );
     -- Resolve O into O_Odd / O_Even via the ring's OddOrEven tag.  If the
     -- ring is stable (length-infinity), the choice of tag does not matter
     -- at the ring level but the specialization point n does: dimSchur is
     -- called with a specific n, so we honor whatever tag was set on R
     -- (default "Odd" for stable O).
     if gr == "O" then (
	  if R.?OddOrEven then gr = "O_" | R.OddOrEven
	  else gr = "O_Odd";
	  );
     L := listForm F;
     sum apply(L, p -> (
     	       lambda := p#0;
	       if instance(p#1,SchurRingElement) then dimSchur(drop(lis,1),p#1) * dimSchur(lis#0,lambda,GroupActing => gr)
                  else p#1 * dimSchur(lis#0,lambda,GroupActing => gr)))
     )

dimSchur(SchurRingElement) := opts -> (F) -> (
     schurdims := (S) -> (
	  if schurLevel S === 0 then {}
	  else prepend(numgens S, schurdims coefficientRing S));
     ns := schurdims ring F;
     if any(ns, i -> not instance(i,ZZ))
     then error "expected finitely generated Schur rings";
     dS := dimSchur(ns,F);
     if liftable(dS,ZZ) then lift(dS,ZZ) else dS
     )

-- Weyl dimension formula for GL(n) at a general integral highest weight w =
-- (w_1 >= w_2 >= ... >= w_n).  Works for weights whose parts can be negative
-- (i.e., rational characters of GL(n)), and reduces to the Schur hook-length
-- formula for dominant polynomial lambda when w_n >= 0.
dimGLweight = (n, w) -> (
     if n === 0 then return 1;
     -- Pad w with zeros to length n (dominant, non-increasing assumed).
     if #w > n then error("dimGLweight: weight has more parts than rank n = " | toString n);
     ww := for i from 0 to n-1 list (if i < #w then w#i else 0);
     -- Weyl dim formula: prod_{i<j} ((w_i - w_j) + (j - i)) / (j - i).
     num := 1; den := 1;
     for i from 0 to n-2 do
	  for j from i+1 to n-1 do (
	       num = num * ((ww#i - ww#j) + (j - i));
	       den = den * (j - i);
	       );
     num / den
     )

-- Dimension of a rational GL(n) irrep indexed by (alpha, beta).  Returns 0 if
-- ell(alpha) + ell(beta) > n.
dimRatGLBasis = (n, alpha, beta) -> (
     p := #alpha; q := #beta;
     if p + q > n then return 0;
     w := for i from 1 to n list (
	       (if i <= p then alpha#(i-1) else 0)
	       - (if i > n - q then beta#(n - i) else 0)
	       );
     dimGLweight(n, w)
     )

-- Dimension of a general element F of a RatGL ring at rank n, summed over
-- (alpha, beta, scalar) triples.  Used by dimSchur(List, SchurRingElement)
-- when gr == "RatGL".
dimRatGLList = (n, F) -> (
     acc := 0;
     iterateRatGLTerms(F, (alpha, beta, scalar) -> (
	       d := dimRatGLBasis(n, alpha, beta);
	       acc = acc + scalar * d;
	       ));
     acc
     )
---------------------------------------------------------------
--------End dimension----------------------------------------------
---------------------------------------------------------------


---------------------------------------------------------------
-----------Partitions-related functions------------------------
---------------------------------------------------------------
--this part might have to be moved elsewhere
--since it's not directly connected to the package
parts := (d, n) -> (
     -- d is an integer >= 0
     -- n is an integer >= 1
     -- returns a list of all of the partitions of d
     --    having <= n parts.
     x := partitions(d);
     select(x, xi -> #xi <= n))     

-------Generate all the partitions of a set
-------with a given shape
locS = local locS;
locL = local locL;
locLengthL = local locLengthL;
locParts = local locParts;
locPartitions = local locPartitions;
locind = local locind;
genPartitions = local genPartitions;

genPartitions = method()
genPartitions(ZZ) := (k)->
(
     if k==length locS then (locind = locind + 1;locPartitions#locind = set toList locParts) else
     (
     for i from 0 to locLengthL-1 do
     	  if (i==0 and #locParts#0 < locL#0) or (((locL#(i-1)>locL#i) or (#locParts#(i-1)>0)) and (#locParts#i<locL#i)) then
	  (
	       locParts#i = locParts#i + set{locS#k};
	       genPartitions(k+1);
	       locParts#i = locParts#i - set{locS#k};
	       );
     )
);

partitions(Set,BasicList) := (S,L)->
(
     locS = toList S;
     locL = L;
     locLengthL = #L;
     locParts = new MutableList;
     for i from 0 to locLengthL-1 do locParts#i = set{};
     locPartitions = new MutableList;
     locind = -1;
     genPartitions(0);
     toList locPartitions
     )

--------end generate partitions

---------------------------------------------------------------
--------End partitions-related functions-----------------------
---------------------------------------------------------------

beginDocumentation()

load "./SchurRings/doc.m2"
load "./SchurRings/tests.m2"

end
