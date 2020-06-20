
doc ///
	Key
		"Quotient Lie algebras and subspaces"
	
	Description
		Text
	       	   The most common situation for a Lie algebra L (in this package)
		   is that it is finitely presented, i.e., $L$ is given by
		   a finite number of generators, yielding a free Lie algebra $F$,
		   modulo a finite list of 
		   homogeneous elements in $F$. The ambient Lie algebra of $L$, 
		   see  @TO "ambient(LieAlgebra)"@, is equal to $F$. 
		Example
		   F = lieAlgebra{a,b,c}
		   M = F/{a a b, a a c}
		   L1 = M/{a b c}
		   describe M
		   describe L1
		
		Text
		   There is also the possibility to build quotients by Lie
		   ideals. A Lie ideal is of type {\tt LieIdeal} and may be 
		   constructed in different ways, e.g., as the kernel of a
		   homomorphism. In general, Lie ideals are not finitely 
		   generated (or not known to be, as $J$ below), 
		   but a finitely generated Lie ideal may be formed
		   using the constructor @TO lieIdeal@. Building a quotient
		   by a finitely generated ideal is the same as above, taking 
		   the Lie algebra modulo the generators of the ideal.
		Example
		   F = lieAlgebra{a,b,c}
		   I = lieIdeal{a a b,a a c,a b c}
		   L2=F/I
		   describe L2
		   L1==L2
	        Text
		   The Lie algebra $L3$ below is a quotient of the finitely 
		   presented Lie algebra $M$ by the ideal $J$, which is not known
		   to be finitely generated. The ambient Lie algebra of $L3$ 
		   is $M$ and 
		   {\tt ideal(L3)} is $J$. The Lie algebras $L2$ and $L3$ 
		   are isomorphic,
		   but are presented in different ways.
		Example		 
		   f = map(L1,M) 
		   J = kernel f
		   L3 = M/J
		   describe L3
		   dims(1,6,L2)
		   dims(1,6,L3)
		Text
		   If two quotients by Lie ideals are performed successively, 
		   then
		   the program converts the final result to a quotient of the first
		   Lie algebra by a singel ideal. 
		   In 
		   the example below, $L5=(M/J)/K$ and this is transformed to 
		   $M/P$, where $P$ is the inverse image of $K$ under 
		   the natural map 
		   $M \ \to\  M/J$. 
		Example
		   L4 = L3/{a b,a c}
		   g = map(L4,L3)
		   K = kernel g
		   L5 = L3/K
		   ambient L5
		   ideal L5===inverse(map(L3,M),K)
		Text
		   If a quotient by a Lie ideal that is not known
		   to be finitely generated is followed by a quotient 
		   with finitely many generators, then the programs converts
		   it by changing the order of the operations. In the example 
		   below, {\tt L6=(M/J)/\{a b\}} and this is transformed to 
		   {\tt (M/\{a b\})/Q}, where $Q$ is the image of $J$ under the natural
		   map $M \ \to\  M/\{a b\}$\ (this in fact is an ideal since the map
		   is surjective).
		   
		Example
		   L6 = (M/J)/{a b}
		   L7 = ambient L6
		   use M
		   L7 == M/{a b}
		   Q = image(map(L7,M),J)
		   ideal L6===new LieIdeal from Q
		   
		   
		   
		Text
		   It may also
		   happen that L has a non-zero differential, see 
		   @TO differentialLieAlgebra@. The differential is given as 
		   the list {\tt diff(L)} of elements
		   in $F$ that consists of the values of the differential on 
		   the generators of $F$, see  @TO "diff(LieAlgebra)"@. 
		   Note that {\tt ideal(D)} (shown below) 
		   has been produced 
		   by the program to get the square of the differential to 
		   be zero. The extra {\tt - (b b a)} in {\tt ideal(L)} 
		   below is added by 
		   the program to ensure that the ideal generated by {\tt b c2} 
		   is invariant under the differential.   	
		
		Example
		   F = lieAlgebra({a,b,c2,c3,c4},Signs=>{0,0,1,0,1},
                             Weights => {{1,0},{1,0},{2,1},{3,2},{5,3}},
		             LastWeightHomological=>true)
	           D=differentialLieAlgebra{0_F,0_F,a b,a c2,a b c3}
		   describe D
		   L=D/{b c2}
		   describe L
		Text
		   In additon to the constructor @TO lieIdeal@ there are also 
		   the constructors @TO lieSubAlgebra@ and @TO lieSubSpace@ 
		   yielding finitely generated Lie subalgebras and finitely 
		   generated subspaces respectively. 
		Example
		   L = lieAlgebra{a,b,c}
		   A = lieSubAlgebra{a,b c}
		   basis(4,A)
		   S=lieSubSpace{a,b c}
		   dims(1,4,S)
		Text
		   Ideals, subalgebras and subspaces are both inputs and
		   possible outputs of several methods. The methods 
		   @TO "image(LieAlgebraMap,LieSubSpace)"@ and
		   @TO "inverse(LieAlgebraMap,LieSubSpace)"@, which
		   are used above, have image and kernel of a 
		   Lie algebra map or derivation as
		   special cases.  The method 
		   @TO "quotient(LieIdeal,FGLieSubAlgebra)"@ has
		   @TO "annihilator(FGLieSubAlgebra)"@ and 
		   @TO center@ as special cases. 
		Example
		   L = lieAlgebra{a,b,c}
		   I = lieIdeal{a a c+b a c-a b a,c c a-b b a }
		   M = L/I
		   J=lieIdeal{a b}
		   A = quotient(J,lieSubAlgebra{a c})
		   dims(1,3,A)		   
		   basis(2,A) 
		   member((c b) (a c),J)
		Text
		   One may also form the sum, 
		   @TO (symbol +,LieSubSpace,LieSubSpace)@,
		   and intersection,
		   @TO (symbol \@,LieSubSpace,LieSubSpace)@,
		    of 
		   two Lie subspaces (in particular 
		   subalgebras or ideals).
	        Example
		   L = lieAlgebra{a,b,c}
		   I = lieIdeal{a b}
		   J = lieIdeal{b c}
		   T = I+J
		   U = I@J
		   dims(1,5,T)
		   dims(1,5,U)
		   2*dims(1,5,I)
		Text
		   Finally, the methods @TO boundaries@ and 
		   @TO cycles@ give the subalgebras 
		   {\tt image(d)} and {\tt kernel(d)} 
		    respectively, where $d$ is the differential, while
		     @TO lieHomology@ gives the homology as a vector 
		   space.
		   
		   
		   
		
		      
		   
		   
		
			
		
						
	SeeAlso		    		        
		 "Second Lie algebra tutorial"
		 "Differential Lie algebra tutorial"	      
		 "Homomorphisms and derivations"
		 
				       	 
///
end			
	
 