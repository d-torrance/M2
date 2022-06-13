--This file computes Betti tables for P^1 for d = 9 and b = 4
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(7,0) => 0, (6,1) => 72, (7,1) => 27, (8,0) => 0, (8,1) => 4, (0,0) => 5, (0,1) => 0, (1,0) => 36, (2,0) => 108, (1,1) => 0, (2,1) => 0, (3,0) => 168, (4,0) => 126, (3,1) => 0, (5,0) => 0, (4,1) => 0, (5,1) => 84, (6,0) => 0},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(7,0) => 0, (6,1) => A_0^41*A_1^26+2*A_0^40*A_1^27+3*A_0^39*A_1^28+4*A_0^38*A_1^29+5*A_0^37*A_1^30+6*A_0^36*A_1^31+7*A_0^35*A_1^32+8*A_0^34*A_1^33+8*A_0^33*A_1^34+7*A_0^32*A_1^35+6*A_0^31*A_1^36+5*A_0^30*A_1^37+4*A_0^29*A_1^38+3*A_0^28*A_1^39+2*A_0^27*A_1^40+A_0^26*A_1^41, (8,0) => 0, (7,1) => A_0^43*A_1^33+2*A_0^42*A_1^34+3*A_0^41*A_1^35+3*A_0^40*A_1^36+3*A_0^39*A_1^37+3*A_0^38*A_1^38+3*A_0^37*A_1^39+3*A_0^36*A_1^40+3*A_0^35*A_1^41+2*A_0^34*A_1^42+A_0^33*A_1^43, (8,1) => A_0^44*A_1^41+A_0^43*A_1^42+A_0^42*A_1^43+A_0^41*A_1^44, (0,0) => A_0^4+A_0^3*A_1+A_0^2*A_1^2+A_0*A_1^3+A_1^4, (0,1) => 0, (1,0) => A_0^12*A_1+2*A_0^11*A_1^2+3*A_0^10*A_1^3+4*A_0^9*A_1^4+4*A_0^8*A_1^5+4*A_0^7*A_1^6+4*A_0^6*A_1^7+4*A_0^5*A_1^8+4*A_0^4*A_1^9+3*A_0^3*A_1^10+2*A_0^2*A_1^11+A_0*A_1^12, (2,0) => A_0^19*A_1^3+2*A_0^18*A_1^4+4*A_0^17*A_1^5+5*A_0^16*A_1^6+7*A_0^15*A_1^7+8*A_0^14*A_1^8+10*A_0^13*A_1^9+11*A_0^12*A_1^10+12*A_0^11*A_1^11+11*A_0^10*A_1^12+10*A_0^9*A_1^13+8*A_0^8*A_1^14+7*A_0^7*A_1^15+5*A_0^6*A_1^16+4*A_0^5*A_1^17+2*A_0^4*A_1^18+A_0^3*A_1^19, (1,1) => 0, (2,1) => 0, (3,0) => A_0^25*A_1^6+2*A_0^24*A_1^7+3*A_0^23*A_1^8+5*A_0^22*A_1^9+7*A_0^21*A_1^10+9*A_0^20*A_1^11+12*A_0^19*A_1^12+14*A_0^18*A_1^13+15*A_0^17*A_1^14+16*A_0^16*A_1^15+16*A_0^15*A_1^16+15*A_0^14*A_1^17+14*A_0^13*A_1^18+12*A_0^12*A_1^19+9*A_0^11*A_1^20+7*A_0^10*A_1^21+5*A_0^9*A_1^22+3*A_0^8*A_1^23+2*A_0^7*A_1^24+A_0^6*A_1^25, (3,1) => 0, (4,0) => A_0^30*A_1^10+A_0^29*A_1^11+2*A_0^28*A_1^12+3*A_0^27*A_1^13+5*A_0^26*A_1^14+6*A_0^25*A_1^15+8*A_0^24*A_1^16+9*A_0^23*A_1^17+11*A_0^22*A_1^18+11*A_0^21*A_1^19+12*A_0^20*A_1^20+11*A_0^19*A_1^21+11*A_0^18*A_1^22+9*A_0^17*A_1^23+8*A_0^16*A_1^24+6*A_0^15*A_1^25+5*A_0^14*A_1^26+3*A_0^13*A_1^27+2*A_0^12*A_1^28+A_0^11*A_1^29+A_0^10*A_1^30, (4,1) => 0, (5,0) => 0, (6,0) => 0, (5,1) => A_0^38*A_1^20+A_0^37*A_1^21+2*A_0^36*A_1^22+3*A_0^35*A_1^23+4*A_0^34*A_1^24+5*A_0^33*A_1^25+7*A_0^32*A_1^26+7*A_0^31*A_1^27+8*A_0^30*A_1^28+8*A_0^29*A_1^29+8*A_0^28*A_1^30+7*A_0^27*A_1^31+7*A_0^26*A_1^32+5*A_0^25*A_1^33+4*A_0^24*A_1^34+3*A_0^23*A_1^35+2*A_0^22*A_1^36+A_0^21*A_1^37+A_0^20*A_1^38},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(7,0) => {}, (6,1) => {({41,26},1)}, (7,1) => {({43,33},1)}, (8,0) => {}, (8,1) => {({44,41},1)}, (0,0) => {({4,0},1)}, (0,1) => {}, (1,0) => {({12,1},1)}, (2,0) => {({19,3},1)}, (1,1) => {}, (2,1) => {}, (3,0) => {({25,6},1)}, (4,0) => {({30,10},1)}, (3,1) => {}, (5,0) => {}, (4,1) => {}, (5,1) => {({38,20},1)}, (6,0) => {}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(7,0) => {}, (6,1) => {{41,26}}, (7,1) => {{43,33}}, (8,0) => {}, (8,1) => {{44,41}}, (0,0) => {{4,0}}, (0,1) => {}, (1,0) => {{12,1}}, (2,0) => {{19,3}}, (1,1) => {}, (2,1) => {}, (3,0) => {{25,6}}, (4,0) => {{30,10}}, (3,1) => {}, (5,0) => {}, (4,1) => {}, (5,1) => {{38,20}}, (6,0) => {}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(7,0) => {}, (6,1) => {41,26}, (7,1) => {43,33}, (8,0) => {}, (8,1) => {44,41}, (0,0) => {4,0}, (0,1) => {}, (1,0) => {12,1}, (2,0) => {19,3}, (1,1) => {}, (2,1) => {}, (3,0) => {25,6}, (4,0) => {30,10}, (3,1) => {}, (5,0) => {}, (4,1) => {}, (5,1) => {38,20}, (6,0) => {}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(7,0) => 0, (6,1) => 1, (7,1) => 1, (8,0) => 0, (8,1) => 1, (0,0) => 1, (0,1) => 0, (1,0) => 1, (2,0) => 1, (1,1) => 0, (2,1) => 0, (3,0) => 1, (4,0) => 1, (3,1) => 0, (5,0) => 0, (4,1) => 0, (5,1) => 1, (6,0) => 0},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(7,0) => 0, (6,1) => 1, (7,1) => 1, (8,0) => 0, (8,1) => 1, (0,0) => 1, (0,1) => 0, (1,0) => 1, (2,0) => 1, (1,1) => 0, (2,1) => 0, (3,0) => 1, (4,0) => 1, (3,1) => 0, (5,0) => 0, (4,1) => 0, (5,1) => 1, (6,0) => 0},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(7,0) => 0, (6,1) => 72, (7,1) => 27, (8,0) => 0, (8,1) => 4, (0,0) => 5, (0,1) => 0, (1,0) => 36, (2,0) => 108, (1,1) => 0, (2,1) => 0, (3,0) => 168, (4,0) => 126, (3,1) => 0, (5,0) => 0, (4,1) => 0, (5,1) => 84, (6,0) => 0},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{362880/1},
}