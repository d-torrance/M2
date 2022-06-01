--This file computes Betti tables for P^1 for d = 9 and b = 2
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(7,0) => 0, (6,1) => 144, (7,1) => 45, (8,0) => 0, (8,1) => 6, (0,0) => 3, (0,1) => 0, (1,0) => 18, (2,0) => 36, (1,1) => 0, (2,1) => 0, (3,0) => 0, (4,0) => 0, (3,1) => 126, (5,0) => 0, (4,1) => 252, (5,1) => 252, (6,0) => 0},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(7,0) => 0, (6,1) => A_0^41*A_1^24+2*A_0^40*A_1^25+4*A_0^39*A_1^26+6*A_0^38*A_1^27+8*A_0^37*A_1^28+10*A_0^36*A_1^29+12*A_0^35*A_1^30+14*A_0^34*A_1^31+15*A_0^33*A_1^32+15*A_0^32*A_1^33+14*A_0^31*A_1^34+12*A_0^30*A_1^35+10*A_0^29*A_1^36+8*A_0^28*A_1^37+6*A_0^27*A_1^38+4*A_0^26*A_1^39+2*A_0^25*A_1^40+A_0^24*A_1^41, (8,0) => 0, (7,1) => A_0^43*A_1^31+2*A_0^42*A_1^32+3*A_0^41*A_1^33+4*A_0^40*A_1^34+5*A_0^39*A_1^35+5*A_0^38*A_1^36+5*A_0^37*A_1^37+5*A_0^36*A_1^38+5*A_0^35*A_1^39+4*A_0^34*A_1^40+3*A_0^33*A_1^41+2*A_0^32*A_1^42+A_0^31*A_1^43, (8,1) => A_0^44*A_1^39+A_0^43*A_1^40+A_0^42*A_1^41+A_0^41*A_1^42+A_0^40*A_1^43+A_0^39*A_1^44, (0,0) => A_0^2+A_0*A_1+A_1^2, (0,1) => 0, (1,0) => A_0^10*A_1+2*A_0^9*A_1^2+2*A_0^8*A_1^3+2*A_0^7*A_1^4+2*A_0^6*A_1^5+2*A_0^5*A_1^6+2*A_0^4*A_1^7+2*A_0^3*A_1^8+2*A_0^2*A_1^9+A_0*A_1^10, (2,0) => A_0^17*A_1^3+A_0^16*A_1^4+2*A_0^15*A_1^5+2*A_0^14*A_1^6+3*A_0^13*A_1^7+3*A_0^12*A_1^8+4*A_0^11*A_1^9+4*A_0^10*A_1^10+4*A_0^9*A_1^11+3*A_0^8*A_1^12+3*A_0^7*A_1^13+2*A_0^6*A_1^14+2*A_0^5*A_1^15+A_0^4*A_1^16+A_0^3*A_1^17, (1,1) => 0, (2,1) => 0, (3,0) => 0, (3,1) => A_0^29*A_1^9+A_0^28*A_1^10+2*A_0^27*A_1^11+3*A_0^26*A_1^12+5*A_0^25*A_1^13+6*A_0^24*A_1^14+8*A_0^23*A_1^15+9*A_0^22*A_1^16+11*A_0^21*A_1^17+11*A_0^20*A_1^18+12*A_0^19*A_1^19+11*A_0^18*A_1^20+11*A_0^17*A_1^21+9*A_0^16*A_1^22+8*A_0^15*A_1^23+6*A_0^14*A_1^24+5*A_0^13*A_1^25+3*A_0^12*A_1^26+2*A_0^11*A_1^27+A_0^10*A_1^28+A_0^9*A_1^29, (4,0) => 0, (4,1) => A_0^34*A_1^13+2*A_0^33*A_1^14+3*A_0^32*A_1^15+5*A_0^31*A_1^16+8*A_0^30*A_1^17+11*A_0^29*A_1^18+14*A_0^28*A_1^19+17*A_0^27*A_1^20+20*A_0^26*A_1^21+22*A_0^25*A_1^22+23*A_0^24*A_1^23+23*A_0^23*A_1^24+22*A_0^22*A_1^25+20*A_0^21*A_1^26+17*A_0^20*A_1^27+14*A_0^19*A_1^28+11*A_0^18*A_1^29+8*A_0^17*A_1^30+5*A_0^16*A_1^31+3*A_0^15*A_1^32+2*A_0^14*A_1^33+A_0^13*A_1^34, (5,0) => 0, (6,0) => 0, (5,1) => A_0^38*A_1^18+2*A_0^37*A_1^19+4*A_0^36*A_1^20+6*A_0^35*A_1^21+9*A_0^34*A_1^22+12*A_0^33*A_1^23+16*A_0^32*A_1^24+19*A_0^31*A_1^25+22*A_0^30*A_1^26+23*A_0^29*A_1^27+24*A_0^28*A_1^28+23*A_0^27*A_1^29+22*A_0^26*A_1^30+19*A_0^25*A_1^31+16*A_0^24*A_1^32+12*A_0^23*A_1^33+9*A_0^22*A_1^34+6*A_0^21*A_1^35+4*A_0^20*A_1^36+2*A_0^19*A_1^37+A_0^18*A_1^38},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(7,0) => {}, (6,1) => {({41,24},1)}, (7,1) => {({43,31},1)}, (8,0) => {}, (8,1) => {({44,39},1)}, (0,0) => {({2,0},1)}, (0,1) => {}, (1,0) => {({10,1},1)}, (2,0) => {({17,3},1)}, (1,1) => {}, (2,1) => {}, (3,0) => {}, (4,0) => {}, (3,1) => {({29,9},1)}, (5,0) => {}, (4,1) => {({34,13},1)}, (5,1) => {({38,18},1)}, (6,0) => {}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(7,0) => {}, (6,1) => {{41,24}}, (7,1) => {{43,31}}, (8,0) => {}, (8,1) => {{44,39}}, (0,0) => {{2,0}}, (0,1) => {}, (1,0) => {{10,1}}, (2,0) => {{17,3}}, (1,1) => {}, (2,1) => {}, (3,0) => {}, (4,0) => {}, (3,1) => {{29,9}}, (5,0) => {}, (4,1) => {{34,13}}, (5,1) => {{38,18}}, (6,0) => {}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(7,0) => {}, (6,1) => {41,24}, (7,1) => {43,31}, (8,0) => {}, (8,1) => {44,39}, (0,0) => {2,0}, (0,1) => {}, (1,0) => {10,1}, (2,0) => {17,3}, (1,1) => {}, (2,1) => {}, (3,0) => {}, (4,0) => {}, (3,1) => {29,9}, (5,0) => {}, (4,1) => {34,13}, (5,1) => {38,18}, (6,0) => {}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(7,0) => 0, (6,1) => 1, (7,1) => 1, (8,0) => 0, (8,1) => 1, (0,0) => 1, (0,1) => 0, (1,0) => 1, (2,0) => 1, (1,1) => 0, (2,1) => 0, (3,0) => 0, (4,0) => 0, (3,1) => 1, (5,0) => 0, (4,1) => 1, (5,1) => 1, (6,0) => 0},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(7,0) => 0, (6,1) => 1, (7,1) => 1, (8,0) => 0, (8,1) => 1, (0,0) => 1, (0,1) => 0, (1,0) => 1, (2,0) => 1, (1,1) => 0, (2,1) => 0, (3,0) => 0, (4,0) => 0, (3,1) => 1, (5,0) => 0, (4,1) => 1, (5,1) => 1, (6,0) => 0},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(7,0) => 0, (6,1) => 144, (7,1) => 45, (8,0) => 0, (8,1) => 6, (0,0) => 3, (0,1) => 0, (1,0) => 18, (2,0) => 36, (1,1) => 0, (2,1) => 0, (3,0) => 0, (4,0) => 0, (3,1) => 126, (5,0) => 0, (4,1) => 252, (5,1) => 252, (6,0) => 0},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{362880/1},
}
