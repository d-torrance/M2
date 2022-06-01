--This file computes Betti tables for P^1 for d = 9 and b = 7
A := degreesRing 2
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(7,0) => 36, (6,1) => 0, (7,1) => 0, (8,0) => 0, (8,1) => 1, (0,0) => 8, (0,1) => 0, (1,0) => 63, (2,0) => 216, (1,1) => 0, (2,1) => 0, (3,0) => 420, (4,0) => 504, (3,1) => 0, (5,0) => 378, (4,1) => 0, (5,1) => 0, (6,0) => 168},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(7,0) => A_0^42*A_1^28+A_0^41*A_1^29+2*A_0^40*A_1^30+2*A_0^39*A_1^31+3*A_0^38*A_1^32+3*A_0^37*A_1^33+4*A_0^36*A_1^34+4*A_0^35*A_1^35+4*A_0^34*A_1^36+3*A_0^33*A_1^37+3*A_0^32*A_1^38+2*A_0^31*A_1^39+2*A_0^30*A_1^40+A_0^29*A_1^41+A_0^28*A_1^42, (6,1) => 0, (8,0) => 0, (7,1) => 0, (8,1) => A_0^44*A_1^44, (0,0) => A_0^7+A_0^6*A_1+A_0^5*A_1^2+A_0^4*A_1^3+A_0^3*A_1^4+A_0^2*A_1^5+A_0*A_1^6+A_1^7, (0,1) => 0, (1,0) => A_0^15*A_1+2*A_0^14*A_1^2+3*A_0^13*A_1^3+4*A_0^12*A_1^4+5*A_0^11*A_1^5+6*A_0^10*A_1^6+7*A_0^9*A_1^7+7*A_0^8*A_1^8+7*A_0^7*A_1^9+6*A_0^6*A_1^10+5*A_0^5*A_1^11+4*A_0^4*A_1^12+3*A_0^3*A_1^13+2*A_0^2*A_1^14+A_0*A_1^15, (2,0) => A_0^22*A_1^3+2*A_0^21*A_1^4+4*A_0^20*A_1^5+6*A_0^19*A_1^6+9*A_0^18*A_1^7+12*A_0^17*A_1^8+15*A_0^16*A_1^9+18*A_0^15*A_1^10+20*A_0^14*A_1^11+21*A_0^13*A_1^12+21*A_0^12*A_1^13+20*A_0^11*A_1^14+18*A_0^10*A_1^15+15*A_0^9*A_1^16+12*A_0^8*A_1^17+9*A_0^7*A_1^18+6*A_0^6*A_1^19+4*A_0^5*A_1^20+2*A_0^4*A_1^21+A_0^3*A_1^22, (1,1) => 0, (2,1) => 0, (3,0) => A_0^28*A_1^6+2*A_0^27*A_1^7+4*A_0^26*A_1^8+7*A_0^25*A_1^9+11*A_0^24*A_1^10+15*A_0^23*A_1^11+21*A_0^22*A_1^12+26*A_0^21*A_1^13+31*A_0^20*A_1^14+35*A_0^19*A_1^15+38*A_0^18*A_1^16+38*A_0^17*A_1^17+38*A_0^16*A_1^18+35*A_0^15*A_1^19+31*A_0^14*A_1^20+26*A_0^13*A_1^21+21*A_0^12*A_1^22+15*A_0^11*A_1^23+11*A_0^10*A_1^24+7*A_0^9*A_1^25+4*A_0^8*A_1^26+2*A_0^7*A_1^27+A_0^6*A_1^28, (3,1) => 0, (4,0) => A_0^33*A_1^10+2*A_0^32*A_1^11+4*A_0^31*A_1^12+7*A_0^30*A_1^13+11*A_0^29*A_1^14+16*A_0^28*A_1^15+22*A_0^27*A_1^16+28*A_0^26*A_1^17+34*A_0^25*A_1^18+39*A_0^24*A_1^19+43*A_0^23*A_1^20+45*A_0^22*A_1^21+45*A_0^21*A_1^22+43*A_0^20*A_1^23+39*A_0^19*A_1^24+34*A_0^18*A_1^25+28*A_0^17*A_1^26+22*A_0^16*A_1^27+16*A_0^15*A_1^28+11*A_0^14*A_1^29+7*A_0^13*A_1^30+4*A_0^12*A_1^31+2*A_0^11*A_1^32+A_0^10*A_1^33, (4,1) => 0, (5,0) => A_0^37*A_1^15+2*A_0^36*A_1^16+4*A_0^35*A_1^17+6*A_0^34*A_1^18+10*A_0^33*A_1^19+14*A_0^32*A_1^20+19*A_0^31*A_1^21+23*A_0^30*A_1^22+28*A_0^29*A_1^23+31*A_0^28*A_1^24+34*A_0^27*A_1^25+34*A_0^26*A_1^26+34*A_0^25*A_1^27+31*A_0^24*A_1^28+28*A_0^23*A_1^29+23*A_0^22*A_1^30+19*A_0^21*A_1^31+14*A_0^20*A_1^32+10*A_0^19*A_1^33+6*A_0^18*A_1^34+4*A_0^17*A_1^35+2*A_0^16*A_1^36+A_0^15*A_1^37, (6,0) => A_0^40*A_1^21+2*A_0^39*A_1^22+3*A_0^38*A_1^23+5*A_0^37*A_1^24+7*A_0^36*A_1^25+9*A_0^35*A_1^26+12*A_0^34*A_1^27+14*A_0^33*A_1^28+15*A_0^32*A_1^29+16*A_0^31*A_1^30+16*A_0^30*A_1^31+15*A_0^29*A_1^32+14*A_0^28*A_1^33+12*A_0^27*A_1^34+9*A_0^26*A_1^35+7*A_0^25*A_1^36+5*A_0^24*A_1^37+3*A_0^23*A_1^38+2*A_0^22*A_1^39+A_0^21*A_1^40, (5,1) => 0},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(7,0) => {({42,28},1)}, (6,1) => {}, (7,1) => {}, (8,0) => {}, (8,1) => {({44,44},1)}, (0,0) => {({7,0},1)}, (0,1) => {}, (1,0) => {({15,1},1)}, (2,0) => {({22,3},1)}, (1,1) => {}, (2,1) => {}, (3,0) => {({28,6},1)}, (4,0) => {({33,10},1)}, (3,1) => {}, (5,0) => {({37,15},1)}, (4,1) => {}, (5,1) => {}, (6,0) => {({40,21},1)}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(7,0) => {{42,28}}, (6,1) => {}, (7,1) => {}, (8,0) => {}, (8,1) => {{44,44}}, (0,0) => {{7,0}}, (0,1) => {}, (1,0) => {{15,1}}, (2,0) => {{22,3}}, (1,1) => {}, (2,1) => {}, (3,0) => {{28,6}}, (4,0) => {{33,10}}, (3,1) => {}, (5,0) => {{37,15}}, (4,1) => {}, (5,1) => {}, (6,0) => {{40,21}}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(7,0) => {42,28}, (6,1) => {}, (7,1) => {}, (8,0) => {}, (8,1) => {44,44}, (0,0) => {7,0}, (0,1) => {}, (1,0) => {15,1}, (2,0) => {22,3}, (1,1) => {}, (2,1) => {}, (3,0) => {28,6}, (4,0) => {33,10}, (3,1) => {}, (5,0) => {37,15}, (4,1) => {}, (5,1) => {}, (6,0) => {40,21}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(7,0) => 1, (6,1) => 0, (7,1) => 0, (8,0) => 0, (8,1) => 1, (0,0) => 1, (0,1) => 0, (1,0) => 1, (2,0) => 1, (1,1) => 0, (2,1) => 0, (3,0) => 1, (4,0) => 1, (3,1) => 0, (5,0) => 1, (4,1) => 0, (5,1) => 0, (6,0) => 1},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(7,0) => 1, (6,1) => 0, (7,1) => 0, (8,0) => 0, (8,1) => 1, (0,0) => 1, (0,1) => 0, (1,0) => 1, (2,0) => 1, (1,1) => 0, (2,1) => 0, (3,0) => 1, (4,0) => 1, (3,1) => 0, (5,0) => 1, (4,1) => 0, (5,1) => 0, (6,0) => 1},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(7,0) => 36, (6,1) => 0, (7,1) => 0, (8,0) => 0, (8,1) => 1, (0,0) => 8, (0,1) => 0, (1,0) => 63, (2,0) => 216, (1,1) => 0, (2,1) => 0, (3,0) => 420, (4,0) => 504, (3,1) => 0, (5,0) => 378, (4,1) => 0, (5,1) => 0, (6,0) => 168},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{362880/1},
}
