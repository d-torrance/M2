--This file computes Betti tables for P^2 for d = 3 and b = 1
A := degreesRing 3
new HashTable from {
--tb stands for Total Betti numbers
"tb"=>new HashTable from {(7,0) => 0, (6,1) => 39, (5,2) => 0, (6,2) => 0, (7,1) => 6, (7,2) => 0, (0,0) => 3, (1,0) => 15, (0,1) => 0, (1,1) => 0, (0,2) => 0, (2,0) => 21, (3,0) => 0, (2,1) => 21, (1,2) => 0, (3,1) => 105, (2,2) => 0, (4,0) => 0, (3,2) => 0, (4,1) => 147, (5,0) => 0, (4,2) => 0, (5,1) => 105, (6,0) => 0},
--mb stands for Multigraded Betti numbers
"mb"=>new HashTable from {(7,0) => 0, (6,1) => A_0^9*A_1^8*A_2^5+A_0^8*A_1^9*A_2^5+2*A_0^9*A_1^7*A_2^6+3*A_0^8*A_1^8*A_2^6+2*A_0^7*A_1^9*A_2^6+2*A_0^9*A_1^6*A_2^7+4*A_0^8*A_1^7*A_2^7+4*A_0^7*A_1^8*A_2^7+2*A_0^6*A_1^9*A_2^7+A_0^9*A_1^5*A_2^8+3*A_0^8*A_1^6*A_2^8+4*A_0^7*A_1^7*A_2^8+3*A_0^6*A_1^8*A_2^8+A_0^5*A_1^9*A_2^8+A_0^8*A_1^5*A_2^9+2*A_0^7*A_1^6*A_2^9+2*A_0^6*A_1^7*A_2^9+A_0^5*A_1^8*A_2^9, (5,2) => 0, (7,1) => A_0^9*A_1^9*A_2^7+A_0^9*A_1^8*A_2^8+A_0^8*A_1^9*A_2^8+A_0^9*A_1^7*A_2^9+A_0^8*A_1^8*A_2^9+A_0^7*A_1^9*A_2^9, (6,2) => 0, (7,2) => 0, (0,0) => A_0+A_1+A_2, (1,0) => A_0^3*A_1+A_0^2*A_1^2+A_0*A_1^3+A_0^3*A_2+2*A_0^2*A_1*A_2+2*A_0*A_1^2*A_2+A_1^3*A_2+A_0^2*A_2^2+2*A_0*A_1*A_2^2+A_1^2*A_2^2+A_0*A_2^3+A_1*A_2^3, (0,1) => 0, (1,1) => 0, (0,2) => 0, (2,0) => A_0^5*A_1*A_2+A_0^4*A_1^2*A_2+2*A_0^3*A_1^3*A_2+A_0^2*A_1^4*A_2+A_0*A_1^5*A_2+A_0^4*A_1*A_2^2+2*A_0^3*A_1^2*A_2^2+2*A_0^2*A_1^3*A_2^2+A_0*A_1^4*A_2^2+2*A_0^3*A_1*A_2^3+2*A_0^2*A_1^2*A_2^3+2*A_0*A_1^3*A_2^3+A_0^2*A_1*A_2^4+A_0*A_1^2*A_2^4+A_0*A_1*A_2^5, (1,2) => 0, (2,1) => A_0^5*A_1^5+A_0^5*A_1^4*A_2+A_0^4*A_1^5*A_2+A_0^5*A_1^3*A_2^2+A_0^4*A_1^4*A_2^2+A_0^3*A_1^5*A_2^2+A_0^5*A_1^2*A_2^3+A_0^4*A_1^3*A_2^3+A_0^3*A_1^4*A_2^3+A_0^2*A_1^5*A_2^3+A_0^5*A_1*A_2^4+A_0^4*A_1^2*A_2^4+A_0^3*A_1^3*A_2^4+A_0^2*A_1^4*A_2^4+A_0*A_1^5*A_2^4+A_0^5*A_2^5+A_0^4*A_1*A_2^5+A_0^3*A_1^2*A_2^5+A_0^2*A_1^3*A_2^5+A_0*A_1^4*A_2^5+A_1^5*A_2^5, (3,0) => 0, (4,0) => 0, (2,2) => 0, (3,1) => A_0^7*A_1^5*A_2+A_0^6*A_1^6*A_2+A_0^5*A_1^7*A_2+A_0^7*A_1^4*A_2^2+3*A_0^6*A_1^5*A_2^2+3*A_0^5*A_1^6*A_2^2+A_0^4*A_1^7*A_2^2+2*A_0^7*A_1^3*A_2^3+4*A_0^6*A_1^4*A_2^3+7*A_0^5*A_1^5*A_2^3+4*A_0^4*A_1^6*A_2^3+2*A_0^3*A_1^7*A_2^3+A_0^7*A_1^2*A_2^4+4*A_0^6*A_1^3*A_2^4+7*A_0^5*A_1^4*A_2^4+7*A_0^4*A_1^5*A_2^4+4*A_0^3*A_1^6*A_2^4+A_0^2*A_1^7*A_2^4+A_0^7*A_1*A_2^5+3*A_0^6*A_1^2*A_2^5+7*A_0^5*A_1^3*A_2^5+7*A_0^4*A_1^4*A_2^5+7*A_0^3*A_1^5*A_2^5+3*A_0^2*A_1^6*A_2^5+A_0*A_1^7*A_2^5+A_0^6*A_1*A_2^6+3*A_0^5*A_1^2*A_2^6+4*A_0^4*A_1^3*A_2^6+4*A_0^3*A_1^4*A_2^6+3*A_0^2*A_1^5*A_2^6+A_0*A_1^6*A_2^6+A_0^5*A_1*A_2^7+A_0^4*A_1^2*A_2^7+2*A_0^3*A_1^3*A_2^7+A_0^2*A_1^4*A_2^7+A_0*A_1^5*A_2^7, (5,0) => 0, (4,1) => A_0^8*A_1^6*A_2^2+A_0^7*A_1^7*A_2^2+A_0^6*A_1^8*A_2^2+2*A_0^8*A_1^5*A_2^3+4*A_0^7*A_1^6*A_2^3+4*A_0^6*A_1^7*A_2^3+2*A_0^5*A_1^8*A_2^3+2*A_0^8*A_1^4*A_2^4+6*A_0^7*A_1^5*A_2^4+9*A_0^6*A_1^6*A_2^4+6*A_0^5*A_1^7*A_2^4+2*A_0^4*A_1^8*A_2^4+2*A_0^8*A_1^3*A_2^5+6*A_0^7*A_1^4*A_2^5+11*A_0^6*A_1^5*A_2^5+11*A_0^5*A_1^6*A_2^5+6*A_0^4*A_1^7*A_2^5+2*A_0^3*A_1^8*A_2^5+A_0^8*A_1^2*A_2^6+4*A_0^7*A_1^3*A_2^6+9*A_0^6*A_1^4*A_2^6+11*A_0^5*A_1^5*A_2^6+9*A_0^4*A_1^6*A_2^6+4*A_0^3*A_1^7*A_2^6+A_0^2*A_1^8*A_2^6+A_0^7*A_1^2*A_2^7+4*A_0^6*A_1^3*A_2^7+6*A_0^5*A_1^4*A_2^7+6*A_0^4*A_1^5*A_2^7+4*A_0^3*A_1^6*A_2^7+A_0^2*A_1^7*A_2^7+A_0^6*A_1^2*A_2^8+2*A_0^5*A_1^3*A_2^8+2*A_0^4*A_1^4*A_2^8+2*A_0^3*A_1^5*A_2^8+A_0^2*A_1^6*A_2^8, (3,2) => 0, (6,0) => 0, (5,1) => A_0^8*A_1^8*A_2^3+A_0^9*A_1^6*A_2^4+3*A_0^8*A_1^7*A_2^4+3*A_0^7*A_1^8*A_2^4+A_0^6*A_1^9*A_2^4+A_0^9*A_1^5*A_2^5+5*A_0^8*A_1^6*A_2^5+6*A_0^7*A_1^7*A_2^5+5*A_0^6*A_1^8*A_2^5+A_0^5*A_1^9*A_2^5+A_0^9*A_1^4*A_2^6+5*A_0^8*A_1^5*A_2^6+9*A_0^7*A_1^6*A_2^6+9*A_0^6*A_1^7*A_2^6+5*A_0^5*A_1^8*A_2^6+A_0^4*A_1^9*A_2^6+3*A_0^8*A_1^4*A_2^7+6*A_0^7*A_1^5*A_2^7+9*A_0^6*A_1^6*A_2^7+6*A_0^5*A_1^7*A_2^7+3*A_0^4*A_1^8*A_2^7+A_0^8*A_1^3*A_2^8+3*A_0^7*A_1^4*A_2^8+5*A_0^6*A_1^5*A_2^8+5*A_0^5*A_1^6*A_2^8+3*A_0^4*A_1^7*A_2^8+A_0^3*A_1^8*A_2^8+A_0^6*A_1^4*A_2^9+A_0^5*A_1^5*A_2^9+A_0^4*A_1^6*A_2^9, (4,2) => 0},
--sb represents the betti numbers as sums of Schur functors
"sb"=>new HashTable from {(7,0) => {}, (6,1) => {({9,8,5},1/1),({9,7,6},1/1)}, (5,2) => {}, (7,1) => {({9,9,7},1/1)}, (6,2) => {}, (7,2) => {}, (0,0) => {({1,0,0},1/1)}, (1,0) => {({3,1,0},1/1)}, (0,1) => {}, (1,1) => {}, (0,2) => {}, (2,0) => {({5,1,1},1/1),({3,3,1},1/1)}, (1,2) => {}, (2,1) => {({5,5,0},1/1)}, (3,0) => {}, (4,0) => {}, (2,2) => {}, (3,1) => {({7,5,1},1/1),({7,3,3},1/1),({6,5,2},1/1),({5,5,3},1/1)}, (5,0) => {}, (4,1) => {({8,6,2},1/1),({8,5,3},1/1),({7,6,3},1/1),({7,5,4},1/1),({6,6,4},1/1)}, (3,2) => {}, (6,0) => {}, (5,1) => {({9,6,4},1/1),({8,8,3},1/1),({8,7,4},1/1),({8,6,5},1/1),({7,6,6},1/1)}, (4,2) => {}},
--dw encodes the dominant weights in each entry
"dw"=>new HashTable from {(7,0) => {}, (6,1) => {{9,8,5}}, (5,2) => {}, (6,2) => {}, (7,1) => {{9,9,7}}, (7,2) => {}, (0,0) => {{1,0,0}}, (1,0) => {{3,1,0}}, (0,1) => {}, (1,1) => {}, (0,2) => {}, (2,0) => {{5,1,1}}, (3,0) => {}, (2,1) => {{5,5,0}}, (1,2) => {}, (3,1) => {{7,5,1}}, (2,2) => {}, (4,0) => {}, (3,2) => {}, (4,1) => {{8,6,2}}, (5,0) => {}, (4,2) => {}, (5,1) => {{9,6,4},{8,8,3}}, (6,0) => {}},
--lw encodes the lex leading weight in each entry
"lw"=>new HashTable from {(7,0) => {}, (6,1) => {9,8,5}, (5,2) => {}, (6,2) => {}, (7,1) => {9,9,7}, (7,2) => {}, (0,0) => {1,0,0}, (1,0) => {3,1,0}, (0,1) => {}, (1,1) => {}, (0,2) => {}, (2,0) => {5,1,1}, (3,0) => {}, (2,1) => {5,5,0}, (1,2) => {}, (3,1) => {7,5,1}, (2,2) => {}, (4,0) => {}, (3,2) => {}, (4,1) => {8,6,2}, (5,0) => {}, (4,2) => {}, (5,1) => {9,6,4}, (6,0) => {}},
--nr encodes the number of disctinct reprsentations in each entry
"nr"=>new HashTable from {(7,0) => 0, (6,1) => 2, (5,2) => 0, (6,2) => 0, (7,1) => 1, (7,2) => 0, (0,0) => 1, (1,0) => 1, (0,1) => 0, (1,1) => 0, (0,2) => 0, (2,0) => 2, (3,0) => 0, (2,1) => 1, (1,2) => 0, (3,1) => 4, (2,2) => 0, (4,0) => 0, (3,2) => 0, (4,1) => 5, (5,0) => 0, (4,2) => 0, (5,1) => 5, (6,0) => 0},
--nrm encodes the number of representations with multiplicity in each entry
"nrm"=>new HashTable from {(7,0) => 0, (6,1) => 2/1, (5,2) => 0, (6,2) => 0, (7,1) => 1/1, (7,2) => 0, (0,0) => 1/1, (1,0) => 1/1, (0,1) => 0, (1,1) => 0, (0,2) => 0, (2,0) => 2/1, (3,0) => 0, (2,1) => 1/1, (1,2) => 0, (3,1) => 4/1, (2,2) => 0, (4,0) => 0, (3,2) => 0, (4,1) => 5/1, (5,0) => 0, (4,2) => 0, (5,1) => 5/1, (6,0) => 0},
--er encodes the errors in the computed multigraded Hilbert series via our Schur method in each entry
"er"=>new HashTable from {(7,0) => 0, (6,1) => 39, (5,2) => 0, (6,2) => 0, (7,1) => 6, (7,2) => 0, (0,0) => 3, (1,0) => 15, (0,1) => 0, (1,1) => 0, (0,2) => 0, (2,0) => 21, (3,0) => 0, (2,1) => 21, (1,2) => 0, (3,1) => 105, (2,2) => 0, (4,0) => 0, (3,2) => 0, (4,1) => 147, (5,0) => 0, (4,2) => 0, (5,1) => 105, (6,0) => 0},
--bs encodes the Boij-Soederberg coefficients each entry
"bs"=>{30240/1,15120/1},
}
