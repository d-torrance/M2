Unmet — and the answer turns out cheaper than the file imagined, because the sparse modulus it wants is
already available inside M2. Reframed onto FLINT, since PARI left the tree in `e79bd82855` (2025-03-07).

### What happens today

`findGalois` (`galois.m2:95-121`) asks a hook; the only hook is `ConwayPolynomials`, whose data is
FLINT's Conway table (`ConwayPolynomials.m2:13`: *"the data comes libflint"*). Outside that table it
falls to `galois.m2:113` — a brute-force search over **dense random monic** polynomials — and then
`findPrimitive`.

Measured:

| | terms | time |
| --- | ---: | ---: |
| `conwayPolynomial(3,100)` | — | `null` (outside the table) |
| `GF(3,100)` | 67, and different every call | 6.5 s, then 4.8 s |
| `rawConwayPolynomial(3,100,true)` — FLINT's `fq_nmod_ctx_init` | **3** | 2.2 ms |

`ConwayPolynomials.m2:16` hard-codes `false`, so `GF` never sees the sparse path.

`GF(3,582)` — the PARI thread's own example — does not finish in any practical sense: left running
without a timeout it was still going after **55 minutes** at 99% of a core and 261 MB, and was killed
rather than completing.

### Two aggravating details

- **The modulus is non-deterministic**, so `GF(3,100)` in two sessions gives *incompatible* fields.
- `findPrimitive` (`galois.m2:118`) is computed and then **discarded** on the FlintBig path
  (`galois.m2:174-181`) — i.e. the wasted work happens precisely in the slow cases.

### Coupled cost for whoever takes it

A non-Conway modulus makes `map(GaloisField, GaloisField)` error by design
(`ConwayPolynomials.m2:42`), which is **#2143**. So switching the fallback changes that behaviour too.

**#1917** is adjacent, in the same `galois.m2` block.
