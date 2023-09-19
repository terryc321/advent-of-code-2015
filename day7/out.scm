
(use-modules (ice-9 match))


(define symbols
  '(hn h ie l jt gd z bg dp jw jy ic jz jx ac cs d q cq cu gh w jv kj hx kv bf kw hj eq ct ae ad ab y aw ju ik ih if id p be hf ep dv aa cl cx cw cv eh ag bt bs fk kc cm kz am lu lw dm cz bw jf lc la en al ee ec ds ev im r ar dx dt dw iq bh hy cr cp cj lr gs du da lo aj ew he br bq fl jg ky kx aq lp cy bd bc lv jl ir lt bj ji jh gv gk dh ga fz fo jr je jd ka ak x ex ei do dn lk ba v fw f e gi ch hp ho hm li dl dk dc gu gt gr gy ki hr fi kk fh dr dq ib fd kp kn km ek ej kf iy ix cg ca c gl ig gc gb fp hb ha bb az at fr fq cf hd gz ja iz a lx eg ef dz dj ez ey lb kh ia hz m k ln fg db ay lh bl as ai ah af in il bm bi cb bz ce bk le ld ap an ao er dy ht hs hq gm hw hu hv jc jb iv ft gx gw bp bn cd cc ij ii iw jo lm ll lj fv fu fs gq gn ck ci j i g jk jj es eo io js jq jp co cn kg jm hh hg ke kd kb gp go de fb fm fy fx kq ko ma lz ly u s t fn fj ff fe em el ip di dg fc fa df dd lg ax av au hl hk hi ks kr kl ed eb ea ku kt gg ge gf o n b it is by bx eu et hc gj bv bu bo jn iu ls lq lf))

;; map over symbols use (define <sym> #f)

;; each symbol has an assigned index
(define store (make-array #f (length symbols)))

;; number of unknown symbol outputs
(define unknown (length symbols))

;; equivalent circuit for problem
(define circuit 
  '(
    (and16 (sym 338 lf) (sym 337 lq) (sym 336 ls))
    (shift-r (sym 335 iu) (const 1) (sym 334 jn))
    (or16 (sym 333 bo) (sym 332 bu) (sym 331 bv))
    (shift-r (sym 330 gj) (const 1) (sym 329 hc))
    (shift-r (sym 328 et) (const 2) (sym 327 eu))
    (and16 (sym 331 bv) (sym 326 bx) (sym 325 by))
    (or16 (sym 324 is) (sym 323 it) (sym 335 iu))
    (or16 (sym 322 b) (sym 321 n) (sym 320 o))
    (or16 (sym 319 gf) (sym 318 ge) (sym 317 gg))
    (not16 (sym 316 kt) (sym 315 ku))
    (and16 (sym 314 ea) (sym 313 eb) (sym 312 ed))
    (or16 (sym 311 kl) (sym 310 kr) (sym 309 ks))
    (and16 (sym 308 hi) (sym 307 hk) (sym 306 hl))
    (and16 (sym 305 au) (sym 304 av) (sym 303 ax))
    (shift-r (sym 338 lf) (const 2) (sym 302 lg))
    (shift-r (sym 301 dd) (const 3) (sym 300 df))
    (and16 (sym 327 eu) (sym 299 fa) (sym 298 fc))
    (and16 (sym 300 df) (sym 297 dg) (sym 296 di))
    (shift-l (sym 295 ip) (const 15) (sym 323 it))
    (not16 (sym 294 el) (sym 293 em))
    (or16 (sym 328 et) (sym 292 fe) (sym 291 ff))
    (shift-l (sym 290 fj) (const 15) (sym 289 fn))
    (or16 (sym 288 t) (sym 287 s) (sym 286 u))
    (or16 (sym 285 ly) (sym 284 lz) (sym 283 ma))
    (and16 (sym 282 ko) (sym 281 kq) (sym 310 kr))
    (not16 (sym 280 fx) (sym 279 fy))
    (shift-r (sym 328 et) (const 1) (sym 278 fm))
    (or16 (sym 327 eu) (sym 299 fa) (sym 277 fb))
    (shift-r (sym 301 dd) (const 2) (sym 276 de))
    (not16 (sym 275 go) (sym 274 gp))
    (and16 (sym 273 kb) (sym 272 kd) (sym 271 ke))
    (or16 (sym 270 hg) (sym 269 hh) (sym 308 hi))
    (shift-l (sym 268 jm) (const 1) (sym 267 kg))
    (not16 (sym 266 cn) (sym 265 co))
    (shift-r (sym 264 jp) (const 2) (sym 263 jq))
    (shift-r (sym 264 jp) (const 5) (sym 262 js))
    (and16 (const 1) (sym 261 io) (sym 295 ip))
    (shift-l (sym 260 eo) (const 15) (sym 259 es))
    (and16 (const 1) (sym 258 jj) (sym 257 jk))
    (and16 (sym 256 g) (sym 255 i) (sym 254 j))
    (shift-r (sym 253 ci) (const 3) (sym 252 ck))
    (and16 (sym 251 gn) (sym 274 gp) (sym 250 gq))
    (and16 (sym 249 fs) (sym 248 fu) (sym 247 fv))
    (and16 (sym 246 lj) (sym 245 ll) (sym 244 lm))
    (shift-l (sym 257 jk) (const 15) (sym 243 jo))
    (shift-r (sym 335 iu) (const 3) (sym 242 iw))
    (not16 (sym 241 ii) (sym 240 ij))
    (and16 (const 1) (sym 239 cc) (sym 238 cd))
    (shift-r (sym 237 bn) (const 3) (sym 236 bp))
    (not16 (sym 235 gw) (sym 234 gx))
    (not16 (sym 233 ft) (sym 248 fu))
    (or16 (sym 334 jn) (sym 243 jo) (sym 264 jp))
    (or16 (sym 232 iv) (sym 231 jb) (sym 230 jc))
    (or16 (sym 229 hv) (sym 228 hu) (sym 227 hw))
    (assign (const 19138) (sym 322 b))
    (shift-r (sym 330 gj) (const 5) (sym 226 gm))
    (and16 (sym 225 hq) (sym 224 hs) (sym 223 ht))
    (shift-r (sym 222 dy) (const 1) (sym 221 er))
    (or16 (sym 220 ao) (sym 219 an) (sym 218 ap))
    (or16 (sym 217 ld) (sym 216 le) (sym 338 lf))
    (shift-l (sym 215 bk) (const 1) (sym 214 ce))
    (and16 (sym 213 bz) (sym 212 cb) (sym 239 cc))
    (shift-l (sym 211 bi) (const 15) (sym 210 bm))
    (and16 (sym 209 il) (sym 208 in) (sym 261 io))
    (and16 (sym 207 af) (sym 206 ah) (sym 205 ai))
    (shift-r (sym 204 as) (const 1) (sym 203 bl))
    (shift-r (sym 338 lf) (const 3) (sym 202 lh))
    (or16 (sym 221 er) (sym 259 es) (sym 328 et))
    (not16 (sym 303 ax) (sym 201 ay))
    (shift-r (sym 253 ci) (const 1) (sym 200 db))
    (and16 (sym 328 et) (sym 292 fe) (sym 199 fg))
    (or16 (sym 302 lg) (sym 244 lm) (sym 198 ln))
    (and16 (sym 197 k) (sym 196 m) (sym 321 n))
    (shift-r (sym 195 hz) (const 2) (sym 194 ia))
    (shift-l (sym 193 kh) (const 1) (sym 192 lb))
    (not16 (sym 191 ey) (sym 190 ez))
    (not16 (sym 296 di) (sym 189 dj))
    (or16 (sym 188 dz) (sym 187 ef) (sym 186 eg))
    (assign (sym 185 lx) (sym 184 a))
    (not16 (sym 183 iz) (sym 182 ja))
    (shift-l (sym 181 gz) (const 15) (sym 180 hd))
    (or16 (sym 214 ce) (sym 238 cd) (sym 179 cf))
    (and16 (sym 178 fq) (sym 177 fr) (sym 233 ft))
    (and16 (sym 176 at) (sym 175 az) (sym 174 bb))
    (or16 (sym 173 ha) (sym 181 gz) (sym 172 hb))
    (and16 (sym 171 fp) (sym 247 fv) (sym 280 fx))
    (not16 (sym 170 gb) (sym 169 gc))
    (and16 (sym 194 ia) (sym 168 ig) (sym 241 ii))
    (or16 (sym 167 gl) (sym 226 gm) (sym 251 gn))
    (assign (const 0) (sym 166 c))
    (not16 (sym 165 ca) (sym 212 cb))
    (shift-r (sym 237 bn) (const 1) (sym 164 cg))
    (shift-l (sym 166 c) (const 1) (sym 288 t))
    (or16 (sym 242 iw) (sym 163 ix) (sym 162 iy))
    (or16 (sym 267 kg) (sym 161 kf) (sym 193 kh))
    (or16 (sym 222 dy) (sym 160 ej) (sym 159 ek))
    (and16 (sym 158 km) (sym 157 kn) (sym 156 kp))
    (not16 (sym 298 fc) (sym 155 fd))
    (shift-r (sym 195 hz) (const 3) (sym 154 ib))
    (not16 (sym 153 dq) (sym 152 dr))
    (not16 (sym 199 fg) (sym 151 fh))
    (shift-r (sym 222 dy) (const 2) (sym 188 dz))
    (shift-r (sym 150 kk) (const 2) (sym 311 kl))
    (and16 (const 1) (sym 149 fi) (sym 290 fj))
    (not16 (sym 148 hr) (sym 224 hs))
    (shift-r (sym 264 jp) (const 1) (sym 147 ki))
    (or16 (sym 203 bl) (sym 210 bm) (sym 237 bn))
    (and16 (const 1) (sym 146 gy) (sym 181 gz))
    (and16 (sym 145 gr) (sym 144 gt) (sym 143 gu))
    (or16 (sym 200 db) (sym 142 dc) (sym 301 dd))
    (or16 (sym 276 de) (sym 141 dk) (sym 140 dl))
    (shift-r (sym 204 as) (const 5) (sym 304 av))
    (shift-r (sym 338 lf) (const 5) (sym 139 li))
    (and16 (sym 138 hm) (sym 137 ho) (sym 136 hp))
    (or16 (sym 164 cg) (sym 135 ch) (sym 253 ci))
    (and16 (sym 330 gj) (sym 143 gu) (sym 235 gw))
    (shift-l (sym 318 ge) (const 15) (sym 134 gi))
    (or16 (sym 133 e) (sym 132 f) (sym 256 g))
    (or16 (sym 171 fp) (sym 247 fv) (sym 131 fw))
    (and16 (sym 277 fb) (sym 155 fd) (sym 292 fe))
    (shift-l (sym 238 cd) (const 15) (sym 135 ch))
    (shift-r (sym 322 b) (const 1) (sym 130 v))
    (or16 (sym 176 at) (sym 175 az) (sym 129 ba))
    (shift-r (sym 237 bn) (const 2) (sym 333 bo))
    (and16 (sym 202 lh) (sym 139 li) (sym 128 lk))
    (and16 (sym 140 dl) (sym 127 dn) (sym 126 do))
    (and16 (sym 186 eg) (sym 125 ei) (sym 160 ej))
    (and16 (sym 124 ex) (sym 190 ez) (sym 299 fa))
    (not16 (sym 156 kp) (sym 281 kq))
    (not16 (sym 128 lk) (sym 245 ll))
    (and16 (sym 123 x) (sym 205 ai) (sym 122 ak))
    (or16 (sym 264 jp) (sym 121 ka) (sym 273 kb))
    (not16 (sym 120 jd) (sym 119 je))
    (and16 (sym 162 iy) (sym 182 ja) (sym 231 jb))
    (shift-r (sym 264 jp) (const 3) (sym 118 jr))
    (or16 (sym 117 fo) (sym 116 fz) (sym 115 ga))
    (or16 (sym 300 df) (sym 297 dg) (sym 114 dh))
    (shift-r (sym 330 gj) (const 2) (sym 113 gk))
    (or16 (sym 330 gj) (sym 143 gu) (sym 112 gv))
    (not16 (sym 111 jh) (sym 110 ji))
    (shift-l (sym 218 ap) (const 1) (sym 109 bj))
    (not16 (sym 336 ls) (sym 108 lt))
    (shift-l (sym 107 ir) (const 1) (sym 106 jl))
    (and16 (sym 237 bn) (sym 325 by) (sym 165 ca))
    (shift-l (sym 105 lv) (const 15) (sym 284 lz))
    (and16 (sym 129 ba) (sym 104 bc) (sym 103 bd))
    (shift-l (sym 102 cy) (const 15) (sym 142 dc))
    (and16 (sym 198 ln) (sym 101 lp) (sym 337 lq))
    (shift-r (sym 123 x) (const 1) (sym 100 aq))
    (or16 (sym 113 gk) (sym 250 gq) (sym 145 gr))
    (not16 (sym 99 kx) (sym 98 ky))
    (and16 (sym 97 jg) (sym 110 ji) (sym 258 jj))
    (or16 (sym 237 bn) (sym 325 by) (sym 213 bz))
    (shift-l (sym 96 fl) (const 1) (sym 319 gf))
    (or16 (sym 236 bp) (sym 95 bq) (sym 94 br))
    (or16 (sym 93 he) (sym 136 hp) (sym 225 hq))
    (shift-r (sym 328 et) (const 5) (sym 92 ew))
    (shift-r (sym 335 iu) (const 2) (sym 232 iv))
    (and16 (sym 167 gl) (sym 226 gm) (sym 275 go))
    (or16 (sym 123 x) (sym 205 ai) (sym 91 aj))
    (or16 (sym 329 hc) (sym 180 hd) (sym 93 he))
    (and16 (sym 302 lg) (sym 244 lm) (sym 90 lo))
    (or16 (sym 202 lh) (sym 139 li) (sym 246 lj))
    (shift-l (sym 89 da) (const 1) (sym 88 du))
    (shift-r (sym 117 fo) (const 2) (sym 171 fp))
    (and16 (sym 113 gk) (sym 250 gq) (sym 87 gs))
    (or16 (sym 109 bj) (sym 211 bi) (sym 215 bk))
    (or16 (sym 338 lf) (sym 337 lq) (sym 86 lr))
    (and16 (sym 85 cj) (sym 84 cp) (sym 83 cr))
    (shift-l (sym 228 hu) (const 15) (sym 82 hy))
    (and16 (const 1) (sym 81 bh) (sym 211 bi))
    (shift-r (sym 117 fo) (const 3) (sym 178 fq))
    (not16 (sym 90 lo) (sym 101 lp))
    (shift-l (sym 227 hw) (const 1) (sym 80 iq))
    (shift-r (sym 301 dd) (const 1) (sym 79 dw))
    (shift-l (sym 78 dt) (const 15) (sym 77 dx))
    (and16 (sym 222 dy) (sym 160 ej) (sym 294 el))
    (shift-l (sym 219 an) (const 15) (sym 76 ar))
    (or16 (sym 100 aq) (sym 76 ar) (sym 204 as))
    (and16 (const 1) (sym 75 r) (sym 287 s))
    (and16 (sym 131 fw) (sym 279 fy) (sym 116 fz))
    (not16 (sym 74 im) (sym 208 in))
    (shift-r (sym 328 et) (const 3) (sym 73 ev))
    (and16 (const 1) (sym 72 ds) (sym 78 dt))
    (and16 (sym 71 ec) (sym 70 ee) (sym 187 ef))
    (not16 (sym 122 ak) (sym 69 al))
    (or16 (sym 106 jl) (sym 257 jk) (sym 268 jm))
    (and16 (const 1) (sym 68 en) (sym 260 eo))
    (or16 (sym 192 lb) (sym 67 la) (sym 66 lc))
    (and16 (sym 335 iu) (sym 65 jf) (sym 111 jh))
    (shift-r (sym 335 iu) (const 5) (sym 163 ix))
    (and16 (sym 333 bo) (sym 332 bu) (sym 64 bw))
    (or16 (sym 63 cz) (sym 102 cy) (sym 89 da))
    (and16 (sym 232 iv) (sym 231 jb) (sym 120 jd))
    (and16 (sym 242 iw) (sym 163 ix) (sym 183 iz))
    (shift-r (sym 338 lf) (const 1) (sym 285 ly))
    (or16 (sym 335 iu) (sym 65 jf) (sym 97 jg))
    (not16 (sym 62 dm) (sym 127 dn))
    (or16 (sym 61 lw) (sym 105 lv) (sym 185 lx))
    (shift-l (sym 317 gg) (const 1) (sym 173 ha))
    (and16 (sym 86 lr) (sym 108 lt) (sym 60 lu))
    (or16 (sym 278 fm) (sym 289 fn) (sym 117 fo))
    (shift-r (sym 93 he) (const 3) (sym 270 hg))
    (and16 (sym 91 aj) (sym 69 al) (sym 59 am))
    (and16 (const 1) (sym 58 kz) (sym 67 la))
    (shift-r (sym 222 dy) (const 5) (sym 313 eb))
    (and16 (sym 230 jc) (sym 119 je) (sym 65 jf))
    (and16 (sym 57 cm) (sym 265 co) (sym 84 cp))
    (and16 (sym 112 gv) (sym 234 gx) (sym 146 gy))
    (or16 (sym 73 ev) (sym 92 ew) (sym 124 ex))
    (and16 (sym 264 jp) (sym 121 ka) (sym 56 kc))
    (or16 (sym 55 fk) (sym 290 fj) (sym 96 fl))
    (shift-r (sym 222 dy) (const 3) (sym 314 ea))
    (not16 (sym 54 bs) (sym 53 bt))
    (not16 (sym 52 ag) (sym 206 ah))
    (and16 (sym 188 dz) (sym 187 ef) (sym 51 eh))
    (shift-l (sym 179 cf) (const 1) (sym 63 cz))
    (not16 (sym 50 cv) (sym 49 cw))
    (and16 (const 1) (sym 48 cx) (sym 102 cy))
    (and16 (sym 276 de) (sym 141 dk) (sym 62 dm))
    (and16 (sym 252 ck) (sym 47 cl) (sym 266 cn))
    (shift-r (sym 123 x) (const 5) (sym 46 aa))
    (shift-l (sym 45 dv) (const 1) (sym 44 ep))
    (shift-r (sym 93 he) (const 2) (sym 43 hf))
    (not16 (sym 64 bw) (sym 326 bx))
    (or16 (sym 252 ck) (sym 47 cl) (sym 57 cm))
    (and16 (sym 236 bp) (sym 95 bq) (sym 54 bs))
    (or16 (sym 204 as) (sym 103 bd) (sym 42 be))
    (and16 (sym 93 he) (sym 136 hp) (sym 148 hr))
    (and16 (sym 73 ev) (sym 92 ew) (sym 191 ey))
    (and16 (const 1) (sym 60 lu) (sym 105 lv))
    (shift-r (sym 150 kk) (const 3) (sym 158 km))
    (and16 (sym 322 b) (sym 321 n) (sym 41 p))
    (not16 (sym 56 kc) (sym 272 kd))
    (shift-l (sym 66 lc) (const 1) (sym 61 lw))
    (or16 (sym 158 km) (sym 157 kn) (sym 282 ko))
    (and16 (sym 40 id) (sym 39 if) (sym 168 ig))
    (and16 (sym 38 ih) (sym 240 ij) (sym 37 ik))
    (and16 (sym 118 jr) (sym 262 js) (sym 36 ju))
    (shift-r (sym 253 ci) (const 5) (sym 47 cl))
    (shift-r (sym 195 hz) (const 1) (sym 324 is))
    (and16 (const 1) (sym 271 ke) (sym 161 kf))
    (not16 (sym 87 gs) (sym 144 gt))
    (and16 (sym 35 aw) (sym 201 ay) (sym 175 az))
    (shift-r (sym 123 x) (const 2) (sym 34 y))
    (and16 (sym 33 ab) (sym 32 ad) (sym 31 ae))
    (and16 (sym 291 ff) (sym 151 fh) (sym 149 fi))
    (and16 (sym 253 ci) (sym 30 ct) (sym 50 cv))
    (shift-l (sym 29 eq) (const 1) (sym 55 fk))
    (shift-r (sym 330 gj) (const 3) (sym 167 gl))
    (shift-l (sym 286 u) (const 1) (sym 220 ao))
    (not16 (sym 174 bb) (sym 104 bc))
    (not16 (sym 28 hj) (sym 307 hk))
    (and16 (sym 27 kw) (sym 98 ky) (sym 58 kz))
    (and16 (sym 204 as) (sym 103 bd) (sym 26 bf))
    (or16 (sym 79 dw) (sym 77 dx) (sym 222 dy))
    (and16 (sym 94 br) (sym 53 bt) (sym 332 bu))
    (and16 (sym 150 kk) (sym 25 kv) (sym 99 kx))
    (or16 (sym 44 ep) (sym 260 eo) (sym 29 eq))
    (shift-r (sym 93 he) (const 1) (sym 24 hx))
    (or16 (sym 147 ki) (sym 23 kj) (sym 150 kk))
    (not16 (sym 36 ju) (sym 22 jv))
    (and16 (sym 159 ek) (sym 293 em) (sym 68 en))
    (shift-r (sym 150 kk) (const 5) (sym 157 kn))
    (not16 (sym 51 eh) (sym 125 ei))
    (or16 (sym 24 hx) (sym 82 hy) (sym 195 hz))
    (or16 (sym 314 ea) (sym 313 eb) (sym 71 ec))
    (shift-l (sym 287 s) (const 15) (sym 21 w))
    (shift-r (sym 117 fo) (const 1) (sym 20 gh))
    (or16 (sym 150 kk) (sym 25 kv) (sym 27 kw))
    (shift-r (sym 237 bn) (const 5) (sym 95 bq))
    (not16 (sym 312 ed) (sym 70 ee))
    (and16 (const 1) (sym 223 ht) (sym 228 hu))
    (and16 (sym 19 cu) (sym 49 cw) (sym 48 cx))
    (shift-r (sym 322 b) (const 5) (sym 132 f))
    (and16 (sym 311 kl) (sym 310 kr) (sym 316 kt))
    (or16 (sym 80 iq) (sym 295 ip) (sym 107 ir))
    (shift-r (sym 253 ci) (const 2) (sym 85 cj))
    (or16 (sym 85 cj) (sym 84 cp) (sym 18 cq))
    (and16 (sym 320 o) (sym 17 q) (sym 75 r))
    (shift-r (sym 301 dd) (const 5) (sym 297 dg))
    (shift-r (sym 322 b) (const 2) (sym 16 d))
    (and16 (sym 309 ks) (sym 315 ku) (sym 25 kv))
    (shift-r (sym 322 b) (const 3) (sym 133 e))
    (or16 (sym 16 d) (sym 254 j) (sym 197 k))
    (not16 (sym 41 p) (sym 17 q))
    (not16 (sym 83 cr) (sym 15 cs))
    (or16 (sym 88 du) (sym 78 dt) (sym 45 dv))
    (shift-l (sym 161 kf) (const 15) (sym 23 kj))
    (not16 (sym 14 ac) (sym 32 ad))
    (shift-r (sym 117 fo) (const 5) (sym 177 fr))
    (or16 (sym 195 hz) (sym 37 ik) (sym 209 il))
    (and16 (sym 13 jx) (sym 12 jz) (sym 121 ka))
    (or16 (sym 20 gh) (sym 134 gi) (sym 330 gj))
    (shift-r (sym 150 kk) (const 1) (sym 217 ld))
    (shift-r (sym 195 hz) (const 5) (sym 11 ic))
    (shift-r (sym 204 as) (const 2) (sym 176 at))
    (not16 (sym 10 jy) (sym 12 jz))
    (and16 (const 1) (sym 59 am) (sym 219 an))
    (or16 (sym 253 ci) (sym 30 ct) (sym 19 cu))
    (and16 (sym 270 hg) (sym 269 hh) (sym 28 hj))
    (or16 (sym 263 jq) (sym 9 jw) (sym 13 jx))
    (or16 (sym 130 v) (sym 21 w) (sym 123 x))
    (shift-l (sym 67 la) (const 15) (sym 216 le))
    (and16 (sym 114 dh) (sym 189 dj) (sym 141 dk))
    (and16 (sym 8 dp) (sym 152 dr) (sym 72 ds))
    (and16 (sym 263 jq) (sym 9 jw) (sym 10 jy))
    (or16 (sym 305 au) (sym 304 av) (sym 35 aw))
    (not16 (sym 26 bf) (sym 7 bg))
    (or16 (sym 6 z) (sym 46 aa) (sym 33 ab))
    (and16 (sym 115 ga) (sym 169 gc) (sym 5 gd))
    (and16 (sym 195 hz) (sym 37 ik) (sym 74 im))
    (and16 (sym 4 jt) (sym 22 jv) (sym 9 jw))
    (and16 (sym 6 z) (sym 46 aa) (sym 14 ac))
    (or16 (sym 118 jr) (sym 262 js) (sym 4 jt))
    (shift-l (sym 172 hb) (const 1) (sym 229 hv))
    (or16 (sym 43 hf) (sym 306 hl) (sym 138 hm))
    (or16 (sym 154 ib) (sym 11 ic) (sym 40 id))
    (or16 (sym 178 fq) (sym 177 fr) (sym 249 fs))
    (and16 (sym 18 cq) (sym 15 cs) (sym 30 ct))
    (or16 (sym 194 ia) (sym 168 ig) (sym 38 ih))
    (or16 (sym 301 dd) (sym 126 do) (sym 8 dp))
    (and16 (sym 16 d) (sym 254 j) (sym 3 l))
    (and16 (sym 154 ib) (sym 11 ic) (sym 2 ie))
    (shift-r (sym 204 as) (const 3) (sym 305 au))
    (and16 (sym 42 be) (sym 7 bg) (sym 81 bh))
    (and16 (sym 301 dd) (sym 126 do) (sym 153 dq))
    (not16 (sym 3 l) (sym 196 m))
    (and16 (const 1) (sym 5 gd) (sym 318 ge))
    (and16 (sym 34 y) (sym 31 ae) (sym 52 ag))
    (and16 (sym 117 fo) (sym 116 fz) (sym 170 gb))
    (not16 (sym 2 ie) (sym 39 if))
    (and16 (sym 133 e) (sym 132 f) (sym 1 h))
    (shift-r (sym 123 x) (const 3) (sym 6 z))
    (or16 (sym 34 y) (sym 31 ae) (sym 207 af))
    (and16 (sym 43 hf) (sym 306 hl) (sym 0 hn))
    (not16 (sym 1 h) (sym 255 i))
    (not16 (sym 0 hn) (sym 137 ho))
    (shift-r (sym 93 he) (const 5) (sym 269 hh))
    ))


;; extended 16 bit
(define (ex16b xs n)
  (cond
   ((<= n 0) xs)
   (#t (ex16b (cons 0 xs) (- n 1)))))

(define (ex16 xs)  
  (let ((len (length xs)))
    (ex16b xs (- 16 len))))


;; MSB first ... LSB 
(define (dec->bin2 d rs)
  (cond
   ((zero? d) rs)
   (#t (dec->bin2 (floor (/ d 2)) (cons (remainder d 2) rs)))))

(define (dec->bin d)
  (ex16 (dec->bin2 d '())))

;; LSB first ... MSB
(define (bin->dec2 xs p pwr)
  (cond
   ((null? xs) p)
   (#t (bin->dec2 (cdr xs) (+ p (* (car xs) pwr)) (* pwr 2)))))

(define (bin->dec xs)
  (bin->dec2 (reverse xs) 0 1))

(define (or16 xs ys)
  (cond
   ((null? xs) '())
   (#t (let ((a (car xs))
	     (b (car ys)))
	 (cond
	  ((= a 1) (cons 1 (or16 (cdr xs) (cdr ys))))
	  ((= b 1) (cons 1 (or16 (cdr xs) (cdr ys))))
	  (#t (cons 0 (or16 (cdr xs) (cdr ys)))))))))

(define (not16 xs)
  (cond
   ((null? xs) '())
   (#t (let ((a (car xs)))
	 (cond
	  ((= a 1) (cons 0 (not16 (cdr xs))))
	  (#t (cons 1 (not16 (cdr xs)))))))))

(define (and16 xs ys)
  (cond
   ((null? xs) '())
   (#t (let ((a (car xs))
	     (b (car ys)))
	 (cond
	  ((and (= a 1) (= b 1)) (cons 1 (and16 (cdr xs) (cdr ys))))
	  (#t (cons 0 (and16 (cdr xs) (cdr ys)))))))))

(define (shift16-l xs n)
  (cond
   ((<= n 0) xs)
   (#t (shift16-l (append (cdr xs) (list 0)) (- n 1)))))

(define (take2 xs ys n)
  (cond
   ((< n 1) (reverse ys))
   (#t (take2 (cdr xs) (cons (car xs) ys)(- n 1)))))

(define (take xs n)
  (take2 xs '() n))

(define (shift16-r xs n)
  (cond
   ((<= n 0) xs)
   (#t
    (shift16-r (cons 0 (take xs 15)) (- n 1)))))



;; repeating loop until no rules to apply
(define (process xs)
  (let ((rs '()))
    (letrec ((recur (lambda (xs)
		      (cond
		       ((null? xs) rs)
		       (#t (let ((command (car xs)))

			     (match command
			       (('and16 ('sym i1 a) ('sym i2 b) ('sym i3 c))
				;; (format #t "a ~a ~a : " i1 a)
				;; (format #t "b ~a ~a : " i2 b)
				;; (format #t "c ~a ~a : ~%" i3 c)
				(set! rs (cons `(let ((a (array-ref store ,i1))
						      (b (array-ref store ,i2)))
						  (when (and a b)
						    (array-set! store (and16 a b) ,i3)))
					       rs))
				)
			       (('shift-r ('sym i1 a) ('const i2) ('sym i3 c))
				;; (format #t "a ~a ~a : " i1 a)
				;; (format #t "b ~a  : " i2)
				;; (format #t "c ~a ~a : ~%" i3 c)
				(set! rs (cons `(let ((a (array-ref store ,i1)))
						  (when a
						    (let ((b ,i2))
						      (array-set! store (shift16-r a b) ,i3))))
					       rs))
				)

			       (('or16 ('sym i1 a) ('sym i2 b) ('sym i3 c))
				;; (format #t "a ~a ~a : " i1 a)
				;; (format #t "b ~a ~a: " i2 b)
				;; (format #t "c ~a ~a : ~%" i3 c)
				(set! rs (cons `(let ((a (array-ref store ,i1))
						      (b (array-ref store ,i2)))
						  (when (and a b)
						    (array-set! store (or16 a b) ,i3)
						    ))
					       rs)))
			       
			       (('not16 ('sym i1 a) ('sym i2 b))
				;; (format #t "a ~a ~a : " i1 a)
				;; (format #t "b ~a ~a:  ~%" i2 b)
				(set! rs (cons `(let ((a (array-ref store ,i1)))
						  (when a
						    (array-set! store (not16 a) ,i2)
						    ))
					       rs)))
			       
			       (('shift-l ('sym i1 a) ('const i2) ('sym i3 c))
				;; (format #t "a ~a ~a : " i1 a)
				;; (format #t "b ~a  : " i2)
				;; (format #t "c ~a ~a : ~%" i3 c)
				(set! rs (cons `(let ((a (array-ref store ,i1)))
						  (when a
						    (let ((b ,i2))
						      (array-set! store (shift16-l a b) ,i3))))
					       rs))
				)


			       (('and16 ('const i1) ('sym i2 b) ('sym i3 c))
				;; (format #t "a ~a  : " i1)
				;; (format #t "b ~a ~a : " i2 b)
				;; (format #t "c ~a ~a : ~%" i3 c)
				(set! rs (cons `(let ((b (array-ref store ,i2)))
						  (when b
						    (let ((a (dec->bin ,i1)))
						      (array-set! store (and16 a b) ,i3))))
					       rs)))

			       (('assign ('const i1) ('sym i2 b))
				;; (format #t "a ~a  : " i1 )
				;; (format #t "b ~a ~a: ~%" i2 b)
				(set! rs (cons `(let ((a (dec->bin ,i1)))
						  (array-set! store a ,i2))
					       rs)))

			       (('assign ('sym i1 a) ('sym i2 b))
				;; (format #t "a ~a ~a : " i1 a)
				;; (format #t "b ~a ~a: ~%" i2 b)
				(set! rs (cons `(let ((a (array-ref store ,i1)))
						  (when a
						    (array-set! store a ,i2)))
					       rs)))

			       
			       ;;(assign (sym 185 lx) (sym 184 a)))	   
			       ;;(assign (const 19138) (sym 322 b))	   
			       ;;(and16 (const 1) (sym 261 io) (sym 295 ip))
			       ;;(shift-l (sym 295 ip) (const 15) (sym 323 it))
			       ;;(not16 (sym 316 kt) (sym 315 ku))
			       ;;(or16 (sym 333 bo) (sym 332 bu) (sym 331 bv))
			       ;;(shift-r (sym 335 iu) (const 1) (sym 334 jn))
			       (#t (error "process" (list "did not match")))) 
			     
			     (recur (cdr xs))))))))
      (recur xs)
      rs)))

	 




