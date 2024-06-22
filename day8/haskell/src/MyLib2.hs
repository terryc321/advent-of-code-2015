module MyLib2 (someFunc) where

import Data.Word (Word16)
import Data.Bits

--- part 2 of puzzle
--- set sym_b to be 16076 , the answer to part 1

leftS :: Bits a => a -> Int -> a
leftS x n = shift x n

rightS :: Bits a => a -> Int -> a
rightS x n = shift x (0 - n)

s_b :: Word16
s_b =  16076  --- was 19138 


s_ls :: Word16
s_ls = s_lf .&. s_lq

s_jn :: Word16
s_jn = rightS s_iu 1

s_bv :: Word16
s_bv = s_bo .|. s_bu

s_hc :: Word16
s_hc = rightS s_gj 1

s_eu :: Word16
s_eu = rightS s_et 2

s_by :: Word16
s_by = s_bv .&. s_bx

s_iu :: Word16
s_iu = s_is .|. s_it

s_o :: Word16
s_o = s_b .|. s_n

s_gg :: Word16
s_gg = s_gf .|. s_ge

s_ku :: Word16
s_ku = complement s_kt 

s_ed :: Word16
s_ed = s_ea .&. s_eb

s_ks :: Word16
s_ks = s_kl .|. s_kr

s_hl :: Word16
s_hl = s_hi .&. s_hk

s_ax :: Word16
s_ax = s_au .&. s_av

s_lg :: Word16
s_lg = rightS s_lf 2

s_df :: Word16
s_df = rightS s_dd 3

s_fc :: Word16
s_fc = s_eu .&. s_fa

s_di :: Word16
s_di = s_df .&. s_dg

s_it :: Word16
s_it = leftS s_ip 15 

s_em :: Word16
s_em = complement s_el 

s_ff :: Word16
s_ff = s_et .|. s_fe

s_fn :: Word16
s_fn = leftS s_fj 15 

s_u :: Word16
s_u = s_t .|. s_s

s_ma :: Word16
s_ma = s_ly .|. s_lz

s_kr :: Word16
s_kr = s_ko .&. s_kq

s_fy :: Word16
s_fy = complement s_fx 

s_fm :: Word16
s_fm = rightS s_et 1

s_fb :: Word16
s_fb = s_eu .|. s_fa

s_de :: Word16
s_de = rightS s_dd 2

s_gp :: Word16
s_gp = complement s_go 

s_ke :: Word16
s_ke = s_kb .&. s_kd

s_hi :: Word16
s_hi = s_hg .|. s_hh

s_kg :: Word16
s_kg = leftS s_jm 1 

s_co :: Word16
s_co = complement s_cn 

s_jq :: Word16
s_jq = rightS s_jp 2

s_js :: Word16
s_js = rightS s_jp 5

s_ip :: Word16
s_ip =  1 .&. s_io 

s_es :: Word16
s_es = leftS s_eo 15 

s_jk :: Word16
s_jk =  1 .&. s_jj 

s_j :: Word16
s_j = s_g .&. s_i

s_ck :: Word16
s_ck = rightS s_ci 3

s_gq :: Word16
s_gq = s_gn .&. s_gp

s_fv :: Word16
s_fv = s_fs .&. s_fu

s_lm :: Word16
s_lm = s_lj .&. s_ll

s_jo :: Word16
s_jo = leftS s_jk 15 

s_iw :: Word16
s_iw = rightS s_iu 3

s_ij :: Word16
s_ij = complement s_ii 

s_cd :: Word16
s_cd =  1 .&. s_cc 

s_bp :: Word16
s_bp = rightS s_bn 3

s_gx :: Word16
s_gx = complement s_gw 

s_fu :: Word16
s_fu = complement s_ft 

s_jp :: Word16
s_jp = s_jn .|. s_jo

s_jc :: Word16
s_jc = s_iv .|. s_jb

s_hw :: Word16
s_hw = s_hv .|. s_hu


s_gm :: Word16
s_gm = rightS s_gj 5

s_ht :: Word16
s_ht = s_hq .&. s_hs

s_er :: Word16
s_er = rightS s_dy 1

s_ap :: Word16
s_ap = s_ao .|. s_an

s_lf :: Word16
s_lf = s_ld .|. s_le

s_ce :: Word16
s_ce = leftS s_bk 1 

s_cc :: Word16
s_cc = s_bz .&. s_cb

s_bm :: Word16
s_bm = leftS s_bi 15 

s_io :: Word16
s_io = s_il .&. s_in

s_ai :: Word16
s_ai = s_af .&. s_ah

s_bl :: Word16
s_bl = rightS s_as 1

s_lh :: Word16
s_lh = rightS s_lf 3

s_et :: Word16
s_et = s_er .|. s_es

s_ay :: Word16
s_ay = complement s_ax 

s_db :: Word16
s_db = rightS s_ci 1

s_fg :: Word16
s_fg = s_et .&. s_fe

s_ln :: Word16
s_ln = s_lg .|. s_lm

s_n :: Word16
s_n = s_k .&. s_m

s_ia :: Word16
s_ia = rightS s_hz 2

s_lb :: Word16
s_lb = leftS s_kh 1 

s_ez :: Word16
s_ez = complement s_ey 

s_dj :: Word16
s_dj = complement s_di 

s_eg :: Word16
s_eg = s_dz .|. s_ef

s_a :: Word16
s_a =  s_lx 

s_ja :: Word16
s_ja = complement s_iz 

s_hd :: Word16
s_hd = leftS s_gz 15 

s_cf :: Word16
s_cf = s_ce .|. s_cd

s_ft :: Word16
s_ft = s_fq .&. s_fr

s_bb :: Word16
s_bb = s_at .&. s_az

s_hb :: Word16
s_hb = s_ha .|. s_gz

s_fx :: Word16
s_fx = s_fp .&. s_fv

s_gc :: Word16
s_gc = complement s_gb 

s_ii :: Word16
s_ii = s_ia .&. s_ig

s_gn :: Word16
s_gn = s_gl .|. s_gm

s_c :: Word16
s_c =  0 

s_cb :: Word16
s_cb = complement s_ca 

s_cg :: Word16
s_cg = rightS s_bn 1

s_t :: Word16
s_t = leftS s_c 1 

s_iy :: Word16
s_iy = s_iw .|. s_ix

s_kh :: Word16
s_kh = s_kg .|. s_kf

s_ek :: Word16
s_ek = s_dy .|. s_ej

s_kp :: Word16
s_kp = s_km .&. s_kn

s_fd :: Word16
s_fd = complement s_fc 

s_ib :: Word16
s_ib = rightS s_hz 3

s_dr :: Word16
s_dr = complement s_dq 

s_fh :: Word16
s_fh = complement s_fg 

s_dz :: Word16
s_dz = rightS s_dy 2

s_kl :: Word16
s_kl = rightS s_kk 2

s_fj :: Word16
s_fj =  1 .&. s_fi 

s_hs :: Word16
s_hs = complement s_hr 

s_ki :: Word16
s_ki = rightS s_jp 1

s_bn :: Word16
s_bn = s_bl .|. s_bm

s_gz :: Word16
s_gz =  1 .&. s_gy 

s_gu :: Word16
s_gu = s_gr .&. s_gt

s_dd :: Word16
s_dd = s_db .|. s_dc

s_dl :: Word16
s_dl = s_de .|. s_dk

s_av :: Word16
s_av = rightS s_as 5

s_li :: Word16
s_li = rightS s_lf 5

s_hp :: Word16
s_hp = s_hm .&. s_ho

s_ci :: Word16
s_ci = s_cg .|. s_ch

s_gw :: Word16
s_gw = s_gj .&. s_gu

s_gi :: Word16
s_gi = leftS s_ge 15 

s_g :: Word16
s_g = s_e .|. s_f

s_fw :: Word16
s_fw = s_fp .|. s_fv

s_fe :: Word16
s_fe = s_fb .&. s_fd

s_ch :: Word16
s_ch = leftS s_cd 15 

s_v :: Word16
s_v = rightS s_b 1

s_ba :: Word16
s_ba = s_at .|. s_az

s_bo :: Word16
s_bo = rightS s_bn 2

s_lk :: Word16
s_lk = s_lh .&. s_li

s_do :: Word16
s_do = s_dl .&. s_dn

s_ej :: Word16
s_ej = s_eg .&. s_ei

s_fa :: Word16
s_fa = s_ex .&. s_ez

s_kq :: Word16
s_kq = complement s_kp 

s_ll :: Word16
s_ll = complement s_lk 

s_ak :: Word16
s_ak = s_x .&. s_ai

s_kb :: Word16
s_kb = s_jp .|. s_ka

s_je :: Word16
s_je = complement s_jd 

s_jb :: Word16
s_jb = s_iy .&. s_ja

s_jr :: Word16
s_jr = rightS s_jp 3

s_ga :: Word16
s_ga = s_fo .|. s_fz

s_dh :: Word16
s_dh = s_df .|. s_dg

s_gk :: Word16
s_gk = rightS s_gj 2

s_gv :: Word16
s_gv = s_gj .|. s_gu

s_ji :: Word16
s_ji = complement s_jh 

s_bj :: Word16
s_bj = leftS s_ap 1 

s_lt :: Word16
s_lt = complement s_ls 

s_jl :: Word16
s_jl = leftS s_ir 1 

s_ca :: Word16
s_ca = s_bn .&. s_by

s_lz :: Word16
s_lz = leftS s_lv 15 

s_bd :: Word16
s_bd = s_ba .&. s_bc

s_dc :: Word16
s_dc = leftS s_cy 15 

s_lq :: Word16
s_lq = s_ln .&. s_lp

s_aq :: Word16
s_aq = rightS s_x 1

s_gr :: Word16
s_gr = s_gk .|. s_gq

s_ky :: Word16
s_ky = complement s_kx 

s_jj :: Word16
s_jj = s_jg .&. s_ji

s_bz :: Word16
s_bz = s_bn .|. s_by

s_gf :: Word16
s_gf = leftS s_fl 1 

s_br :: Word16
s_br = s_bp .|. s_bq

s_hq :: Word16
s_hq = s_he .|. s_hp

s_ew :: Word16
s_ew = rightS s_et 5

s_iv :: Word16
s_iv = rightS s_iu 2

s_go :: Word16
s_go = s_gl .&. s_gm

s_aj :: Word16
s_aj = s_x .|. s_ai

s_he :: Word16
s_he = s_hc .|. s_hd

s_lo :: Word16
s_lo = s_lg .&. s_lm

s_lj :: Word16
s_lj = s_lh .|. s_li

s_du :: Word16
s_du = leftS s_da 1 

s_fp :: Word16
s_fp = rightS s_fo 2

s_gs :: Word16
s_gs = s_gk .&. s_gq

s_bk :: Word16
s_bk = s_bj .|. s_bi

s_lr :: Word16
s_lr = s_lf .|. s_lq

s_cr :: Word16
s_cr = s_cj .&. s_cp

s_hy :: Word16
s_hy = leftS s_hu 15 

s_bi :: Word16
s_bi =  1 .&. s_bh 

s_fq :: Word16
s_fq = rightS s_fo 3

s_lp :: Word16
s_lp = complement s_lo 

s_iq :: Word16
s_iq = leftS s_hw 1 

s_dw :: Word16
s_dw = rightS s_dd 1

s_dx :: Word16
s_dx = leftS s_dt 15 

s_el :: Word16
s_el = s_dy .&. s_ej

s_ar :: Word16
s_ar = leftS s_an 15 

s_as :: Word16
s_as = s_aq .|. s_ar

s_s :: Word16
s_s =  1 .&. s_r 

s_fz :: Word16
s_fz = s_fw .&. s_fy

s_in :: Word16
s_in = complement s_im 

s_ev :: Word16
s_ev = rightS s_et 3

s_dt :: Word16
s_dt =  1 .&. s_ds 

s_ef :: Word16
s_ef = s_ec .&. s_ee

s_al :: Word16
s_al = complement s_ak 

s_jm :: Word16
s_jm = s_jl .|. s_jk

s_eo :: Word16
s_eo =  1 .&. s_en 

s_lc :: Word16
s_lc = s_lb .|. s_la

s_jh :: Word16
s_jh = s_iu .&. s_jf

s_ix :: Word16
s_ix = rightS s_iu 5

s_bw :: Word16
s_bw = s_bo .&. s_bu

s_da :: Word16
s_da = s_cz .|. s_cy

s_jd :: Word16
s_jd = s_iv .&. s_jb

s_iz :: Word16
s_iz = s_iw .&. s_ix

s_ly :: Word16
s_ly = rightS s_lf 1

s_jg :: Word16
s_jg = s_iu .|. s_jf

s_dn :: Word16
s_dn = complement s_dm 

s_lx :: Word16
s_lx = s_lw .|. s_lv

s_ha :: Word16
s_ha = leftS s_gg 1 

s_lu :: Word16
s_lu = s_lr .&. s_lt

s_fo :: Word16
s_fo = s_fm .|. s_fn

s_hg :: Word16
s_hg = rightS s_he 3

s_am :: Word16
s_am = s_aj .&. s_al

s_la :: Word16
s_la =  1 .&. s_kz 

s_eb :: Word16
s_eb = rightS s_dy 5

s_jf :: Word16
s_jf = s_jc .&. s_je

s_cp :: Word16
s_cp = s_cm .&. s_co

s_gy :: Word16
s_gy = s_gv .&. s_gx

s_ex :: Word16
s_ex = s_ev .|. s_ew

s_kc :: Word16
s_kc = s_jp .&. s_ka

s_fl :: Word16
s_fl = s_fk .|. s_fj

s_ea :: Word16
s_ea = rightS s_dy 3

s_bt :: Word16
s_bt = complement s_bs 

s_ah :: Word16
s_ah = complement s_ag 

s_eh :: Word16
s_eh = s_dz .&. s_ef

s_cz :: Word16
s_cz = leftS s_cf 1 

s_cw :: Word16
s_cw = complement s_cv 

s_cy :: Word16
s_cy =  1 .&. s_cx 

s_dm :: Word16
s_dm = s_de .&. s_dk

s_cn :: Word16
s_cn = s_ck .&. s_cl

s_aa :: Word16
s_aa = rightS s_x 5

s_ep :: Word16
s_ep = leftS s_dv 1 

s_hf :: Word16
s_hf = rightS s_he 2

s_bx :: Word16
s_bx = complement s_bw 

s_cm :: Word16
s_cm = s_ck .|. s_cl

s_bs :: Word16
s_bs = s_bp .&. s_bq

s_be :: Word16
s_be = s_as .|. s_bd

s_hr :: Word16
s_hr = s_he .&. s_hp

s_ey :: Word16
s_ey = s_ev .&. s_ew

s_lv :: Word16
s_lv =  1 .&. s_lu 

s_km :: Word16
s_km = rightS s_kk 3

s_p :: Word16
s_p = s_b .&. s_n

s_kd :: Word16
s_kd = complement s_kc 

s_lw :: Word16
s_lw = leftS s_lc 1 

s_ko :: Word16
s_ko = s_km .|. s_kn

s_ig :: Word16
s_ig = s_id .&. s_if

s_ik :: Word16
s_ik = s_ih .&. s_ij

s_ju :: Word16
s_ju = s_jr .&. s_js

s_cl :: Word16
s_cl = rightS s_ci 5

s_is :: Word16
s_is = rightS s_hz 1

s_kf :: Word16
s_kf =  1 .&. s_ke 

s_gt :: Word16
s_gt = complement s_gs 

s_az :: Word16
s_az = s_aw .&. s_ay

s_y :: Word16
s_y = rightS s_x 2

s_ae :: Word16
s_ae = s_ab .&. s_ad

s_fi :: Word16
s_fi = s_ff .&. s_fh

s_cv :: Word16
s_cv = s_ci .&. s_ct

s_fk :: Word16
s_fk = leftS s_eq 1 

s_gl :: Word16
s_gl = rightS s_gj 3

s_ao :: Word16
s_ao = leftS s_u 1 

s_bc :: Word16
s_bc = complement s_bb 

s_hk :: Word16
s_hk = complement s_hj 

s_kz :: Word16
s_kz = s_kw .&. s_ky

s_bf :: Word16
s_bf = s_as .&. s_bd

s_dy :: Word16
s_dy = s_dw .|. s_dx

s_bu :: Word16
s_bu = s_br .&. s_bt

s_kx :: Word16
s_kx = s_kk .&. s_kv

s_eq :: Word16
s_eq = s_ep .|. s_eo

s_hx :: Word16
s_hx = rightS s_he 1

s_kk :: Word16
s_kk = s_ki .|. s_kj

s_jv :: Word16
s_jv = complement s_ju 

s_en :: Word16
s_en = s_ek .&. s_em

s_kn :: Word16
s_kn = rightS s_kk 5

s_ei :: Word16
s_ei = complement s_eh 

s_hz :: Word16
s_hz = s_hx .|. s_hy

s_ec :: Word16
s_ec = s_ea .|. s_eb

s_w :: Word16
s_w = leftS s_s 15 

s_gh :: Word16
s_gh = rightS s_fo 1

s_kw :: Word16
s_kw = s_kk .|. s_kv

s_bq :: Word16
s_bq = rightS s_bn 5

s_ee :: Word16
s_ee = complement s_ed 

s_hu :: Word16
s_hu =  1 .&. s_ht 

s_cx :: Word16
s_cx = s_cu .&. s_cw

s_f :: Word16
s_f = rightS s_b 5

s_kt :: Word16
s_kt = s_kl .&. s_kr

s_ir :: Word16
s_ir = s_iq .|. s_ip

s_cj :: Word16
s_cj = rightS s_ci 2

s_cq :: Word16
s_cq = s_cj .|. s_cp

s_r :: Word16
s_r = s_o .&. s_q

s_dg :: Word16
s_dg = rightS s_dd 5

s_d :: Word16
s_d = rightS s_b 2

s_kv :: Word16
s_kv = s_ks .&. s_ku

s_e :: Word16
s_e = rightS s_b 3

s_k :: Word16
s_k = s_d .|. s_j

s_q :: Word16
s_q = complement s_p 

s_cs :: Word16
s_cs = complement s_cr 

s_dv :: Word16
s_dv = s_du .|. s_dt

s_kj :: Word16
s_kj = leftS s_kf 15 

s_ad :: Word16
s_ad = complement s_ac 

s_fr :: Word16
s_fr = rightS s_fo 5

s_il :: Word16
s_il = s_hz .|. s_ik

s_ka :: Word16
s_ka = s_jx .&. s_jz

s_gj :: Word16
s_gj = s_gh .|. s_gi

s_ld :: Word16
s_ld = rightS s_kk 1

s_ic :: Word16
s_ic = rightS s_hz 5

s_at :: Word16
s_at = rightS s_as 2

s_jz :: Word16
s_jz = complement s_jy 

s_an :: Word16
s_an =  1 .&. s_am 

s_cu :: Word16
s_cu = s_ci .|. s_ct

s_hj :: Word16
s_hj = s_hg .&. s_hh

s_jx :: Word16
s_jx = s_jq .|. s_jw

s_x :: Word16
s_x = s_v .|. s_w

s_le :: Word16
s_le = leftS s_la 15 

s_dk :: Word16
s_dk = s_dh .&. s_dj

s_ds :: Word16
s_ds = s_dp .&. s_dr

s_jy :: Word16
s_jy = s_jq .&. s_jw

s_aw :: Word16
s_aw = s_au .|. s_av

s_bg :: Word16
s_bg = complement s_bf 

s_ab :: Word16
s_ab = s_z .|. s_aa

s_gd :: Word16
s_gd = s_ga .&. s_gc

s_im :: Word16
s_im = s_hz .&. s_ik

s_jw :: Word16
s_jw = s_jt .&. s_jv

s_ac :: Word16
s_ac = s_z .&. s_aa

s_jt :: Word16
s_jt = s_jr .|. s_js

s_hv :: Word16
s_hv = leftS s_hb 1 

s_hm :: Word16
s_hm = s_hf .|. s_hl

s_id :: Word16
s_id = s_ib .|. s_ic

s_fs :: Word16
s_fs = s_fq .|. s_fr

s_ct :: Word16
s_ct = s_cq .&. s_cs

s_ih :: Word16
s_ih = s_ia .|. s_ig

s_dp :: Word16
s_dp = s_dd .|. s_do

s_l :: Word16
s_l = s_d .&. s_j

s_ie :: Word16
s_ie = s_ib .&. s_ic

s_au :: Word16
s_au = rightS s_as 3

s_bh :: Word16
s_bh = s_be .&. s_bg

s_dq :: Word16
s_dq = s_dd .&. s_do

s_m :: Word16
s_m = complement s_l 

s_ge :: Word16
s_ge =  1 .&. s_gd 

s_ag :: Word16
s_ag = s_y .&. s_ae

s_gb :: Word16
s_gb = s_fo .&. s_fz

s_if :: Word16
s_if = complement s_ie 

s_h :: Word16
s_h = s_e .&. s_f

s_z :: Word16
s_z = rightS s_x 3

s_af :: Word16
s_af = s_y .|. s_ae

s_hn :: Word16
s_hn = s_hf .&. s_hl

s_i :: Word16
s_i = complement s_h 

s_ho :: Word16
s_ho = complement s_hn 

s_hh :: Word16
s_hh = rightS s_he 5

someFunc :: IO ()
someFunc = do putStrLn ("Final value on wire 'a' is  : [ " ++ show s_a ++ " ] "  )



-- Hello, Haskell!
-- Final value on wire 'a' is  : [ 16076 ] 
-- Final value on wire 'a' is  : [ 2797 ] 
-- 
-- real	0m0.032s
-- user	0m0.014s
-- sys	0m0.002s

-- can take advantage of declarative nature of haskell
-- whereas in scheme each expression strictly evaluated as soon as assignment is met
-- whereass haskell can have multiple parallel definitions
-- they are not evaluated until required

