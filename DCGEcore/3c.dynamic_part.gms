* changes for next period
 alphava(A, RG) =        alphava(A, RG) * (1 + alphavagr(A, RG, MM_T));
* first, policies
 fprd(F,A,RG)   = fprd(F,A,RG) * (1 + fprdgr(F, A, RG, MM_T));
* then climate
fprdgr(F, A, RG, MM_T)=0;
fprdgr(FLND, A, 'DT', MM_T) =        mm_I_dynamic('DT')/100;
fprdgr(FLND, A, 'NO', MM_T) =        mm_I_dynamic('NO')/100;
fprdgr(FLND, A, 'CE', MM_T) =        mm_I_dynamic('CE')/100;
fprdgr(FLND, A, 'SO', MM_T) =        mm_I_dynamic('SO')/100;
fprdgr('fcap-c-DT', A, 'DT', MM_T) =        mm_I_dynamic('DT')/100;
fprdgr('fcap-c-NO', A, 'NO', MM_T) =        mm_I_dynamic('NO')/100;
fprdgr('fcap-c-CE', A, 'CE', MM_T) =        mm_I_dynamic('CE')/100;
fprdgr('fcap-c-SO', A, 'SO', MM_T) =        mm_I_dynamic('SO')/100;
fprdgr('fcap-l-DT', A, 'DT', MM_T) =        mm_I_dynamic('DT')/100;
fprdgr('fcap-l-NO', A, 'NO', MM_T) =        mm_I_dynamic('NO')/100;
fprdgr('fcap-l-CE', A, 'CE', MM_T) =        mm_I_dynamic('CE')/100;
fprdgr('fcap-l-SO', A, 'SO', MM_T) =        mm_I_dynamic('SO')/100;

fprd(F,A,RG)   = fprd(F,A,RG) * (1 + fprdgr(F, A, RG, MM_T));

 pwe(C) =        pwe(C) * (1 + pwe_gr(C, MM_T));
 pwm(C) =        pwm(C) * (1 + pwm_gr(C, MM_T));
 QFS.L(FLND) =  QFS.L(FLND) * (1 + QFSgr(FLND, MM_T));
 QFSwithout(FLAB) = QFSwithout(FLAB)*(1 + QFSgr(FLAB, MM_T)) ;
 hpop_without(H)   =       hpop_without(H) * (1 + hpopgr(H, MM_T));
  hpop(H)  = hpop_without(H) ;

 QFS.L(FLAB) = QFS.L(FLAB) * (1 + QFSgr(FLAB, MM_T));
 hpopprev = SUM(H, hpop(H));

 FSAV.L =        FSAV.L * (1 + FSAVgr(MM_T));
* assume real public goods and serv are growing proportional to pop growth
  GADJ.L = GADJ.l * (SUM(H, hpop(H)) / hpopprev);



******* capital
*Capital accumulation -----------------------------------
*$ontext
*Capital accumulation -----------------------------------
*For capital that is not entirely fixed
     CAPSHR1TOT              = SUM((FCAP,A,RG)$ANFX(A,RG), QF.L(FCAP,A,RG));
     CAPSHR1(FCAP)           = SUM((RG,A)$ANFX(A,RG), QF.L(FCAP,A,RG))/CAPSHR1TOT;
     CAPSHR2TOT(FCAP)        = SUM((RG,A)$ANFX(A,RG), QF.L(FCAP,A,RG));
     CAPSHR2(FCAP,A,RG)$(ANFX(A,RG) AND CAPSHR2TOT(FCAP)) = QF.L(FCAP,A,RG)/CAPSHR2TOT(FCAP);
     QINVK                   = alphainv*PROD(CP, QINV.L(CP)**iwts(CP));
*     NGFCF                   = QINVK ;
     NGFCF                   = QINVK - SUM((RG,A,FCAPP)$AFX(A,RG), QF.L(FCAPP,A,RG)*AFXGR(A,RG));
     WFADJ(FCAP)$CAPSHR2TOT(FCAP) = SUM((RG,A)$ANFX(A,RG), WF.L(FCAP)*WFDIST.L(FCAP,A,RG)*QF.L(FCAP,A,RG))/SUM((RG,A), QF.L(FCAP,A,RG));
     WFDISTADJ(FCAP,A,RG)$(ANFX(A,RG) AND WFADJ(FCAP)) = WF.L(FCAP)*WFDIST.L(FCAP,A,RG)/WFADJ(FCAP);
     WFK2AV(FCAP)            = SUM((RG,A)$ANFX(A,RG), WFADJ(FCAP)*WFDISTADJ(FCAP,A,RG)*CAPSHR2(FCAP,A,RG));
     WFDIST2(FCAP,A,RG)$(ANFX(A,RG) AND WFK2AV(FCAP)) = WFADJ(FCAP)*WFDISTADJ(FCAP,A,RG)/WFK2AV(FCAP);
     WFK1AV                  = SUM(FCAPP, WFK2AV(FCAPP)*CAPSHR1(FCAPP));
* with
     INVSHR1(FCAP)           = CAPSHR1(FCAP) * (1 + beta1 * ( (WFK2AV(FCAP)-WFK1AV) / WFK1AV ) ) ;
     INVSHR2(FCAP,A,RG) = CAPSHR2(FCAP,A,RG)*(1 + beta2*(WFDIST2(FCAP,A,RG) - 1)) ;
     DKAPS(FCAP)             = INVSHR1(FCAP)*NGFCF;
     DKAP(FCAP,A,RG) = INVSHR2(FCAP,A,RG)*DKAPS(FCAP) ;
     RKAPS(FCAP)$QFS0(FCAP)             = DKAPS(FCAP)/QFS.L(FCAP) - SUM((RG,A), natdrate*QF.L(FCAP,A,RG)/SUM((RGP,AP), QF.L(FCAP,AP,RGP)));
     RKAP(FCAP,A,RG)$QF0(FCAP,A,RG) = DKAP(FCAP,A,RG)/QF.L(FCAP,A,RG) - natdrate;
*For capital that is entirely fixed
     RKAP(FCAP,A,RG)$(AFX(A,RG) AND QF0(FCAP,A,RG)) = AFXGR(A,RG);
*Apply annual growth rates
     QF.L(FCAP,A,RG) = QF.L(FCAP,A,RG)*(1 + RKAP(FCAP,A,RG));
     QFS.L(FCAP) = SUM((RG,A), QF.L(FCAP,A,RG));
* without
     QFSwithout(FCAP) = QFSwithout(FCAP) + NGFCF*(WF0(FCAP)*QFS0(FCAP)/SUM(FCAPP, WF0(FCAPP)*QFS0(FCAPP)));
     net_balance(FCAP) = QFS.L(FCAP)       -  QFSwithout(FCAP);
*$ontext
* distrubute net_balance (very different from FLAB!!!)
*$ontext
LOOP(FCAP,
IF(net_balance(FCAP) > 0,
native_cap(INSDNG)=no;
native_cap(INSDNG)$shif0(INSDNG,FCAP)  = YES;
ownership(INSDNG)=0;
ownership(native_cap) = shif0(native_cap,FCAP)*QFSwithout(FCAP);

foreign_cap(INSDNG)=NO;
foreign_cap(INSDNG)$(SUM((fcapnational,FCAPP, RG),
shif0(INSDNG,FCAPP)*QFSwithout(FCAPP)$fcap_map(fcapnational, FCAPP, RG))
AND NOT native_cap(INSDNG))        = YES;

dist(INSDNG) = 0;
foreigners_assets(INSDNG)=0;
* here based on total capital shares, no separation by types !!!
foreigners_assets(foreign_cap) = SUM((fcapnational,FCAPP, RG),
(shif0(foreign_cap,FCAPP)*(QFSwithout(FCAPP) ))$fcap_map(fcapnational, FCAPP, RG));

dist(foreign_cap)$SUM(foreign_capP, foreigners_assets(foreign_capP)) =
foreigners_assets(foreign_cap)/SUM(foreign_capP, foreigners_assets(foreign_capP));

ownership(foreign_cap) = net_balance(FCAP)*dist(foreign_cap);
shif(INSDNG,FCAP) = ownership(INSDNG) /
     SUM(INSDNGP, ownership(INSDNGP) ) ;
);
);


qfs_nat.fx(flabnational)$w_nat(flabnational) =  SUM((FLAB,RG), QFSwithout(FLAB)$flab_map(flabnational, FLAB, RG));
$include DCGEcore/3d.closures.inc
 QFS.LO(FLAB)$SUM((flabnational,RG), qfs_rg0(flabnational,RG)$flab_map(flabnational, FLAB, RG)) = -INF;
 QFS.UP(FLAB)$SUM((flabnational,RG), qfs_rg0(flabnational,RG)$flab_map(flabnational, FLAB, RG)) = +INF;

 YIF.LO(INS,F)$(shif(INS,F))     = -INF;
 YIF.UP(INS,F)$(shif(INS,F))     = +INF;
 YIF.FX(INS,F)$(NOT shif(INS,F))     = 0;

 TINS.lo(H)$(hpop(H))     = -INF;
 TINS.UP(H)$(hpop(H))     = +INF;
 TINS.FX(H)$(NOT hpop(H))     = 0;

 MPS.lo(H)$(hpop(H))     = -INF;
 MPS.UP(H)$(hpop(H))     = +INF;
 MPS.FX(H)$(NOT hpop(H))     = 0;

 QH.LO(C,H)$(betam(C,H))     = -INF;
 QH.UP(C,H)$(betam(C,H))      = +INF;
 QH.FX(C, H)$(NOT betam(C,H))     = 0;


option iterlim = 50;
  SOLVE CET_macro USING MCP;
 if((CET_macro.solvestat = 1) AND (CET_macro.modelstat = 1),

$ontext
* ACET update
 share_nat(flabnational, RG)$QFS_rg0(flabnational, RG) =
    (p_rg.l(flabnational, RG)*(QFS_rg.l(flabnational, RG))**(1-v_nat(flabnational)))/
    SUM(RGP, p_rg.l(flabnational,RGP)*(QFS_rg.l(flabnational,RGP))**(1-v_nat(flabnational))) ;

 alpha_nat(flabnational)$w_nat(flabnational) = qfs_nat.l(flabnational)/
          (SUM(RG,share_nat(flabnational, RG)*QFS_rg.l(flabnational, RG)**v_nat(flabnational)))**(1/v_nat(flabnational));
$offtext

  GDP = SUM((A,RG), PVA.L(A,RG)*(1-tva(A,RG))*QVA.L(A,RG));
  GDPS(A, RG) = (PVA.L(A,RG)*(1-tva(A,RG))*QVA.L(A,RG)) / GDP;
* stock of emigrants
 net_balance(FLAB) = QFS.L(FLAB)       -  QFSwithout(FLAB);
LOOP(FLAB,
type_nat(facnational)=NO;
type_nat(flabnational)$SUM(RG, flab_map(flabnational, FLAB, RG)) =YES;

IF(net_balance(FLAB) > 0,
native(H)=no;
native(H)$shif0(H,FLAB)  = YES;
ownership(INSDNG)=0;
foreigners_assets(INSDNG)=0;
dist(INSDNG) = 0;
foreign(H)=NO;
act(RG)=NO;
immigr_H(H)= NO;
shif_pre(immigr_H,FLAB)=NO;

* establish ownership(native) in QFS.L(FLAB)
ownership(native) = shif0(native,FLAB)*QFSwithout(FLAB);
shif_pre(native,FLAB) = ownership(native) / QFS.L(FLAB) ;
* now: establish ownership(foreign) in QFS.L(FLAB)

* all other owners that had type_nat
foreign(H)$(NOT native(H) AND
SUM((type_nat,FLABP, RG),
shif0(H,FLABP)$flab_map(type_nat, FLABP, RG)))        = YES;

* emigrants!!!
foreigners_assets(foreign) = SUM((type_nat,FLABP, RG),
(shif0(foreign,FLABP)*(QFSwithout(FLABP) - QFS.L(FLABP)))$flab_map(type_nat, FLABP, RG));
foreigners_assets(foreign)$(foreigners_assets(foreign)<=0)=NO;

dist(foreign)$SUM(foreignP, foreigners_assets(foreignP))
 = foreigners_assets(foreign)/SUM(foreignP, foreigners_assets(foreignP));

ownership(foreign) = net_balance(FLAB)*dist(foreign);
foreign(H)$(NOT ownership(H)) = NO;

* active region

act(RG)$SUM(type_nat, flab_map(type_nat, FLAB,RG)) =YES;

* assign current immigrants to active region
immigr_H(H)$(
    SUM((foreign,act),
    emigr_immigr(foreign, H,act))
    )=  YES;
ownership(immigr_H) =
 SUM((foreign,act),
    ownership(foreign)$emigr_immigr(foreign, immigr_H,act)
  );
ownership(foreign)=NO;

* works
shif_pre(immigr_H,FLAB)=NO;
shif_pre(immigr_H,FLAB)$ownership(immigr_H) = ownership(immigr_H) /
     SUM((type_nat,act), QFS.L(FLAB)$flab_map(type_nat, FLAB, act)) ;
* check that labor receiving region cannot have emigrants abroad
shif_pre(H,FLAB)$(NOT ownership(H)) = 0;

****** population
hpop(immigr_H)  = ownership(immigr_H);
hpop(foreign)  = hpop(foreign) - SUM((immigr_H,act), hpop(immigr_H)$emigr_immigr(foreign, immigr_H, act));
ELSE
* if balance is negative
* for THIS FLAB: everything is for natives
shif_pre(natH,FLAB) = shif0(natH,FLAB);
* migrants get nothing
shif_pre(migrh,FLAB)=0;
*end condition
);

*end loop
);

share_emigr_of_mob_flab2(FLAB)=0;
share_emigr_of_mob_flab2(FLAB)$(net_balance(FLAB)<0) =
  -net_balance(FLAB)/QFSwithout(FLAB);

fprd_adj(FLAB)=1;
* prod in sending regions (if productivity of emigrated relative to remained is higher)
fprd_adj(FLAB)$share_emigr_of_mob_flab2(FLAB)=
(1-prd_emigr(FLAB)*share_emigr_of_mob_flab2(FLAB))/
(1-share_emigr_of_mob_flab2(FLAB))
;

* prod in DT (if productivity of imigrated relative to DT is lower)
fprd_adj('flab-DT') =
shif_pre('hhd-NO-DT','flab-DT')*mm_I_static('NO_prod_imigr')+
shif_pre('hhd-CE-DT','flab-DT')*mm_I_static('CE_prod_imigr')+
shif_pre('hhd-SO-DT','flab-DT')*mm_I_static('SO_prod_imigr')+
shif_pre('hhd-DT','flab-DT')*1;

* correct shif
shif('hhd-NO-DT','flab-DT') = (shif_pre('hhd-NO-DT','flab-DT')*mm_I_static('NO_prod_imigr'))
 / fprd_adj('flab-DT')  ;
shif('hhd-CE-DT','flab-DT') = (shif_pre('hhd-CE-DT','flab-DT')*mm_I_static('CE_prod_imigr'))
/ fprd_adj('flab-DT')  ;
shif('hhd-SO-DT','flab-DT') = (shif_pre('hhd-SO-DT','flab-DT')*mm_I_static('SO_prod_imigr'))
 / fprd_adj('flab-DT')  ;

shif('hhd-DT','flab-DT') = 1 -shif('hhd-NO-DT','flab-DT') -shif('hhd-CE-DT','flab-DT') -
shif('hhd-SO-DT','flab-DT');

share_emigr_of_tot(migrh) =0;
share_emigr_of_tot(migrh)$hpop(migrh) =
hpop(migrh)/
SUM((H,rg), hpop_without(H)$emigr_immigr(H, migrh, rg));


* !!! shii(INSDNG,INSDNGP) of migr_H = 0
* works
* % of mobile flab
share_emigr_of_mob_flab(migrh)=0;
share_emigr_of_mob_flab(migrh)$hpop(migrh) =
hpop(migrh)/
sum((natH, FLABP, rg),
  QFSwithout(FLABP)*shif0(natH,FLABP)$
  (
    immigr_orig_FLAB(migrh, FLABP)
    and
  emigr_immigr(natH, migrh, rg)
  )
  );


* % of total
share_emigr_of_tot2(migrh) = 0;
share_emigr_of_tot2(migrh)$hpop(migrh)=
hpop(migrh)/
sum((natH, FLABP, rg),
  QFSwithout(FLABP)*shif0(natH,FLABP)$
  emigr_immigr(natH, migrh, rg)
  );


$ontext
trnsfr(migrh, GOV) = share_emigr_of_tot(migrh)*
SUM((H,rg), trnsfr0(H, GOV)$emigr_immigr(H, migrh, rg))
;

trnsfr(migrh, ROW) = share_emigr_of_tot(migrh)*
SUM((H,rg), trnsfr0(H, ROW)$emigr_immigr(H, migrh, rg))
;

trnsfr(natH, GOV) = trnsfr0(natH, GOV) - SUM((migrh,rg),trnsfr(migrh, GOV)$emigr_immigr(natH, migrh, rg));
trnsfr(natH, ROW) = trnsfr0(natH, ROW) - SUM((migrh,rg),trnsfr(migrh, ROW)$emigr_immigr(natH, migrh, rg));
$offtext


*LES re-calibration like destination counterpart
LESELAS1(C, migrh) =0;
LESELAS1(C, migrh)$hpop(migrh) =
SUM(HP,
LESELAS1(C, HP)$immigr_dest(migrh, HP));

 FRISCH(migrh)=0;
 FRISCH(migrh)$hpop(migrh)=
 SUM(HP,
 FRISCH(HP)$immigr_dest(migrh, HP));

 BUD(migrh)=0;
 BUD(migrh)$hpop(migrh) = SUM(FLABP, 0.98*shif(migrh,FLABP)*wf.l(FLABP));

 BUDSHR(C,migrh)=0;
 BUDSHR(C,migrh)$hpop(migrh)    = SUM(HP, BUDSHR(C, HP)$immigr_dest(migrh, HP));

 BUDSHRCHK(migrh)=0;
 BUDSHRCHK(migrh)$hpop(migrh)   = SUM(C, BUDSHR(C,migrh)) - 1 ;
 ELASCHK(migrh) = 0;
 ELASCHK(migrh)$hpop(migrh)     = SUM(C, BUDSHR(C,migrh)*LESELAS1(C,migrh)) - 1 ;

*Correct elasticities to make them satisfy Engle aggregation exactly
 LESELAS1(C,migrh)$LESELAS1(C,migrh)   = LESELAS1(C,migrh)/(ELASCHK(migrh) + 1);

 betam(C,migrh) = 0;
 betam(C,migrh)$hpop(migrh)   = BUDSHR(C,migrh)*LESELAS1(C,migrh);

 gammam(C,migrh) = 0;
 gammam(C,migrh)$betam(C,migrh)
     =  ( BUD(migrh)  / PQ0(C) )
                      * ( BUDSHR(C,migrh) + betam(C,migrh)/FRISCH(migrh));
MPS.fx(migrh) = 0;
MPS.fx(migrh)$hpop(migrh) =
  SUM(HP,
  MPS.l(HP)$immigr_dest(migrh, HP));

TINS.fx(migrh)=0;
TINS.fx(migrh)$hpop(migrh) =
  SUM(HP,
  TINS.l(HP)$immigr_dest(migrh, HP));

Pc_cons(H)$hpop(H) = (SUM(C, PQ0(C)*QH.l(C, H))*1000000)/hpop(H);
Pc_cons(H)$(NOT hpop(H)) = 0;

Tot_cons(H)$hpop(H) = (SUM(C, PQ0(C)*QH.l(C, H))*1000000);
Tot_cons(H)$(NOT hpop(H)) = 0;
facVAsh(F,A,RG) = (QF.l(F,A,RG)*WF.l(F)*WFDIST.l(F,A,RG))/
SUM((FP,AP,RGP), QF.l(FP,AP,RGP)*WF.l(FP)*WFDIST.l(FP,AP,RGP));

$BATINCLUDE DCGEcore/3e.results.inc MM_T
* if not solved
ELSE
    stop=1;
    mm_suc(MM_T, hZ)=NO;
  );
