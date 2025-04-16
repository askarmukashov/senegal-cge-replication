* current static inputs
execute_load "tmp/static_I.gdx" mm_I_static;


prd_emigr('flab-NO') = mm_I_static('NO_prod_emigr');
prd_emigr('flab-CE') = mm_I_static('CE_prod_emigr');
prd_emigr('flab-SO') = mm_I_static('SO_prod_emigr');


$include DCGEcore/2b.static_part.gms
* calculations for the first year
 w_nat('flab')   = mm_I_static('reg_lab_elast_NO');
*  w_nat('flab')   =0.05;
 v_nat(flabnational)$w_nat(flabnational)  = (1/w_nat(flabnational)) + 1;

 qfs_rg0(flabnational, RG)$w_nat(flabnational) =
 SUM(FLAB, qfs0(FLAB)$flab_map(flabnational, FLAB, RG));

 qfs_rg_cal('flab', 'NO') = qfs_rg0('flab', 'NO')*0.915;
 qfs_rg_cal('flab', 'CE') = qfs_rg0('flab', 'CE')*0.92;
 qfs_rg_cal('flab', 'SO') = qfs_rg0('flab', 'SO')*1.05;

 qfs_rg_cal('flab', 'DT') = qfs_rg0('flab', 'DT')+
 qfs_rg0('flab', 'NO')*0.085 +
 qfs_rg0('flab', 'CE')*0.08 +
 qfs_rg0('flab', 'SO')*(-0.05);

 qfs_nat0(flabnational)$w_nat(flabnational) = SUM(RG, qfs_rg_cal(flabnational, RG));

* wages
 p_rg0(flabnational, RG)$qfs_rg_cal(flabnational, RG) =
 SUM(FLAB, wf0(FLAB)$flab_map(flabnational, FLAB, RG));

 p_rg.l(flabnational, RG)$qfs_rg_cal(flabnational, RG) = p_rg0(flabnational, RG) ;

 share_nat(flabnational, RG)$qfs_rg_cal(flabnational, RG) =
    (p_rg0(flabnational, RG)*(qfs_rg_cal(flabnational, RG))**(1-v_nat(flabnational)))/
    SUM(RGP, p_rg0(flabnational,RGP)*(qfs_rg_cal(flabnational,RGP))**(1-v_nat(flabnational))) ;

 alpha_nat(flabnational)$w_nat(flabnational) = qfs_nat0(flabnational)/
          (SUM(RG,share_nat(flabnational, RG)*qfs_rg_cal(flabnational, RG)**v_nat(flabnational)))**(1/v_nat(flabnational));

* same result for all flab
 P_ind0(flabnational)$w_nat(flabnational) =
 (qfs_nat0(flabnational)**(v_nat(flabnational)-1))*(alpha_nat(flabnational)**(-v_nat(flabnational)))*p_rg0(flabnational,'DT')*1/(qfs_rg_cal(flabnational, 'DT')**(v_nat(flabnational)-1)*share_nat(flabnational,'DT'));
   P_ind.l(flabnational)=P_ind0(flabnational);

 qfs_nat.l(flabnational) =  qfs_nat0(flabnational);

 qfs_rg.L(flabnational, RG) = qfs_rg0(flabnational, RG);
 QFSwithout(FLAB) =QFS0(FLAB) ;
 QFSwithout(FCAP) =QFS0(FCAP) ;

  beta1 = 2;
  beta2 = 2;
Pc_cons(H)$hpop(H) = (SUM(C, PQ0(C)*QH0(C, H))*1000000)/hpop(H);
Tot_cons(H)$hpop(H) = (SUM(C, PQ0(C)*QH0(C, H))*1000000);
net_balance(F)=0;
facVAsh(F,A,RG) = (QF0(F,A,RG)*WF0(F)*WFDIST0(F,A,RG))/
SUM((FP,AP,RGP), QF0(FP,AP,RGP)*WF0(FP)*WFDIST0(FP,AP,RGP));
