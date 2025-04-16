* current dynamic inputs
execute_load "tmp/dynamic_I.gdx" mm_I_dynamic MM_XCURR;


fprdgr('fcap-c-DT', A, 'DT', MM_T)   =   mm_I_dynamic('DT_ag_pol')/100;
fprdgr('fcap-c-NO', A, 'NO', MM_T)   =   mm_I_dynamic('NO_ag_pol')/100;
fprdgr('fcap-c-CE', A, 'CE', MM_T)   =   mm_I_dynamic('CE_ag_pol')/100;
fprdgr('fcap-c-SO', A, 'SO', MM_T)   =   mm_I_dynamic('SO_ag_pol')/100;
fprdgr('fcap-l-DT', A, 'DT', MM_T)   =   mm_I_dynamic('DT_ag_pol')/100;
fprdgr('fcap-l-NO', A, 'NO', MM_T)   =   mm_I_dynamic('NO_ag_pol')/100;
fprdgr('fcap-l-CE', A, 'CE', MM_T)   =   mm_I_dynamic('CE_ag_pol')/100;
fprdgr('fcap-l-SO', A, 'SO', MM_T)   =   mm_I_dynamic('SO_ag_pol')/100;
fprdgr('flnd-DT', A, 'DT', MM_T)   =   mm_I_dynamic('DT_ag_pol')/100;
fprdgr('flnd-NO', A, 'NO', MM_T)   =   mm_I_dynamic('NO_ag_pol')/100;
fprdgr('flnd-CE', A, 'CE', MM_T)   =   mm_I_dynamic('CE_ag_pol')/100;
fprdgr('flnd-SO', A, 'SO', MM_T)   =   mm_I_dynamic('SO_ag_pol')/100;

fprdgr('fcap-n-DT', A, 'DT', MM_T)   =   mm_I_dynamic('DT_non_ag_pol')/100;
fprdgr('fcap-n-NO', A, 'NO', MM_T)   =   mm_I_dynamic('NO_non_ag_pol')/100;
fprdgr('fcap-n-CE', A, 'CE', MM_T)   =   mm_I_dynamic('CE_non_ag_pol')/100;
fprdgr('fcap-n-SO', A, 'SO', MM_T)   =   mm_I_dynamic('SO_non_ag_pol')/100;
fprdgr('fcap-m-DT', A, 'DT', MM_T)   =   mm_I_dynamic('DT_non_ag_pol')/100;
fprdgr('fcap-m-NO', A, 'NO', MM_T)   =   mm_I_dynamic('NO_non_ag_pol')/100;
fprdgr('fcap-m-CE', A, 'CE', MM_T)   =   mm_I_dynamic('CE_non_ag_pol')/100;
fprdgr('fcap-m-SO', A, 'SO', MM_T)   =   mm_I_dynamic('SO_non_ag_pol')/100;






pwe_gr('ccrop', MM_T)=                 mm_I_dynamic('pw_ccrop')/100;
pwe_gr('coagr', MM_T)=                 mm_I_dynamic('pw_oagr')/100;
pwm_gr(C, MM_T) = pwe_gr(C, MM_T);


 AFXGR('acrop','DT') = FIXED('hpopgr_DT')/100;
 AFXGR('aoagr','DT') = FIXED('hpopgr_DT')/100;
 AFXGR('acrop','CE') = FIXED('hpopgr_CE')/100;
 AFXGR('aoagr','CE') = FIXED('hpopgr_CE')/100;
 AFXGR('acrop','NO') = FIXED('hpopgr_NO')/100;
 AFXGR('aoagr','NO') = FIXED('hpopgr_NO')/100;
 AFXGR('acrop','SO') = FIXED('hpopgr_SO')/100;
 AFXGR('aoagr','SO') = FIXED('hpopgr_SO')/100;
 AFXGR('acrop',RG) = 0;
 AFXGR('aoagr',RG) = 0;


 AFX(A,RG)$AFXGR(A,RG) =  YES;
 ANFX(A,RG)$(NOT AFX(A,RG)) = YES;

*----------------------------------------------------------------------
* Closures (valid for all scenarios)
*----------------------------------------------------------------------

*Macroeconomic closures
 NUMERAIRE = 3;
 ROWCLOS   = 3;
 SICLOS   = 3;

*Factor markets closures
 FMOBFE(FLND) = 1;

 FACTFE(FLAB)=0;


 FMOBFE(FCAP)=  1;
 FMOBUE(F) = 0;

*If no value is specified for a factor, impose FMOBFE:
 FMOBFE(F)$(FMOBFE(F) + FACTFE(F) + FMOBUE(F) EQ 0) = 1;

$include DCGEcore/3b.dynamic_loop.gms
