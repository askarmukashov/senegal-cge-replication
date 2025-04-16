*----------------- z calculations
* no home cons
mm_suc(MM_T, 'ZMAC1')$(hZ('ZMAC1'))  =
SUM((C,H), PQ0(C)*QHX(C,H, MM_T)) +
 SUM(C, PQ0(C)*QINVX(C, MM_T))+
 SUM(C, PQ0(C)*QDSTX(C,MM_T)) +
  SUM(C, PQ0(C)*QGX(C,MM_T))+
  SUM(CE, PWE0(CE)*EXR0*QEX(CE,MM_T))
    -SUM(CM, PWM0(CM)*EXR0*QMX(CM,MM_T))
 ;
mm_suc(MM_T, 'ZMAC2')$(hZ('ZMAC2'))  = SUM((C,H), PQ0(C)*QHX(C,H, MM_T))  ;
mm_suc(MM_T, 'ZMAC3')$(hZ('ZMAC3'))  =  SUM(C, PQ0(C)*QINVX(C, MM_T));
mm_suc(MM_T, 'ZMAC4')$(hZ('ZMAC4'))  =  SUM(C, PQ0(C)*QDSTX(C,MM_T));
mm_suc(MM_T, 'ZMAC5')$(hZ('ZMAC5'))  =  SUM(C, PQ0(C)*QGX(C,MM_T));
mm_suc(MM_T, 'ZMAC6')$(hZ('ZMAC6'))  =  SUM(CE, PWE0(CE)*EXR0*QEX(CE,MM_T));
mm_suc(MM_T, 'ZMAC7')$(hZ('ZMAC7'))  = -SUM(CM, PWM0(CM)*EXR0*QMX(CM,MM_T));


mm_suc(MM_T, 'ZMAC8')$(hZ('ZMAC8'))  = mm_suc(MM_T, 'ZMAC1')$(hZ('ZMAC1')) -
mm_suc(MM_T, 'ZMAC7')$(hZ('ZMAC7'))-
mm_suc(MM_T, 'ZMAC6')$(hZ('ZMAC6')) ;

mm_suc(MM_T, 'ZMAC9')$(hZ('ZMAC9'))  =
            SUM((A,RG), ta0(A,RG)*PAR0(A,RG)*QARX(A,RG, MM_T))
          + SUM((A,RG), tva0(A,RG)*PVA0(A,RG)*QVAX(A,RG,MM_T))
          + SUM(CM, tm0(CM)*PWM0(CM)*EXR0*QMX(CM,MM_T))
          + SUM(CE, te0(CE)*PWE0(CE)*EXR0*QEX(CE,MM_T))
          + SUM(C, tq0(C)*PQ0(C)*QQX(C,MM_T));




* GDP FC
mm_suc(MM_T, 'ZGDP1')$(hZ('ZGDP1'))  =
 sum((A, RG), PVA0(A,RG)*(1-tva0(A,RG))*QVAX(A,RG, MM_T));

mm_suc(MM_T, 'ZGDP2')$(hZ('ZGDP2'))  =
 sum((AAGR, RG), PVA0(AAGR,RG)*(1-tva0(AAGR,RG))*QVAX(AAGR,RG, MM_T));

mm_suc(MM_T, 'ZGDP3')$(hZ('ZGDP3'))  =  sum(RG, PVA0('acrop',RG)*(1-tva0('acrop',RG))*QVAX('acrop',RG, MM_T));
mm_suc(MM_T, 'ZGDP4')$(hZ('ZGDP4'))  =  sum(RG, PVA0('aoagr',RG)*(1-tva0('aoagr',RG))*QVAX('aoagr',RG, MM_T));

mm_suc(MM_T, 'ZGDP5')$(hZ('ZGDP5'))  =
sum(RG, PVA0('amini',RG)*(1-tva0('amini',RG))*QVAX('amini',RG, MM_T));

mm_suc(MM_T, 'ZGDP6')$(hZ('ZGDP6'))  =
 sum((RG,AAGRPR), PVA0(AAGRPR, RG)*(1-tva0(AAGRPR,RG))*QVAX(AAGRPR,RG, MM_T));

mm_suc(MM_T, 'ZGDP7')$(hZ('ZGDP7'))  =  sum(RG, PVA0('aoprc',RG)*(1-tva0('aoprc',RG))*QVAX('aoprc',RG, MM_T));
mm_suc(MM_T, 'ZGDP8')$(hZ('ZGDP8'))  =  sum(RG, PVA0('acprc',RG)*(1-tva0('acprc',RG))*QVAX('acprc',RG, MM_T));

mm_suc(MM_T, 'ZGDP9')$(hZ('ZGDP9')) =
 sum((RG,AOIND), PVA0(AOIND,RG)*(1-tva0(AOIND,RG))*QVAX(AOIND,RG, MM_T));

mm_suc(MM_T, 'ZGDP10')$(hZ('ZGDP10'))  =        sum(RG, PVA0('aolig',RG)*(1-tva0('aolig',RG))*QVAX('aolig',RG, MM_T));
mm_suc(MM_T, 'ZGDP11')$(hZ('ZGDP11'))  =        sum(RG, PVA0('aoman',RG)*(1-tva0('aoman',RG))*QVAX('aoman',RG, MM_T));
mm_suc(MM_T, 'ZGDP12')$(hZ('ZGDP12'))  =        sum(RG, PVA0('autil',RG)*(1-tva0('autil',RG))*QVAX('autil',RG, MM_T));
mm_suc(MM_T, 'ZGDP13')$(hZ('ZGDP13'))  =        sum(RG, PVA0('acons',RG)*(1-tva0('acons',RG))*QVAX('acons',RG, MM_T));

mm_suc(MM_T, 'ZGDP14')$(hZ('ZGDP14'))  =
 sum((RG, APSERV), PVA0(APSERV,RG)*(1-tva0(APSERV,RG))*QVAX(APSERV,RG, MM_T));

mm_suc(MM_T, 'ZGDP15')$(hZ('ZGDP15'))  =        sum(RG, PVA0('atrad',RG)*(1-tva0('atrad',RG))*QVAX('atrad',RG, MM_T));
mm_suc(MM_T, 'ZGDP16')$(hZ('ZGDP16'))  =        sum(RG, PVA0('atran',RG)*(1-tva0('atran',RG))*QVAX('atran',RG, MM_T));
mm_suc(MM_T, 'ZGDP17')$(hZ('ZGDP17'))  =        sum(RG, PVA0('aacfo',RG)*(1-tva0('aacfo',RG))*QVAX('aacfo',RG, MM_T));
mm_suc(MM_T, 'ZGDP18')$(hZ('ZGDP18'))  =        sum(RG, PVA0('acomm',RG)*(1-tva0('acomm',RG))*QVAX('acomm',RG, MM_T));
mm_suc(MM_T, 'ZGDP19')$(hZ('ZGDP19'))  =        sum(RG, PVA0('afsrv',RG)*(1-tva0('afsrv',RG))*QVAX('afsrv',RG, MM_T));
mm_suc(MM_T, 'ZGDP20')$(hZ('ZGDP20'))  =        sum(RG, PVA0('areal',RG)*(1-tva0('areal',RG))*QVAX('areal',RG, MM_T));
mm_suc(MM_T, 'ZGDP21')$(hZ('ZGDP21'))  =        sum(RG, PVA0('absrv',RG)*(1-tva0('absrv',RG))*QVAX('absrv',RG, MM_T));
mm_suc(MM_T, 'ZGDP22')$(hZ('ZGDP22'))  =        sum(RG, PVA0('aeduc',RG)*(1-tva0('aeduc',RG))*QVAX('aeduc',RG, MM_T));
mm_suc(MM_T, 'ZGDP23')$(hZ('ZGDP23'))  =        sum(RG, PVA0('aosrv',RG)*(1-tva0('aosrv',RG))*QVAX('aosrv',RG, MM_T));

mm_suc(MM_T, 'ZGDP24')$(hZ('ZGDP24'))  =
 sum(RG, PVA0('apadm',RG)*(1-tva0('apadm',RG))*QVAX('apadm',RG, MM_T));

mm_suc(MM_T, 'ZGDP2DT')$(hZ('ZGDP2DT'))  =      sum(AAGR, PVA0(AAGR, 'DT')*(1-tva0(AAGR, 'DT'))*QVAX(AAGR, 'DT', MM_T));
*mm_suc(MM_T, 'ZGDP2TH')$(hZ('ZGDP2TH'))  =      sum(AAGR, PVA0(AAGR, 'TH')*(1-tva0(AAGR, 'TH'))*QVAX(AAGR, 'TH', MM_T));
mm_suc(MM_T, 'ZGDP2NO')$(hZ('ZGDP2NO'))  =      sum(AAGR, PVA0(AAGR, 'NO')*(1-tva0(AAGR, 'NO'))*QVAX(AAGR, 'NO', MM_T));
mm_suc(MM_T, 'ZGDP2CE')$(hZ('ZGDP2CE'))  =      sum(AAGR, PVA0(AAGR, 'CE')*(1-tva0(AAGR, 'CE'))*QVAX(AAGR, 'CE', MM_T));
mm_suc(MM_T, 'ZGDP2SO')$(hZ('ZGDP2SO'))  =      sum(AAGR, PVA0(AAGR, 'SO')*(1-tva0(AAGR, 'SO'))*QVAX(AAGR, 'SO', MM_T));
mm_suc(MM_T, 'ZGDP5DT')$(hZ('ZGDP5DT'))  =       PVA0('amini', 'DT')*(1-tva0('amini', 'DT'))*QVAX('amini', 'DT', MM_T);
*mm_suc(MM_T, 'ZGDP5TH')$(hZ('ZGDP5TH'))  =       PVA0('amini', 'TH')*(1-tva0('amini', 'TH'))*QVAX('amini', 'TH', MM_T);
mm_suc(MM_T, 'ZGDP5NO')$(hZ('ZGDP5NO'))  =       PVA0('amini', 'NO')*(1-tva0('amini', 'NO'))*QVAX('amini', 'NO', MM_T);
mm_suc(MM_T, 'ZGDP5CE')$(hZ('ZGDP5CE'))  =       PVA0('amini', 'CE')*(1-tva0('amini', 'CE'))*QVAX('amini', 'CE', MM_T);
mm_suc(MM_T, 'ZGDP5SO')$(hZ('ZGDP5SO'))  =       PVA0('amini', 'SO')*(1-tva0('amini', 'SO'))*QVAX('amini', 'SO', MM_T);
mm_suc(MM_T, 'ZGDP6DT')$(hZ('ZGDP6DT'))  =      sum(AAGRPR, PVA0(AAGRPR, 'DT')*(1-tva0(AAGRPR, 'DT'))*QVAX(AAGRPR, 'DT', MM_T));
*mm_suc(MM_T, 'ZGDP6TH')$(hZ('ZGDP6TH'))  =      sum(AAGRPR, PVA0(AAGRPR, 'TH')*(1-tva0(AAGRPR, 'TH'))*QVAX(AAGRPR, 'TH', MM_T));
mm_suc(MM_T, 'ZGDP6NO')$(hZ('ZGDP6NO'))  =      sum(AAGRPR, PVA0(AAGRPR, 'NO')*(1-tva0(AAGRPR, 'NO'))*QVAX(AAGRPR, 'NO', MM_T));
mm_suc(MM_T, 'ZGDP6CE')$(hZ('ZGDP6CE'))  =      sum(AAGRPR, PVA0(AAGRPR, 'CE')*(1-tva0(AAGRPR, 'CE'))*QVAX(AAGRPR, 'CE', MM_T));
mm_suc(MM_T, 'ZGDP6SO')$(hZ('ZGDP6SO'))  =      sum(AAGRPR, PVA0(AAGRPR, 'SO')*(1-tva0(AAGRPR, 'SO'))*QVAX(AAGRPR, 'SO', MM_T));
mm_suc(MM_T, 'ZGDP9DT')$(hZ('ZGDP9DT'))  =      sum(AOIND, PVA0(AOIND, 'DT')*(1-tva0(AOIND, 'DT'))*QVAX(AOIND, 'DT', MM_T));
*mm_suc(MM_T, 'ZGDP9TH')$(hZ('ZGDP9TH'))  =      sum(AOIND, PVA0(AOIND, 'TH')*(1-tva0(AOIND, 'TH'))*QVAX(AOIND, 'TH', MM_T));
mm_suc(MM_T, 'ZGDP9NO')$(hZ('ZGDP9NO'))  =      sum(AOIND, PVA0(AOIND, 'NO')*(1-tva0(AOIND, 'NO'))*QVAX(AOIND, 'NO', MM_T));
mm_suc(MM_T, 'ZGDP9CE')$(hZ('ZGDP9CE'))  =      sum(AOIND, PVA0(AOIND, 'CE')*(1-tva0(AOIND, 'CE'))*QVAX(AOIND, 'CE', MM_T));
mm_suc(MM_T, 'ZGDP9SO')$(hZ('ZGDP9SO'))  =      sum(AOIND, PVA0(AOIND, 'SO')*(1-tva0(AOIND, 'SO'))*QVAX(AOIND, 'SO', MM_T));
mm_suc(MM_T, 'ZGDP14DT')$(hZ('ZGDP14DT'))  =    sum(APSERV, PVA0(APSERV, 'DT')*(1-tva0(APSERV, 'DT'))*QVAX(APSERV, 'DT', MM_T));
*mm_suc(MM_T, 'ZGDP14TH')$(hZ('ZGDP14TH'))  =    sum(APSERV, PVA0(APSERV, 'TH')*(1-tva0(APSERV, 'TH'))*QVAX(APSERV, 'TH', MM_T));
mm_suc(MM_T, 'ZGDP14NO')$(hZ('ZGDP14NO'))  =    sum(APSERV, PVA0(APSERV, 'NO')*(1-tva0(APSERV, 'NO'))*QVAX(APSERV, 'NO', MM_T));
mm_suc(MM_T, 'ZGDP14CE')$(hZ('ZGDP14CE'))  =    sum(APSERV, PVA0(APSERV, 'CE')*(1-tva0(APSERV, 'CE'))*QVAX(APSERV, 'CE', MM_T));
mm_suc(MM_T, 'ZGDP14SO')$(hZ('ZGDP14SO'))  =    sum(APSERV, PVA0(APSERV, 'SO')*(1-tva0(APSERV, 'SO'))*QVAX(APSERV, 'SO', MM_T));
mm_suc(MM_T, 'ZGDP24DT')$(hZ('ZGDP24DT'))  =    PVA0('apadm', 'DT')*(1-tva0('apadm', 'DT'))*QVAX('apadm', 'DT', MM_T);
*mm_suc(MM_T, 'ZGDP24TH')$(hZ('ZGDP24TH'))  =    PVA0('apadm', 'TH')*(1-tva0('apadm', 'TH'))*QVAX('apadm', 'TH', MM_T);
mm_suc(MM_T, 'ZGDP24NO')$(hZ('ZGDP24NO'))  =  PVA0('apadm', 'NO')*(1-tva0('apadm', 'NO'))*QVAX('apadm', 'NO', MM_T);
mm_suc(MM_T, 'ZGDP24CE')$(hZ('ZGDP24CE'))  =    PVA0('apadm', 'CE')*(1-tva0('apadm', 'CE'))*QVAX('apadm', 'CE', MM_T);
mm_suc(MM_T, 'ZGDP24SO')$(hZ('ZGDP24SO'))  =    PVA0('apadm', 'SO')*(1-tva0('apadm', 'SO'))*QVAX('apadm', 'SO', MM_T);


mm_suc(MM_T, 'ZEMP1')$(hZ('ZEMP1'))  =  SUM((FLAB, RG), QFX(FLAB,'acrop',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP2')$(hZ('ZEMP2'))  =  SUM((FLAB, RG), QFX(FLAB,'aoagr',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP3')$(hZ('ZEMP3'))  =  SUM((FLAB, RG), QFX(FLAB,'amini',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP4')$(hZ('ZEMP4'))  =  SUM((FLAB, RG), QFX(FLAB,'aoprc',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP5')$(hZ('ZEMP5'))  =  SUM((FLAB, RG), QFX(FLAB,'acprc',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP6')$(hZ('ZEMP6'))  =  SUM((FLAB, RG), QFX(FLAB,'aolig',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP7')$(hZ('ZEMP7'))  =  SUM((FLAB, RG), QFX(FLAB,'aoman',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP8')$(hZ('ZEMP8'))  =  SUM((FLAB, RG), QFX(FLAB,'autil',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP9')$(hZ('ZEMP9'))  =  SUM((FLAB, RG), QFX(FLAB,'acons',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP10')$(hZ('ZEMP10'))  =        SUM((FLAB, RG), QFX(FLAB,'atrad',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP11')$(hZ('ZEMP11'))  =        SUM((FLAB, RG), QFX(FLAB,'atran',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP12')$(hZ('ZEMP12'))  =        SUM((FLAB, RG), QFX(FLAB,'aacfo',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP13')$(hZ('ZEMP13'))  =        SUM((FLAB, RG), QFX(FLAB,'acomm',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP14')$(hZ('ZEMP14'))  =        SUM((FLAB, RG), QFX(FLAB,'afsrv',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP15')$(hZ('ZEMP15'))  =        SUM((FLAB, RG), QFX(FLAB,'areal',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP16')$(hZ('ZEMP16'))  =        SUM((FLAB, RG), QFX(FLAB,'absrv',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP17')$(hZ('ZEMP17'))  =        SUM((FLAB, RG), QFX(FLAB,'aeduc',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP18')$(hZ('ZEMP18'))  =        SUM((FLAB, RG), QFX(FLAB,'aosrv',RG,MM_T)$flab_map('flab', FLAB, RG));
mm_suc(MM_T, 'ZEMP19')$(hZ('ZEMP19'))  =        SUM((FLAB, RG), QFX(FLAB,'apadm',RG,MM_T)$flab_map('flab', FLAB, RG));



* --------------
* trade - national level
mm_suc(MM_T, 'ZTRD1')$(hZ('ZTRD1'))  = DPIX(MM_T);
mm_suc(MM_T, 'ZTRD2')$(hZ('ZTRD2'))  = CPIX(MM_T);
mm_suc(MM_T, 'ZTRD3')$(hZ('ZTRD3'))  =PDIND(MM_T);
mm_suc(MM_T, 'ZTRD4')$(hZ('ZTRD4'))  =PWEIND(MM_T);
mm_suc(MM_T, 'ZTRD5')$(hZ('ZTRD5'))  = PWMIND(MM_T);
mm_suc(MM_T, 'ZTRD6')$(hZ('ZTRD6'))  =TOFT(MM_T) ;
mm_suc(MM_T, 'ZTRD7')$(hZ('ZTRD7'))  =PWIND(MM_T) ;
mm_suc(MM_T, 'ZTRD8')$(hZ('ZTRD8'))  =REXR(MM_T);
mm_suc(MM_T, 'ZTRD9')$(hZ('ZTRD9'))  = sum(CAGR, PM0(CAGR)*QMX(CAGR, MM_T));
mm_suc(MM_T, 'ZTRD10')$(hZ('ZTRD10'))  = sum(CAGR, PE0(CAGR)*QEX(CAGR, MM_T));
* --------------

* GDP - regions level
mm_suc(MM_T, 'ZGDPDT')$(hZ('ZGDPDT'))  =        sum(A, PVA0(A, 'DT')*(1-tva0(A, 'DT'))*QVAX(A, 'DT', MM_T));
mm_suc(MM_T, 'ZGDPNO')$(hZ('ZGDPNO'))  =        sum(A, PVA0(A, 'NO')*(1-tva0(A, 'NO'))*QVAX(A, 'NO', MM_T));
mm_suc(MM_T, 'ZGDPCE')$(hZ('ZGDPCE'))  =        sum(A, PVA0(A, 'CE')*(1-tva0(A, 'CE'))*QVAX(A, 'CE', MM_T));
mm_suc(MM_T, 'ZGDPSO')$(hZ('ZGDPSO'))  =        sum(A, PVA0(A, 'SO')*(1-tva0(A, 'SO'))*QVAX(A, 'SO', MM_T));

mm_suc(MM_T, 'ZDTflab')$(hZ('ZDTflab'))  =      WFX('flab-DT', MM_T);
mm_suc(MM_T, 'ZNOflab')$(hZ('ZNOflab'))  =      WFX('flab-NO', MM_T);
mm_suc(MM_T, 'ZCEflab')$(hZ('ZCEflab'))  =      WFX('flab-CE', MM_T);
mm_suc(MM_T, 'ZSOflab')$(hZ('ZSOflab'))  =      WFX('flab-SO', MM_T);

* --------------
* factor payments
mm_suc(MM_T, 'ZFAC1')$(hZ('ZFAC1'))  =  QFSX('flnd-DT', MM_T);
mm_suc(MM_T, 'ZFAC3')$(hZ('ZFAC3'))  =  QFSX('flnd-NO', MM_T);
mm_suc(MM_T, 'ZFAC4')$(hZ('ZFAC4'))  =  QFSX('flnd-CE', MM_T);
mm_suc(MM_T, 'ZFAC5')$(hZ('ZFAC5'))  =  QFSX('flnd-SO', MM_T);
mm_suc(MM_T, 'ZFAC6')$(hZ('ZFAC6'))  =  QFSX('fcap-c-DT', MM_T);
mm_suc(MM_T, 'ZFAC7')$(hZ('ZFAC7'))  =  QFSX('fcap-l-DT', MM_T);
mm_suc(MM_T, 'ZFAC8')$(hZ('ZFAC8'))  =  QFSX('fcap-m-DT', MM_T);
mm_suc(MM_T, 'ZFAC9')$(hZ('ZFAC9'))  =  QFSX('fcap-n-DT', MM_T);
mm_suc(MM_T, 'ZFAC14')$(hZ('ZFAC14'))  =        QFSX('fcap-c-NO', MM_T);
mm_suc(MM_T, 'ZFAC15')$(hZ('ZFAC15'))  =        QFSX('fcap-l-NO', MM_T);
mm_suc(MM_T, 'ZFAC16')$(hZ('ZFAC16'))  =        QFSX('fcap-m-NO', MM_T);
mm_suc(MM_T, 'ZFAC17')$(hZ('ZFAC17'))  =        QFSX('fcap-n-NO', MM_T);
mm_suc(MM_T, 'ZFAC18')$(hZ('ZFAC18'))  =        QFSX('fcap-c-CE', MM_T);
mm_suc(MM_T, 'ZFAC19')$(hZ('ZFAC19'))  =        QFSX('fcap-l-CE', MM_T);
mm_suc(MM_T, 'ZFAC20')$(hZ('ZFAC20'))  =        QFSX('fcap-m-CE', MM_T);
mm_suc(MM_T, 'ZFAC21')$(hZ('ZFAC21'))  =        QFSX('fcap-n-CE', MM_T);
mm_suc(MM_T, 'ZFAC22')$(hZ('ZFAC22'))  =        QFSX('fcap-c-SO', MM_T);
mm_suc(MM_T, 'ZFAC23')$(hZ('ZFAC23'))  =        QFSX('fcap-l-SO', MM_T);
mm_suc(MM_T, 'ZFAC24')$(hZ('ZFAC24'))  =        QFSX('fcap-m-SO', MM_T);
mm_suc(MM_T, 'ZFAC25')$(hZ('ZFAC25'))  =        QFSX('fcap-n-SO', MM_T);

mm_suc(MM_T, 'ZFAC26')$(hZ('ZFAC26'))  =        QFSX('flab-DT', MM_T);
mm_suc(MM_T, 'ZFAC34')$(hZ('ZFAC34'))  =        QFSX('flab-NO', MM_T);
mm_suc(MM_T, 'ZFAC38')$(hZ('ZFAC38'))  =        QFSX('flab-CE', MM_T);
mm_suc(MM_T, 'ZFAC42')$(hZ('ZFAC42'))  =        QFSX('flab-SO', MM_T);

mm_suc(MM_T, 'ZFACCHECK1')$(hZ('ZFACCHECK1'))  =        SUM((RG, FLAB), QFSX(FLAB, MM_T)$flab_map('flab', FLAB, RG));

mm_suc(MM_T, 'ZFAC46')$(hZ('ZFAC46'))  =        WFX('flnd-DT', MM_T);
*mm_suc(MM_T, 'ZFAC47')$(hZ('ZFAC47'))  =        WFX('flnd-TH', MM_T);
mm_suc(MM_T, 'ZFAC48')$(hZ('ZFAC48'))  =        WFX('flnd-NO', MM_T);
mm_suc(MM_T, 'ZFAC49')$(hZ('ZFAC49'))  =        WFX('flnd-CE', MM_T);
mm_suc(MM_T, 'ZFAC50')$(hZ('ZFAC50'))  =        WFX('flnd-SO', MM_T);
mm_suc(MM_T, 'ZFAC51')$(hZ('ZFAC51'))  =        WFX('fcap-c-DT', MM_T);
mm_suc(MM_T, 'ZFAC52')$(hZ('ZFAC52'))  =        WFX('fcap-l-DT', MM_T);
mm_suc(MM_T, 'ZFAC53')$(hZ('ZFAC53'))  =        WFX('fcap-m-DT', MM_T);
mm_suc(MM_T, 'ZFAC54')$(hZ('ZFAC54'))  =        WFX('fcap-n-DT', MM_T);
*mm_suc(MM_T, 'ZFAC55')$(hZ('ZFAC55'))  =        WFX('fcap-c-TH', MM_T);
*mm_suc(MM_T, 'ZFAC56')$(hZ('ZFAC56'))  =        WFX('fcap-l-TH', MM_T);
*mm_suc(MM_T, 'ZFAC57')$(hZ('ZFAC57'))  =        WFX('fcap-m-TH', MM_T);
*mm_suc(MM_T, 'ZFAC58')$(hZ('ZFAC58'))  =        WFX('fcap-n-TH', MM_T);
mm_suc(MM_T, 'ZFAC59')$(hZ('ZFAC59'))  =        WFX('fcap-c-NO', MM_T);
mm_suc(MM_T, 'ZFAC60')$(hZ('ZFAC60'))  =        WFX('fcap-l-NO', MM_T);
mm_suc(MM_T, 'ZFAC61')$(hZ('ZFAC61'))  =        WFX('fcap-m-NO', MM_T);
mm_suc(MM_T, 'ZFAC62')$(hZ('ZFAC62'))  =        WFX('fcap-n-NO', MM_T);
mm_suc(MM_T, 'ZFAC63')$(hZ('ZFAC63'))  =        WFX('fcap-c-CE', MM_T);
mm_suc(MM_T, 'ZFAC64')$(hZ('ZFAC64'))  =        WFX('fcap-l-CE', MM_T);
mm_suc(MM_T, 'ZFAC65')$(hZ('ZFAC65'))  =        WFX('fcap-m-CE', MM_T);
mm_suc(MM_T, 'ZFAC66')$(hZ('ZFAC66'))  =        WFX('fcap-n-CE', MM_T);
mm_suc(MM_T, 'ZFAC67')$(hZ('ZFAC67'))  =        WFX('fcap-c-SO', MM_T);
mm_suc(MM_T, 'ZFAC68')$(hZ('ZFAC68'))  =        WFX('fcap-l-SO', MM_T);
mm_suc(MM_T, 'ZFAC69')$(hZ('ZFAC69'))  =        WFX('fcap-m-SO', MM_T);
mm_suc(MM_T, 'ZFAC70')$(hZ('ZFAC70'))  =        WFX('fcap-n-SO', MM_T);


* --------------
* welfare
mm_suc(MM_T, 'ZPOP1')$(hZ('ZPOP1'))  =  hpopX('hhd-DT', MM_T);
mm_suc(MM_T, 'ZPOP3')$(hZ('ZPOP3'))  =  hpopX('hhd-NO', MM_T);
mm_suc(MM_T, 'ZPOP4')$(hZ('ZPOP4'))  =  hpopX('hhd-CE', MM_T);
mm_suc(MM_T, 'ZPOP5')$(hZ('ZPOP5'))  =  hpopX('hhd-SO', MM_T);
mm_suc(MM_T, 'ZPOP7')$(hZ('ZPOP7'))  =  hpopX('hhd-NO-DT', MM_T);
mm_suc(MM_T, 'ZPOP8')$(hZ('ZPOP8'))  =  hpopX('hhd-CE-DT', MM_T);
mm_suc(MM_T, 'ZPOP9')$(hZ('ZPOP9'))  =  hpopX('hhd-SO-DT', MM_T);

mm_suc(MM_T, 'ZCONS1')$(hZ('ZCONS1'))  =        Tot_consX('hhd-DT', MM_T);
mm_suc(MM_T, 'ZCONS3')$(hZ('ZCONS3'))  =        Tot_consX('hhd-NO', MM_T);
mm_suc(MM_T, 'ZCONS4')$(hZ('ZCONS4'))  =        Tot_consX('hhd-CE', MM_T);
mm_suc(MM_T, 'ZCONS5')$(hZ('ZCONS5'))  =        Tot_consX('hhd-SO', MM_T);
mm_suc(MM_T, 'ZCONS7')$(hZ('ZCONS7'))  =        Tot_consX('hhd-NO-DT', MM_T);
mm_suc(MM_T, 'ZCONS8')$(hZ('ZCONS8'))  =        Tot_consX('hhd-CE-DT', MM_T);
mm_suc(MM_T, 'ZCONS9')$(hZ('ZCONS9'))  =        Tot_consX('hhd-SO-DT', MM_T);

mm_suc(MM_T, 'ZYIF1')$(hZ('ZYIF1'))  =   YIFX('hhd-DT',    'flab-DT',  MM_T);
mm_suc(MM_T, 'ZYIF3')$(hZ('ZYIF3'))  =   YIFX('hhd-NO',    'flab-NO',  MM_T);
mm_suc(MM_T, 'ZYIF5')$(hZ('ZYIF5'))  =   YIFX('hhd-CE',    'flab-CE',  MM_T);
mm_suc(MM_T, 'ZYIF7')$(hZ('ZYIF7'))  =   YIFX('hhd-SO',    'flab-SO',  MM_T);
mm_suc(MM_T, 'ZYIF9')$(hZ('ZYIF9'))  =   YIFX('hhd-DT',    'flnd-DT',  MM_T);
mm_suc(MM_T, 'ZYIF11')$(hZ('ZYIF11'))  =         YIFX('hhd-NO',    'flnd-NO',  MM_T);
mm_suc(MM_T, 'ZYIF13')$(hZ('ZYIF13'))  =         YIFX('hhd-CE',    'flnd-CE',  MM_T);
mm_suc(MM_T, 'ZYIF15')$(hZ('ZYIF15'))  =         YIFX('hhd-SO',    'flnd-SO',  MM_T);
mm_suc(MM_T, 'ZYIF17')$(hZ('ZYIF17'))  =         YIFX('hhd-DT',    'fcap-c-DT',  MM_T);
mm_suc(MM_T, 'ZYIF19')$(hZ('ZYIF19'))  =         YIFX('hhd-NO',    'fcap-c-NO',  MM_T);
mm_suc(MM_T, 'ZYIF21')$(hZ('ZYIF21'))  =         YIFX('hhd-CE',    'fcap-c-CE',  MM_T);
mm_suc(MM_T, 'ZYIF23')$(hZ('ZYIF23'))  =         YIFX('hhd-SO',    'fcap-c-SO',  MM_T);
mm_suc(MM_T, 'ZYIF25')$(hZ('ZYIF25'))  =         YIFX('hhd-DT',    'fcap-l-DT',  MM_T);
mm_suc(MM_T, 'ZYIF27')$(hZ('ZYIF27'))  =         YIFX('hhd-NO',    'fcap-l-NO',  MM_T);
mm_suc(MM_T, 'ZYIF29')$(hZ('ZYIF29'))  =         YIFX('hhd-CE',    'fcap-l-CE',  MM_T);
mm_suc(MM_T, 'ZYIF31')$(hZ('ZYIF31'))  =         YIFX('hhd-SO',    'fcap-l-SO',  MM_T);
mm_suc(MM_T, 'ZYIF33')$(hZ('ZYIF33'))  =        SUM(F, YIFX('hhd-DT',    F,  MM_T));
mm_suc(MM_T, 'ZYIF35')$(hZ('ZYIF35'))  =        SUM(F, YIFX('hhd-NO',    F,  MM_T));
mm_suc(MM_T, 'ZYIF37')$(hZ('ZYIF37'))  =        SUM(F, YIFX('hhd-CE',    F,  MM_T));
mm_suc(MM_T, 'ZYIF39')$(hZ('ZYIF39'))  =        SUM(F, YIFX('hhd-SO',    F,  MM_T));
mm_suc(MM_T, 'ZYIF41')$(hZ('ZYIF41'))  =        share_emigr_of_mob_flab('hhd-NO-DT');
mm_suc(MM_T, 'ZYIF43')$(hZ('ZYIF43'))  =        share_emigr_of_mob_flab('hhd-CE-DT');
mm_suc(MM_T, 'ZYIF45')$(hZ('ZYIF45'))  =        share_emigr_of_mob_flab('hhd-SO-DT');
mm_suc(MM_T, 'ZYIF47')$(hZ('ZYIF47'))  =        share_emigr_of_tot('hhd-NO-DT');
mm_suc(MM_T, 'ZYIF49')$(hZ('ZYIF49'))  =        share_emigr_of_tot('hhd-CE-DT');
mm_suc(MM_T, 'ZYIF51')$(hZ('ZYIF51'))  =        share_emigr_of_tot('hhd-SO-DT');
