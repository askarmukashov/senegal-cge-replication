$setglobal ide "ide=%gams.ide% lo=%gams.lo% errorlog=%gams.errorlog% errmsg=1"
$ONSYMLIST ONSYMXREF OFFUPPER
$ONEMPTY
$offlisting

$offsymxref offsymlist
option
  limrow = 0,
  limcol = 0,
  solprint = off,
  sysout = off
  SOLVELINK=5
;

* constant part before the loop
execute "gams DCGEcore/1.constant_parameters.gms r=tmp/setup s=tmp/constant gdx=debug %ide%";
*$exit
loop(MM_XC,
  mm_suc(MM_T, vars) = 0;
  MM_XCURR(MM_XC) = YES;

  if(card(static) > 0,
*   current static inputs
    mm_I_static(static) = mm_inputs(MM_XC, static);
    execute_unload "tmp/static_I.gdx" mm_I_static;
  );
  execute "gams DCGEcore/2a.static_inputs.gms r=tmp/constant s=tmp/static gdx=debug %ide%";

  if(errorlevel < 1,
*   current dynamic inputs
    mm_I_dynamic(dynamic) = mm_inputs(MM_XC, dynamic);
    execute_unload "tmp/dynamic_I.gdx" mm_I_dynamic MM_XCURR;
    execute "gams DCGEcore/3a.dynamic_inputs.gms r=tmp/static gdx=debug %ide%";
    execute_load "tmp/dynamic.gdx" mm_suc;
    data(MM_XC, MM_T, hZ) = mm_suc(MM_T, hZ);
  else
    data(MM_XC, MM_T, hZ) = NO;
  );

* loop close
);

* save simulation results
execute_unload "result.gdx" data MM_XC;
