$ontext
$ONSYMLIST ONSYMXREF OFFUPPER
$ONEMPTY
$offlisting
$offsymxref offsymlist
option
    limrow = 0,
    limcol = 0,
    solprint = off,
    sysout = off;
OPTION SOLVELINK=5;
$offtext
*----------------------------------------------------------------------
* Time period loop
*----------------------------------------------------------------------

 stop=0;
LOOP(MM_T$(stop=0),
IF(ord(MM_T)=1,
* first year - don't solve
$BATINCLUDE DCGEcore/3e.results.inc MM_T
ELSE
$include DCGEcore/3c.dynamic_part.gms
* first year check
 );


$include DCGEcore/4.outcomes.gms
execute_unload "tmp/dynamic.gdx" mm_suc;


*time loop main
);



execute_unload "tmp/dynamic.gdx" mm_suc;
