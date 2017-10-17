#!/bin/bash
rm -f dbtest.pool dbtest.index dbtest.autoindex *.LCK dbtest.symbols dbtest.files dbtst.locks
${HELPEXE} ../bin/fdserver dbtst.fdz &
sleep 5;
${HELPEXE} ../bin/fdscript dbtestn.fdx make dbtst@localhost make
sleep 5;
${HELPEXE} ../bin/fdscript dbtestn.fdx test dbtst@localhost alpha &
${HELPEXE} ../bin/fdscript dbtestn.fdx test dbtst@localhost beta &
${HELPEXE} ../bin/fdscript dbtestn.fdx test dbtst@localhost gamma &
${HELPEXE} ../bin/fdscript dbtestn.fdx test dbtst@localhost delta &
${HELPEXE} dtcall dbtst@localhost timestring
sleep 10
for x in *.running
do while (test -f $x) do sleep 5; done
done
if (test -f dbtst.pid) then kill `cat dbtst.pid`; fi 

