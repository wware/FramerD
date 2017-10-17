#!/bin/bash
rm -f *.running
${HELPEXE} ../bin/fdscript fakedb.scm clean
${HELPEXE} ../bin/fdscript fakedb.scm init
${HELPEXE} ../bin/fdserver fdtemp.fdz &
${HELPEXE} ../bin/fdscript sleep5.scm
${HELPEXE} ../bin/fdscript fakedb.scm make fdtemp@localhost
${HELPEXE} ../bin/fdscript fakedb.scm test fdtemp@localhost alpha &
${HELPEXE} ../bin/fdscript fakedb.scm test fdtemp@localhost beta &
${HELPEXE} ../bin/fdscript fakedb.scm test fdtemp@localhost gamma &
${HELPEXE} ../bin/fdscript fakedb.scm test fdtemp@localhost delta &
${HELPEXE} ../bin/fdscript fakedb.scm test fdtemp@localhost epsilon &
${HELPEXE} ../bin/fdscript fakedb.scm test fdtemp@localhost phi &
sleep 10
${HELPEXE} ../bin/dtcall fdtemp@localhost session-id
${HELPEXE} ../bin/dtcall fdtemp@localhost timestring
for x in *.running
do while (test -f $x) do sleep 5; done
done
kill `cat fdtemp.pid`
