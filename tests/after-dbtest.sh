if (test -f dbtest.running) then
  echo "Problems with DBTEST: Unexpected abort";
else
  echo "No problems with dbtest; dbtest.running removed";
fi
