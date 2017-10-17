#!/bin/bash

# We call newgrp to see if the framerd group exists; if it does,
# we echo framerd, otherwise we echo sys.  The /dev/null stuff is
# because newgrp normally pushes a new shell and we want it to just
# return immediately
if grep "^framerd:" /etc/group > /dev/null; then
   echo "framerd";
else echo "bin";
fi


