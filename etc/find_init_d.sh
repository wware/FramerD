#!/bin/sh

if test -d /etc/init.d; then
   echo "/etc/init.d/";
elif test -d /etc/rc.d/init.d; then 
     echo "/etc/rc.d/init.d/";
elif test -d /etc/rc.d; then   
     echo "/etc/rc.d/";
else echo "/etc/";
fi


