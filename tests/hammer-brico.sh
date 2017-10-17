server=$1
arg2=$2
arg3=$3
declare -i nprocs=${arg2:=32}
declare -i ncalls=${arg3:=16}
declare -i counter=0
echo "Starting " $nprocs " processes, each exercising " $server " with " $ncalls " complex queries"
until test $counter -ge $nprocs;
do counter=$counter+1; \
   ../bin/fdscript exercise-brico.fdx $server $ncalls &
done

