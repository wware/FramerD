mkdir %1
mkdir %1\exe
mkdir %1\modules
mkdir %1\docs
copy bin\*.dll %1\exe
copy bin\*.exe %1\exe
copy modules\*.fdx %1\modules
copy docs\* %1\docs
bin\fdscript.exe etc\setup.fdx %1
