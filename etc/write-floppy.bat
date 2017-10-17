mkdir %1\bin
mkdir %1\scripts
mkdir %1\docs
mkdir %1\etc
mkdir %1\lib
mkdir %1\include
mkdir %1\include\framerd
COPY LICENSE %1
COPY NEWS %1
COPY bin\*.exe %1\bin
COPY bin\*.dll %1\bin
COPY lib\*.exp %1\lib
COPY lib\*.a %1\lib
COPY include\framerd\*.h %1\include\framerd
COPY scripts\*.fdx %1\scripts
COPY docs\*.html %1\docs
COPY docs\*.css %1\docs
COPY docs\*.png %1\docs
COPY etc\* %1\etc
COPY etc\install.bat %1
COPY version.major %1
COPY version.minor %1

