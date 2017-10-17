# General options
Name "FramerD"
OutFile fdinstall243.exe
CRCCheck on
LicenseText "FramerD is released under the GPL"
LicenseData LICENSE
Icon docs\fdlogo.ico
WindowIcon on
# Directory Selection options
InstallDir $PROGRAMFILES\FramerD
InstallDirRegKey HKLM SOFTWARE\FramerD "InstallDir"
DirShow show
DirText "Select installation directory"
AllowRootDirInstall false
# Installation options
InstProgressFlags smooth
ShowInstDetails show
# Writing Files options
SetOverwrite on
SetCompress auto
SetDateSave on
# Writing the files
Section Files
# Save the installation dir in the registry
WriteRegStr HKLM SOFTWARE\FramerD "InstallDir" "$INSTDIR"
SetOutPath $INSTDIR
File LICENSE
File GPL
File LGPL
SetOutPath $INSTDIR\bin
File bin\*.exe
File bin\*.dll
File \usr\bin\libreadline.dll
SetOutPath $INSTDIR\lib
File lib\*.exp
File lib\*.a
SetOutPath $INSTDIR\scripts
File scripts\*.in
SetOutPath $INSTDIR\docs
File docs\*.html
File docs\*.css
File docs\*.png
SetOutPath $INSTDIR\include\framerd
File include\framerd\*.h
SetOutPath $INSTDIR\etc
File etc\*.fdx
File etc\fdscript.el
SetOutPath $INSTDIR\modules
File modules\*.fdx
SectionEnd
# Setting up scripts
Section Scripts
SetOutPath $INSTDIR
ExecWait 'bin\fdscript.exe etc\scripts2bat.fdx "$INSTDIR/scripts" "$INSTDIR/bin"'
SectionEnd
# Running the FramerD setup
Section Configure
SetOutPath $INSTDIR
ExecWait 'bin\fdscript.exe etc\setup.fdx "$INSTDIR"'
SectionEnd
