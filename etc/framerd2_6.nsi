# General options
Name "FramerD"
OutFile fdinstall261.exe
CRCCheck on
LicenseData "LICENSE"
# Directory Selection options
InstallDir "C:\Program Files\FramerD"
DirText "Which directory do you want to install FramerD in?"
DirShow show
AllowRootDirInstall false
# Installation options
InstProgressFlags smooth
ShowInstDetails show
# Writing Files options
SetOverwrite ifnewer
SetCompress auto
SetDateSave on
# Writing the files
Section Files
SetOutPath $INSTDIR\bin
File bin\*.exe
File bin\*.dll
SetOutPath $INSTDIR\include\framerd
File include\framerd\*.h
SetOutPath $INSTDIR\etc
File etc\setup.fdx
File etc\scripts2bat.fdx
File etc\fdscript.el
SetOutPath $INSTDIR\scripts
File scripts\*.in
SectionEnd
# Setting up scripts
Section Scripts
SetOutPath $INSTDIR
ExecWait 'bin\fdscript.exe etc\scripts2bat.fdx scripts bin'
SectionEnd
# Running the FramerD setup
Section Configure
SetOutPath $INSTDIR
ExecWait 'bin\fdscript.exe etc\setup.fdx "$INSTDIR"'
SectionEnd



