# General options
Name "FramerD"
CRCCheck on
LicenseData "FramerD is released under the GNU General Public License (GPL)"
# Directory Selection options
InstallDir "\Program Files\FramerD"
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
File \fdinstall\bin\*.exe
File \fdinstall\bin\*.dll
SetOutPath $INSTDIR\include\framerd
File \fdinstall\include\framerd\*.h
SetOutPath $INSTDIR\etc
# Setting up scripts
Section Scripts
Exec $INSTDIR\bin\fdscript.exe $INSTDIR\etc\setup.fdx $INSTDIR
# Running the FramerD setup
Section Configure
Exec $INSTDIR\bin\fdscript.exe $INSTDIR\etc\setup.fdx $INSTDIR


