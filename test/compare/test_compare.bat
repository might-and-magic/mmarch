cd %~dp0

copy /Y ..\..\mmarch.exe

mmarch k "compare test from" "./\compare test to"

mmarch compare "compare test from" "./\compare test to" fileonly "diff folder"
mmarch cf2n "diff folder" "nsis folder2/script.nsi" files

mmarch compare "compare test from" "./\compare test to" fileonly "diff folder"
mmarch cf2b "diff folder" "batch folder2/script.bat" files

mmarch compare ".///\/compare test from" "./\///\compare test to" nsis "nsis folder/\//script.nsi" files
mmarch k ".///\/compare test from" "./\///\compare test to" batch "batch folder/\//script.bat" files

mmarch compare "nsis folder" "nsis folder2"
mmarch compare "nsis folder" "nsis folder2" nsis "nsis folder3/\//script.nsi" files
mmarch k "nsis folder" "nsis folder2" batch "batch folder3/\//script.bat" files

mmarch compare "batch folder" "batch folder2"
mmarch compare "batch folder" "batch folder2" nsis "nsis folder3/\//script.nsi" files
mmarch k "batch folder" "batch folder2" batch "batch folder3/\//script.bat" files

rmdir /s /q "nsis folder2" "batch folder2"


xcopy /s /y /e /v /i "compare test from" "compare test from to test nsis"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "compare test from to test nsis"
"compare test from to test nsis/patch.exe"
del "compare test from to test nsis\patch.exe"
mmarch compare "compare test from to test nsis" "compare test to"


xcopy /s /y /e /v /i "compare test from" "compare test from to test batch"
copy /Y mmarch.exe "batch folder/mmarch.exe"
move "batch folder\*" "compare test from to test batch"
move "batch folder\files" "compare test from to test batch"
"compare test from to test batch/script.bat"
cd ..
del "compare test from to test batch\script.bat" "compare test from to test batch\mmarch.exe"
rmdir /s /q "compare test from to test batch\files"
mmarch k "compare test from to test batch" "compare test to"


rmdir /s /q "nsis folder" "batch folder" "compare test from to test nsis" "compare test from to test batch"




mmarch k ".\//\\\compare test from\folder w modified files\icons.lod" "compare test to\folder w modified files\//icons.lod"

mmarch compare ".\//\\\compare test from\folder w modified files\icons.lod" "compare test to\folder w modified files\//icons.lod" fileonly "diff folder"
mmarch compare-files-to-nsis "diff folder" "nsis folder2/script.nsi" files

mmarch compare ".\//\\\compare test from\folder w modified files\icons.lod" "compare test to\folder w modified files\//icons.lod" fileonly "diff folder"
mmarch compare-files-to-batch "diff folder" "batch folder2/script.bat" files

mmarch compare ".\//\\\compare test from\folder w modified files\icons.lod" "compare test to\folder w modified files\//icons.lod" nsis "nsis folder/\//script.nsi" files
mmarch k ".\//\\\compare test from\folder w modified files\icons.lod" "compare test to\folder w modified files\//icons.lod" batch "batch folder/\//script.bat" files

mmarch compare "nsis folder" "nsis folder2"
mmarch compare "batch folder" "batch folder2"

rmdir /s /q "nsis folder2" "batch folder2"

mkdir "compare test from to test nsis"
xcopy /y "compare test from\folder w modified files\icons.lod" "compare test from to test nsis"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "compare test from to test nsis"
"compare test from to test nsis/patch.exe"
del "compare test from to test nsis\patch.exe"
mmarch compare "compare test from to test nsis\icons.lod" "compare test to\folder w modified files\icons.lod"


mkdir "compare test from to test batch"
xcopy /y "compare test from\folder w modified files\icons.lod" "compare test from to test batch"
copy /Y mmarch.exe "batch folder/mmarch.exe"
move "batch folder\*" "compare test from to test batch"
move "batch folder\files" "compare test from to test batch"
"compare test from to test batch/script.bat"
cd ..
del "compare test from to test batch\script.bat" "compare test from to test batch\mmarch.exe"
rmdir /s /q "compare test from to test batch\files"
mmarch compare "compare test from to test batch\icons.lod" "compare test to\folder w modified files\icons.lod"


rmdir /s /q "nsis folder" "batch folder" "compare test from to test nsis" "compare test from to test batch"
del mmarch.exe
