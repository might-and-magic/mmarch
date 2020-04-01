cd %~dp0

copy /Y ..\..\mmarch.exe

mmarch k "compare test v1" "./\compare test v2"

mmarch compare ".///\/compare test v1" "./\///\compare test v2" nsis "nsis folder/\//script.nsi" files
mmarch k ".///\/compare test v1" "./\///\compare test v2" batch "batch folder/\//script.bat" files

mmarch compare "compare test v1" "./\compare test v2" filesonly "diff folder"
mmarch df2n "diff folder" "nsis folder2/script.nsi" files
mmarch compare "compare test v1" "./\compare test v2" filesonly "diff folder"
mmarch df2b "diff folder" "batch folder2/script.bat" files

mmarch compare "nsis folder" "nsis folder2"
mmarch compare "nsis folder" "nsis folder2" nsis "nsis folder3/\//script.nsi" files
mmarch k "nsis folder" "nsis folder2" batch "batch folder3/\//script.bat" files

mmarch compare "batch folder" "batch folder2"
mmarch compare "batch folder" "batch folder2" nsis "nsis folder3/\//script.nsi" files
mmarch k "batch folder" "batch folder2" batch "batch folder3/\//script.bat" files

rmdir /s /q "nsis folder2" "batch folder2"


xcopy /s /y /e /v /i "compare test v1" "compare test v1 to check nsis"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "compare test v1 to check nsis"
"compare test v1 to check nsis/patch.exe"

del "compare test v1 to check nsis\patch.exe"
mmarch compare "compare test v1 to check nsis" "compare test v2"


xcopy /s /y /e /v /i "compare test v1" "compare test v1 to check batch"
copy /Y mmarch.exe "batch folder/mmarch.exe"
move "batch folder\*" "compare test v1 to check batch"
move "batch folder\files" "compare test v1 to check batch"
"compare test v1 to check batch/script.bat"

cd ..
del "compare test v1 to check batch\script.bat" "compare test v1 to check batch\mmarch.exe"
rmdir /s /q "compare test v1 to check batch\files"
mmarch k "compare test v1 to check batch" "compare test v2"


rmdir /s /q "nsis folder" "batch folder" "compare test v1 to check nsis" "compare test v1 to check batch"




mmarch k "compare test v2" "./\compare test v3"

mmarch compare ".///\/compare test v2" "./\///\compare test v3" nsis "nsis folder/\//script.nsi" files
mmarch k ".///\/compare test v2" "./\///\compare test v3" batch "batch folder/\//script.bat" files

mmarch compare "compare test v2" "./\compare test v3" filesonly "diff folder"
mmarch df2n "diff folder" "nsis folder2/script.nsi" files
mmarch compare "compare test v2" "./\compare test v3" filesonly "diff folder"
mmarch df2b "diff folder" "batch folder2/script.bat" files

mmarch compare "nsis folder" "nsis folder2"
mmarch compare "nsis folder" "nsis folder2" nsis "nsis folder3/\//script.nsi" files
mmarch k "nsis folder" "nsis folder2" batch "batch folder3/\//script.bat" files

mmarch compare "batch folder" "batch folder2"
mmarch compare "batch folder" "batch folder2" nsis "nsis folder3/\//script.nsi" files
mmarch k "batch folder" "batch folder2" batch "batch folder3/\//script.bat" files

rmdir /s /q "nsis folder2" "batch folder2"


xcopy /s /y /e /v /i "compare test v2" "compare test v2 to check nsis"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "compare test v2 to check nsis"
"compare test v2 to check nsis/patch.exe"

del "compare test v2 to check nsis\patch.exe"
mmarch compare "compare test v2 to check nsis" "compare test v3"


xcopy /s /y /e /v /i "compare test v2" "compare test v2 to check batch"
copy /Y mmarch.exe "batch folder/mmarch.exe"
move "batch folder\*" "compare test v2 to check batch"
move "batch folder\files" "compare test v2 to check batch"
"compare test v2 to check batch/script.bat"

cd ..
del "compare test v2 to check batch\script.bat" "compare test v2 to check batch\mmarch.exe"
rmdir /s /q "compare test v2 to check batch\files"
mmarch k "compare test v2 to check batch" "compare test v3"


rmdir /s /q "nsis folder" "batch folder" "compare test v2 to check nsis" "compare test v2 to check batch"




mmarch k ".\//\\\compare test v1\folder w modified files\icons.lod" "compare test v2\folder w modified files\//icons.lod"

mmarch compare ".\//\\\compare test v1\folder w modified files\icons.lod" "compare test v2\folder w modified files\//icons.lod" filesonly "diff folder"
mmarch diff-files-to-nsis "diff folder" "nsis folder2/script.nsi" files

mmarch compare ".\//\\\compare test v1\folder w modified files\icons.lod" "compare test v2\folder w modified files\//icons.lod" filesonly "diff folder"
mmarch diff-files-to-batch "diff folder" "batch folder2/script.bat" files

mmarch compare ".\//\\\compare test v1\folder w modified files\icons.lod" "compare test v2\folder w modified files\//icons.lod" nsis "nsis folder/\//script.nsi" files
mmarch k ".\//\\\compare test v1\folder w modified files\icons.lod" "compare test v2\folder w modified files\//icons.lod" batch "batch folder/\//script.bat" files

mmarch compare "nsis folder" "nsis folder2"
mmarch compare "batch folder" "batch folder2"

rmdir /s /q "nsis folder2" "batch folder2"

mkdir "compare test v1 to check nsis"
xcopy /y "compare test v1\folder w modified files\icons.lod" "compare test v1 to check nsis"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "compare test v1 to check nsis"
"compare test v1 to check nsis/patch.exe"

del "compare test v1 to check nsis\patch.exe"
mmarch compare "compare test v1 to check nsis\icons.lod" "compare test v2\folder w modified files\icons.lod"


mkdir "compare test v1 to check batch"
xcopy /y "compare test v1\folder w modified files\icons.lod" "compare test v1 to check batch"
copy /Y mmarch.exe "batch folder/mmarch.exe"
move "batch folder\*" "compare test v1 to check batch"
move "batch folder\files" "compare test v1 to check batch"
"compare test v1 to check batch/script.bat"

cd ..
del "compare test v1 to check batch\script.bat" "compare test v1 to check batch\mmarch.exe"
rmdir /s /q "compare test v1 to check batch\files"
mmarch compare "compare test v1 to check batch\icons.lod" "compare test v2\folder w modified files\icons.lod"


rmdir /s /q "nsis folder" "batch folder" "compare test v1 to check nsis" "compare test v1 to check batch"
del mmarch.exe
