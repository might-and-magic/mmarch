cd %~dp0

copy /Y ..\..\mmarch.exe

mmarch compare "compare test v1" "compare test v2" filesonly "diff folder 12-23"
mmarch compare "compare test v2" "compare test v3" filesonly "diff folder 12-23"

mmarch compare "compare test v1" "compare test v3" filesonly "diff folder 13-23"
mmarch compare "compare test v2" "compare test v3" filesonly "diff folder 13-23"

mmarch compare "diff folder 12-23" "diff folder 13-23"



xcopy /s /y /e /v /i "compare test v1" "compare test v1 to check 12-23"
xcopy /s /y /e /v /i "compare test v2" "compare test v2 to check 12-23"

mmarch cf2n "diff folder 12-23" "nsis folder/script.nsi" "files"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "compare test v1 to check 12-23"
"compare test v1 to check 12-23/patch.exe"

move "compare test v1 to check 12-23\patch.exe" "compare test v2 to check 12-23"
"compare test v2 to check 12-23/patch.exe"
del "compare test v2 to check 12-23\patch.exe"

mmarch compare "compare test v1 to check 12-23" "compare test v3"
mmarch compare "compare test v2 to check 12-23" "compare test v3"


rmdir /s /q "nsis folder"


xcopy /s /y /e /v /i "compare test v1" "compare test v1 to check 13-23"
xcopy /s /y /e /v /i "compare test v2" "compare test v2 to check 13-23"

mmarch cf2n "diff folder 13-23" "nsis folder/script.nsi" "files"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "compare test v1 to check 13-23"
"compare test v1 to check 13-23/patch.exe"

move "compare test v1 to check 13-23\patch.exe" "compare test v2 to check 13-23"
"compare test v2 to check 13-23/patch.exe"
del "compare test v2 to check 13-23\patch.exe"

mmarch compare "compare test v1 to check 13-23" "compare test v3"
mmarch compare "compare test v2 to check 13-23" "compare test v3"



rmdir /s /q "compare test v1 to check 12-23" "compare test v1 to check 13-23" "compare test v2 to check 12-23" "compare test v2 to check 13-23" "nsis folder"
del mmarch.exe
