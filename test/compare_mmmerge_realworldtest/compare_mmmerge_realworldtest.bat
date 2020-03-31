cd %~dp0

copy /Y ..\..\mmarch.exe


mmarch compare mmmerge-2019-09-22 mmmerge-2019-10-08 filesonly diff_folder_temp_12-23
mmarch compare mmmerge-2019-10-08 mmmerge-2020-03-17 filesonly diff_folder_temp_12-23
mmarch compare mmmerge-2020-03-17 mmmerge-2020-03-29 filesonly diff_folder_temp_12-23

mmarch compare mmmerge-2019-09-22 mmmerge-2020-03-29 filesonly diff_folder_temp_13-23
mmarch compare mmmerge-2019-10-08 mmmerge-2020-03-29 filesonly diff_folder_temp_13-23
mmarch compare mmmerge-2020-03-17 mmmerge-2020-03-29 filesonly diff_folder_temp_13-23

xcopy /s /y /e /v /i "mmmerge-2019-09-22" "mmmerge-2019-09-22 to check 12-23"
xcopy /s /y /e /v /i "mmmerge-2019-10-08" "mmmerge-2019-10-08 to check 12-23"
xcopy /s /y /e /v /i "mmmerge-2020-03-17" "mmmerge-2020-03-17 to check 12-23"

xcopy /s /y /e /v /i "mmmerge-2019-09-22" "mmmerge-2019-09-22 to check 13-23"
xcopy /s /y /e /v /i "mmmerge-2019-10-08" "mmmerge-2019-10-08 to check 13-23"
xcopy /s /y /e /v /i "mmmerge-2020-03-17" "mmmerge-2020-03-17 to check 13-23"


mmarch cf2n "diff_folder_temp_12-23" "nsis folder/script.nsi" "files"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "mmmerge-2019-09-22 to check 12-23"
"mmmerge-2019-09-22 to check 12-23/patch.exe"

move "mmmerge-2019-09-22 to check 12-23\patch.exe" "mmmerge-2019-10-08 to check 12-23"
"mmmerge-2019-10-08 to check 12-23/patch.exe"

move "mmmerge-2019-10-08 to check 12-23\patch.exe" "mmmerge-2020-03-17 to check 12-23"
"mmmerge-2020-03-17 to check 12-23/patch.exe"

del "mmmerge-2020-03-17 to check 12-23\patch.exe"

mmarch compare "mmmerge-2019-09-22 to check 12-23" "mmmerge-2020-03-29"
mmarch compare "mmmerge-2019-10-08 to check 12-23" "mmmerge-2020-03-29"
mmarch compare "mmmerge-2020-03-17 to check 12-23" "mmmerge-2020-03-29"

rmdir /s /q "nsis folder"


mmarch cf2n "diff_folder_temp_13-23" "nsis folder/script.nsi" "files"
copy /Y mmarch.exe "nsis folder/mmarch.exe"
"C:\Program Files (x86)\NSIS\makensis" "nsis folder/script.nsi"
move "nsis folder\patch.exe" "mmmerge-2019-09-22 to check 13-23"
"mmmerge-2019-09-22 to check 13-23/patch.exe"

move "mmmerge-2019-09-22 to check 13-23\patch.exe" "mmmerge-2019-10-08 to check 13-23"
"mmmerge-2019-10-08 to check 13-23/patch.exe"

move "mmmerge-2019-10-08 to check 13-23\patch.exe" "mmmerge-2020-03-17 to check 13-23"
"mmmerge-2020-03-17 to check 13-23/patch.exe"

del "mmmerge-2020-03-17 to check 13-23\patch.exe"

mmarch compare "mmmerge-2019-09-22 to check 13-23" "mmmerge-2020-03-29"
mmarch compare "mmmerge-2019-10-08 to check 13-23" "mmmerge-2020-03-29"
mmarch compare "mmmerge-2020-03-17 to check 13-23" "mmmerge-2020-03-29"


rmdir /s /q "nsis folder" "mmmerge-2019-09-22 to check 12-23" "mmmerge-2019-09-22 to check 13-23" "mmmerge-2019-10-08 to check 12-23" "mmmerge-2019-10-08 to check 13-23" "mmmerge-2020-03-17 to check 12-23" "mmmerge-2020-03-17 to check 13-23"
del mmarch.exe
