echo this is original test.txt>test_orig.txt
echo this is new test.txt>test_new.txt
echo this is new `test` file without extension>test
echo this is test.str>test.str

copy /Y ..\mmarch.exe

mkdir "L1-folder-你好-2\L2 folder\L3folder"


mmarch create h3lod.lod h3lod "L1 folder\L2 folder h3lod" test test.str test.wav testpal.bmp test_orig.txt test_new.txt
mmarch list "./L1 folder\L2 folder h3lod/h3lod.lod"

mmarch delete "L1 folder/L2 folder h3lod/h3lod.lod" test_orig.txt test_new.txt
mmarch rename "./\/L1 folder\L2 folder h3lod/h3lod.lod" test.str test_ren.str
mmarch optimize "L1 folder\L2 folder h3lod\h3lod.lod"
mmarch list "L1 folder\L2 folder h3lod//h3lod.lod"

mmarch create h3snd.snd h3snd "L1 folder\L2 folder h3snd" test.wav
mmarch list "L1 folder/L2 folder h3snd/h3snd.snd"

mmarch create h3mm78vid.vid h3mm78vid "L1 folder/L2 folder h3mm78vid/" JVC.bik
mmarch list "L1 folder\L2 folder h3mm78vid\h3mm78vid.vid"

mmarch create mmbitmapslod.bitmaps.lod mmbitmapslod . pal994.act testpal.bmp
mmarch list mmbitmapslod.bitmaps.lod "|"

mmarch create mmspriteslod.sprites.lod mmspriteslod .
mmarch add "./mmspriteslod.sprites.lod" testpal.bmp /p 3
mmarch add "mmspriteslod.sprites.lod" testpal.bmp



mmarch create mmiconslod.icons.lod mmiconslod L1-folder-你好-2 test.bin test.wav
mmarch list L1-folder-你好-2/mmiconslod.icons.lod

copy /Y test_orig.txt test.txt
mmarch add L1-folder-你好-2/mmiconslod.icons.lod test.txt test.str test.pcx
mmarch list L1-folder-你好-2/mmiconslod.icons.lod

copy /Y test_new.txt test.txt
mmarch add .\L1-folder-你好-2\mmiconslod.icons.lod test.txt
mmarch rename .\L1-folder-你好-2/mmiconslod.icons.lod test.txt test_ren.txt
mmarch optimize L1-folder-你好-2/mmiconslod.icons.lod
mmarch list L1-folder-你好-2/mmiconslod.icons.lod

mmarch rename .\L1-folder-你好-2/mmiconslod.icons.lod test_ren.txt test.txt
copy /Y test_orig.txt test.txt
mmarch create icons.lod mmiconslod . test.txt test
mmarch create mm78save.lod mm78save "L1-folder-你好-2\L2 folder" test.bin
mmarch merge L1-folder-你好-2/mmiconslod.icons.lod "./L1-folder-你好-2\L2 folder/mm78save.lod"
mmarch list ./L1-folder-你好-2/mmiconslod.icons.lod

mmarch merge icons.lod L1-folder-你好-2/mmiconslod.icons.lod
mmarch list icons.lod

mmarch d icons.lod test.bin
mmarch d L1-folder-你好-2/mmiconslod.icons.lod test.bin

mmarch create mm78gameslod.games.lod mm78gameslod "L1 folder\L2 folder mm78gameslod"
mmarch create mmsnd.snd mmsnd .
mmarch create mm6vid.vid mm6vid .\
mmarch create mm8loclod.T.lod mm8loclod "L1 folder"
mmarch create mm6gameslod.games.lod mm6gameslod "L1 folder\"
mmarch create mm78save.lod mm78save "L1-folder-你好-2\L2 folder"
mmarch create mm6save.mm6 mm6save "L1-folder-你好-2\L2 folder\"

mmarch extract "./L1 folder\L2 folder h3lod/h3lod.lod" my-h3lod-resource
mmarch extract mmbitmapslod.bitmaps.lod ./my-mmbitmapslod-resource/ testpal
mmarch extract mmbitmapslod.bitmaps.lod ./my-mmbitmapslod-resource2/ testpal.bmp


mmarch extract **/* resource_folder

mmarch extract */*.lod . *.txt

mmarch extract "**\//\/*.lod|vid" ../resource_folder




del *.txt *.lod *.vid *.snd *.exe *.str test
rmdir /s /q "L1 folder" "L1-folder-你好-2" my-h3lod-resource my-mmbitmapslod-resource my-mmbitmapslod-resource2 resource_folder "failsafe_test/thisfileisbroken_icons_lod" "../resource_folder"
