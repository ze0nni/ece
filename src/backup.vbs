'Скрипт предназначен для создания backup архивов
'папок с проектами.
'Есть проблемы с именами файлов на кириллице
'Но мы тру, поэтому обойдемся
'================================================
'	Отредактируйте следующие переменные
'	под свои нужды
'
'Название проекта. Оторажается в имени архива
ProjectName = "Ece"

'Папка расположения проекта
ProjectFolder = ".\"

'Папка, куда следует положить архив
BackUpFolder = ".\__backup\"

'Файлы которые следует не включать в архив
ExceptFiles = ".*\.(vbs|zip|rar|exe|dll|dcu|local|identcache|tvsconfig|~*~|qpf|qsf)"

'Игнорированть ргистр букв
IgnoreCase = true

'================================================


function GetFileName
'Функция формирования имени архива вида
'НАЗВАНИЕПРОЕКТА_src_ГодМесяцДень_ЧасыМинуты.zip
'Может быть есть в vb и форматированый вывод, я не знаю
'буду делать как школьники
	y = Year(now)
	m = month(now)
	if m < 10 then m = "0" & m
	d = Day(now)
	if d < 10 then d = "0" & d
	hrs = Hour(now)
	if hrs < 10 then hrs = "0" & hrs
	mts = Minute(now)
	if mts < 10 then mts = "0" & mts	
	GetFileName = ProjectName & "_src_" & y & m & d & "_" & hrs & mts
end function

set RegExpObj = CreateObject("VBScript.RegExp")
RegExpObj.IgnoreCase = IgnoreCase
RegExpObj.Pattern = ExceptFiles

function CheckFileName(FileName)
'Пропускаем имя файла через регексп и 
	CheckFileName = not RegExpObj.Test(FileName)
end function

'Создаем объект для работы с cab-архивами
set CabObj = CreateObject("MakeCab.MakeCab")
'Файловая система
set Fs = CreateObject("Scripting.FileSystemObject")

filescount = 0

'Создаем cab-файл в указанной папке
CabObj.CreateCab BackUpFolder & GetFileName & ".zip", False, False, False
'Вызываем рекурсивную функцию добавления папок и файлов в архив
InserDir Fs.GetFolder(ProjectFolder), ""

	sub InserDir(Folder, FolderName)
		'Просматриваем все файлы в папке
		for each ObjFile in Folder.Files
			if CheckFileName(ObjFile.Name) then
				'Если файл подходит, добавляем в архив
				CabObj.AddFile ObjFile.Path, FolderName & ObjFile.Name
				filescount = filescount + 1
			end if
		next
		'Пробегаем все папки
		for each ObjFolder in Folder.SubFolders
			'Рекурсивный вызов для всех вложенных папок
			InserDir ObjFolder, FolderName & ObjFolder.Name & "\"
		next
	end sub	
'Закрываем и сохраняем 
CabObj.CloseCab

if msgbox("Операция архивирования завершена!" & chr(13) & _
	"Файлов в архиве:" & chr(9) & filescount & chr(13) & _
	"Показать файл в проводнике?", _
	vbYesNo or vbDefaultButton2, _
	"123") = vbYes then

end if

'Тут было бы правильно освободить все объекты
'но мне лень делать правильно. ВСе равно все
'ссылки пропадут и объекты уничтожатся сами