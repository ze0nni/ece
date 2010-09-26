'==============================================================================
'Стандартный ввод-вывод
'==============================================================================

'Вывод сообщения в консоль
sub print (str)
	Application.stdout str
end sub

'Вывод сообщения об ошибке в консоль
sub debug (str)
	Application.stderr str
end sub

'процедура выводит строку в активном редакторе на месте курсора
'и смещает курсор
sub Write (str)
	set doc = Application.ActiveDocument
	set Caret = doc.Caret
	doc.Lines(Caret.y).Insert str, Caret.x + 1
	caret.x = caret.x + len(str)
end sub

'==============================================================================
'Навигация по папкам
'==============================================================================

public CourientDir
CourientDir = "."

sub ls
	set Fs = CreateObject("Scripting.FileSystemObject")
	set FOlder =  Fs.GetFolder(CourientDir)
	for each ObjFolder in Folder.SubFolders
		Print "[" & ObjFolder.Name & "]"
	next
	for each ObjFile in Folder.Files
		Print ObjFile.Name
	next
	set Folder=nothing
	set Fs = nothing
end sub

sub cd(newDir)
	CourientDir = NewDir	
end sub

function dir
	dir = CourientDir
end function

'==============================================================================
'Утилиты работы с редактором
'==============================================================================
'Выводит краткую ифнормацию о документе
sub DocInfo
	set Doc = Application.ActiveDocument
	print "Документ: " & Doc.FileName
	print "Строк: " & Doc.LinesCount
    'Подсчет символов и слов (пробелов)
    dim CharsCount
    dim WordsCount
    CharsCount = 0
    WordsCount = 0
    for i = 0 to Doc.LinesCount - 1
        CharsCount = CharsCount + Doc.Lines(i).Length
    next

	print "Слов: " & WordsCount
	print "Символов: " & CharsCount
	set Doc = nothing
end sub

'Удаляет пробелы в конце строки
sub ClearR
    set Doc = Application.ActiveDocument
    for i = 0 to Doc.LinesCount - 1
        Doc.Lines(i).Text = RTrim(Doc.Lines(i))
    next
    Doc.Invalidate
    set Doc = nothing
end sub
'==============================================================================
'Возня с классами
'==============================================================================
Class ProjectClass
	public function GetName
		GetName = "Noname"
	end function
	
	public Sub InsertDocument
		Print """" & Application.ActiveDocument.FileName & """ добавлен к проекту."
	end sub
end class

dim Project 
set Project = new ProjectClass
