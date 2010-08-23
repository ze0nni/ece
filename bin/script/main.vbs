'==============================================================================
'Стандартный ввод-вывод
'==============================================================================
sub print (str)
	Application.stdout str
end sub

sub debug (str)
	Application.stderr str
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
sub DocInfo
	set Doc = Application.Documents(0)
	print "Документ: " & Doc.FileName
	print "Строк: " & Doc.LinesCount
	print "Слов: "
	print "Символов: "
	set Doc = nothing
end sub