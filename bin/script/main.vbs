'==============================================================================
'Стандартный ввод-вывод
'==============================================================================
sub print (str)
	app.stdout str
end sub

sub debug (str)
	app.stderr str
end sub

'==============================================================================
'Навигация по папкам
'==============================================================================
sub ls
	set Fs = CreateObject("Scripting.FileSystemObject")
	set FOlder =  Fs.GetFolder(".")
	for each ObjFolder in Folder.SubFolders
		Print ObjFolder.Name
	next
	for each ObjFile in Folder.Files
		Print ObjFile.Name
	next
	set Folder=nothing
	set Fs = nothing
end sub