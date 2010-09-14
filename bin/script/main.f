( Удаляем "висячие" пробелы )
:clearr ( -- )
	GetEditorLinesCount 1 - var i
	i 0 do
		i GetEditorLine
		( todo: Добавить код для "срезания" пробелов справа )
		i Swap SetEditorLine
	loop
	drop
	InvalidateEditor
;

( Очищаем редактор )
:cleara ( -- )
	GetEditorLinesCount 1 - 0 do
		0 DeleteEditorLine
	loop
	AddEditorLine
	InvaLidateEditor drop
;
