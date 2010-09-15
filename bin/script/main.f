( ”дал€ем "вис€чие" пробелы )
:clr ( -- )
	GetEditorLinesCount 1 -  var i i 0 do
		i GetEditorLine

		i SetEditorLine
	loop
	drop		
;


:cls ( очищает строку )
( -- )
	"" GetEditorCaretLine SetEditorLine
;

( ќчищаем редактор )
:da ( -- )
	GetEditorLinesCount 1 - 0 do
		0 DeleteEditorLine
	loop
	AddEditorLine
	InvaLidateEditor
;

:du ( удал€ет n строк вверх, начита€ с текущей )
    ( n -- )

;

:dd ( удал€ет n строк вниз, начита€ с текущей )
    dup 0 <> if
	GetEditorCaretLine + 1 -  var i
    i GetEditorCaretLine do
        i DeleteEditorLine
    loop
    drop
else
    drop
    then
;

:ETime Now Time EditorInsert;
:EDate Now Date EditorInsert;
:EDTime Now DateTime EditorInsert;
