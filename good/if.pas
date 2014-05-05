// Program prezentuje implementacje instrukcji warunkowej: if
var
	jeden, dwa, result : Integer;
begin
	jeden:=1;
	dwa:=2;
	result:=0;
	if jeden < 2 then print "OK" ;
	if 2 < 3 then result := 2; else result:= 3;
	print result;

end.