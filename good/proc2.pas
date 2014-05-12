var
	globA : Integer ;
	globB : Integer;
	x : Integer;
	s : String;
	b : Boolean;
proc
	procedure wart ( zm : Boolean; ) ;
	var
		locX : Integer ;
	begin
		print <-- (zm);
	end
begin
	s:= "Ala mam kota";
	x:= 3;
	b := True;
	wart();	
	//wart(3*2);
end.