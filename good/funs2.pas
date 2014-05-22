var
	b : Boolean;
	s : String;
	I : Integer;
	tabI : Array[0..10] of String;
proc

	function druk(zm : String;) : Array[0..10] of String;
	begin
		print <-- (zm);
		druk[0] := "Dzialam";
	end

begin
	b := True;
	s := "ala ma kota...";
	I := 34;
	tabI[1] := "sdfsd";
	print <-- ( druk(tabI[1]) );
end.