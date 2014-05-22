var
	b : Boolean;
	s : String;
	I : Integer;
	tabI : Array[0..10] of Integer;
proc

	function druk(zm : Integer;) : Array[0..10] of String;
	begin
		print <-- (zm);
		druk[0] := "Dzialam";
	end

begin
	b := True;
	s := "ala ma kota...";
	I := 34;
	tabI[1] := 3;
	print <-- ( druk(tabI[1]) );
end.