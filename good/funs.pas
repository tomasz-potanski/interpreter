var
	a : Integer;
	table : Array[0..10] of String;
proc
	procedure p( x : Array[0..10] of String ;);
	begin
		print <-- (x[1]);
	end

	function zwracam(zm : String;) : Boolean ;
	var
		locX : Integer ;
	begin
	//	return 3;
		//zwracam := 3;
		if ("1".equals(zm)) then
			zwracam := True;
		else
			zwracam := False;
		endif
	end

begin
	//print <-- ( zwracam() );
	a := 2;

	print <- ( 2*zwracam("1") - 1 );
	
	//table[1]:="MMM";
	//print <--(table[0]);
	//p(table);
end.