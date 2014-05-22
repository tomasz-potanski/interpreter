var
	a : Integer;
	table : Array[0..10] of String;
proc
	procedure p( x : Array[0..10] of String ;);
	begin
		print <-- (x[1]);
	end

	function zwracam(zm : Array[0..10] of String;) : Boolean ;
	var
		locX : Integer ;
		str : String;
	begin
	//	return 3;
		//zwracam := 3;
		str := "1";
		if (zm[1].equals("1")) then
			zwracam := True;
		else
			zwracam := False;
		endif
	end

begin
	//print <-- ( zwracam() );
	a := 2;
	table[1]:="1";
	print ( table[1].equals("1") );
	//print <- ( 2*zwracam(table) - 1 );
	
	//table[1]:="MMM";
	//print <--(table[0]);
	//p(table);
end.