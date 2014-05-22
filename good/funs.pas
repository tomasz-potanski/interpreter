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
		str : String;
	begin
	//	return 3;
		//zwracam := 3;
		str := "1";
		if (zm.equals(str)) then
			zwracam := True;
		else
			zwracam := False;
		endif
	end

begin
	//print <-- ( zwracam() );
	a := 2;

	print <- ( 2*zwracam("1sdf") - 1 );
	
	//table[1]:="MMM";
	//print <--(table[0]);
	//p(table);
end.