var
	a : Integer;
	table : Array[0..10] of String;
proc
	procedure p( x : Array[0..10] of String ;);
	begin
		print <-- (x[1]);
	end

	function zwracam() : Integer ;
	var
		locX : Integer ;
	begin
	//	return 3;
		zwracam := 3;
	end

begin
	print <-- ( 3*zwracam(); );
	//table[1]:="MMM";
	//print <--(table[0]);
	//p(table);
end.