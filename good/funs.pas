var
	a : Integer;
	table : Array[0..10] of Integer;
proc
	procedure p( x : Integer ;);
	begin
		print <-- (x);
	end

	function zwracam() : Boolean ;
	var
		locX : Integer ;
	begin
	//	return 3;
		zwracam := True;
	end

begin
	//print <-- ( zwracam(); );
	table[0]:=7;
	//print <--(table[0]);
	p(table[0]);
end.