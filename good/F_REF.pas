var
	x : Integer;

proc
	procedure zwieksz( a : Integer ;);
	begin
		a := a + 7;
		print <-- (a);
	end

	procedure zwieksz2( ref a : Integer ;);
	begin
		a := a + 7;
		print <-- (a);
	end

begin
	x := 10;
	zwieksz(x);
	print <-- (x);

	zwieksz2(x);
	print <-- (x);
	
end.