var
	x : Integer;
	xx : Integer;

proc
	procedure print3( x : Integer ;);
	begin
		print <- (3);
		aa(3);
	end

begin
	x := 80;	
	xx := 6;

	function aa(a : Integer ;) : Void;
	var
		x : Integer;
	begin
		x := 4;
		a := x + a;
		print <-- (a);
	end;

	
	aa(xx);
	print <-- (xx);

	print3();
end.