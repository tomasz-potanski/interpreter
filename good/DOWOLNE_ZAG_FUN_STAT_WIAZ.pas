var
	x : Integer;
	xx : Integer;
begin
	x := 80;	
	xx := 6;

	function aa(a : Integer ;) : Void;
	var
		x : Integer;
	begin
		x := 4;
		a := x + a;
		print <- (a);
	end;

	
	aa(xx);
	print <-- (xx);
end.