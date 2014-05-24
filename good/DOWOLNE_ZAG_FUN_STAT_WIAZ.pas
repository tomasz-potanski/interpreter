begin
	function aa ( a : Integer ;) : Void;
	var
		x : Integer;
	begin
		x := 4;
		a := x + x;
		print <-- (a);
	end;

	aa(6);
end.