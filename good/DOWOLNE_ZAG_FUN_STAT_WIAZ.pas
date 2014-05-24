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
		
		function druk ( asd : Integer ; ) : Void;
		begin
			print <-- (asd);
		end;

		function druk2(co : Integer;) : Void;
		begin
			druk(co);
		end;
		
		druk2(a);
	end;

	
	aa(xx);
	print <-- (xx);

	//druk(xx); <-- FAILS
	//druk2(xx); <-- FAILS
end.