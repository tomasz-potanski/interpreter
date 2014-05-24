var
	x : Integer;
	s : String;
	b : Boolean;
	tab : Array[0..10] of Integer;

proc
	procedure zwieksz( a : Integer ;);
	begin
		a := a + 7;
		print <-- (a);
	end

	procedure zwieksz2( a : Integer ;);
	begin
		a := a + 7;
		print <-- (a);
	end

	procedure zwiekszA( a : Array [1..10] of Integer ;);
	begin
		a[1] := a + 7;
		print <-- (a[1]);
	end

	procedure zwiekszA2( a : Array [1..10] of Integer ;);
	begin
		a[1] := a + 7;
		print <-- (a[1]);
	end

	procedure changeS( a : String ;);
	begin
		a := "Po przypisaniu w funkcji";
		print <-- (a);
	end

	procedure changeS2( a : String ;);
	begin
		a := "Po przypisaniu w funkcji";
		print <-- (a);
	end

	procedure changeB( a : Boolean ;);
	begin
		a := True;
		print <-- (a);
	end

	procedure changeB2( a : Boolean ;);
	begin
		a := True;
		print <-- (a);
	end

begin
/*	x := 10;
	zwieksz(x);
	print <-- (x);

	zwieksz2(ref x);
	print <-- (x);

	s := "Przed wywolaniem";
	changeS(s);
	print <-- (s);

	changeS2(ref s);
	print <-- (s);

	b := False;
	changeB(b);
	print <-- (b);

	changeB2(ref b);
	print <-- (b);

	tab[1] := 10;
	zwiekszA(tab);
	print <-- (tab[1]);

	zwiekszA(ref tab);
	print <-- (tab[1]);*/
	
end.