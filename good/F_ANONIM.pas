var
	f : Function ( Integer ) : Void;
	global : Integer;
	s1, s2 : String;
	i : Integer;
	b : Boolean;
	tab : Array[0..10] of Function ( Integer) : Void;

proc
	function wart ( g : Function ( Integer) : Void; ) : Void;
	begin
		g(global+1);
	end

	function drukuj10 ( aa : Integer ; ) : Void;
	begin
		print <- (aa*10);
	end

	function wykonaj ( f : Function ( Integer) : Void; ) : Void;
	begin
		f(10);
		f(15);
	end

	function drukuj ( aa : Integer ; ) : Void;
	begin
		print <-- (aa);
	end

	function zwroc( aaa : String;) : Function ( Integer) : Void;
	begin
		//print <- ("sdfsdf");
		//print <-- (a);		
		print <-- (aaa);
		if aaa.equals("aaa") then
			zwroc := drukuj10;
		else
			zwroc := drukuj;
		endif
	end

	function zwroc2( ) : Function ( Integer) : Void;
	begin
		print <- ("sfsfd");
		zwroc2 := drukuj10;
	end

	function rint() : Integer;
	begin
		rint := 3;
	end

	function test() : Boolean;
	begin
		test := True;
	end

	

begin
	global := 8;
	wykonaj (
		function aa( x : Integer ;) : Void;
		var
			c : String;
		begin
			c := "ala ma kota..";
			print <-- (c);
			print <-- (x);
		end
	);


end.