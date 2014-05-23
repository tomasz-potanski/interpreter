var
	f : Function ( Integer ) : Void;
	global : Integer;
	s1, s2 : String;
	i : Integer;
	b : Boolean;
	tab : Array[0..10] of Function ( Integer) : Void;

proc
	procedure wart ( g : Function ( Integer) : Void; ) ;
	begin
		g(global+1);
	end

	procedure drukuj10 ( aa : Integer ; );
	begin
		print <- (aa*10);
	end

	function drukuj ( aa : Integer ; ) : Void;
	begin
		print <-- (aa);
	end

	function zwroc( aaa : Boolean;) : Function ( Integer) : Void;
	begin
		//print <- ("sdfsdf");
		//print <-- (a);		
		print <-- (aaa);
		if aaa then
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

	f:= zwroc(True);
	f(10);
	f:= zwroc(False);
	f(10);


end.