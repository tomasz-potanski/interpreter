var
	f : Function ( Integer) : Void;
	global : Integer;
	s1, s2 : String;
	i : Integer;
	b : Boolean;

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

	function zwroc( a : Integer;) : Function ( Integer) : Void;
	begin
		zwroc := drukuj10;
	end

	function test() : Boolean;
	begin
		test := True;
	end

begin
	global := 8;
	wart( drukuj );
	wart( drukuj10 );

	f:= zwroc(global);
	f(10);
	wart ( f );

end.