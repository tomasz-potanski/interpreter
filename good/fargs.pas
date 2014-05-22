var
	f : Function ( Integer) : Void;
	global : Integer;

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

	function zwroc() : Function ( Integer) : Void;
	begin
		zwroc := drukuj10;
	end

begin
	global := 8;
	wart( drukuj );
	wart( drukuj10 );
	f:= zwroc();
end.