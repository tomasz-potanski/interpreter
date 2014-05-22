var
	//f : Function ( Integer) : Integer;
	global : Integer;

proc
	procedure wart ( g : Function ( Integer) : Void; ) ;
	begin
		g(global+1);
	end

	function drukuj ( aa : Integer ; ) : Void;
	begin
		print <-- (aa);
	end

	function drukuj10 ( aa : Integer ; ) : Void;
	begin
		print <- (aa*10);
	end

begin
	global := 8;
	wart( drukuj10 );
	
end.