var
	//f : Function ( Integer) : Integer;
	global : Integer;

proc
	procedure wart ( g : Function ( Integer) : Integer; ) ;
	begin
		g(global+1);
	end

	function druk ( aa : Integer ; ) : Integer;
	begin
		print <-- (global+1);
	end

begin
	global := 8;
	wart( drukuj );
	
end.