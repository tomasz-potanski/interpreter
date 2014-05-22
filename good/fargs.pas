var
	f : Function ( Integer) : Integer;
	global : Integer;

proc
	procedure wart ( zm : Integer; ) ;
	begin
		g(global+1);
	end

begin
	global := 8;
	wart( drukuj{ global } );
	
end.