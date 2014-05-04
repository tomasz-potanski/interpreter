//	Program prezentuje działanie przesłaniania zmiennych przy blokach
//

var 
	x : Integer;

begin
	x:=1;
	begin
		x:= 7;
		print x;
	end
	print x;

end.