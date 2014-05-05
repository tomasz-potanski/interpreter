var

	table : Array[1..10] of Integer ;
	tableBool : Array[1..10] of Boolean;
	jeden, dwa : Integer;
	b1, b2 : Boolean;

begin
	jeden := 1;
	dwa := 2;
	dwa := jeden;
	table[2]:= jeden;
	table[3]:= 3;


//	table[4]:="Zly napis";
//	dwa:="Zly napis";
	

	jeden:=table[5];
	table[6]:=6;
	table[7]:=7;
	table[8]:=8;
	print table[5];
	print table[6];
	print table[7];

	tableBool[1] := 2 > 1;
	tableBool[2] := 4 == 4;
	b1:=True;

	//if True then print "OK";

	print b1;
	print tableBool[1];
	print tableBool[2];
end.