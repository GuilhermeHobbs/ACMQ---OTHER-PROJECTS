PROGRAM TOUPPER;

VAR
  entrada,saida:TEXT;
  c:CHAR;
  nome1,nome2:STRING;

BEGIN
  Write('Entrada: '); ReadLn(nome1);
  Write('Saida: '); ReadLn(nome2);
  Assign(entrada,nome1);
  Assign(saida,nome2);
  Reset(entrada);
  ReWrite(saida);
  WHILE not Eof(entrada) DO BEGIN
    Read(entrada,c);
    Write(saida,UpCase(c));
  END;
  Close(entrada);
  Close(saida);
END.