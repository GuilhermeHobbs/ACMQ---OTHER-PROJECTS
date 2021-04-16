PROGRAM Ana;
{*****************************************************************************
 * Analise de Amplificadores                                                 *
 * Antonio Carlos Moreirao de Queiroz - UFRJ - 1990                          *
 * Versao 1.0 de 14/4/90                                                     *
 * Versao 1.1 de 20/6/90                                                     *
 * Versao 1.2 de 27/7/90 1a. iteracao corrigida                              *
 * Versao 1.3 de 22/8/90 Preparacao de arquivo para analise AC incluida      *
 * Versao 1.4 de 17/10/90 Calculo das capacitancias dos transistores         *
 * Versao 1.5 de 12/01/91 Modelo de Ebers-Moll para os transistores          *
 * Versao 1.6 de 07/11/91 Corrigido calculo de correntes no tr. PNP; .net    *
 * Versao 1.7 de 11/02/92 Eliminada inversao de Yn, ganhos e impedancias     *
 * Versao 1.7a de 24/04/92 Limitacao de variacao, novos CA e transistores    *
 * Versao 1.8 de 25/05/92 Cbe>0, Uso de gmin para ajudar convergencia.       *
 * Versao 1.8a de 07/06/93 Aceita CA/CC ou AC/DC                             *
 * Versao 1.8b de 20/04/94 Fonte Z                                           *
 * Versao 1.8c de 11/12/95 Melhor listagem durante execucao, teste de elmax  *
 *****************************************************************************}
{$IFDEF DOUBLE}
{$N+,E+}
{$ENDIF}

USES Crt;

{$IFDEF DOUBLE}
TYPE REAL=EXTENDED;
{$ENDIF}

CONST
  versao='1.8c de 11/12/95';
  maxnos=70;        {Maximo numero de nos}
  maxel=170;        {Maximo numero de elementos}
  cm=15;            {Tamanho de campo em reais}
  dc=9;             {Decimais em reais}
  dig=10;           {Digitos em reais em forma cientifica}
  tolv1=1e-2;       {Tolerancia inicial para tensoes nodais}
  tolv2=1e-6;       {Tolerancia final para tensoes nodais}
  tolp=0;           {Tolerancia para valor de pivot}
  dmaxit=100;       {Iteracoes ate interrupcao automatica}
  Vt=25e-3;         {Vt}
  Is=3.7751345e-14; {Is para Vd=0.6V com Id=1mA nos transistores}
  vdmax=0.7;        {Limite superior de tensao em diodo para calculo de Gd}
  vdmin=-0.5;       {Limite inferior "}
  alfar=0.5;        {alfa reverso}
  gminmin=1e-14;    {Condutancia gmin final}
  gminmax=1e-3;     {Condutancia gmin inicial}
  fator=20;         {Fator de divisao para gmin}

TYPE
  elemento=(resistor,transcondutor,fontev,fontei,transistor,diodo,capacitor,indutor);
  modo=(ativo,cortado,saturado,invertido);

VAR
  Yn:ARRAY[0..maxnos,0..maxnos+1] OF REAL;
  E:ARRAY[0..maxnos] OF REAL;
  NetList:ARRAY[1..maxel] of
    RECORD
      nome:STRING[10];
      n1,n2:INTEGER;
      CASE elemento OF
        resistor:      (resistencia:REAL);
        transcondutor: (n21,n22:INTEGER; Gm:REAL);
        fontev:        (rserie,tensao:REAL; vcc:BOOLEAN);
        fontei:        (corrente:REAL; icc:BOOLEAN);
        transistor:    (n3:INTEGER; alfa,Gbc,Gbe,Ibc,Ibe:REAL; npn:BOOLEAN);
        diodo:         (Gd,Id:REAL);
        capacitor:     (capacitancia:REAL);
        indutor:       (indutancia:REAL);
    END;
  n,m,el,i,j,l,iter,maxit:INTEGER;
  ch:CHAR;
  default,nomearq,arqca:STRING;
  convergencia:BOOLEAN;
  tolv,gmin,maxdelta,Vd,HFE,Vbc,Vbe,Vce,Ic,Ie,Ib,Cbc,Cbe,ft,exponencial:REAL;
  estado:modo;
  arquivo:TEXT;

LABEL Inicio,Ler,DeNovo,Insistir;

PROCEDURE Espera;
BEGIN
  Write('Toque Return para continuar...');
  ReadLn
END;

PROCEDURE ErroFatal(txt:STRING);
BEGIN
  WriteLn;
  WriteLn('* Erro: ',txt);
  Espera;
  Halt
END;

PROCEDURE Centrar(txt:STRING);
BEGIN
  WriteLn(txt:(80+Length(txt)) div 2);
END;

PROCEDURE LerNetList;
VAR
  ok:BOOLEAN;
  cacc:STRING[2];
BEGIN
  REPEAT
    ok:=FALSE;
    Write('Arquivo de entrada: (.net) ou ? (',default,') '); ReadLn(nomearq);
    IF nomearq='?' THEN BEGIN
      ClrScr;
      WriteLn('Este programa calcula tensoes de polarizacao CC para circuitos compostos');
      WriteLn('de transistores bipolares e outros componentes resistivos.');
      WriteLn('O circuito a analisar e descrito em arquivo no formato:');
      WriteLn('1a. linha: No. de nos (exceto o terra, no 0).');
      WriteLn('Linhas seguintes: Uma descricao de componente por linha:');
      WriteLn;
      WriteLn(' - Resistor: R<nome> <no1> <no2> <resistencia em Ohms>');
      WriteLn(' - Fonte de corrente: I<nome> <noI+> <noI-> <corrente em Amperes> CA/CC');
      WriteLn(' - Fonte de tens„o: Z<nome> <noV-> <noV+> <rserie em Ohms> <tensao em V> CA/CC');
      WriteLn(' - Fonte de corrente controlada a tensao:');
      WriteLn('   G<nome> <noI+> <noI-> <noV+> <noV-> <transcondutancia (Gm) em Mhos>');
      WriteLn(' - Diodo: D<nome> <noAnodo> <noCatodo>');
      WriteLn(' - Transistor: Q<nome> <noC> <noB> <noE> <HFE> ... NPN/PNP');
      WriteLn(' - Capacitor: C<nome> <no1> <no2> <capacitancia em Farads>');
      WriteLn(' - Indutor: L<nome> <no1> <no2> <indutancia em Henrys>');
      WriteLn;
      WriteLn('-O arquivo pode ser gerado pelo programa EdFil.');
      WriteLn('-Capacitores e indutores sao tratados como resistores de 1e9 ou 1e-9 Ohms.');
      WriteLn('-Fontes CA sao consideradas nulas na analise.');
      WriteLn('-Os transistores sao modelados pelo modelo de Ebers-Moll.');
      WriteLn('-Um arquivo pode ser gerado para analise CA em outro programa, incluindo');
      WriteLn(' os modelos CA dos transistores, com capacitancias aproximadas.');
      WriteLn;
    END
    ELSE BEGIN
      IF nomearq='' THEN nomearq:=default ELSE default:=nomearq;
      IF Pos('.',nomearq)=0 THEN nomearq:=nomearq+'.net';
      Assign(arquivo,nomearq);
      {$I-} Reset(arquivo); {$I-}
      IF IOResult<>0 THEN
        WriteLn('Arquivo inexistente')
      ELSE ok:=TRUE
    END;
    IF ok THEN BEGIN
      ReadLn(arquivo,n);
      WriteLn(#10'Nos= ',n);
      IF n>maxnos THEN ErroFatal('Numero excessivo de nos');
      m:=n+1;
      el:=0;
      WHILE not SeekEof(arquivo) DO BEGIN
        Inc(el);
        IF el>maxel THEN ErroFatal('Numero excessivo de elementos');
        WITH NetList[el] DO BEGIN
          nome:='';
          {$I-}
          REPEAT Read(arquivo,ch); nome:=nome+ch UNTIL (ch=' ') or (Eof(arquivo));
          IF Eof(arquivo) THEN ErroFatal('Fim inesperado do arquivo');
          Write(nome);
          CASE UpCase(nome[1]) OF
            'C':BEGIN {Capacitor}
                  ReadLn(arquivo,n1,n2,capacitancia);
                  WriteLn('c=',capacitancia:dig,' Farads, entre ',n1,' e ',n2);
                END;
            'L':BEGIN {Indutor}
                  ReadLn(arquivo,n1,n2,indutancia);
                  WriteLn('l=',indutancia:dig,' Henrys, entre ',n1,' e ',n2);
                END;
            'R':BEGIN {Resistor}
                  ReadLn(arquivo,n1,n2,resistencia);
                  WriteLn('r=',resistencia:dig,' Ohms, entre ',n1,' e ',n2);
                  IF resistencia=0 THEN ErroFatal('resistencias nao podem ser nulas');
                END;
            'G':BEGIN {Fonte de corrente controlada a tensao}
                  ReadLn(arquivo,n1,n2,n21,n22,Gm);
	          WriteLn('gm=',Gm:dig,' Mhos, entre  ',n1,' (i+), ',n2,' (i-), ',n21,' (v+) e ',n22,' (v-)')
                END;
            'Z':BEGIN {Fonte de tensao com resistor em serie}
                  ReadLn(arquivo,n2,n1,rserie,tensao,ch,cacc);
	          Write('rs=',resistencia:dig,' Ohms, tensao=',tensao:dig,
                  ' Volts, entre ',n1,' (v+) e ',n2,' (v-), ');
                  vcc:=(cacc='CC') or (cacc='DC');
                  IF vcc THEN WriteLn('CC') ELSE WriteLn('CA');
                  IF rserie=0 THEN ErroFatal('resistencias nao podem ser nulas');
                END;
            'I':BEGIN {Fonte de corrente}
	          ReadLn(arquivo,n1,n2,corrente,ch,cacc);
	          Write('i=',corrente:dig,' Amperes entre ',n1,' (i+) e ',n2,' (i-), ');
                  icc:=(cacc='CC') or (cacc='DC');
                  IF icc THEN WriteLn('CC') ELSE WriteLn('CA');
                END;
            'Q':BEGIN {Transistor bipolar - modelo de Ebers-Moll}
	          Read(arquivo,n1,n2,n3,HFE);
                  REPEAT Read(arquivo,ch) UNTIL ch in ['N','P',#10,^Z];
                  IF ch<>#10 THEN ReadLn(arquivo);
                  npn:=UpCase(ch)<>'P'; {Se nada for dito, e NPN}
	          Write('HFE=',HFE:dig);
                  IF npn THEN Write(' NPN') ELSE Write(' PNP');
	          WriteLn(' entre ',n1,' (c), ',n2,' (b) e ',n3,' (e)');
                  alfa:=HFE/(HFE+1);
                END;
            'D':BEGIN {Diodo de juncao}
                  ReadLn(arquivo,n1,n2);
                  WriteLn(' entre ',n1,' (anodo) e ',n2,' (catodo)')
                END;
            ELSE ErroFatal('Elemento desconhecido');
            END
          END;
          {$I+}
          IF IOResult<>0 THEN ErroFatal('Formato incorreto');
          IF el mod 15=0 THEN Espera;
        END;
      Close(arquivo);
      WriteLn('Lidos ',el,' elementos')
    END;
  UNTIL ok;
END;

PROCEDURE MontarSistema;

  PROCEDURE Condutancia(G:REAL; n1,n2:INTEGER);
  {Bastaria a transcondutancia, mas isto deixa mais claro o codigo}
  BEGIN
    Yn[n1,n1]:=Yn[n1,n1]+G;
    Yn[n2,n2]:=Yn[n2,n2]+G;
    Yn[n1,n2]:=Yn[n1,n2]-G;
    Yn[n2,n1]:=Yn[n2,n1]-G;
  END;

  PROCEDURE Transcondutancia(G:REAL; n1,n2,n3,n4:INTEGER);
  BEGIN
    Yn[n1,n3]:=Yn[n1,n3]+G;
    Yn[n2,n4]:=Yn[n2,n4]+G;
    Yn[n1,n4]:=Yn[n1,n4]-G;
    Yn[n2,n3]:=Yn[n2,n3]-G;
  END;

  PROCEDURE Fonte(I:REAL; n1,n2:INTEGER);
  BEGIN
    Yn[n1,n+1]:=Yn[n1,n+1]-I;
    Yn[n2,n+1]:=Yn[n2,n+1]+I;
  END;

  PROCEDURE Diodo(n1,n2:INTEGER; VAR Gd,Id:REAL);
  BEGIN
    Vd:=E[n1]-E[n2];
    {Limita valores de Vd, para evitar "overflow" ou "underflow"}
    IF Vd>vdmax THEN Vd:=vdmax
    ELSE IF Vd<vdmin THEN Vd:=vdmin;
    exponencial:=Exp(Vd/Vt);
    Gd:=(Is/Vt)*exponencial;
    Id:=Is*(exponencial-1)-Gd*Vd;
    Condutancia(Gd+gmin,n1,n2); {gmin facilita a convergencia}
    Fonte(Id,n1,n2);
  END;

BEGIN
  {Zeramento do sistema nodal}
  FOR i:=0 TO n DO
    FOR j:=0 TO m DO
      Yn[i,j]:=0;
  {Percorrimento do net-list, montando estampas}
  FOR i:=1 TO el DO WITH NetList[i] DO
    CASE UpCase(nome[1]) OF
      'R':Condutancia(1/resistencia,n1,n2);
      'C':Condutancia(1e-9,n1,n2);
      'L':Condutancia(1e9,n1,n2);
      'G':Transcondutancia(Gm,n1,n2,n21,n22);
      'Z':BEGIN
	    Condutancia(1/rserie,n1,n2);
	    IF vcc THEN Fonte(tensao/rserie,n2,n1);
	  END;
      'I':IF icc THEN Fonte(corrente,n1,n2);
      'D':Diodo(n1,n2,Gd,Id);
      'Q':IF npn THEN BEGIN
            Diodo(n2,n3,Gbe,Ibe);
            Fonte(alfa*Ibe,n1,n2);
            Transcondutancia(alfa*Gbe,n1,n2,n2,n3);
            Diodo(n2,n1,Gbc,Ibc);
            Fonte(alfar*Ibc,n3,n2);
            Transcondutancia(alfar*Gbc,n3,n2,n2,n1)
	  END
          ELSE BEGIN
            Diodo(n3,n2,Gbe,Ibe);
            Fonte(alfa*Ibe,n2,n1);
            Transcondutancia(alfa*Gbe,n1,n2,n2,n3);
            Diodo(n1,n2,Gbc,Ibc);
            Fonte(alfar*Ibc,n2,n3);
            Transcondutancia(alfar*Gbc,n3,n2,n2,n1)
          END;
      END;
END;

PROCEDURE ResolverSistema;
{Resolve o sistema nodal pelo metodo de Gauss-Jordan (diagonalizacao)}
VAR
  t,p:REAL;
  i,j,l,a:INTEGER;
BEGIN
  FOR i:=1 TO n DO BEGIN
    {Condensacao pivotal parcial. Resolve quase qualquer circuito}
    t:=0; a:=i;
    FOR l:=i TO n DO
      IF ABS(Yn[l,i])>ABS(t) THEN BEGIN a:=l; t:=Yn[l,i] END;
    IF i<>a THEN
      FOR l:=i TO m DO
	BEGIN p:=Yn[i,l]; Yn[i,l]:=Yn[a,l]; Yn[a,l]:=p END;
    {Testa "pivot" nulo}
    IF ABS(t)<=tolp THEN ErroFatal('Sistema singular'); {Quase certamente}
    FOR j:=m DOWNTO i+1 DO BEGIN
      Yn[i,j]:=Yn[i,j]/t; p:=Yn[i,j];
      FOR l:=1 TO n DO
	IF i<>l THEN
	  Yn[l,j]:=Yn[l,j]-Yn[l,i]*p;
    END
  END
END;

PROCEDURE TestarConvergencia;
VAR
  delta:REAL;
BEGIN
  convergencia:=TRUE;
  FOR i:=1 TO n DO BEGIN
    delta:=Yn[i,n+1]-E[i];
    convergencia:=convergencia and (Abs(delta)<tolv);
    IF Abs(delta)<maxdelta THEN E[i]:=Yn[i,n+1]
    {Limita a variacao se grande demais, para evitar divergencia}
    ELSE IF delta>0 THEN E[i]:=E[i]+maxdelta
    ELSE E[i]:=E[i]-maxdelta
  END;
  IF iter>0 THEN GoToXY(1,WhereY-n-1);
  ClrScr;
  FOR i:=1 TO n DO BEGIN
    Write('e',i:2,': ',E[i]:cm:dc,'; ');
    IF i mod 3=0 THEN WriteLn
  END;
  WriteLn;
  WriteLn('Iteracoes:',iter:3,' Gmin=',gmin:7);
END;

PROCEDURE AcharPonto(q:INTEGER); {Ponto de operacao dos transistores}
BEGIN
  WITH netlist[q] DO BEGIN
    Vbe:=E[n2]-E[n3];
    Vbc:=E[n2]-E[n1];
    Vce:=E[n1]-E[n3];
    IF npn THEN BEGIN
      Ic:=-Gbc*Vbc-Ibc+alfa*(Ibe+Gbe*Vbe);
      Ie:=-Gbe*Vbe-Ibe+alfar*(Ibc+Gbc*Vbc);
    END
    ELSE BEGIN
      Ic:=-Gbc*Vbc+Ibc+alfa*(-Ibe+Gbe*Vbe);
      Ie:=-Gbe*Vbe+Ibe+alfar*(-Ibc+Gbc*Vbc);
    END;
    Ib:=-Ie-Ic;
    IF ((Vbe>0) and npn) or ((Vbe<0) and not npn) THEN BEGIN
      IF ((Vbc<=0) and npn) or ((Vbc>=0) and not npn) THEN estado:=ativo
      ELSE estado:=saturado
     END
     ELSE BEGIN
       IF ((Vbc<=0) and npn) or ((Vbc>=0) and not npn) THEN estado:=cortado
       ELSE estado:=invertido
     END;
  END
END;

BEGIN
  ClrScr;
  Centrar('Analise de Amplificadores');
  Centrar('-------------------------');
  Centrar('Programa demonstrativo de analise nodal para circuitos resistivos nao lineares');
  Centrar('Antonio Carlos Moreirao de Queiroz - UFRJ/1990');
  Centrar('Versao '+versao);
  WriteLn;
  IF ParamCount>0 THEN default:=ParamStr(1) ELSE default:='';
 Inicio:
  LerNetList;
 DeNovo:
  iter:=0;
  gmin:=gminmax;
  {Inicializa as tensoes nodais, inclusive E[0] (para evitar "overflow")}
  FOR i:=0 TO n DO E[i]:=0;
  Write('Maxima variacao de tensao nodal entre passos (0.1-10): ');
  ReadLn(maxdelta);
  maxit:=dmaxit; tolv:=tolv1;
 Insistir:
  ClrScr;
  {Ciclo do metodo de Newton-Raphson}
  WriteLn('Calculando o ponto de operacao:');
  REPEAT
    MontarSistema;
    {Soma condutancia gmin a todos os nos, para suavizar nao linearidades}
    {gmin tambem e somada a condutancias de diodos na rotina Diodo}
    FOR i:=1 TO n DO Yn[i,i]:=Yn[i,i]+gmin;
    ResolverSistema;
    TestarConvergencia;
    {Reduz gmin apos cada convergencia preliminar, eliminando-a ao final}
    IF convergencia THEN BEGIN
      IF gmin>gminmin THEN BEGIN
        gmin:=gmin/fator;
        convergencia:=FALSE;
      END
      ELSE IF gmin<>0 THEN BEGIN
        tolv:=tolv2;
        gmin:=0;
        convergencia:=FALSE
      END
    END;
    Inc(iter);
  UNTIL convergencia or (iter>maxit) or KeyPressed;
  IF convergencia THEN WriteLn('Solucao encontrada')
  ELSE BEGIN
    Write('Convergencia nao atingida: [I]nsistir [T]entar de novo [D]esistir'#13);
    ch:=ReadKey;
    IF UpCase(ch)='T' THEN GoTo DeNovo;
    IF UpCase(ch)='I' THEN BEGIN maxit:=maxit+dmaxit; GoTo Insistir END
  END;
 Ler:
  ClrEol;
  Write('[T]ensoes nodais [P]ontos de operacao [A]rquivo CA [R]einicio [F]im'#13);
  ch:=ReadKey;
  ClrEol;
  CASE UpCase(ch) OF
    'T':BEGIN
	  WriteLn('Tensoes nodais CC:');
	  FOR i:=1 TO n DO BEGIN
	    WriteLn('e',i:2,': ',E[i]:cm:dc);
            IF i mod 20=0 THEN Espera
          END;
	  GoTo Ler
	END;
    'P':BEGIN
          WriteLn('Pontos de operacao dos transistores:');
          j:=0;
          FOR i:=1 TO el DO WITH NetList[i] DO
            IF UpCase(nome[1])='Q' THEN BEGIN
              Write(nome,' (c=',n1,', b=',n2,', e=',n3,', ');
              IF npn THEN Write('NPN) ') ELSE Write('PNP) ');
              AcharPonto(i);
              CASE estado OF
                ativo:WriteLn('ativo');
                cortado:WriteLn('cortado');
                saturado:WriteLn('saturado');
                invertido:WriteLn('invertido')
              END;
              WriteLn('   ic: ',Ic:cm:dc,';   ie: ',Ie:cm:dc,';   ib: ',Ib:cm:dc);
              WriteLn('  vbc: ',Vbc:cm:dc,';  vbe: ',Vbe:cm:dc,';  vce: ',Vce:cm:dc);
              WriteLn('  Gbe: ',Gbe:cm:dc,';  Ibe: ',Ibe:cm:dc);
              WriteLn('  Gbc: ',Gbc:cm:dc,';  Ibc: ',Ibc:cm:dc);
              Inc(j);
              IF j mod 4=0 THEN Espera
            END;
          GoTo Ler
        END;
    'R':GoTo Inicio;
    'A':BEGIN
          arqca:=Copy(default+'CA',1,8);
          Write('Arquivo a criar para analise CA (',arqca,'): '); ReadLn(nomearq);
          Write('Cbc(F) e ft(Hz) a usar: '); ReadLn(Cbc,ft);
          IF nomearq='' THEN nomearq:=arqca;
          IF Pos('.',nomearq)=0 THEN nomearq:=nomearq+'.net';
          Assign(arquivo,nomearq);
          {Reescreve o "netlist", ignorando fontes CC}
          ReWrite(arquivo);
          WriteLn(arquivo,n);
          FOR i:=1 TO el DO WITH NetList[i] DO CASE UpCase(nome[1]) OF
            'C':WriteLn(arquivo,nome,' ',n1,' ',n2,' ',capacitancia,' ',E[n1]-E[n2]);
            'L':WriteLn(arquivo,nome,' ',n1,' ',n2,' ',indutancia,' ',(E[n1]-E[n2])*1e9);
            'R':WriteLn(arquivo,nome,' ',n1,' ',n2,' ',resistencia);
            'G':WriteLn(arquivo,nome,' ',n1,' ',n2,' ',n21,' ',n22,' ',Gm);
            'Z':IF not vcc THEN WriteLn(arquivo,nome,' ',n2,' ',n1,' ',rserie,' ',tensao)
                ELSE WriteLn(arquivo,'R',Copy(nome,2,9),' ',n2,' ',n1,' ',rserie);
            'I':IF not icc THEN WriteLn(arquivo,nome,' ',n1,' ',n2,' ',corrente);
            'D':WriteLn(arquivo,'R',Copy(nome,2,9),' ',n1,' ',n2,' ',1/Gd);
                {Deveria haver uma capacitancia tambem}
            'Q':BEGIN
                  WriteLn(arquivo,'G',Copy(nome,2,9),' ',n1,' ',n2,' ',n2,' ',n3,' ',alfa*Gbe);
                  WriteLn(arquivo,'GMEB ',n3,' ',n2,' ',n2,' ',n1,' ',alfar*Gbc);
                  WriteLn(arquivo,'RBE ',n2,' ',n3,' ',1/Gbe);
                  WriteLn(arquivo,'RBC ',n2,' ',n1,' ',1/Gbc);
                  {Este calculo de capacitancias so vale, aproximadamente, na regiao ativa}
                  AcharPonto(i);
                  IF estado=ativo THEN BEGIN
                    Cbe:=alfa*Gbe/(2*Pi*ft);
                    WriteLn(arquivo,'CBC ',n1,' ',n2,' ',Cbc,' ',E[n1]-E[n2]);
                    WriteLn(arquivo,'CBE ',n2,' ',n3,' ',Cbe,' ',E[n2]-E[n3])
                  END
                END
          END;
          Close(arquivo);
          WriteLn('Arquivo criado');
          GoTo Ler
        END;
    'F':BEGIN WriteLn('--//--'); Halt END;
    ELSE Goto Ler;
  END
END.