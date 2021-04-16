{****************************************************************************
 * Calculo de resistencias planares por matriz de transferencia             *
 * Por Antonio Carlos Moreirao de Queiroz                                   *
 * COPPE/UFRJ - 1993                                                        *
 * V. 1.0 de 3/3/93                                                         *
 ****************************************************************************}
{
Calculo funcionando corretamente, com faixa horizontal e canto.
Calculo de tensoes e correntes, com replotagem possivel.
Nao calcula nem armazena (mas aloca) a metade triangular inferior de A.
Aterra quaisquer dos tres lados livres.
Processo de difusao em testes.
Cristais com limites lineares.
}

{
Ideias a implementar:
Permitir o surgimento de novos centros alem dos iniciais.
O aumento da area difundida deve ser quadratico, e nao linear.
Implementar um algoritmo de crescimento dendritico.
Salvar o estado atual do trabalho, talvez para continuar depois.
Reorientar o desenho para corresponder as listagens de v e i.
}

{$IFDEF DOUBLE}
{$N+}
{$ENDIF}

USES Crt,Graph,XView;

{$IFDEF DOUBLE}
TYPE REAL=DOUBLE;
{$ENDIF}

CONST
  versao='1.0 de 3/3/93';
  maxn=200;           {Maxima altura}
  maxm=200;           {Maxima largura}
  maxc=100;           {Maximo numero de centros de difusao}
  n:INTEGER=-1;       {Altura atual da rede}
  m:INTEGER=-1;       {Largura atual da rede}
  grid:BOOLEAN=TRUE;  {Se ha grade}
TYPE
  Linha=ARRAY[1..maxn] OF REAL;
  Ponto=RECORD x,y:INTEGER END;
  PtrLinha=^Linha;
VAR
  A:ARRAY[1..maxn] OF PtrLinha;        {Matriz de transferencia}
  Rede:ARRAY[1..maxn,1..maxm] OF BYTE; {Estado atual da rede resistiva}
  arqa:FILE of REAL;                   {Arquivos das matrizes A}
  arquivo:TEXT;                        {Para salvar mensagens e tabelas}
  Tr:ARRAY[0..1] of REAL;              {Resistencias}
  resistencia:REAL;                    {Resistencia total}
  areadifusao:REAL;                    {Area difundida normalizada}
  maiorcorrente:REAL;                  {Maior corrente encontrada}
  fatorcorrente:REAL;                  {Fator de escalamento de corrente}
  centro:ARRAY[1..maxc] of Ponto;      {Centros de difusao}
  matrizesdisponiveis,centrosalocados:BOOLEAN; {Estado dos calculos}
  esquerda,base,topo:BOOLEAN;                  {Lados aterrados}
  z:INTEGER;                                   {Nivel de zoom}
  ch:CHAR;                                     {Uso geral}
  iii,jjj:INTEGER;                             {Usado no prog. principal}

{Interface}
VAR
  menuprincipal,menugrafico,menutty:Xv_opaque;
  fprincipal,canvas1:Xv_opaque;
  fparametros,biniciar,bdifundir,btudo,tcalculos,taltura,tlargura,
  trmaterial,trdifusao,bcalcular,smanual,tcentros,tseparacao,
  tdistancia,tdifusoes,stipo,mcoluna,sopcoes:Xv_opaque;
  farquivo,bescrever,tarquivo:Xv_opaque;
  fmensagens,ttymsg:Xv_opaque;

PROCEDURE SaidaLn(txt:STRING);
BEGIN
  ttysw_output(ttymsg,txt+^M^J)
END;

FUNCTION Si(x:LONGINT):STRING;
VAR
  t:STRING;
BEGIN
  Str(x,t);
  Si:=t
END;

FUNCTION Sr(x:REAL;cm,dc:INTEGER):STRING;
VAR
  t:STRING;
BEGIN
  Str(x:cm:dc,t);
  Sr:=t
END;

PROCEDURE ListarMatriz;
VAR
  i,j:INTEGER;
  t:REAL;
BEGIN
  SaidaLn(^M^J'Matriz A: no. da linha e elementos:');
  FOR i:=1 TO n DO BEGIN
    ttysw_output(ttymsg,Si(i));
    FOR j:=1 TO n DO BEGIN
      IF j>=i THEN ttysw_output(ttymsg,' '+Sr(A[i]^[j],5,3))
      ELSE ttysw_output(ttymsg,' '+Sr(A[j]^[i],5,3))
    END;
    ttysw_output(ttymsg,^M^J)
  END;
END;

FUNCTION Distancia(x1,y1,x2,y2:LONGINT):REAL;
BEGIN
  Distancia:=Sqrt((Sqr(x1-x2)+Sqr(y1-y2)))
END;

FUNCTION DistanciaBissetriz(x1,y1,x2,y2,x,y:LONGINT):REAL;
VAR
  A,B,C:LONGINT;
BEGIN
  A:=x1-x2;
  B:=y1-y2;
  C:=A*(x1+x2)+B*(y1+y2);
  DistanciaBissetriz:=(A*x+B*y-0.5*C)/Sqrt(Sqr(A)+Sqr(B))
END;

PROCEDURE ResolverCircuito(tensoes:BOOLEAN);
VAR
  R,T1,I1,I2,I3,I4,fator:REAL;
  i,j,k,l:INTEGER;
  listar,tentardesenhar:BOOLEAN;
  V,V1:Linha;
BEGIN
  IF not matrizesdisponiveis THEN BEGIN
    SaidaLn('As matrizes necessarias nao estao disponiveis. Recalcule');
    Exit
  END;
  listar:=sopcoes^.sel_setting and 4=4;
  IF not listar THEN BEGIN
    open_window(fprincipal);
    WHILE active_w<>fprincipal DO close_window(active_w)
  END;
  Tr[0]:=trmaterial^.panel_real;
  Tr[1]:=trdifusao^.panel_real;
  maiorcorrente:=0;
  fator:=resistencia/2;
  tentardesenhar:=TRUE;
  IF listar THEN
    IF tensoes THEN SaidaLn('Coluna, e tensoes de cima para baixo (Vin=1):')
    ELSE SaidaLn('Coluna, e correntes de cima para baixo (Iin='+Sr(1/fatorcorrente,5,2)+')');
  FOR i:=1 TO n DO V1[i]:=1;
  FOR l:=m DOWNTO 1 DO BEGIN
    V:=V1;
    Assign(arqa,'$A'+Si(l));
    Reset(arqa);
    FOR i:=1 TO n DO
      FOR j:=i TO n DO Read(arqa,A[i]^[j]);
    Close(arqa);
    IF listar THEN ttysw_output(ttymsg,Si(l));
    {Calculo de tensoes}
    FOR i:=1 TO n DO BEGIN
      T1:=0;
      FOR j:=1 TO n DO
        IF j>=i THEN T1:=T1+A[i]^[j]*V[j]
        ELSE T1:=T1+A[j]^[i]*V[j];
      IF l=m THEN R:=Tr[Rede[i,l]]
      ELSE R:=Tr[Rede[i,l]]+Tr[Rede[i,l+1]];
      V1[i]:=V[i]-R*T1;
      IF tensoes THEN BEGIN
        IF listar THEN ttysw_output(ttymsg,' '+Sr(V1[i],5,3));
        IF tentardesenhar THEN BEGIN
          open_window(fprincipal);
          IF xv_ok THEN BEGIN
            SetFillStyle(SolidFill,Trunc(V1[i]*10+0.4999));
            Bar(10+(l-1)*z,10+(i-1)*z,9+l*z,9+i*z)
          END
          ELSE tentardesenhar:=FALSE {evita montes de beeps}
        END
      END
    END;
    IF not tensoes THEN BEGIN
      {Calculo de correntes}
      FOR i:=1 TO n DO BEGIN
        IF l=m THEN I1:=(V[i]-V1[i])/Tr[Rede[i,l]]
        ELSE I1:=(V[i]-V1[i])/(Tr[Rede[i,l]]+Tr[Rede[i,l+1]]);
        IF i>1 THEN I2:=-I3
        ELSE BEGIN
          IF topo THEN I2:=-V1[1]/Tr[Rede[i,l]] ELSE I2:=0
        END;
        IF i=n THEN BEGIN
          IF base THEN I3:=-V1[n]/Tr[Rede[n,l]] ELSE I3:=0
        END
        ELSE I3:=(V1[i+1]-V1[i])/(Tr[Rede[i,l]]+Tr[Rede[i+1,l]]);
        I4:=-I1-I2-I3;
        T1:=(Abs(I1)+Abs(I2)+Abs(I3)+Abs(I4))*fator;
        IF T1>maiorcorrente THEN maiorcorrente:=T1;
        T1:=T1/fatorcorrente;
        IF listar THEN ttysw_output(ttymsg,' '+Sr(T1,5,3));
        IF tentardesenhar THEN BEGIN
          open_window(fprincipal);
          IF xv_ok THEN BEGIN
            SetFillStyle(SolidFill,Trunc(T1*10+0.4999));
            Bar(10+(l-1)*z,10+(i-1)*z,9+l*z,9+i*z)
          END
          ELSE tentardesenhar:=FALSE
        END
      END
    END;
    IF listar THEN SaidaLn('');
  END;
  IF not tensoes THEN fatorcorrente:=maiorcorrente
END;

PROCEDURE CalcularCondutancia;
VAR
  P:PtrLinha;
  G,R,T1,T2:REAL;
  alpha,l,k,i,j:INTEGER;
  area:LONGINT;
  Aa:Linha; {Copia da linha de A do resistor horizontal}
BEGIN
  esquerda:=stipo^.sel_setting and 1=1;
  base:=stipo^.sel_setting and 2=2;
  topo:=stipo^.sel_setting and 4=4;
  IF stipo^.sel_setting=0 THEN BEGIN
    SaidaLn(' Aterre algum lado...');
    Exit
  END;
  {Achar a area ocupada}
  area:=0;
  FOR i:=1 TO n DO
    FOR j:=1 TO m DO area:=area+Rede[i,j];
  areadifusao:=1.0*area/(m*n);
  ttysw_output(ttymsg,Sr(areadifusao,6,4)+' ');
  IF sopcoes^.sel_setting and 8=8 THEN BEGIN
    {Calcular resistencia e preparar o calculo de V e I}
    Tr[0]:=trmaterial^.panel_real;
    Tr[1]:=trdifusao^.panel_real;
    {Zerar matriz, significando circuito aberto no lado esquerdo}
    FOR i:=1 TO n DO
      FOR j:=i TO n DO A[i]^[j]:=0;
    IF esquerda THEN
      {Conexoes a terra no lado esquerdo}
      FOR i:=1 TO n DO A[i]^[i]:=A[i]^[i]+1/Tr[Rede[i,1]];
    FOR l:=1 TO m DO BEGIN
      xv_set(mcoluna,'coluna:'+Si(l));
      IF KeyPressed THEN BEGIN
        SaidaLn(' *** Calculo interrompido ***');
        Exit;
      END;
      {Colocar resistores verticais}
      IF topo THEN BEGIN
        {Resistores simples aterrados na 1a. linha}
        A[1]^[1]:=A[1]^[1]+1/Tr[Rede[1,l]];
      END;
      FOR alpha:=1 TO n-1 DO BEGIN
        {Somas de resistencias de secoes sucessivas, entre um no e o seguinte}
        k:=alpha+1;
        G:=1/(Tr[Rede[alpha,l]]+Tr[Rede[alpha+1,l]]);
        A[alpha]^[alpha]:=A[alpha]^[alpha]+G;
        A[k]^[k]:=A[k]^[k]+G;
        A[alpha]^[k]:=A[alpha]^[k]-G;
      END;
      IF base THEN
        {Resistores simples para terra na ultima linha}
        A[n]^[n]:=A[n]^[n]+1/(Tr[Rede[n,l]]);
      {Colocar resistores horizontais em todas as linhas}
      FOR alpha:=1 TO n DO BEGIN
        {Copiar a linha do resistor a incluir, Aa*, em "L"}
        FOR j:=alpha TO n DO Aa[j]:=A[alpha]^[j];
        FOR i:=1 TO alpha-1 DO Aa[i]:=A[i]^[alpha];
        {Guardar R/(1+AaaR)}
        IF l<>m THEN
          {Colocar resistores de duas secoes em serie nas colunas normais}
          R:=Tr[Rede[alpha,l]]+Tr[Rede[alpha,l+1]]
        ELSE
          {O ultimo resistor e simples}
          R:=Tr[Rede[alpha,l]];
        T1:=R/(1+Aa[alpha]*R);
        FOR i:=n DOWNTO 1 DO BEGIN
          {Guardar (AiaR)/(1+AaaR)}
          P:=A[i]; {Isto ganha uma fracao de segundo}
          IF alpha>=i THEN T2:=T1*P^[alpha] ELSE T2:=T1*A[alpha]^[i];
          FOR j:=i TO n DO BEGIN
            {Quase todo o tempo e passado aqui}
            P^[j]:=P^[j]-Aa[j]*T2
          END
        END
      END;
      IF sopcoes^.sel_setting and 3<>0 THEN BEGIN
        {Salvar matrizes A para usar no calculo de tensoes e correntes}
        {Tem que ser feito assim, pois o calculo direto
         a partir da ultima matriz acumula erro explosivamente}
        Assign(arqa,'$A'+Si(l));
        ReWrite(arqa);
        FOR i:=1 TO n DO
          FOR j:=i TO n DO
            Write(arqa,A[i]^[j]);
        Close(arqa);
        IF l=m THEN matrizesdisponiveis:=TRUE
      END
    END;
    {Somar todos os termos da matriz A para achar a condutancia}
    G:=0;
    FOR i:=1 TO n DO BEGIN
      G:=G+A[i]^[i];
      FOR j:=i+1 TO n DO G:=G+2*A[i]^[j]
    END;
    resistencia:=1/G;
    SaidaLn(Sr(resistencia,20,-1));
    IF sopcoes^.sel_setting and 1=1 THEN BEGIN
      SaidaLn('Toque uma tecla para ver as tensoes');
      ch:=ReadKey;
      IF ch<>#27 THEN ResolverCircuito(TRUE)
    END;
    IF sopcoes^.sel_setting and 2=2 THEN BEGIN
      SaidaLn('Toque uma tecla para ver as correntes');
      ch:=ReadKey;
      IF ch<>#27 THEN ResolverCircuito(FALSE);
      SaidaLn('Toque uma tecla para ver as correntes normalizadas p/Imax=1');
      ch:=ReadKey;
      IF ch<>#27 THEN ResolverCircuito(FALSE)
    END
  END
  ELSE SaidaLn('')
END;

PROCEDURE ApagarMatrizes;
VAR
  i:INTEGER;
BEGIN
  IF matrizesdisponiveis THEN BEGIN
    FOR i:=1 TO m DO BEGIN
      Assign(arqa,'$A'+Si(i));
      Erase(arqa)
    END;
    SaidaLn('Matrizes apagadas')
  END;
  matrizesdisponiveis:=FALSE;
END;

{$F+}

{Callbacks}

PROCEDURE Redesenhar(obj:Xv_opaque);
VAR
  i,j:INTEGER;
BEGIN
  IF n<0 THEN BEGIN
    SaidaLn('Inicialize os parametros');
    open_window(fparametros);
    Exit
  END;
  IF active_w<>fprincipal THEN open_window(fprincipal);
  WHILE active_w<>fprincipal DO close_window(active_w);
  WITH canvas1^ DO BEGIN
    SetFillStyle(SolidFill,canvas1^.back_color);
    Bar(0,0,dx,dy);
  END;
  SetFillStyle(SolidFill,yellow);
  Bar(10,10,9+m*z,9+n*z);
  IF grid THEN BEGIN
    FOR i:=1 TO n-1 DO BEGIN
      IF i mod 10=0 THEN SetColor(2) ELSE SetColor(7);
      Line(10,9+i*z,9+m*z,9+i*z)
    END;
    FOR i:=1 TO m-1 DO BEGIN
      IF i mod 10=0 THEN SetColor(2) ELSE SetColor(7);
      Line(10+i*z,10,10+i*z,9+n*z)
    END
  END;
  SetFillStyle(SolidFill,black);
  FOR i:=1 TO n DO
    FOR j:=1 TO m DO
      IF Rede[i,j]=1 THEN Bar(10+(j-1)*z,10+(i-1)*z,9+j*z,9+i*z);
  IF centrosalocados THEN BEGIN
    SetFillStyle(SolidFill,12);
    FOR i:=1 TO tcentros^.panel_int DO
       WITH Centro[i] DO Bar(10+(x-1)*z,10+(y-1)*z,9+x*z,9+y*z);
  END;
  SetFillStyle(SolidFill,12);
  Bar(12+m*z,10,13+m*z,9+n*z);
  SetFillStyle(SolidFill,black);
  WITH stipo^ DO BEGIN
    IF sel_setting and 1=1 THEN Bar(6,10,7,9+n*z);
    IF sel_setting and 2=2 THEN Bar(10,12+n*z,9+m*z,13+n*z);
    IF sel_setting and 4=4 THEN Bar(10,6,9+m*z,7)
  END;
END;

PROCEDURE TratarEventos(obj:Xv_opaque);
VAR
  i,j:INTEGER;
BEGIN
  IF n<0 THEN Exit;
  IF (ie_shiftcode=1) or (ie_shiftcode=4) THEN BEGIN
    j:=(ie_locx-10) div z+1;
    i:=(ie_locy-10) div z+1;
    IF (i>0) and (j>0) and (i<=n) and (j<=m) THEN BEGIN
      IF ie_shiftcode=1 THEN Rede[i,j]:=1 ELSE Rede[i,j]:=0;
      IF Rede[i,j]=0 THEN SetFillStyle(SolidFill,yellow) ELSE SetFillStyle(SolidFill,black);
      Bar(10+(j-1)*z,10+(i-1)*z,9+j*z,9+i*z);
      ApagarMatrizes
    END
  END
END;

PROCEDURE InvalidarTudo(obj:Xv_opaque);
LABEL
  continuar;
VAR
  i,j:INTEGER;
BEGIN
  xv_set(mcoluna,'');
  FOR i:=1 TO n DO FreeMem(A[i],n*SizeOf(REAL));
  ApagarMatrizes;
  centrosalocados:=FALSE;
  m:=tlargura^.panel_int;
  n:=taltura^.panel_int;
  FOR i:=1 TO n DO
    IF (MaxAvail>n*SizeOf(REAL)) THEN GetMem(A[i],n*SizeOf(REAL))
    ELSE BEGIN
      SaidaLn('Memoria insuficiente para mais que '+Si(i-1)+' linhas');
      taltura^.panel_int:=i-1;
      n:=i-1;
      xv_set(taltura,taltura^.xv_label);
      GoTo continuar;
    END;
 continuar:
  SaidaLn('Matriz A ('+Si(n)+'x'+Si(n)+') alocada. Restam '+Si(MemAvail)+' bytes');
  FOR i:=1 TO n DO
    FOR j:=1 TO m DO Rede[i,j]:=0;
  z:=32;
  WHILE ((13+m*z>canvas1^.dx) or (9+n*z>canvas1^.dy)) and (z>1) DO z:=z div 2;
  Redesenhar(nil);
END;

PROCEDURE Calcular(obj:Xv_opaque);
VAR
  i:INTEGER;

  PROCEDURE AlocarCentros;
  VAR
    tentativas,i,l:INTEGER;
    pertodemais:BOOLEAN;
  BEGIN
    FOR i:=1 TO tcentros^.panel_int DO
      WITH centro[i] DO BEGIN
        tentativas:=0;
        REPEAT
          x:=Random(m)+1;
          y:=Random(n)+1;
          l:=0;
          pertodemais:=FALSE;
          REPEAT
            Inc(l);
            IF l<>i THEN
              IF Distancia(centro[l].x,centro[l].y,x,y)<tdistancia^.panel_int THEN pertodemais:=TRUE;
          UNTIL pertodemais or (l=i);
          Inc(tentativas)
        UNTIL not pertodemais or (tentativas>100);
        IF pertodemais THEN SaidaLn('O centro '+Si(i)+' nao pode ser bem colocado');
        Rede[y,x]:=1;
        open_window(fprincipal);
        WHILE active_w<>fprincipal DO close_window(active_w);
        SetFillStyle(SolidFill,12);
        Bar(10+(x-1)*z,10+(y-1)*z,9+x*z,9+y*z)
      END;
    centrosalocados:=TRUE
  END;

  PROCEDURE Difundir;
  VAR
    j,k,l,px,py:INTEGER;
    dx,dy,angulo,separacao:REAL;
    dentro,maisperto:BOOLEAN;
  BEGIN
    separacao:=tseparacao^.panel_real/2;
    FOR k:=1 TO tcentros^.panel_int DO
      WITH centro[k] DO BEGIN
         IF KeyPressed THEN BEGIN
           SaidaLn(' *** Difusao interrompida ***');
           Exit
         END;
         FOR j:=1 TO tdifusoes^.panel_int DO BEGIN
          {Seleciona direcao aleatoria}
          angulo:=Random*2*Pi;
          dx:=Cos(angulo); dy:=Sin(angulo);
          {Procura primeiro ponto livre nesta direcao, dentro da area valida}
          l:=0;
          REPEAT
            Inc(l);
            px:=x+Round(l*dx); py:=y+Round(l*dy);
            dentro:=(px>=1) and (px<=m) and (py>=1) and (py<=n);
          UNTIL not dentro or (Rede[py,px]=0);
          IF dentro THEN BEGIN
            {Verifica se esta mais perto da origem do que de outros centros}
            maisperto:=TRUE;
            l:=0;
            REPEAT
              Inc(l);
              IF l<>k THEN
                IF DistanciaBissetriz(x,y,centro[l].x,centro[l].y,px,py)<separacao
                   THEN maisperto:=FALSE
            UNTIL not maisperto or (l>=tcentros^.panel_int);
            IF maisperto THEN BEGIN
              {Marca o ponto como difusao}
              Rede[py,px]:=1;
              open_window(fprincipal);
              WHILE active_w<>fprincipal DO close_window(active_w);
              SetFillStyle(SolidFill,black);
              Bar(10+(px-1)*z,10+(py-1)*z,9+px*z,9+py*z)
            END
          END
        END
      END
  END;

BEGIN
  IF n<0 THEN InvalidarTudo(nil);
  IF (obj=btudo) and
     (sopcoes^.sel_setting and 3<>0) THEN BEGIN
    sopcoes^.sel_setting:=sopcoes^.sel_setting xor 3;
    xv_set(sopcoes,sopcoes^.xv_label);
  END;
  IF obj=btudo THEN BEGIN
    ApagarMatrizes;
    IF not centrosalocados THEN AlocarCentros;
    SaidaLn('Tabela de areas (e resistencias):');
    FOR i:=1 TO tcalculos^.panel_int DO BEGIN
      Difundir;
      ttysw_output(ttymsg,Si(i)+' ');
      CalcularCondutancia;
      IF KeyPressed THEN BEGIN
        SaidaLn(' *** Tabela interrompida ***');
        Exit
      END
    END;
    SaidaLn('Calculo completo');
  END
  ELSE IF obj=bcalcular THEN BEGIN
    SaidaLn('Area (e resistencia):');
    fatorcorrente:=1;
    CalcularCondutancia
  END
  ELSE BEGIN
    IF not centrosalocados THEN AlocarCentros;
    Difundir
  END
END;

PROCEDURE EscreverArquivo(obj:Xv_opaque);
VAR
  i:INTEGER;
BEGIN
  close_window(farquivo);
  {$I-}
  Assign(arquivo,tarquivo^.panel_value);
  Rewrite(arquivo);
  WriteLn(arquivo,'R-2D - versao ',versao,#13#10);
  WITH ttymsg^ DO BEGIN
    i:=bstart;
    WHILE i<>tend DO BEGIN
      Write(arquivo,Pb^[i]);
      IF i<bsize THEN Inc(i) ELSE i:=0
    END
  END;
  Close(arquivo);
  {$I+}
  IF IOResult=0 THEN SaidaLn('Mensagens escritas')
  ELSE SaidaLn('Impossivel escrever mensagens');
END;

PROCEDURE TratarMenuPrincipal(obj:Xv_opaque);
BEGIN
  CASE obj^.sel_menu OF
    1:{parametros}
      open_window(fparametros);
    2:{mensagens}
      open_window(fmensagens);
    3:{terminar}
      BEGIN
        ApagarMatrizes;
        xv_end:=TRUE
      END
  END;
END;

PROCEDURE TratarMenuGrafico(obj:Xv_opaque);
BEGIN
  CASE obj^.sel_menu OF
    1:Redesenhar(nil);
    2:IF z<32 THEN BEGIN
        z:=z*2;
        Redesenhar(nil)
      END;
    3:IF z>1 THEN BEGIN
        z:=z div 2;
        Redesenhar(nil)
      END;
    4:BEGIN
        grid:=not grid;
        Redesenhar(nil)
      END;
    5:ResolverCircuito(TRUE);
    6:ResolverCircuito(FALSE);
  END;
END;

PROCEDURE TratarMenuTty(obj:Xv_opaque);
BEGIN
  CASE obj^.sel_menu OF
    1:BEGIN
        SaidaLn('Calculo de resistencias planares');
        SaidaLn('por: Antonio Carlos Moreirao de Queiroz');
        SaidaLn('versao '+versao)
      END;
    2:WITH ttymsg^ DO BEGIN
        bstart:=0; tstart:=0; tend:=0;
        ttysw_output(ttymsg,'')
      END;
    3:open_window(farquivo);
    4:IF n>0 THEN ListarMatriz
      ELSE SaidaLn('Inicialize os parametros');
  END
END;

PROCEDURE MedirDisco(obj:Xv_opaque);
VAR
  n:REAL;
BEGIN
  IF sopcoes^.sel_setting and 3<>0 THEN BEGIN
    n:=taltura^.panel_int;
    SaidaLn('Sao necessarios '+Cpct(tlargura^.panel_int*n*(n+1)/2*SizeOf(REAL))+' bytes em disco');
  END
END;

PROCEDURE InvalidarCentros(obj:Xv_opaque);
BEGIN
  centrosalocados:=FALSE;
  Redesenhar(nil)
END;

PROCEDURE InvalidarCalculo(obj:Xv_opaque);
BEGIN
  ApagarMatrizes;
  Redesenhar(nil)
END;

PROCEDURE MudarR(obj:Xv_opaque);
BEGIN
  ApagarMatrizes
END;

{$F-}

BEGIN
  {Inicializacao}
  xv_init(0,0);
  normal_bsize:=10000;
  FOR iii:=0 TO 10 DO BEGIN
    SetPalette(iii,iii);
    jjj:=Trunc(iii*6.3);
    SetRGBPalette(iii,jjj,jjj,jjj);
  END;
  c_active:=11;
  matrizesdisponiveis:=FALSE;
  centrosalocados:=FALSE;
  {Menus}
  {Menu para fprincipal}
  menuprincipal:=xv_create(menu);
  WITH menuprincipal^ DO BEGIN
    xv_label:='menu';
    item_create('parametros');
    item_create('mensagens');
    item_create('terminar');
    sel_menu:=2;
    notify_handler:=TratarMenuPrincipal;
  END;
  {Menu para canvas1}
  menugrafico:=xv_create(menu);
  WITH menugrafico^ DO BEGIN
    xv_label:='grafico';
    item_create('redesenhar');
    item_create('zoom +');
    item_create('zoom -');
    item_create('grade');
    item_create('tensoes');
    item_create('correntes');
    notify_handler:=TratarMenuGrafico;
  END;
  {Menu para ttymsg}
  menutty:=xv_create(menu);
  WITH menutty^ DO BEGIN
    xv_label:='mensagens';
    item_create('informacoes');
    item_create('limpar mensagens');
    item_create('salvar mensagens');
    item_create('listar matriz A');
    sel_menu:=2;
    notify_handler:=TratarMenuTty;
  END;
  {Interface}
  fmensagens:=xv_create(frame);
  WITH fmensagens^ DO BEGIN
    xv_label:='mensagens';
    y:=304;
    dx:=383;
    dy:=175;
  END;
  ttymsg:=xv_create(tty);
  WITH ttymsg^ DO BEGIN
    menu_name:=menutty;
  END;
  fparametros:=xv_create(frame);
  WITH fparametros^ DO BEGIN
    xv_label:='parametros';
    x:=384;
    y:=304;
    dx:=255;
    dy:=175;
  END;
  tdifusoes:=xv_create(textfield);
  WITH tdifusoes^ DO BEGIN
    xv_label:='difusoes por centro';
    y:=90;
    value_length:=6;
    field_type:=int_field;
    panel_int:=10;
    min_value:=1;
  END;
  tcentros:=xv_create(textfield);
  WITH tcentros^ DO BEGIN
    xv_label:='centros';
    y:=75;
    value_length:=4;
    field_type:=int_field;
    min_value:=0;
    max_value:=maxc;
    panel_int:=5;
    notify_handler:=InvalidarCentros;
  END;
  tdistancia:=xv_create(textfield);
  WITH tdistancia^ DO BEGIN
    xv_label:='distancia>';
    y:=75;
    x:=105;
    value_length:=5;
    field_type:=int_field;
    min_value:=0;
    panel_int:=5;
  END;
  bcalcular:=xv_create(button);
  WITH bcalcular^ DO BEGIN
    xv_label:='calcular';
    y:=135;
    notify_handler:=Calcular;
  END;
  trdifusao:=xv_create(textfield);
  WITH trdifusao^ DO BEGIN
    xv_label:='resistencia da difusao';
    y:=60;
    value_length:=7;
    field_type:=real_field;
    panel_real:=5E+2;
    notify_handler:=MudarR;
  END;
  trmaterial:=xv_create(textfield);
  WITH trmaterial^ DO BEGIN
    xv_label:='resistencia normal';
    y:=45;
    value_length:=7;
    field_type:=real_field;
    panel_real:=5E-1;
    notify_handler:=MudarR;
  END;
  sopcoes:=xv_create(setting);
  WITH sopcoes^ DO BEGIN
    xv_label:='opcoes';
    y:=30;
    item_create('V');
    item_create('I');
    item_create('listar');
    item_create('R');
    sel_setting:=8;
    notify_handler:=MedirDisco;
  END;
  stipo:=xv_create(setting);
  WITH stipo^ DO BEGIN
    xv_label:='aterrar';
    y:=15;
    item_create('esquerda');
    item_create('base');
    item_create('topo');
    sel_setting:=1;
    notify_handler:=InvalidarCalculo;
  END;
  mcoluna:=xv_create(message);
  WITH mcoluna^ DO BEGIN
    xv_label:='';
    y:=135; x:=160;
  END;
  tlargura:=xv_create(textfield);
  WITH tlargura^ DO BEGIN
    xv_label:='largura';
    x:=128;
    value_length:=6;
    field_type:=int_field;
    panel_int:=10;
    min_value:=1;
    max_value:=maxm;
    notify_handler:=InvalidarTudo;
  END;
  taltura:=xv_create(textfield);
  WITH taltura^ DO BEGIN
    xv_label:='altura';
    value_length:=6;
    field_type:=int_field;
    panel_int:=10;
    min_value:=1;
    max_value:=maxn;
    notify_handler:=InvalidarTudo;
  END;
  tcalculos:=xv_create(textfield);
  WITH tcalculos^ DO BEGIN
    xv_label:='calculos';
    y:=105;
    value_length:=5;
    field_type:=int_field;
    panel_int:=5;
    min_value:=0;
    max_value:=200;
  END;
  tseparacao:=xv_create(textfield);
  WITH tseparacao^ DO BEGIN
    xv_label:='separacao';
    y:=105; x:=120;
    value_length:=5;
    field_type:=real_field;
    panel_real:=2;
  END;
  biniciar:=xv_create(button);
  WITH biniciar^ DO BEGIN
    xv_label:='inicializar';
    x:=107;
    y:=120;
    notify_handler:=InvalidarTudo;
  END;
  bdifundir:=xv_create(button);
  WITH bdifundir^ DO BEGIN
    xv_label:='difundir';
    x:=75;
    y:=135;
    notify_handler:=Calcular;
  END;
  btudo:=xv_create(button);
  WITH btudo^ DO BEGIN
    xv_label:='gerar tabela';
    y:=120;
    notify_handler:=Calcular;
  END;
  farquivo:=xv_create(frame);
  WITH farquivo^ DO BEGIN
    xv_label:='arquivo';
    x:=163;
    y:=234;
    dx:=319;
    dy:=56;
    dymin:=27;
  END;
  tarquivo:=xv_create(textfield);
  WITH tarquivo^ DO BEGIN
    xv_label:='arquivo';
    value_length:=30;
    panel_value:='saida.r2d';
  END;
  bescrever:=xv_create(button);
  WITH bescrever^ DO BEGIN
    xv_label:='escrever';
    y:=15;
    notify_handler:=EscreverArquivo;
  END;
  fprincipal:=xv_create(frame);
  WITH fprincipal^ DO BEGIN
    xv_label:='R-2D';
    dx:=GetMaxX;
    dy:=GetMaxY-fparametros^.dy-1;
    menu_name:=menuprincipal;
  END;
  canvas1:=xv_create(canvas);
  WITH canvas1^ DO BEGIN
    notify_handler:=Redesenhar;
    event_handler:=TratarEventos;
    menu_name:=menugrafico;
  END;
  xv_main_loop(fprincipal);
  RestoreCrtMode;
END.
