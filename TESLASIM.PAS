{Simulador de Transformadores de Tesla}
{V. 1.0 de 16/12/97}
{V. 1.0a de 27/12/97 - Escalamento inicial automatico}
{V. 1.0b de 31/12/97 - Restart apaga os graficos}
{V. 1.0c de 16/02/98 - Corrigida normalizacao de escalas}
{V. 2.0  de 20/02/98 - Calculo dos parametros, correntes}
{V. 2.0a de 16/03/98 - Evitada divisao por zero em sistemas sem perdas}
{V. 2.1  de 30/03/98 - Rac=Rdd para d=2*sd, sintonia no painel RLC}
{V. 2.2  de 07/07/98 - Mais digitos no cursor e nomes nas curvas}
{V. 2.3  de 08/07/98 - Calculos da tabela AWG incluidos}
{V. 2.4  de 22/02/02 - Ajustes dos tamanhos das bobinas}
{V. 2.5  de 28/08/02 - C lculo do coeficiente ¢timo de acoplamento}
{V. 2.6  de 22/09/02 - Corrigida desnormaliza‡Æo da resposta ao impulso}
{V. 2.7  de 22/02/03 - Inclu¡do c lculo do coeficiente de acoplamento}
{V. 2.7a de 23/02/03 - N real em k, evitando erros quando o n£mero de espiras ‚ pequeno}
{V. 2.7b de 14/04/03 - Corrigida indutƒncia listada na corre‡Æo de pitch}
{V. 2.8  de 23/09/03 - Arredondado fator de normaliza‡Æo}

{$IFDEF DOUBLE}
  {$N+,E+}
{$ENDIF}

USES Crt,Graph,XView,GJ;

{$I extkeys.inc}

CONST
  ptmax=600;      {Maximo numero de pontos no grafico}
  tmin0=0;        {Limites iniciais}
  tmax0=100;
  ymin0=-2;
  ymax0=2;
  grafico_valido:BOOLEAN=FALSE;  {Se existe grafico valido}
  funcoes_lidas:BOOLEAN=FALSE;   {Se existem funcoes lidas}
  escalas_normalizadas:BOOLEAN=FALSE;
  mg=12;                         {Maximo grau de polinomio}
  mf=6;                          {Maximo numero de funcoes}
  zero=1e-6;
  raio=1.1;
  info=17;
  informacao:ARRAY[1..info] OF STRING[45]=(
  {---------------------------------------------}
  '+-------------------------------------------+',
  'Tesla Coil Simulator',
  'By Antonio Carlos Moreirao de Queiroz',
  'COPPE/UFRJ; e-mail acmq@ieee.org',
  'Version 2.8 - 23/09/2003',
  'The program runs an exact linear simulation',
  'of an ideal Tesla coil circuit, computing the',
  'transient voltages over the primary and',
  'secondary capacitances, and the currents in',
  'the primary and secondary inductors.',
  'The input can be in circuit parameters or in',
  'physical parameters of the coils.',
  'It can also read polynomials describing the',
  'Laplace transform of the impulse response of',
  'an arbitrary circuit and compute the time',
  'response to some input signals.',
  '+-------------------------------------------+'
  );
  r10=0.1;
  l10=100e-6;
  c10=1e-8;
  r20=100;
  l20=100e-3;
  c20=1e-11;
  k0=4;
  l0=5;
  kl0=0.219;
  vc10=10000;
  comp20=0.32;
  raio20=0.044;
  n20=1152;
  comp10=0.1;
  raio10=0.111;
  n10=12;
  pitch1=2;
  pitch2=1.091;

TYPE
  polinomio=ARRAY [0..mg+1] of REAL;
  raizes=ARRAY [1..mg+2] of REAL;
  tipopolo=(simples,complexo,conjugado,infinito);
  parametros=
    RECORD
      rede:STRING[40];
      nsaida:INTEGER;
      nz,np,npt,xg,yg,ultimo:INTEGER;
      k1,k2,p1,p2:raizes;
      num,den:polinomio;
      cte:REAL;
      gy:ARRAY[1..ptmax] of REAL;
      tipo:ARRAY[1..mg+2] of tipopolo;
      ordem:ARRAY[1..mg+2] of INTEGER;
      par:ARRAY[1..mg+2] of INTEGER;
      r1,l1,c1,r2,l2,c2,kl,vc1,fator: REAL;
    END;
  cores=ARRAY[0..mf] OF BYTE;

CONST
  cor:cores=(lightgray,yellow,white,lightgreen,lightred,lightblue,red);
  corCGA:cores=(2,3,1,2,3,1,2);
  cormono:cores=(1,1,1,1,1,1,1);

VAR
  n,i,j,k,placa,modo:INTEGER;
  ok:BOOLEAN;
  fdet: ARRAY[1..mf] of parametros;
  arquivo:TEXT;
  sufixo:STRING[4];
  w,t,t1,t2,n1,n2,d1,d2,ang,z1,z2,maximo,aa,bb:REAL;
  xmin,xmax,ymin,ymax:INTEGER;    {Limites do grafico em pixels no cgrafico}
  cursorx,cursory:INTEGER;        {Cursor, em pixels no cgrafico}
  atual:INTEGER;                  {Curva atual}
  ax,bx,ay,by,delta:REAL;         {Mapeamento}
  gx:ARRAY[1..ptmax] OF REAL; {Valores X e Y no grafico}
  txt:STRING;

VAR
  MenuPrincipal,MenuTty:Xv_opaque;
  fEscrever,tEscrever,bEscrever:Xv_opaque;
  fLer,trede,tnsaida,bler,sentrada,tw,biniciar:Xv_opaque;
  fprincipal,cgrafico:Xv_opaque;
  fmensagens,tmsg:Xv_opaque;
  fescalas,bplotar,tymax,tymin,txmax,
  fparametros,traio,tzero,ttol,ttolm,treal,timag,stestar,sarredondar:Xv_opaque;
  txmin,tpontos:Xv_opaque;
  fRLC,tr1,tl1,tc1,tr2,tl2,tc2,tkl,tk,tl,tvc1,ssaida,tfator,bacrescentar,biniciarRLC,
  btunec12,btunec22,btunel12,btunel22:Xv_opaque;
  fdimensoes,tcomprimento1,traio1,tcomprimento2,traio2,
  tn1,tn2,td1,tawg1,td2,tawg2,trq,tden,
  stipo1,tcap1,tcap2,baplicardimensoes,btunec1,btunec2,btunel1,
  btunel2,bcorrect,tpitch1,tpitch2,tb1,tb2,bk12:Xv_opaque;

PROCEDURE Msg(txt:STRING);
BEGIN
  ttysw_output(tmsg,txt+^M^J)
END;

FUNCTION Sre(x:REAL; cm,dc:INTEGER):STRING;
VAR
  txt:STRING;
BEGIN
  Str(x:cm:dc,txt);
  Sre:=txt
END;

FUNCTION Sri(x:INTEGER):STRING;
VAR
  txt:STRING;
BEGIN
  Str(x,txt);
  Sri:=txt
END;

PROCEDURE Msgt(txt:STRING);
BEGIN
  IF stestar^.sel_setting=1 THEN ttysw_output(tmsg,txt+^M^J)
END;

FUNCTION indutancia(b,l,r,N:REAL):REAL;
BEGIN
  IF (b=2) or (stipo1^.sel_setting=1) THEN
  {Formula de Wheeler para indutancia de um solenoide}
    indutancia:=4*pi*1e-7*pi*r*r*N*N/(l+0.9*r)
  ELSE
  {Formula para indutancia de uma espiral}
    indutancia:=100e-6/2.54*sqr(l+r)*sqr(N)/(60*r-28*l);
END;

FUNCTION voltas(b,ind,l,r:REAL):REAL;
BEGIN
   IF (b=2) or (stipo1^.sel_setting=1) THEN
   {Voltas de um indutor em solenoide}
     voltas:=sqrt(ind*(l+0.9*r)/(4*sqr(pi)*1e-7))/r
   ELSE
   {Voltas de um indutor em espiral}
     voltas:=sqrt(ind*(60*r-28*l)*2.54/100e-6)/(l+r);
END;

FUNCTION capacitancia(b,l,r:REAL):REAL;
BEGIN
  IF (b=2) or (stipo1^.sel_setting=1) THEN
  {Formula de Medhurst}
    capacitancia:=(11.26*l+16*r+76.4*sqrt(r*r*r/l))*1e-12
  ELSE
  {Capacitancia de uma bobina em espiral chata=?}
    capacitancia:=0
END;

FUNCTION comprimento(b,l,r,N:REAL):REAL;
BEGIN
  IF (b=2) or (stipo1^.sel_setting=1) THEN
    {solenoide}
    comprimento:=sqrt(sqr(2*pi*r*N)+sqr(l))
  ELSE
    {espiral plana}
    comprimento:=sqrt(sqr(N*pi*(l+r))+sqr(r-l))
END;

FUNCTION frequencia1:REAL;
BEGIN
  frequencia1:=1/(2*pi*sqrt(indutancia(1,tcomprimento1^.panel_real,traio1^.panel_real,tn1^.panel_real)*
  (capacitancia(1,tcomprimento1^.panel_real,traio1^.panel_real)+tcap1^.panel_real)));
END;

FUNCTION frequencia2:REAL;
BEGIN
  frequencia2:=1/(2*pi*sqrt(indutancia(2,tcomprimento2^.panel_real,traio2^.panel_real,tn2^.panel_real)*
  (capacitancia(2,tcomprimento2^.panel_real,traio2^.panel_real)+tcap2^.panel_real)));
END;

FUNCTION frequencia12:REAL;
BEGIN
  frequencia12:=1/(2*pi*sqrt(tl1^.panel_real*tc1^.panel_real));
END;

FUNCTION frequencia22:REAL;
BEGIN
  frequencia22:=1/(2*pi*sqrt(tl2^.panel_real*tc2^.panel_real));
END;

FUNCTION resistencia(l,d,f:REAL):REAL;
VAR
  rdc,rac,rac1,sd:REAL;
BEGIN
  sd:=sqrt(2*trq^.panel_real/(2*pi*f)/(4e-7*pi));
  Msg('Frequency=    '+Sre(f,15,6)+' Hz');
  Msg('Wire length=  '+Sre(l,15,6)+' m');
  Msg('Wire diameter='+Sre(d,15,6)+' m');
  Msg('Wire weigth=  '+Sre(l*pi*sqr(d)*tden^.panel_real/4,15,6)+' Kg');
  Msg('Skin depth=   '+Sre(sd,15,6)+' m');
  rdc:=4*l/pi/sqr(d)*trq^.panel_real;
  Msg('rdc='+Sre(rdc,15,6)+' Ohms');
  IF sd>0 THEN rac:=trq^.panel_real*l/((d-sd)*pi*sd) ELSE rac:=0;
  Msg('rac='+Sre(rac,15,6)+' Ohms, approximately');
  {
  IF sd>0 THEN rac1:=trq^.panel_real*l/(d*pi*sd) ELSE rac:=0;
  Msg('rac (r*l/(d*pi*sd))'+Sre(rac1,15,6)+' Ohms');
  IF sd>0 THEN rac1:=0.996e-6*sqrt(f)/d*(l/12) ELSE rac:=0;
  Msg('rac (RDFRE, Cu)='+Sre(rac1,15,6)+' Ohms');
  IF sd>0 THEN rac1:=(0.0960*d*100/2.54*sqrt(f)+0.26)*rdc ELSE rac:=0;
  Msg('rac (RDFRE corrected, Cu)='+Sre(rac1,15,6)+' Ohms');
  }
  IF rac>rdc THEN BEGIN
    resistencia:=rac;
    Msg('Used rac');
  END
  ELSE BEGIN
    resistencia:=rdc;
    Msg('Used rdc');
  END
END;

FUNCTION LimY(y:REAL):INTEGER;
BEGIN
  IF y>5000 THEN LimY:=5000
  ELSE IF y<-5000 THEN LimY:=-5000
    ELSE LimY:=Round(y)
END;

PROCEDURE Informacoes;
BEGIN
  FOR i:=1 TO info DO ttysw_output(tmsg,informacao[i]+^M^J);
END;

PROCEDURE Biv(n0:INTEGER; a1:polinomio; VAR Re,Im:raizes);
CONST
  imax=50;
VAR
  a2,c1,c2:polinomio;
  tolm,t,tol,p1,p2,d,xr,xi,p,d1,d2,e1,e2:REAL;
  feito:BOOLEAN;
  i,j,nn,n,ordem:INTEGER;

BEGIN
  IF n0=0 THEN BEGIN Msg('No finite roots'); Exit END;
  tol:=ttol^.panel_real; tolm:=ttolm^.panel_real;
  xr:=treal^.panel_real; xi:=timag^.panel_real;
  feito:=FALSE; ordem:=0; nn:=0; n:=n0;
  FOR i:=0 TO n DO a2[i]:=0;
  {Elimina‡„o de ra¡zes na origem}
  WHILE (n>1) and (a1[0]=0) DO BEGIN
    Re[n]:=0; Im[n]:=0;
    n:=n-1;
    FOR i:=0 TO n DO a1[i]:=a1[i+1]
  END;
  WHILE NOT feito DO BEGIN
    IF n>1 THEN BEGIN
      {Calculo dos valores do polin“mio (p) e de sua derivada (d)}
      d1:=a1[n]; p1:=d1;
      d2:=a2[n]; p2:=d2;
      FOR i:=n-1 DOWNTO 0 DO BEGIN
        p1:=Cmult(p1,p2,xr,xi)+a1[i];
        p2:=Imag+a2[i];
        IF i>0 THEN BEGIN
          d1:=Cmult(d1,d2,xr,xi)+p1;
          d2:=Imag+p2;
        END
      END;
      {C lculo do erro. Esta forma dificulta overflow}
      IF (d1=0) or (d2=0) THEN BEGIN
        d:=Sqr(d1)+Sqr(d2);
        e1:=(p1*d1+p2*d2)/d;
        e2:=(p2*d1-p1*d2)/d
      END
      ELSE BEGIN
        d:=d1/d2+d2/d1;
        e1:=(p1/d2+p2/d1)/d;
        e2:=(p2/d2-p1/d1)/d
      END;
      {Testa poss¡vel ra¡z m£ltipla}
      d:=Abs(d1)+Abs(d2);
      p:=Abs(p1)+Abs(p2);
      IF (d<tolm) and (p<tolm) THEN BEGIN
        {deriva o polin“mio e continua}
        IF ordem=0 THEN BEGIN c1:=a1; c2:=a2 END;
        FOR i:=1 TO n DO BEGIN
          a1[i-1]:=a1[i]*i/n;
          a2[i-1]:=a2[i]*i/n;
        END;
        n:=n-1; ordem:=ordem+1;
      END
      ELSE BEGIN
        {Atualiza ra¡zes}
        xr:=xr-e1;
        xi:=xi-e2;
        {Testa convergˆncia}
        t:=Abs(e1)+Abs(e2);
        IF t<tol THEN BEGIN
          {Armazena ra¡zes calculadas}
          FOR i:=n+ordem DOWNTO n DO BEGIN
            Re[i]:=xr; Im[i]:=xi;
          END;
          {Rep”e polin“mio original, se for o caso}
          IF ordem>0 THEN BEGIN
            a1:=c1; a2:=c2; n:=n+ordem;
          END;
          {Deflaciona polin“mio}
          FOR i:=0 TO ordem DO BEGIN
            FOR j:=n-1 DOWNTO 1 DO BEGIN
              a1[j]:=Cmult(xr,xi,a1[j+1],a2[j+1])+a1[j];
              a2[j]:=Imag+a2[j];
            END;
            n:=n-1;
            FOR j:=0 TO n DO
              BEGIN a1[j]:=a1[j+1]; a2[j]:=a2[j+1] END
          END;
          {Prepara c lculo da pr¢xima ra¡z}
          IF (Abs(xi)>0.01) THEN xi:=-xi ELSE xi:=0.1;
          IF ordem>0 THEN xr:=xr-0.01; {evita derivada 0 a seguir}
          ordem:=0; nn:=0
        END
        ELSE BEGIN
          nn:=nn+1;
          {Demorando a convergir}
          IF nn>imax THEN BEGIN
            IF KeyPressed THEN BEGIN
              Msg('* Interrupted');
              Exit
            END;
            tol:=tol*10;
            Msg('*  Tolerance increased to '+Sre(tol,10,-1));
            nn:=0;
          END
        END
      END
    END
    ELSE BEGIN
      {Ultimas ra¡zes}
      d:=-(Sqr(a1[1])+Sqr(a2[1]));
      xr:=(a1[0]*a1[1]+a2[0]*a2[1])/d;
      xi:=(a2[0]*a1[1]-a1[0]*a2[1])/d;
      feito:=TRUE; nn:=0;
      FOR i:=n+ordem DOWNTO n DO BEGIN
        Re[i]:=xr; Im[i]:=xi;
      END
    END
  END;
END; {Biv}

PROCEDURE CalcularExpansao(n:INTEGER);
BEGIN
  WITH fdet[n] DO BEGIN
    Msg('Normalization factor:'+Sre(fator,7,3)+' rad/s');
    IF nz>=np THEN BEGIN
      FOR i:=np+1 TO nz+1 DO BEGIN tipo[i]:=infinito; ordem[i]:=i-np-1 END;
      Msg('There are poles at infinity up to order '+Sri(nz-np));
      Msg('The response includes impulses and derivarives'^M^J'up to order '+
          Sri(nz-np)+' (not plotted)');
      npt:=nz+1
    END
    ELSE npt:=np;
    {Verificar p¢los}
    Msgt('Testing the poles:');
    FOR i:=1 TO np DO BEGIN
      ordem[i]:=1; par[i]:=0;
      IF Abs(p2[i])<tzero^.panel_real THEN tipo[i]:=simples
      ELSE tipo[i]:=complexo;
      FOR k:=1 TO i-1 DO BEGIN
        IF (Abs(p2[i]-p2[k])<tzero^.panel_real) and (Abs(p1[i]-p1[k])<tzero^.panel_real) THEN Inc(ordem[i]);
        IF (tipo[i]=complexo) and (par[i]=0) and
           (tipo[k]=complexo) and (par[k]=0) and
           (Abs(p2[i]+p2[k])<tzero^.panel_real) and (Abs(p1[i]-p1[k])<tzero^.panel_real) THEN BEGIN
             tipo[i]:=conjugado;
             par[i]:=k; par[k]:=i;
           END
      END
    END;
    ok:=TRUE;
    FOR i:=1 TO np DO BEGIN
      CASE tipo[i] OF
        simples:txt:='is real';
        complexo:BEGIN
            txt:='is complex';
            IF par[i]<>0 THEN txt:=txt+'; conjugate is p'+Sri(par[i])
            ELSE BEGIN
              txt:=txt+'; the conjugate is missing';
              ok:=FALSE
            END
          END;
        conjugado:txt:='is the conjugate of p'+Sri(par[i]);
      END;
      Msgt('Pole p'+Sri(i)+': '+txt+'; order: '+Sri(ordem[i]));
    END;
    IF not ok THEN BEGIN
      Msg('* Complex poles must be in conjugate pairs');
      Dec(n);
      Exit
    END;
    {Calcular residuos}
    Msgt('Poles and their residues:');
    {Montar sistema de equacoes}
    IF not AlocarSistema(npt,1) THEN BEGIN
      Msg('* Not enough memory');
      Dec(n);
      Exit
    END;
    dmin:=tzero^.panel_real;
    ang:=2*Pi/npt;
    FOR i:=1 TO npt DO BEGIN {Cada linha}
      z1:=traio^.panel_real*Cos(ang*(i-1)); z2:=traio^.panel_real*Sin(ang*(i-1));
      FOR j:=1 TO npt DO BEGIN {Cada coluna}
        IF tipo[j]=infinito THEN BEGIN
          d1:=1; d2:=0;
          FOR k:=1 TO ordem[j] DO BEGIN
            d1:=Cmult(d1,d2,z1,z2); d2:=Imag
          END;
          Yr^[i]^[j]:=d1; Yi^[i]^[j]:=d2;
        END
        ELSE BEGIN
          t1:=z1-p1[j]; t2:=z2-p2[j];
          d1:=1; d2:=0;
          FOR k:=1 TO ordem[j] DO BEGIN
            d1:=Cmult(d1,d2,t1,t2); d2:=Imag
          END;
          IF Abs(d1)+Abs(d2)<tzero^.panel_real THEN BEGIN
            Msg('* Please use another interpolation radius');
            Dec(n);
            DesalocarSistema;
            open_window(fparametros);
            Exit
          END;
          Yr^[i]^[j]:=Cdiv(1,0,d1,d2); Yi^[i]^[j]:=Imag
        END
      END;
      d1:=1; d2:=0;
      FOR j:=1 TO np DO BEGIN
        d1:=Cmult(d1,d2,z1-p1[j],z2-p2[j]); d2:=Imag
      END;
      n1:=num[nz]; n2:=0;
      FOR j:=nz-1 DOWNTO 0 DO BEGIN
        n1:=Cmult(n1,n2,z1,z2)+num[j]; n2:=Imag
      END;
      Yr^[i]^[npt+1]:=Cdiv(n1*cte,n2*cte,d1,d2); Yi^[i]^[npt+1]:=Imag
    END;
    {Resolver sistema de equacoes}
    IF not ResolverSistema THEN BEGIN
      Msg('* Singular system of equations');
      Msg('  Check parameters and normalization');
      Dec(n);
      DesalocarSistema;
      open_window(fparametros);
      Exit;
    END;
    FOR k:=1 TO npt DO BEGIN
      k1[k]:=Yr^[k]^[npt+1];
      k2[k]:=Yi^[k]^[npt+1];
      IF tipo[k]=infinito THEN Msgt('P'+Sri(k)+': Infinity (#'+Sri(ordem[k])+')')
      ELSE Msgt('P'+Sri(k)+':'+Sre(p1[k],14,8)+Sre(p2[k],14,8)+'j (#'+Sri(ordem[k])+')');
      Msgt('K'+Sri(k)+':'+Sre(k1[k],14,8)+Sre(k2[k],14,8)+'j')
    END;
    {Testando a expansao}
    IF stestar^.sel_setting=1 THEN BEGIN
      Randomize;
      z1:=traio^.panel_real*Random; z2:=traio^.panel_real*Random;
      Msg('Random test: s='+Sre(z1,12,8)+' '+Sre(z2,12,8)+'j');
      Msg('The two values listed below must be equal');
      d1:=1; d2:=0;
      FOR j:=1 TO np DO BEGIN
        d1:=Cmult(d1,d2,z1-p1[j],z2-p2[j]); d2:=Imag
      END;
      n1:=num[nz]; n2:=0;
      FOR j:=nz-1 DOWNTO 0 DO BEGIN
        n1:=Cmult(n1,n2,z1,z2)+num[j]; n2:=Imag
      END;
      t1:=Cdiv(n1*cte,n2*cte,d1,d2); t2:=Imag;
      Msg('Ratio of polynomials:'^M^J+Sre(t1,20,-1)+' '+Sre(t2,20,-1)+'j');
      t1:=0; t2:=0;
      FOR j:=1 TO npt DO BEGIN
        IF tipo[j]=infinito THEN BEGIN
          d1:=k1[j]; d2:=k2[j];
          FOR k:=1 TO ordem[j] DO BEGIN
            d1:=Cmult(d1,d2,z1,z2); d2:=Imag
          END;
          t1:=t1+d1; t2:=t2+d2
        END
        ELSE BEGIN
          d1:=1; d2:=0;
          FOR k:=1 TO ordem[j] DO BEGIN
            d1:=Cmult(d1,d2,z1-p1[j],z2-p2[j]); d2:=Imag
          END;
          t1:=t1+Cdiv(k1[j],k2[j],d1,d2); t2:=t2+Imag
        END
      END;
      Msg('Partial fractions:'^M^J+Sre(t1,20,-1)+' '+Sre(t2,20,-1)+'j');
    END;
    DesalocarSistema;
    Msg('The time response components are:');
    FOR i:=1 TO npt DO BEGIN
      IF ordem[i]=2 THEN txt:='*t'
        ELSE IF ordem[i]>2 THEN txt:='*t^'+Sri(ordem[i]-1) ELSE txt:='';
      IF Abs(p1[i])>tzero^.panel_real THEN txt:=txt+'*e^('+Sre(p1[i],7,5)+'*t)';
      CASE tipo[i] OF
        simples:Msg('#'+Sri(i)+': '+Sre(k1[i],7,5)+txt);
        complexo:Msg('#'+Sri(i)+': ('+Sre(2*k1[i],7,5)+'*cos('+Sre(p2[i],7,5)+'*t)+'
                     +^M^J+'     '+Sre(-2*k2[i],7,5)+'*sin('+Sre(p2[i],7,5)+'*t))'
                     +txt);
        infinito:BEGIN
                   txt:='i';
                   FOR j:=1 TO ordem[i] DO txt:=txt+'''';
                   Msg('#'+Sri(i)+': '+Sre(k1[i],7,5)+txt+'(t)');
                 END
      END
    END;
    Msg('with t multiplied by '+Sre(fator,12,3));
    funcoes_lidas:=TRUE;
    grafico_valido:=FALSE;
    atual:=n;
    IF not escalas_normalizadas THEN BEGIN
      txmax^.panel_real:=100/fator;
      escalas_normalizadas:=TRUE;
    END;
    open_window(fescalas)
  END
END;

PROCEDURE CorrectLength(comprimento,n,pitch,d:Xv_opaque);
BEGIN
  comprimento^.panel_real:=n^.panel_real*d^.panel_real*pitch^.panel_real;
  xv_set(comprimento,comprimento^.xv_label);
END;

PROCEDURE CorrectRadius(raio1,raio2,n,pitch,d:Xv_opaque);
BEGIN
  raio2^.panel_real:=raio1^.panel_real+n^.panel_real*d^.panel_real*pitch^.panel_real;
  xv_set(raio2,raio2^.xv_label);
END;

{$F+}

{Callbacks}

PROCEDURE EscreverArquivo(obj:Xv_opaque);
BEGIN
  close_window(obj^.owner);
  IF not grafico_valido THEN BEGIN
    Msg('* Nothing to write');
    Exit
  END;
  Assign(arquivo,tEscrever^.panel_value);
  ReWrite(arquivo);
  FOR i:=1 TO fdet[n].ultimo DO BEGIN
    Write(arquivo,gx[i],' ');
    FOR k:=1 TO n DO Write(arquivo,fdet[k].gy[i],' ');
    WriteLn(arquivo)
  END;
  Close(arquivo);
  Msg('File '+tEscrever^.panel_value+' written'^M^J);
END;

PROCEDURE LerArquivo(obj:Xv_opaque);
BEGIN
  {Notify handler para trede}
  IF n=mf THEN BEGIN
    Msg('* Only '+Sri(mf)+' functions allowed');
    Exit
  END;
  Inc(n);
  {Ler funcao}
  WITH fdet[n] DO BEGIN
    nsaida:=tnsaida^.panel_int;
    rede:=trede^.panel_value;
    Msg(^M^J'Reading function # '+Sri(n)+': '+rede+'; function (node) # '+Sri(nsaida));
    sufixo:='.d';
    Assign(arquivo,rede+sufixo);
    {$I-} Reset(arquivo); {$I+}
    ok:=IOResult=0;
    IF not ok THEN BEGIN
    Msg('* Missing denominator file '+rede+sufixo);
      Dec(n);
      Exit;
    END;
    ReadLn(arquivo,np);
    IF np>mg THEN BEGIN
      Msg('* Excessive number of poles (max='+Sri(mg)+')');
      Close(arquivo); Dec(n); Exit
    END;
    FOR i:=0 TO np DO
      ReadLn(arquivo,den[i]);
    IF not SeekEof(arquivo) THEN ReadLn(arquivo,t) ELSE t:=1;
    IF not SeekEof(arquivo) THEN ReadLn(arquivo,fator) ELSE fator:=1;
    Close(arquivo);
    Msg('Denominator read from file '+rede+sufixo);
    IF nsaida=0 THEN sufixo:='' ELSE Str(nsaida,sufixo);
    sufixo:='.n'+sufixo;
    Assign(arquivo,rede+sufixo);
    {$I-} Reset(arquivo); {$I+}
    ok:=IOResult=0;
    IF not ok THEN BEGIN
      Msg('* Missing file '+rede+sufixo);
      Dec(n); Exit
    END;
    ReadLn(arquivo,nz);
    IF nz>mg THEN BEGIN
      Msg('* Numerator with excessive degree (max='+Sri(mg)+')');
      Dec(n); Close(arquivo); Exit
    END;
    FOR i:=0 TO nz DO
      ReadLn(arquivo,num[i]);
    IF not SeekEof(arquivo) THEN ReadLn(arquivo,cte)
    ELSE cte:=1;
    cte:=cte/t;
    Msg('Numerator read from file '+rede+sufixo);
    Close(arquivo);
    Msg('Laplace transform denominator:');
    FOR i:=0 TO np DO
      Msg('a'+Sri(i)+':'+Sre(den[i],14,8));
    Msg('Laplace transform numerator:');
    FOR i:=0 TO nz DO
      Msg('b'+Sri(i)+':'+Sre(num[i],14,8));
    Msg('Multiplying cte.:'+Sre(cte,14,8));
    Biv(np,den,p1,p2);
    Msg('Poles:');
    FOR i:=1 TO np DO
      Msg('p'+Sri(i)+':'+Sre(p1[i],14,8)+' '+Sre(p2[i],14,8)+'j');
    {Colocar a entrada}
    CASE sentrada^.sel_setting OF
      1:BEGIN
          cte:=cte*fator;
          Msg('Impulse: Response multiplied by '+Sre(fator,10,-1));
        END;
      2:BEGIN
        np:=np+1;
        p1[np]:=0; p2[np]:=0;
        Msg('Step: added pole p'+Sri(np)+' at origin');
      END;
      3,4:BEGIN
        w:=tw^.panel_real/fator;
        IF sentrada^.sel_setting=3 THEN ttysw_output(tmsg,'Sinusoid')
        ELSE ttysw_output(tmsg,'Cosinusoid');
        Msg(' of normalized frequency '+Cpct(w)+' rad/s');
        np:=np+2;
        p1[np-1]:=0; p1[np]:=0;
        p2[np-1]:=w; p2[np]:=-w;
        Msg('Added poles p'+Sri(np-1)+' and p'+Sri(np)+' in jw)');
        IF sentrada^.sel_setting=4 THEN BEGIN
          nz:=nz+1;
          FOR i:=nz DOWNTO 1 DO num[i]:=num[i-1];
          num[0]:=0;
          Msg('Numerator multiplied by s');
        END
        ELSE BEGIN
          cte:=cte*w;
          Msg('Cte. multiplied by w');
        END
      END;
    END;
    rede:=rede+'-n'+Sri(nsaida);
  END;
  CalcularExpansao(n);
END;

PROCEDURE DesenharGrafico(obj:Xv_opaque);
VAR
  i,ii:INTEGER;
  x,xn,y:REAL;
BEGIN
  WHILE active_w<>fprincipal DO close_window(active_w);
  IF obj<>cgrafico THEN BEGIN
    SetFillStyle(SolidFill,cgrafico^.back_color);
    Bar(0,0,cgrafico^.dx,cgrafico^.dy);
  END;
  IF not funcoes_lidas THEN Exit;
  IF not grafico_valido THEN FOR i:=1 TO n DO fdet[i].ultimo:=0;
  {Mapeamento}
  xmin:=80;
  xmax:=cgrafico^.dx-10;
  ymin:=10;
  ymax:=cgrafico^.dy-12;
  ay:=(ymax-ymin)/(tymin^.panel_real-tymax^.panel_real);
  by:=ymax-ay*tymin^.panel_real;
  ax:=(xmax-xmin)/(txmax^.panel_real-txmin^.panel_real);
  bx:=xmax-ax*txmax^.panel_real;
  {grade X}
  SetLineStyle(DottedLn,0,NormWidth);
  SetColor(cor[0]);
  d1:=txmax^.panel_real-txmin^.panel_real;
  IF d1>0 THEN BEGIN
    t1:=Exp(Ln(10)*Round(Ln(d1)/Ln(10)-0.499999));
    t2:=Round(txmin^.panel_real/t1+0.5)*t1;
    WHILE t2<txmax^.panel_real DO BEGIN
      i:=Round(ax*t2+bx);
      Line(i,ymin,i,ymax);
      t2:=t2+t1
    END
  END;
  {grade Y}
  d1:=tymax^.panel_real-tymin^.panel_real;
  IF d1>0 THEN BEGIN
    t1:=Exp(Ln(10)*Round(Ln(d1)/Ln(10)-0.499999));
    t2:=Round(tymin^.panel_real/t1+0.5)*t1;
    WHILE t2<tymax^.panel_real DO BEGIN
      i:=Round(ay*t2+by);
      Line(xmin,i,xmax,i);
      t2:=t2+t1
    END
  END;
  SetLineStyle(SolidLn,0,NormWidth);
  {Eixos}
  SetColor(cgrafico^.fore_color);
  Line(xmin,ymin,xmin,cgrafico^.dy-12);
  Line(xmin,cgrafico^.dy-12,cgrafico^.dx-10,cgrafico^.dy-12);
  SetTextJustify(RightText,TopText);
  OutTextXY(xmin,ymin,Sre(tymax^.panel_real,5,-1));
  SetTextJustify(RightText,BottomText);
  OutTextXY(xmin,cgrafico^.dy-10,Sre(tymin^.panel_real,5,-1));
  SetTextJustify(RightText,TopText);
  OutTextXY(cgrafico^.dx-10,cgrafico^.dy-10,Sre(txmax^.panel_real,5,-1));
  SetTextJustify(LeftText,TopText);
  OutTextXY(xmin,cgrafico^.dy-10,Sre(txmin^.panel_real,5,-1));
  {Calculo e plotagem}
  cursorx:=-10; cursory:=-10;
  grafico_valido:=TRUE;
  delta:=(txmax^.panel_real-txmin^.panel_real)/(tpontos^.panel_int-1);
  x:=txmin^.panel_real;
  maximo:=0;
  FOR i:=1 TO tpontos^.panel_int DO BEGIN
    gx[i]:=x;
    FOR ii:=1 TO n DO WITH fdet[ii] DO BEGIN
      {Calcula, se necessario}
      IF i>ultimo THEN BEGIN
        xn:=x*fator;
        n1:=0;
        FOR k:=1 TO np DO BEGIN
          d1:=p1[k]*xn;
          t:=1;
          FOR j:=1 TO ordem[k]-1 DO t:=t*xn/j;
          IF d1>-88 THEN d1:=Exp(d1) ELSE d1:=0;
          IF tipo[k]=simples THEN n1:=n1+t*d1*k1[k]
          ELSE IF tipo[k]=complexo THEN BEGIN
            d2:=p2[k]*xn;
            n1:=n1+2*t*d1*(k1[k]*Cos(d2)-k2[k]*Sin(d2));
          END
        END;
        gy[i]:=n1;
      END;
      {Plota}
      IF i>1 THEN MoveTo(xg,yg);
      xg:=Round(ax*gx[i]+bx);
      yg:=LimY(ay*gy[i]+by);
      SetColor(cor[ii]);
      IF i>1 THEN LineTo(xg,yg) ELSE MoveTo(xg,yg);
      IF Abs(gy[i])>maximo THEN maximo:=Abs(gy[i]);
    END;
    x:=x+delta;
    IF KeyPressed THEN BEGIN
      fdet[n].ultimo:=i;
      Exit;
    END
  END;
  fdet[n].ultimo:=tpontos^.panel_int
END;

PROCEDURE TratarEventos(obj:Xv_opaque);
VAR
  i,k:INTEGER;
BEGIN
  IF not grafico_valido THEN Exit;
  IF (ie_shiftcode=1) or (ie_code=MS_MIDDLE) or (ie_code=9) THEN BEGIN
    {Acha o ponto mais proximo}
    {Precisa do 1.0 multiplicando}
    i:=Round(1.0*(tpontos^.panel_int-1)*(ie_locx-xmin)/(xmax-xmin))+1;
    IF (i>=1) and (i<=fdet[n].ultimo) THEN BEGIN
      IF (ie_code=MS_MIDDLE) or (ie_code=9) THEN BEGIN
        Inc(atual); IF atual>n THEN atual:=1
      END;
      SetWriteMode(XorPut);
      SetColor(c_white);
      Line(cursorx,ymin,cursorx,ymax);
      Rectangle(cursorx-3,cursory-3,cursorx+3,cursory+3);
      cursorx:=Round(ax*gx[i]+bx);
      cursory:=Round(ay*fdet[atual].gy[i]+by);
      Line(cursorx,ymin,cursorx,ymax);
      Rectangle(cursorx-3,cursory-3,cursorx+3,cursory+3);
      SetWriteMode(NormalPut);
      SetFillStyle(SolidFill,cgrafico^.back_color);
      SetColor(cgrafico^.fore_color);
      Bar3d(cgrafico^.dx-129,7,cgrafico^.dx-7,33,0,FALSE);
      k:=cgrafico^.dx-127;
      OutTextXY(k,9,fdet[atual].rede+' (#'+Sri(atual)+')');
      OutTextXY(k,17,'t:'+Sre(gx[i],13,-1));
      OutTextXY(k,25,'v:'+Sre(fdet[atual].gy[i],13,-1));
    END
  END
  ELSE
    CASE ie_code of
      Ord('+'):BEGIN
          tymax^.panel_real:=tymin^.panel_real+(tymax^.panel_real-tymin^.panel_real)/2;
          DesenharGrafico(nil);
        END;
      Ord('-'):BEGIN
          tymax^.panel_real:=tymin^.panel_real+(tymax^.panel_real-tymin^.panel_real)*2;
          DesenharGrafico(nil);
        END;
      kUpArrow:BEGIN
           t:=(tymax^.panel_real-tymin^.panel_real)/4;
           tymax^.panel_real:=tymax^.panel_real+t;
           tymin^.panel_real:=tymin^.panel_real+t;
           DesenharGrafico(nil);
         END;
      kDownArrow:BEGIN
           t:=(tymax^.panel_real-tymin^.panel_real)/4;
           tymax^.panel_real:=tymax^.panel_real-t;
           tymin^.panel_real:=tymin^.panel_real-t;
           DesenharGrafico(nil);
         END;
      kRightArrow:BEGIN
           t:=(txmax^.panel_real-txmin^.panel_real)/2;
           txmax^.panel_real:=txmax^.panel_real+t;
           txmin^.panel_real:=txmin^.panel_real+t;
           grafico_valido:=FALSE;
           DesenharGrafico(nil);
         END;
      kLeftArrow:BEGIN
           t:=(txmax^.panel_real-txmin^.panel_real)/2;
           txmax^.panel_real:=txmax^.panel_real-t;
           txmin^.panel_real:=txmin^.panel_real-t;
           IF txmin^.panel_real<0 THEN BEGIN
             txmax^.panel_real:=2*t;
             txmin^.panel_real:=0;
           END;
           grafico_valido:=FALSE;
           DesenharGrafico(nil);
         END;
      kHome,Ord('a'):BEGIN
           txmax^.panel_real:=txmin^.panel_real+(txmax^.panel_real-txmin^.panel_real)/2;
           grafico_valido:=FALSE;
           DesenharGrafico(nil);
         END;
      kEnd,Ord('r'):BEGIN
           txmax^.panel_real:=txmin^.panel_real+(txmax^.panel_real-txmin^.panel_real)*2;
           grafico_valido:=FALSE;
           DesenharGrafico(nil);
         END;
      kF1,Ord(' '):IF maximo<>0 THEN BEGIN
           tymax^.panel_real:=1.1*maximo;
           tymin^.panel_real:=-1.1*maximo;
           DesenharGrafico(nil);
         END;
      kF2:menu_show(menuprincipal);
      END;
END;


PROCEDURE Plotar(obj:Xv_opaque);
BEGIN
  DesenharGrafico(nil);
END;

PROCEDURE InvalidarGrafico(obj:Xv_opaque);
BEGIN
  grafico_valido:=FALSE
END;

PROCEDURE Reiniciar(obj:Xv_opaque);
BEGIN
  n:=0;
  grafico_valido:=FALSE;
  funcoes_lidas:=FALSE;
  Msg('Plots invalidated.');
  DesenharGrafico(nil);
  open_window(obj^.owner);
END;

PROCEDURE TratarMenuPrincipal(obj:Xv_opaque);
BEGIN
  CASE obj^.sel_menu OF
    2:{Ler circuito}
      open_window(fRLC);
    1:{Especificar dimensoes}
      open_window(fdimensoes);
    3:{Ler transformada}
      open_window(fLer);
    4:{Remover ultima curva}
      IF n>1 THEN BEGIN
        Dec(n);
        DesenharGrafico(nil);
      END
      ELSE Reiniciar(biniciarRLC);
    5:{Escrever tabela}
      open_window(fEscrever);
    6:{Processar Dados}
      open_window(fescalas);
    7:{Mensagens}
      open_window(fMensagens);
    8:{Parametros}
      open_window(fparametros);
    9:{Informacoes}
      Informacoes;
   10:{Terminar}
      xv_end:=TRUE;
   11:{Cancel}
      IF active_w<>fprincipal THEN close_window(active_w);
  END;
END;

PROCEDURE CriarCircuito(obj:Xv_opaque);
BEGIN
  close_window(fRLC);
  WITH tmsg^ DO yc:=dy;
  IF n>=mf THEN BEGIN
    Msg('* Only '+Sri(mf)+' functions allowed');
    Exit
  END;
  if tkl^.panel_real>=1.0 THEN BEGIN
    Msg('* The coupling coefficient must be < 1');
    Exit
  END;
  Inc(n);
  {Preencher funcao}
  Msg('Tesla circuit #'+Sri(n)+': '+ssaida^.item_setting[ssaida^.sel_setting]^+' voltage');
  WITH fdet[n] DO BEGIN
    r1:=tr1^.panel_real;
    l1:=tl1^.panel_real;
    c1:=tc1^.panel_real;
    r2:=tr2^.panel_real;
    l2:=tl2^.panel_real;
    c2:=tc2^.panel_real;
    kl:=tkl^.panel_real;
    vc1:=tvc1^.panel_real;
    Msg('R1='+Cpct(r1));
    Msg('L1='+Cpct(l1));
    Msg('C1='+Cpct(c1));
    Msg('R2='+Cpct(r2));
    Msg('L2='+Cpct(l2));
    Msg('C2='+Cpct(c2));
    Msg('K= '+Cpct(kl));
    Msg('Primary resonance frequency: '+Sre(1/2/Pi/Sqrt(l1*c1),13,-1)+' Hz');
    Msg('Secondary resonance frequency: '+Sre(1/2/Pi/Sqrt(l2*c2),13,-1)+' Hz');
    IF r1=0 THEN Msg('The primary circuit is lossless')
    ELSE BEGIN
      Msg('Primary time constant: '+Sre(2*l1/r1,14,-1)+' s');
      Msg('Primary Q: '+Sre(Sqrt(l1/c1)/r1,10,3));
    END;
    IF r2=0 THEN Msg('The secondary circuit is lossless')
    ELSE BEGIN
      Msg('Secondary time constant: '+Sre(2*l2/r2,14,-1)+' s');
      Msg('Secondary Q: '+Sre(Sqrt(l2/c2)/r2,10,3));
    END;
    t:=1-Sqr(kl);
    fator:=1/sqrt(sqrt(t*l1*l2*c1*c2));
    IF sarredondar^.sel_setting=1 THEN fator:=exp(Ln(10)*Int(Ln(fator)/Ln(10)));
    c1:=c1*fator;
    c2:=c2*fator;
    l1:=l1*fator;
    l2:=l2*fator;
    np:=4;
    den[0]:=1/(l1*l2*c1*c2)/t;
    den[1]:=(r2/(l2*l1*c1)+r1/(l1*l2*c2))/t;
    den[2]:=(1/(l1*c1)+1/(l2*c2)+r1*r2/(l1*l2))/t;
    den[3]:=(r1/l1+r2/l2)/t;
    den[4]:=1;
    CASE ssaida^.sel_setting OF
    1:BEGIN
        nz:=3;
        num[0]:=r1/(l1*l2*c2)/t;
        num[1]:=(1/(l2*c2)+r1*r2/(l1*l2))/t;
        num[2]:=(r1/l1+r2/l2)/t;
        num[3]:=1;
        cte:=vc1;
        rede:='VC1';
      END;
    2:BEGIN
        nz:=1;
        num[0]:=0;
        num[1]:=1;
        cte:=vc1*kl/c2/sqrt(l1*l2)/t;
        rede:='VC2';
      END;
    3:BEGIN
        nz:=2;
        num[0]:=1/(l2*c2);
        num[1]:=r2/l2;
        num[2]:=1;
        cte:=vc1/l1/t*tfator^.panel_real;
        rede:='IL1';
      END;
    4:BEGIN
        nz:=2;
        num[0]:=0;
        num[1]:=0;
        num[2]:=1;
        cte:=-vc1*kl/sqrt(l1*l2)/t*tfator^.panel_real;
        rede:='IL2';
      END;
    END;
    Biv(np,den,p1,p2);
    Msg('Laplace transform denominator:');
    FOR i:=0 TO np DO
      Msg('a'+Sri(i)+':'+Sre(den[i],14,8));
    Msg('Laplace transform numerator:');
    FOR i:=0 TO nz DO
      Msg('b'+Sri(i)+':'+Sre(num[i],14,8));
    Msg('Multiplying cte.:'+Sre(cte,14,8));
    Msg('Poles:');
    FOR i:=1 TO np DO
      Msg('p'+Sri(i)+':'+Sre(p1[i],14,8)+' '+Sre(p2[i],14,8)+'j');
    IF not escalas_normalizadas THEN BEGIN
      if ssaida^.sel_setting=1 THEN t:=1.1*vc1
      ELSE t:=vc1*sqrt(l2/l1)*1.1;
      tymax^.panel_real:=t;
      tymin^.panel_real:=-t;
    END;
  END;
  CalcularExpansao(n);
  WITH fdet[n] DO BEGIN
    Msg('Resonance frequencies:'^M^J+
    'f1='+Sre(Abs(p2[1])*fator/2/Pi,14,-1)+' Hz; f2='+Sre(Abs(p2[3])*fator/2/Pi,14,-1)+' Hz');
    {
    Msg('Time constants:'^M^J+
    't1='+Sre(-1/(fator*p1[1]),14,-1)+' s; t2='+Sre(-1/(fator*p1[3]),14,-1)+' s');
    Msg('Quality factors:');
    IF Abs(p1[1])>tzero^.panel_real THEN
      ttysw_output(tmsg,'Q1='+Sre(Abs(p2[1]/2/p1[1]),10,3))
    ELSE ttysw_output(tmsg,'Q1=infinite');
    IF Abs(p1[3])>tzero^.panel_real THEN
      Msg('; Q2='+Sre(Abs(p2[3]/2/p1[3]),10,3))
    ELSE Msg('; Q2=infinite');
    }
    Msg('Time constants and quality factors:');
    IF Abs(p1[1])>tzero^.panel_real THEN
      Msg('t1='+Sre(-1/(fator*p1[1]),14,-1)+' s; Q1='+Sre(Abs(p2[1]/2/p1[1]),10,3))
    ELSE Msg('t1=infinite; Q1=infinite');
    IF Abs(p1[3])>tzero^.panel_real THEN
       Msg('t2='+Sre(-1/(fator*p1[3]),14,-1)+' s; Q2='+Sre(Abs(p2[3]/2/p1[3]),10,3))
    ELSE Msg('t2=infinite; Q2=infinite');
    IF not escalas_normalizadas THEN BEGIN
      if ssaida^.sel_setting=1 THEN t:=1.1*vc1
      ELSE t:=vc1*sqrt(l2/l1)*1.1;
      tymax^.panel_real:=t;
      tymin^.panel_real:=-t;
    END;
  END;
END;

PROCEDURE TratarMenuTty(obj:Xv_opaque);
BEGIN
  CASE obj^.sel_menu OF
  1:BEGIN
      WITH tmsg^ DO yc:=dy;
      ttysw_output(tmsg,'');
    END;
  2:WITH tmsg^ DO BEGIN
      Assign(arquivo,'tesla.msg');
      ReWrite(arquivo);
      i:=bstart;
      WHILE i<>tend DO BEGIN
        Write(arquivo,Pb^[i]);
        IF i<bsize THEN Inc(i) ELSE i:=0
      END;
      Close(arquivo);
      Msg('Messages saved in file tesla.msg')
    END;
  3:IF active_w<>fprincipal THEN close_window(active_w);
  END;
END;

PROCEDURE AplicarDimensoes(obj:Xv_opaque);
VAR
  comprimento1,comprimento2,res1,res2:REAL;
BEGIN
  {Notify handler for baplicardimensoes}
  comprimento1:=comprimento(1,tcomprimento1^.panel_real,traio1^.panel_real,tn1^.panel_real);
  comprimento2:=comprimento(2,tcomprimento2^.panel_real,traio2^.panel_real,tn2^.panel_real);
  Msg('Primary resistance:');
  res1:=resistencia(comprimento1,td1^.panel_real,frequencia1);
  Msg('Secondary resistance:');
  res2:=resistencia(comprimento2,td2^.panel_real,frequencia2);
  tl1^.panel_real:=indutancia(1,tcomprimento1^.panel_real,traio1^.panel_real,tn1^.panel_real);
  xv_set(tl1,tl1^.xv_label);
  tl2^.panel_real:=indutancia(2,tcomprimento2^.panel_real,traio2^.panel_real,tn2^.panel_real);
  xv_set(tl2,tl2^.xv_label);
  tc1^.panel_real:=capacitancia(1,tcomprimento1^.panel_real,traio1^.panel_real)+tcap1^.panel_real;
  xv_set(tc1,tc1^.xv_label);
  tc2^.panel_real:=capacitancia(2,tcomprimento2^.panel_real,traio2^.panel_real)+tcap2^.panel_real;
  xv_set(tc2,tc2^.xv_label);
  tr1^.panel_real:=res1;
  xv_set(tr1,tr1^.xv_label);
  tr2^.panel_real:=res2;
  xv_set(tr2,tr2^.xv_label);
  {
  Msg('Primary inductance: '+Sre(tl1^.panel_real,15,-1)+' H');
  Msg('Primary capacitance: '+Sre(tc1^.panel_real,15,-1)+' H');
  Msg('Secondary inductance: '+Sre(tl2^.panel_real,15,-1)+' F');
  Msg('Secondary capacitance: '+Sre(tc2^.panel_real,15,-1)+' F');
  }
END;

PROCEDURE TuneC1(obj:Xv_opaque);
VAR
  f:REAL;
BEGIN
  {Notify handler for btunec1}
  f:=frequencia2;
  Msg('Primary capacitance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
  tcap1^.panel_real:=1/sqr(2*pi*f)/indutancia(1,tcomprimento1^.panel_real,traio1^.panel_real,tn1^.panel_real)-
                     capacitancia(1,tcomprimento1^.panel_real,traio1^.panel_real);
  xv_set(tcap1,tcap1^.xv_label);
END;

PROCEDURE TuneC2(obj:Xv_opaque);
VAR
  f,c:REAL;
BEGIN
  {Notify handler for btunec2}
  f:=frequencia1;
  c:=1/sqr(2*pi*f)/indutancia(2,tcomprimento2^.panel_real,traio2^.panel_real,tn2^.panel_real)-
      capacitancia(2,tcomprimento2^.panel_real,traio2^.panel_real);
  IF c<0 THEN Msg('* Insuficient primary capacitance for tuning C2')
  ELSE BEGIN
    tcap2^.panel_real:=c;
    xv_set(tcap2,tcap2^.xv_label);
    Msg('Secondary top capacitance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
  END;
END;

PROCEDURE TuneL1(obj:Xv_opaque);
VAR
  f,n:REAL;
BEGIN
  {Notify handler for btunel1}
  f:=frequencia2;
  n:=voltas(1,1/(tcap1^.panel_real+capacitancia(1,tcomprimento1^.panel_real,traio1^.panel_real))
     /sqr(2*pi*f),tcomprimento1^.panel_real,traio1^.panel_real);
  tn1^.panel_real:=n;
  xv_set(tn1,tn1^.xv_label);
  Msg('Primary inductance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
END;

PROCEDURE TuneL2(obj:Xv_opaque);
VAR
  f,n:REAL;
BEGIN
  {Notify handler for btunel2}
  f:=frequencia1;
  n:=voltas(2,1/(tcap2^.panel_real+capacitancia(2,tcomprimento2^.panel_real,traio2^.panel_real))
     /sqr(2*pi*f),tcomprimento2^.panel_real,traio2^.panel_real);
  tn2^.panel_real:=n;
  xv_set(tn2,tn2^.xv_label);
  Msg('Secondary inductance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
END;

PROCEDURE TuneC12(obj:Xv_opaque);
VAR
  f,c:REAL;
BEGIN
  {Notify handler for btunec12}
  f:=frequencia22;
  c:=1/sqr(2*pi*f)/tl1^.panel_real;
  tc1^.panel_real:=c;
  xv_set(tc1,tc1^.xv_label);
  Msg('Primary capacitance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
END;

PROCEDURE TuneC22(obj:Xv_opaque);
VAR
  f,c:REAL;
BEGIN
  {Notify handler for btunec22}
  f:=frequencia12;
  c:=1/sqr(2*pi*f)/tl2^.panel_real;
  tc2^.panel_real:=c;
  xv_set(tc2,tc2^.xv_label);
  Msg('Secondary capacitance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
END;

PROCEDURE TuneL12(obj:Xv_opaque);
VAR
  f,l:REAL;
BEGIN
  {Notify handler for btunel12}
  f:=frequencia22;
  l:=1/sqr(2*pi*f)/tc1^.panel_real;
  tl1^.panel_real:=l;
  xv_set(tl1,tl1^.xv_label);
  Msg('Primary inductance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
END;

PROCEDURE TuneL22(obj:Xv_opaque);
VAR
  f,l:REAL;
BEGIN
  {Notify handler for btunel22}
  f:=frequencia12;
  l:=1/sqr(2*pi*f)/tc2^.panel_real;
  tl2^.panel_real:=l;
  xv_set(tl2,tl2^.xv_label);
  Msg('Secondary inductance tuned for resonance at:'^M^J'f='+Sre(f,10,2)+' Hz');
END;

PROCEDURE Notifystipo1(obj:Xv_opaque);
BEGIN
  {Notify handler for stipo1}
  IF obj^.sel_setting=1 THEN BEGIN
    xv_set(tcomprimento1,'Primary length');
    xv_set(traio1,'Primary radius');
  END
  ELSE BEGIN
    xv_set(tcomprimento1,'Prim. int. radius');
    xv_set(traio1,'Prim. ext. radius');
  END;
END;

PROCEDURE AWG(obj:Xv_opaque);
VAR
  d:REAL;
BEGIN
  d:=exp(aa*obj^.panel_int+bb)/100000*2.54;
  IF obj=tawg1 THEN BEGIN
    td1^.panel_real:=d;
    xv_set(td1,td1^.xv_label);
  END
  ELSE BEGIN
    td2^.panel_real:=d;
    xv_set(td2,td2^.xv_label);
  END;
END;

PROCEDURE AWGI(obj:Xv_opaque);
VAR
  n:INTEGER;
BEGIN
  n:=Round((Ln(obj^.panel_real*100000/2.54)-bb)/aa);
  IF obj=td1 THEN BEGIN
    tawg1^.panel_int:=n;
    xv_set(tawg1,tawg1^.xv_label);
  END
  ELSE BEGIN
    tawg2^.panel_int:=n;
    xv_set(tawg2,tawg2^.xv_label);
  END
END;

PROCEDURE Adjust(obj:Xv_opaque);
BEGIN
   IF stipo1^.sel_setting=1 THEN BEGIN
     CorrectLength(tcomprimento1,tn1,tpitch1,td1);
     Msg('Primary length adjusted to '+Cpct(tcomprimento1^.panel_real)+' m');
     Msg('Primary inductance: '+Cpct(1e6*
       indutancia(1,tcomprimento1^.panel_real,traio1^.panel_real,tn1^.panel_real))
       +' uH')
   END
   ELSE BEGIN
     CorrectRadius(tcomprimento1,traio1,tn1,tpitch1,td1);
     Msg('Primary external radius adjusted to '+Cpct(traio1^.panel_real)+' m');
     Msg('Primary inductance: '+Cpct(1e6*
       indutancia(2,tcomprimento1^.panel_real,traio1^.panel_real,tn1^.panel_real))
       +' uH')
   END;
   CorrectLength(tcomprimento2,tn2,tpitch2,td2);
   Msg('Secondary length adjusted to '+Cpct(tcomprimento2^.panel_real)+' m');
   Msg('Secondary inductance: '+Cpct(1e3*
       indutancia(2,tcomprimento2^.panel_real,traio2^.panel_real,tn2^.panel_real))
       +' mH')
END;

PROCEDURE Changek(obj:Xv_opaque);
VAR
  k12:REAL;
  k,l:INTEGER;
BEGIN
  k:=tk^.panel_int;
  l:=tl^.panel_int;
  IF l<=k THEN BEGIN
    IF obj=tk THEN l:=k+1 ELSE k:=l-1;
    tl^.panel_int:=l;
    xv_set(tl,tl^.xv_label);
    tk^.panel_int:=k;
    xv_set(tk,tk^.xv_label)
  END;
  k12:=(l*l-k*k)/(l*l+k*k);
  tkl^.panel_real:=k12;
  xv_set(tkl,tkl^.xv_label);
END;

PROCEDURE K12(obj:Xv_Opaque); {C lculo da indutƒncia m£tua pela f¢rmula de Neumann}
CONST
  ns1=20;
  ns2=20;
VAR
   ccos,csin:ARRAY[0..ns1-1] OF REAL;
   M,k,integral,a1,a2,g1,g2,dt1,dt2,t1,t2,ct1,ct2,st1,st2,t10,t20,
   x1,y1,z1,x2,y2,z2,dx1,dy1,dz1,dx2,dy2,dz2,rr1,rr2,n1,n2:REAL;
   ia1,ia2:INTEGER;
   r1,r2,s1,s2,h1,h2,b1,b2:real;
   i,j,m1,m2:LONGINT;
BEGIN
  Aplicardimensoes(obj); {Para atualizar as indutƒncias}
  n1:=tn1^.panel_real;
  n2:=tn2^.panel_real;
  IF (n1<1/ns1) or (n2<1/ns2) THEN BEGIN
    Msg('Too small number of turns');
    Exit
  END;
  IF stipo1^.sel_setting=1 THEN BEGIN
    r1:=traio1^.panel_real;
    s1:=traio1^.panel_real;
    h1:=tcomprimento1^.panel_real;
  END
  ELSE BEGIN
    r1:=tcomprimento1^.panel_real;
    s1:=traio1^.panel_real;
    h1:=0
  END;
  r2:=traio2^.panel_real;
  s2:=traio2^.panel_real;
  h2:=tcomprimento2^.panel_real;
  b1:=tb1^.panel_real;
  b2:=tb2^.panel_real;
  a1:=h1/(2*pi*n1);
  a2:=h2/(2*pi*n2);
  g1:=(s1-r1)/(2*pi*n1);
  g2:=(s2-r2)/(2*pi*n2);
  dt1:=2*pi/ns1;
  dt2:=2*pi/ns2;
  m1:=trunc(n1*ns1)-1;
  m2:=trunc(n2*ns2)-1;
  t10:=dt1/2;
  t20:=dt2/2;
  dz1:=a1;
  dz2:=a2;
  FOR i:=0 TO ns1-1 DO BEGIN {precalcula para acelerar}
    ccos[i]:=cos((i+0.5)*dt1);
    csin[i]:=sin((i+0.5)*dt1)
  END;
  integral:=0;
  t1:=t10;
  ia1:=0;
  ttysw_output(tmsg,'Computing k12');
  FOR i:=0 TO m1 DO BEGIN
    ct1:=ccos[ia1];
    st1:=csin[ia1];
    rr1:=r1+g1*t1;
    x1:=rr1*ct1;
    y1:=rr1*st1;
    z1:=a1*t1+b1;
    dx1:=-y1+g1*ct1;
    dy1:=x1+g1*st1;
    t2:=t20;
    ia2:=0;
    FOR j:=0 TO m2 DO BEGIN
      ct2:=ccos[ia2];
      st2:=csin[ia2];
      rr2:=r2+g2*t2;
      x2:=rr2*ct2;
      y2:=rr2*st2;
      z2:=a2*t2+b2;
      dx2:=-y2+g2*ct2;
      dy2:=x2+g2*st2;
      integral:=integral+(dx1*dx2+dy1*dy2+dz1*dz2)/sqrt(sqr(x1-x2)+sqr(y1-y2)+sqr(z1-z2));
      t2:=t2+dt2;
      ia2:=(ia2+1) mod ns2;
    END;
    t1:=t1+dt1;
    ia1:=(ia1+1) mod ns1;
    IF ia1=0 THEN ttysw_output(tmsg,'.');
  END;
  M:=1e-7*integral*dt1*dt2;
  Msg(^J^M'Mutual Inductance M: '+Sre(M/1e-3,15,10)+' mH');
  k:=M/sqrt(tl1^.panel_real*tl2^.panel_real);
  Msg('L1: '+Sre(tl1^.panel_real/1e-3,15,10)+' mH; '+Sre(n1,6,2)+' turns');
  Msg('L2: '+Sre(tl2^.panel_real/1e-3,15,10)+' mH; '+Sre(n2,6,2)+' turns');
  Msg('Coupling coefficient k12: '+Sre(k,12,10));
  tkl^.panel_real:=k;
  xv_set(tkl,tkl^.xv_label)
END;

{$F-}

PROCEDURE Default;
BEGIN
  o_base^.owner^.mouse_obj:=o_base
END;

BEGIN
  {Constantes para valores AWG: diametro em mils=exp(aa*n+bb)}
  aa:=-Ln(460/5)/39;
  bb:=(36*Ln(460)+3*Ln(5))/39;
  normal_bsize:=10000;
  c_active:=3;
  n:=0;
  DetectGraph(placa,modo);
  IF placa=CGA THEN modo:=CGAC1;
  IF ParamCount=2 THEN BEGIN
    Val(ParamStr(1),placa,i);
    Val(ParamStr(2),modo,i)
  END;
  xv_init(placa,modo);
  IF GetMaxColor=3 THEN cor:=corCGA
  ELSE IF GetMaxColor=1 THEN cor:=cormono;
  MenuPrincipal:=xv_create(menu);
  WITH MenuPrincipal^ DO BEGIN
    xv_label:='Menu:';
    item_create('specify geometry');
    item_create('specify circuit parameters');
    item_create('read Laplace transform');
    item_create('remove last curve');
    item_create('write table');
    item_create('set scales');
    item_create('messages');
    item_create('parameters');
    item_create('informations');
    item_create('quit');
    item_create('close');
    sel_menu:=2;
    notify_handler:=TratarMenuPrincipal;
  END;
  MenuTty:=xv_create(menu);
  WITH MenuTty^ DO BEGIN
    xv_label:='Messages:';
    item_create('clear messages');
    item_create('save messages');
    item_create('close');
    notify_handler:=TratarMenuTty;
  END;
  {Interface objects creation}
  fEscrever:=xv_create(frame);
  WITH fEscrever^ DO BEGIN
    xv_label:='Write table';
    dx:=319;
    dy:=55;
    dymin:=55;
    menu_name:=MenuPrincipal;
  END;
  tEscrever:=xv_create(textfield);
  WITH tEscrever^ DO BEGIN
    xv_label:='File';
    value_length:=33;
    panel_value:='tesla.tab';
    notify_handler:=EscreverArquivo;
    owner^.mouse_obj:=tEscrever;
  END;
  bEscrever:=xv_create(button);
  WITH bEscrever^ DO BEGIN
    xv_label:='Write';
    y:=15;
    notify_handler:=EscreverArquivo;
    owner^.mouse_obj:=bEscrever;
  END;

  fescalas:=xv_create(frame);
  WITH fescalas^ DO BEGIN
    xv_label:='Scales';
    dx:=250;
    dy:=114;
    menu_name:=MenuPrincipal;
  END;
  tpontos:=xv_create(textfield);
  WITH tpontos^ DO BEGIN
    xv_label:='Points';
    y:=60;
    field_type:=int_field;
    panel_int:=ptmax;
    min_value:=1;
    max_value:=ptmax;
    notify_handler:=InvalidarGrafico;
  END;
  normal_length:=20;
  txmin:=xv_create(textfield);
  WITH txmin^ DO BEGIN
    xv_label:='t minimum';
    field_type:=real_field;
    panel_real:=tmin0;
    notify_handler:=InvalidarGrafico;
  END;
  txmax:=xv_create(textfield);
  WITH txmax^ DO BEGIN
    xv_label:='t maximum';
    y:=15;
    panel_real:=tmax0;
    field_type:=real_field;
    notify_handler:=InvalidarGrafico;
  END;
  tymin:=xv_create(textfield);
  WITH tymin^ DO BEGIN
    xv_label:='y minimum';
    y:=30;
    panel_real:=ymin0;
    field_type:=real_field;
  END;
  tymax:=xv_create(textfield);
  WITH tymax^ DO BEGIN
    xv_label:='y maximum';
    y:=45;
    panel_real:=ymax0;
    field_type:=real_field;
  END;
  bplotar:=xv_create(button);
  WITH bplotar^ DO BEGIN
    xv_label:='Plot';
    y:=75;
    notify_handler:=Plotar;
    owner^.mouse_obj:=bplotar;
  END;
  fLer:=xv_create(frame);
  WITH fLer^ DO BEGIN
    xv_label:='Read Laplace transform';
    dx:=319;
    dy:=99;
    menu_name:=MenuPrincipal;
  END;
  trede:=xv_create(textfield);
  WITH trede^ DO BEGIN
    xv_label:='Function name';
    value_length:=24;
    owner^.mouse_obj:=trede
  END;
  tnsaida:=xv_create(textfield);
  WITH tnsaida^ DO BEGIN
    xv_label:='Function (node) #';
    y:=15;
    value_length:=5;
    field_type:=int_field;
    panel_int:=1;
    min_value:=0;
  END;
  sentrada:=xv_create(setting);
  WITH sentrada^ DO BEGIN
    xv_label:='Input';
    y:=30;
    item_create('impulse');
    item_create('step');
    item_create('sine');
    item_create('cosine');
    exclusive:=TRUE;
    sel_setting:=2;
  END;
  tw:=xv_create(textfield);
  WITH tw^ DO BEGIN
    xv_label:='Frequency (rad/s)';
    y:=45;
    field_type:=real_field;
    panel_real:=1;
  END;
  biniciar:=xv_create(button);
  WITH biniciar^ DO BEGIN
    xv_label:='Restart';
    y:=60;
    notify_handler:=Reiniciar;
  END;
  bler:=xv_create(button);
  WITH bler^ DO BEGIN
    xv_label:='Add curve';
    x:=67;
    y:=60;
    notify_handler:=LerArquivo;
  END;
  fprincipal:=xv_create(frame);
  WITH fprincipal^ DO BEGIN
    xv_label:='TeslaSim';
    dx:=GetMaxX;
    dy:=GetMaxY;
    menu_name:=MenuPrincipal;
    adjust_exit:=FALSE;
  END;
  cgrafico:=xv_create(canvas);
  WITH cgrafico^ DO BEGIN
    back_color:=c_black;
    fore_color:=c_white;
    notify_handler:=DesenharGrafico;
    event_handler:=TratarEventos;
    menu_name:=MenuPrincipal;
  END;
  fmensagens:=xv_create(frame);
  WITH fmensagens^ DO BEGIN
    xv_label:='Messages';
    x:=320;
    dx:=319;
    dy:=GetMaxY;
    menu_name:=MenuPrincipal;
  END;
  tmsg:=xv_create(tty);
  tmsg^.menu_name:=MenuTty;

  fparametros:=xv_create(frame);
  WITH fparametros^ DO BEGIN
    xv_label:='Numerical control parameters';
    dx:=319;
    dy:=145;
    menu_name:=MenuPrincipal;
  END;
  traio:=xv_create(textfield);
  WITH traio^ DO BEGIN
    xv_label:='Interpolation radius';
    value_length:=17;
    field_type:=real_field;
    panel_real:=raio;
  END;
  tzero:=xv_create(textfield);
  WITH tzero^ DO BEGIN
    xv_label:='Minimum non-zero number';
    y:=15;
    value_length:=14;
    field_type:=real_field;
    panel_real:=zero;
  END;
  ttol:=xv_create(textfield);
  WITH ttol^ DO BEGIN
    xv_label:='Root tolerance';
    field_type:=real_field;
    panel_real:=1e-11;
    y:=30;
  END;
  ttolm:=xv_create(textfield);
  WITH ttolm^ DO BEGIN
    xv_label:='Derivative tol.';
    field_type:=real_field;
    panel_real:=1e-11;
    y:=45;
  END;
  treal:=xv_create(textfield);
  WITH treal^ DO BEGIN
    xv_label:='Real approx.';
    field_type:=real_field;
    panel_real:=1.1;
    y:=60;
  END;
  timag:=xv_create(textfield);
  WITH timag^ DO BEGIN
    xv_label:='Imag approx.';
    field_type:=real_field;
    panel_real:=1.1;
    y:=75;
  END;
  stestar:=xv_create(setting);
  WITH stestar^ DO BEGIN
    xv_label:='List tests';
    item_create('yes');
    item_create('no');
    y:=90;
    exclusive:=TRUE;
    sel_setting:=2;
  END;
  sarredondar:=xv_create(setting);
  WITH sarredondar^ DO BEGIN
    xv_label:='Round normalization factor';
    item_create('yes');
    item_create('no');
    y:=105;
    exclusive:=TRUE;
    sel_setting:=1;
  END;

  fRLC:=xv_create(frame);
  WITH fRLC^ DO BEGIN
    dx:=350;
    dy:=174;
    x:=(GetMaxX-dx) div 2;
    y:=(GetMaxY-dy) div 2+144;
    xv_label:='Tesla coil circuit parameters';
    menu_name:=MenuPrincipal;
  END;
  tr1:=xv_create(textfield);
  WITH tr1^ DO BEGIN
    xv_label:='Primary resistance   ';
    field_type:=real_field;
    panel_real:=r10;
  END;
  tl1:=xv_create(textfield);
  WITH tl1^ DO BEGIN
    xv_label:='Primary inductance   ';
    y:=15;
    field_type:=real_field;
    panel_real:=l10;
  END;
  tc1:=xv_create(textfield);
  WITH tc1^ DO BEGIN
    xv_label:='Primary capacitance  ';
    y:=30;
    field_type:=real_field;
    panel_real:=c10;
  END;
  tr2:=xv_create(textfield);
  WITH tr2^ DO BEGIN
    xv_label:='Secondary resistance ';
    y:=45;
    field_type:=real_field;
    panel_real:=r20;
  END;
  tl2:=xv_create(textfield);
  WITH tl2^ DO BEGIN
    xv_label:='Secondary inductance ';
    y:=60;
    field_type:=real_field;
    panel_real:=l20;
  END;
  tc2:=xv_create(textfield);
  WITH tc2^ DO BEGIN
    xv_label:='Secondary capacitance';
    y:=75;
    field_type:=real_field;
    panel_real:=c20;
  END;
  tkl:=xv_create(textfield);
  WITH tkl^ DO BEGIN
    xv_label:='Coupling coeff. k12  ';
    y:=90;
    field_type:=real_field;
    value_length:=9;
    panel_real:=kl0;
  END;
  tk:=xv_create(textfield);
  WITH tk^ DO BEGIN
    xv_label:='k';
    y:=90; x:=253;
    field_type:=int_field;
    value_length:=3;
    panel_int:=k0;
    min_value:=1;
    notify_handler:=Changek;
  END;
  tl:=xv_create(textfield);
  WITH tl^ DO BEGIN
    xv_label:='l';
    y:=90; x:=296;
    field_type:=int_field;
    value_length:=3;
    min_value:=1;
    panel_int:=l0;
    notify_handler:=Changek;
  END;
  tvc1:=xv_create(textfield);
  WITH tvc1^ DO BEGIN
    xv_label:='Primary peak voltage ';
    y:=105;
    field_type:=real_field;
    panel_real:=vc10;
  END;
  ssaida:=xv_create(setting);
  WITH ssaida^ DO BEGIN
    xv_label:='Plot';
    item_create('Vc1');
    item_create('Vc2');
    item_create('Il1xRt');
    item_create('Il2xRt');
    exclusive:=TRUE;
    sel_setting:=2;
    y:=120;
  END;
  tfator:=xv_create(textfield);
  WITH tfator^ DO BEGIN
    xv_label:='Rt';
    field_type:=real_field;
    panel_real:=1000;
    value_length:=12;
    y:=120; x:=216;
  END;
  bacrescentar:=xv_create(button);
  WITH bacrescentar^ DO BEGIN
    xv_label:='Add curve';
    y:=135; x:=66;
    notify_handler:=CriarCircuito;
    owner^.mouse_obj:=bacrescentar;
  END;
  biniciarRLC:=xv_create(button);
  WITH biniciarRLC^ DO BEGIN
    xv_label:='Restart';
    y:=135;
    notify_handler:=Reiniciar;
  END;
  btunec12:=xv_create(message);
  WITH btunec12^ DO BEGIN
    xv_label:='Tune';
    y:=135; x:=150;
  END;
  btunec12:=xv_create(button);
  WITH btunec12^ DO BEGIN
    xv_label:='C1';
    y:=135; x:=190;
    notify_handler:=TuneC12;
  END;
  btunec22:=xv_create(button);
  WITH btunec22^ DO BEGIN
    xv_label:='C2';
    y:=135; x:=215;
    notify_handler:=TuneC22;
  END;
  btunel12:=xv_create(button);
  WITH btunel12^ DO BEGIN
    xv_label:='L1';
    y:=135; x:=240;
    notify_handler:=TuneL12;
  END;
  btunec22:=xv_create(button);
  WITH btunec22^ DO BEGIN
    xv_label:='L2';
    y:=135; x:=265;
    notify_handler:=TuneL22;
  END;

  fdimensoes:=xv_create(frame);
  WITH fdimensoes^ DO BEGIN
    xv_label:='Coil parameters (SI units)';
    dx:=318;
    dy:=294;
    menu_name:=MenuPrincipal;
  END;
  normal_length:=21;
  tcomprimento1:=xv_create(textfield);
  WITH tcomprimento1^ DO BEGIN
    xv_label:='Primary length  ';
    field_type:=real_field;
    panel_real:=comp10;
  END;
  traio1:=xv_create(textfield);
  WITH traio1^ DO BEGIN
    xv_label:='Primary radius  ';
    y:=15;
    field_type:=real_field;
    panel_real:=raio10;
  END;
  tn1:=xv_create(textfield);
  WITH tn1^ DO BEGIN
    xv_label:='Primary turns   ';
    y:=30;
    field_type:=real_field;
    panel_real:=n10;
  END;
  tcomprimento2:=xv_create(textfield);
  WITH tcomprimento2^ DO BEGIN
    xv_label:='Secondary length';
    y:=45;
    field_type:=real_field;
    panel_real:=comp20;
  END;
  traio2:=xv_create(textfield);
  WITH traio2^ DO BEGIN
    xv_label:='Secondary radius';
    y:=60;
    field_type:=real_field;
    panel_real:=raio20;
  END;
  tn2:=xv_create(textfield);
  WITH tn2^ DO BEGIN
    xv_label:='Secondary turns ';
    y:=75;
    field_type:=real_field;
    panel_real:=n20;
  END;
  td1:=xv_create(textfield);
  WITH td1^ DO BEGIN
    xv_label:='Pri wire diam';
    y:=90;
    field_type:=real_field;
    panel_real:=0.002;
    value_length:=14;
    notify_handler:=AWGI;
  END;
  tawg1:=xv_create(textfield);
  WITH tawg1^ DO BEGIN
    xv_label:='AWG';
    y:=90; x:=224;
    field_type:=int_field;
    panel_int:=12;
    value_length:=6;
    notify_handler:=AWG;
  END;
  td2:=xv_create(textfield);
  WITH td2^ DO BEGIN
    xv_label:='Sec wire diam';
    y:=105;
    field_type:=real_field;
    panel_real:=0.0002;
    value_length:=14;
    notify_handler:=AWGI;
  END;
  tawg2:=xv_create(textfield);
  WITH tawg2^ DO BEGIN
    xv_label:='AWG';
    y:=105; x:=224;
    field_type:=int_field;
    panel_int:=32;
    value_length:=6;
    notify_handler:=AWG;
  END;
  trq:=xv_create(textfield);
  WITH trq^ DO BEGIN
    xv_label:='Wire resistivity';
    y:=120;
    field_type:=real_field;
    panel_real:=1.7241e-8;
  END;
  tden:=xv_create(textfield);
  WITH tden^ DO BEGIN
    xv_label:='Wire density    ';
    y:=135;
    field_type:=real_field;
    panel_real:=8.96e3;
  END;
  tpitch1:=xv_create(textfield);
  WITH tpitch1^ DO BEGIN
    xv_label:='Pri pitch';
    y:=150;
    field_type:=real_field;
    panel_real:=2;
    value_length:=8;
  END;
  tpitch2:=xv_create(textfield);
  WITH tpitch2^ DO BEGIN
    xv_label:='Sec pitch';
    y:=150; x:=160;
    field_type:=real_field;
    panel_real:=1.091;
    value_length:=8;
  END;
  stipo1:=xv_create(setting);
  WITH stipo1^ DO BEGIN
    xv_label:='Primary type';
    y:=165;
    item_create('solenoid');
    item_create('flat spiral');
    exclusive:=TRUE;
    sel_setting:=1;
    notify_handler:=Notifystipo1;
  END;
  tcap1:=xv_create(textfield);
  WITH tcap1^ DO BEGIN
    xv_label:='Primary cap.';
    y:=180;
    value_length:=22;
    panel_real:=10e-9;
    field_type:=real_field;
  END;
  tcap2:=xv_create(textfield);
  WITH tcap2^ DO BEGIN
    xv_label:='Topload cap.';
    y:=195;
    value_length:=22;
    panel_real:=5e-12;
    field_type:=real_field;
  END;
  baplicardimensoes:=xv_create(button);
  WITH baplicardimensoes^ DO BEGIN
    xv_label:='Update circuit parameters';
    y:=225;
    notify_handler:=AplicarDimensoes;
  END;
  btunec1:=xv_create(message);
  WITH btunec1^ DO BEGIN
    xv_label:='Tune';
    y:=210;
  END;
  btunec1:=xv_create(button);
  WITH btunec1^ DO BEGIN
    xv_label:='C1';
    y:=210; x:=40;
    notify_handler:=TuneC1;
  END;
  btunec2:=xv_create(button);
  WITH btunec2^ DO BEGIN
    xv_label:='C2';
    y:=210; x:=66;
    notify_handler:=TuneC2;
  END;
  btunel1:=xv_create(button);
  WITH btunel1^ DO BEGIN
    xv_label:='L1';
    y:=210; x:=92;
    notify_handler:=TuneL1;
  END;
  btunec2:=xv_create(button);
  WITH btunec2^ DO BEGIN
    xv_label:='L2';
    y:=210; x:=118;
    notify_handler:=TuneL2;
  END;
  bcorrect:=xv_create(button);
  WITH bcorrect^DO BEGIN
    xv_label:='Adjust pitches';
    y:=210; x:=190;
    notify_handler:=Adjust;
  END;
  tb1:=xv_create(textfield);
  WITH tb1^ DO BEGIN
    xv_label:='Primary base height  ';
    y:=240;
    value_length:=10;
    panel_real:=0;
    field_type:=real_field;
  END;
  tb2:=xv_create(textfield);
  WITH tb2^ DO BEGIN
    xv_label:='Secondary base height';
    y:=255;
    value_length:=10;
    panel_real:=0;
    field_type:=real_field;
  END;
  bk12:=xv_create(button);
  WITH bk12^DO BEGIN
    xv_label:='Compute k12';
    y:=225; x:=214;
    notify_handler:=K12;
  END;

  open_window(fprincipal);
  open_window(fdimensoes);
  Informacoes;
  xv_main_loop(fRLC);
  RestoreCrtMode
END.
