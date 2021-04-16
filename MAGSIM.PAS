PROGRAM MagSim;
{
Transporte do programa detmag para a interface grafica, em desenvolvimento
Versao 0.0 de 12/7/1998 Incompleto
Versao 0.1 de 27/7/1999 Projeto ideal funcionando corretamente
Vesrao 1.0 de 08/02/2000 Funcionando com L3 dado e mostrando formulas
Versao 1.0a de 06/05/2000 Mensagem dizendo como k, l, m devem ser
Versao 1.0b de 11/08/2000 Unidades no calculo otimo
Versao 1.1  de 04/02/2001 Gera tabela
Vers∆o 1.2 de 26/04/2003 Corrigida amplitude (/f), Le e salva valores
}

USES Dos,Graph,XView;

CONST
  version='1.2 26/04/2003';
  l1m0=0.1e-3;
  c1m0=10e-9;
  l2m0=1e-3;
  c2m0=100e-12;
  l3m0=21.7e-3;
  c3m0=44e-12;
  km0=0.5;
  r1m0=0.1;
  r2m0=10;
  r3m0=100;
  Vc1m0=10000;

TYPE
  polinomio=ARRAY[0..6] OF REAL;

VAR
  det,i1,i2,i3,v1,v2,v3:polinomio;
  i:INTEGER;
  arquivo:TEXT;
  l1,r1,c1,l2,r2,c2,l3,r3,c3,m,k,Vc1,f,t,ti:REAL;
  n1,d1,n2,d2,n3,d3,n4,d4,n5,d5,n6,d6,n7,d7:REAL;

{Declaration of the interface objects}
VAR
  MenuPrincipal,MenuTty:Xv_opaque;
  fmag,tr1m,tl1m,tc1m,tr2m,
  tl2m,tc2m,tklm,tvc1m,tfile,
  tfatorm,bacrescentar,tr3m,tl3m,
  tc3m:Xv_opaque;
  fmensagens,tmsg:Xv_opaque;
  fdesign,tk,tl,tm,tw0,stesla,
  tc10,tc30,tl30,bdesign,suse,sloss,smodo,shz:Xv_opaque;
  ftable,tmaxk,tmaxlk,tmaxml,tfileklm,btable:Xv_opaque;

PROCEDURE Msg(txt:STRING);
BEGIN
  ttysw_output(tmsg,txt+^M^J)
END;

PROCEDURE Escreve(VAR p:polinomio; n:INTEGER; nome:STRING; title:STRING; fator:real);
BEGIN
  Assign(arquivo,nome);
  ReWrite(arquivo);
  WriteLn(arquivo,n);
  FOR i:=0 TO n DO
    WriteLn(arquivo,p[i]);
  WriteLn(arquivo,fator); {Resposta a impulso exige numeradores / f}
  WriteLn(arquivo,f);
  Close(arquivo);
  Msg('File '+nome+' saved ('+title+').')
END;

PROCEDURE Formulas(k,l,m:REAL);
BEGIN
  n1:=2*m*m*k*k+(m*m-l*l)*(l*l-k*k);
  d1:=2*k*k*l*l*m*m;
  n2:=l*l;
  d2:=k*k*m*m;
  n3:=1;
  d3:=l*l;
  n4:=(k*k-l*l)*(l*l-m*m);
  d4:=k*k*(l*l+m*m)-l*l*(l*l-m*m);
  n5:=(l*l-m*m)*(k*k-l*l);
  d5:=2*k*k*m*m;
  {Tesla}
  n6:=k*k+l*l;
  d6:=2*k*k*l*l;
  n7:=l*l-k*k;
  d7:=n6;
END;

{$F+}

{Callback procedures}

PROCEDURE CriarMagnifier(obj:Xv_opaque);
VAR
  nome:STRING;
BEGIN
  nome:=tfile^.panel_value+'.mag';
  Assign(arquivo,nome);
  Rewrite(arquivo);
  WriteLn(arquivo,tr1m^.panel_real);
  WriteLn(arquivo,tl1m^.panel_real);
  WriteLn(arquivo,tc1m^.panel_real);
  WriteLn(arquivo,tr2m^.panel_real);
  WriteLn(arquivo,tl2m^.panel_real);
  WriteLn(arquivo,tc2m^.panel_real);
  WriteLn(arquivo,tr3m^.panel_real);
  WriteLn(arquivo,tl3m^.panel_real);
  WriteLn(arquivo,tc3m^.panel_real);
  WriteLn(arquivo,tklm^.panel_real);
  Close(arquivo);
  Msg('Values saved in file '+nome);

  c1:=tc1m^.panel_real;
  c2:=tc2m^.panel_real;
  c3:=tc3m^.panel_real;
  l1:=tl1m^.panel_real;
  l2:=tl2m^.panel_real;
  l3:=tl3m^.panel_real;
  r1:=tr1m^.panel_real;
  r2:=tr2m^.panel_real;
  r3:=tr3m^.panel_real;
  k:=tklm^.panel_real;
  Vc1:=tvc1m^.panel_real;
  nome:=tfile^.panel_value+'.val';
  Assign(arquivo,nome);
  ReWrite(arquivo);
  WriteLn(arquivo,'c1 ',c1);
  WriteLn(arquivo,'c2 ',c2);
  WriteLn(arquivo,'c3 ',c3);
  WriteLn(arquivo,'l1 ',l1);
  WriteLn(arquivo,'l2 ',l2);
  WriteLn(arquivo,'l3 ',l3);
  WriteLn(arquivo,'r1 ',r1);
  WriteLn(arquivo,'r2 ',r2);
  WriteLn(arquivo,'r3 ',r3);
  WriteLn(arquivo,'m',k*sqrt(l1*l2));
  WriteLn(arquivo,'vc1 ',vc1);
  WriteLn(arquivo,'k12 ',k);
  Close(arquivo);
  Msg('Value list saved in file '+tfile^.panel_value+'.val');

  f:=exp(Ln(1/(l1*l2*l3*(1-Sqr(k))*c1*c2*c3))/6);
  c1:=c1*f;
  c2:=c2*f;
  c3:=c3*f;
  l1:=l1*f;
  l2:=l2*f;
  l3:=l3*f;

  t:=l1*l2*l3*(1-Sqr(k));
  m:=k*sqrt(l1*l2);
  ti:=tfatorm^.panel_real/t;

  det[6]:=1;
  det[5]:=(l1*(l2*r3+l3*r2)+l2*l3*r1-r3*Sqr(m))/t;
  det[4]:=(c1*(c2*(c3*(l1*r2*r3+r1*(l2*r3+l3*r2))+l1*l2-Sqr(m))+c3*(l1*(l2+l3)-Sqr(m)))+c2*c3*l2*l3)/(c1*c2*c3)/t;
  det[3]:=(c1*(c2*(c3*r1*r2*r3+l1*r2+l2*r1)+c3*(l1*(r2+r3)+r1*(l2+l3)))+c2*c3*(l2*r3+l3*r2))/(c1*c2*c3)/t;
  det[2]:=(c1*(c2*r1*r2+c3*r1*(r2+r3)+l1)+c2*(c3*r2*r3+l2)+c3*(l2+l3))/(c1*c2*c3)/t;
  det[1]:=(c1*r1+c2*r2+c3*(r2+r3))/(c1*c2*c3)/t;
  det[0]:=1/(c1*c2*c3)/t;

  i1[4]:=l2*l3*vc1*ti;
  i1[3]:=vc1*(l2*r3+l3*r2)*ti;
  i1[2]:=vc1*(c2*(c3*r2*r3+l2)+c3*(l2+l3))/(c2*c3)*ti;
  i1[1]:=vc1*(c2*r2+c3*(r2+r3))/(c2*c3)*ti;
  i1[0]:=vc1/(c2*c3)*ti;

  i2[4]:=l3*vc1*m*ti;
  i2[3]:=r3*vc1*m*ti;
  i2[2]:=vc1*m*(c2+c3)/(c2*c3)*ti;
  i2[1]:=0;
  i2[0]:=0;

  i3[2]:=vc1*m/c2*ti;
  i3[1]:=0;
  i3[0]:=0;

  v1[5]:=l3*vc1*(l1*l2-SQR(m))/t;
  v1[4]:=vc1*(l1*(l2*r3+l3*r2)+l2*l3*r1-r3*SQR(m))/t;
  v1[3]:=vc1*(c2*(c3*(l1*r2*r3+r1*(l2*r3+l3*r2))+l1*l2-SQR(m))+c3*(l1*(l2+l3)-SQR(m)))/(c2*c3)/t;
  v1[2]:=vc1*(c2*(c3*r1*r2*r3+l1*r2+l2*r1)+c3*(l1*(r2+r3)+r1*(l2+l3)))/(c2*c3)/t;
  v1[1]:=vc1*(c2*r1*r2+c3*r1*(r2+r3)+l1)/(c2*c3)/t;
  v1[0]:=r1*vc1/(c2*c3)/t;

  v2[3]:=l3*vc1*m/c2/t;
  v2[2]:=r3*vc1*m/c2/t;
  v2[1]:=vc1*m/(c2*c3)/t;
  v2[0]:=0;

  v3[1]:=vc1*m/(c2*c3)/t;
  v3[0]:=0;

  Msg('Saving Laplace transforms:');
  Escreve(det,6,tfile^.panel_value+'.d','denominator D(s)',1);
  Escreve(i1,4,tfile^.panel_value+'.n4','numerator of Il1(s)',1/f);
  Escreve(i2,4,tfile^.panel_value+'.n5','numerator of Il2(s)',1/f);
  Escreve(i3,2,tfile^.panel_value+'.n6','numerator of Il3(s)',1/f);
  Escreve(v1,5,tfile^.panel_value+'.n1','numerator of Vc1(s)',1/f);
  Escreve(v2,3,tfile^.panel_value+'.n2','numerator of Vc2(s)',1/f);
  Escreve(v3,1,tfile^.panel_value+'.n3','numerator of Vc3(s)',1/f);
END;

PROCEDURE ProcessMenu(obj:Xv_opaque);
VAR
  nome:STRING;
BEGIN
  {Notify handler for MenuPrincipal}
  CASE obj^.sel_menu OF
  1:open_window(fmag);
  2:open_window(fdesign);
  3:BEGIN
      nome:=tfile^.panel_value+'.mag';
      Assign(arquivo,nome);
      {$I-}
      Reset(arquivo);
      {$I+}
      IF IOResult<>0 THEN Msg('File '+nome+' not found')
      ELSE BEGIN
        ReadLn(arquivo,tr1m^.panel_real);
        ReadLn(arquivo,tl1m^.panel_real);
        ReadLn(arquivo,tc1m^.panel_real);
        ReadLn(arquivo,tr2m^.panel_real);
        ReadLn(arquivo,tl2m^.panel_real);
        ReadLn(arquivo,tc2m^.panel_real);
        ReadLn(arquivo,tr3m^.panel_real);
        ReadLn(arquivo,tl3m^.panel_real);
        ReadLn(arquivo,tc3m^.panel_real);
        ReadLn(arquivo,tklm^.panel_real);
        Close_window(fmag);
        Open_window(fmag);
        Close(arquivo);
        Msg('Values read from file '+nome);
      END
    END;
  4:open_window(ftable);
  5:xv_end:=TRUE;
  END
END;

PROCEDURE Ndesign(obj:Xv_opaque);
VAR
  k,l,m:REAL;
  l1,l2,l3,c2,w0,k12:REAL;

BEGIN
  {Notify handler for bdesign}
  k:=tk^.panel_int;
  l:=tl^.panel_int;
  m:=tm^.panel_int;
  IF (l<=k) OR (m<=l) OR (not Odd(Round(l-k))) OR (not Odd(Round(m-l))) THEN BEGIN
    Msg('The combination k,l,m is invalid.'^M^J'They must be different integers,'^M^J'in ascending order,'
        +' with odd differences.');
    Exit
  END;
  Formulas(k,l,m);
  Msg(^j^m'Magnifier: Mode '+Cpct(k)+','+Cpct(l)+','+Cpct(m));
  Msg('Design equations:');
  Msg('w0^2*L1*C1='+Cpct(n1)+'/'+Cpct(d1)+'='+Cpct(n1/d1));
  Msg('w0^2*L2*C2='+Cpct(n2)+'/'+Cpct(d2)+'='+Cpct(n2/d2));
  Msg('w0^2*L3*C3='+Cpct(n3)+'/'+Cpct(d3)+'='+Cpct(n3/d3));
  Msg('K12^2=     '+Cpct(n4)+'/'+Cpct(d4)+'='+Cpct(n4/d4));
  Msg('L2/L3=     '+Cpct(n5)+'/'+Cpct(d5)+'='+Cpct(n5/d5));
  IF smodo^.sel_setting=1 THEN BEGIN
    w0:=tw0^.panel_real;
    IF shz^.sel_setting=2 THEN w0:=w0*2*pi;
    c1:=tc10^.panel_real;
    c3:=tc30^.panel_real;
    l1:=n1/d1/w0/w0/c1;
    l3:=n3/d3/w0/w0/c3;
    l2:=l3*n5/d5;
    c2:=n2/d2/l2/w0/w0;
  END
  ELSE BEGIN
    c3:=tc30^.panel_real;
    l3:=tl30^.panel_real;
    c1:=tc10^.panel_real;
    t:=n3/d3/l3/c3;
    l2:=n5/d5*l3;
    c2:=n2/d2/t/l2;
    l1:=n1/d1/t/c1;
    w0:=sqrt(t);
  END;
  k12:=sqrt(n4/d4);
  Msg('Magnifier: Component values:');
  Msg('L1='+Cpct(l1*1e3)+' mH');
  Msg('L2='+Cpct(l2*1e3)+' mH');
  Msg('L3='+Cpct(l3*1e3)+' mH');
  Msg('C1='+Cpct(c1/1e-12)+' pF');
  Msg('C2='+Cpct(c2/1e-12)+' pF');
  Msg('C3='+Cpct(c3/1e-12)+' pF');
  Msg('k12='+Cpct(k12));
  {
  Msg('sqrt(L2*C3/(L1*C1))='+Cpct(sqrt(L2*C3/(L1*C1))));
  Msg('L1*C1='+Cpct(l1*c1));
  Msg('(L2+L3)*C3='+Cpct((l2+l3)*c3));
  }
  IF stesla^.sel_setting=2 THEN BEGIN
    Msg(^j^m'Tesla coil: Mode '+Cpct(k)+','+Cpct(l));
    Msg('(simplification for large m, C2=0, L23=L2+L3)');
    Msg('Design equations:');
    Msg('w0^2*L1*C1= '+Cpct(n6)+'/'+Cpct(d6)+'='+Cpct(n6/d6));
    Msg('w0^2*L23*C3='+Cpct(n6)+'/'+Cpct(d6)+'='+Cpct(n6/d6));
    Msg('K1,23=      '+Cpct(n7)+'/'+Cpct(d7)+'='+Cpct(n7/d7));
    Msg('Tesla coil: Component values');
    Msg('L1=    '+Cpct(n6/d6/(w0*w0*c1)*1000)+' mH');
    Msg('L23=   '+Cpct(n6/d6/(w0*w0*c3)*1000)+' mH');
    Msg('C1=    '+Cpct(c1/1e-12)+' pF');
    Msg('C3=    '+Cpct(c3/1e-12)+' pF');
    Msg('k1,23= '+Cpct(n7/d7));
  END;
  Msg(^j^m'w0='+Cpct(w0)+' rad/s');
  Msg('f0='+Cpct(w0/2/pi)+' Hz');
  Msg('Transfer time: '+Cpct(pi/w0/1e-6)+' us');
  Msg('Voltage gain: '+Cpct(sqrt(c1/c3)));
  IF suse^.sel_setting=2 THEN BEGIN
    close_window(fmag);
    tl1m^.panel_real:=l1;
    tl2m^.panel_real:=l2;
    tl3m^.panel_real:=l3;
    tc1m^.panel_real:=c1;
    tc2m^.panel_real:=c2;
    tc3m^.panel_real:=c3;
    tklm^.panel_real:=k12;
    IF sloss^.sel_setting=1 THEN BEGIN
      tr1m^.panel_real:=0;
      tr2m^.panel_real:=0;
      tr3m^.panel_real:=0;
    END;
    tfile^.panel_value:='m'+tk^.panel_value+tl^.panel_value+tm^.panel_value;
    open_window(fmag);
  END;
END;

PROCEDURE WriteTable(obj:Xv_opaque);
VAR
  ik,il,im:INTEGER;
  k,l,m:REAL;
  l1,l2,l3,c2,w0,k12:REAL;
  filename:STRING;
BEGIN
  {Notify handler for btable}
  filename:=tfileklm^.panel_value;
  Assign(arquivo,filename);
  ReWrite(arquivo);
  FOR ik:=1 TO tmaxk^.panel_int DO BEGIN
    FOR il:=ik+1 TO ik+tmaxlk^.panel_int DO IF Odd(il-ik) THEN BEGIN
      FOR im:=il+1 TO il+tmaxml^.panel_int DO IF Odd(im-il) THEN BEGIN
        k:=ik;
        l:=il;
        m:=im;
        Formulas(k,l,m);
        IF smodo^.sel_setting=1 THEN BEGIN
          w0:=tw0^.panel_real;
          IF shz^.sel_setting=2 THEN w0:=w0*2*pi;
          c1:=tc10^.panel_real;
          c3:=tc30^.panel_real;
          l1:=n1/d1/w0/w0/c1;
          l3:=n3/d3/w0/w0/c3;
          l2:=l3*n5/d5;
          c2:=n2/d2/l2/w0/w0;
        END
        ELSE BEGIN
          c3:=tc30^.panel_real;
          l3:=tl30^.panel_real;
          c1:=tc10^.panel_real;
          t:=n3/d3/l3/c3;
          l2:=n5/d5*l3;
          c2:=n2/d2/t/l2;
          l1:=n1/d1/t/c1;
          w0:=sqrt(t);
        END;
        k12:=sqrt(n4/d4);
        WriteLn(arquivo,'Mode= ',ik:2,',',il:2,',',im:2,': L1=',l1*1e3:7:5,' mH',
        ' L2=',l2*1e3:7:3,' mH',
        ' L3=',l3*1e3:7:3,' mH',
        ' C1=',c1/1e-12:6:1,' pF',
        ' C2=',c2/1e-12:6:1,' pF',
        ' C3=',c3/1e-12:6:1,' pF',
        ' k12=',k12:5:3,
        ' time: ',pi/w0/1e-6:7:2,' us');
      END
    END
  END;
  WriteLn(arquivo,'Voltage gain: ',sqrt(c1/c3):7:2);
  Close(arquivo);
  Msg('Table written in the file '+filename);
END;

PROCEDURE TratarMenuTty(obj:Xv_opaque);
VAR
  dia,mes,ano,dsem,hora,minuto,segundo,seg100:WORD;
BEGIN
 CASE obj^.sel_menu OF
  1:BEGIN
      WITH tmsg^ DO BEGIN bstart:=0; tstart:=0; tend:=0; END;
      ttysw_output(tmsg,'');
      GetDate(ano,mes,dia,dsem);
      GetTime(hora,minuto,segundo,seg100);
      Msg('MagSim - '+Cpct(dia)+'/'+Cpct(mes)+'/'+Cpct(ano)+' '+Cpct(hora)+':'+Cpct(minuto)+':'+Cpct(segundo));
    END;
  2:WITH tmsg^ DO BEGIN
      Assign(arquivo,'mag.msg');
      ReWrite(arquivo);
      i:=bstart;
      WHILE i<>tend DO BEGIN
        Write(arquivo,Pb^[i]);
        IF i<bsize THEN Inc(i) ELSE i:=0
      END;
      Close(arquivo);
      Msg('Messages saved in file mag.msg')
    END;
  END;
END;

{$F-}

BEGIN
  {Inicialization}
  xv_init(0,0);
  normal_bsize:=50000;
  {Interface objects creation}
  MenuPrincipal:=xv_create(menu);
  WITH MenuPrincipal^ DO BEGIN
    xv_label:='Options';
    Item_create('Parameters');
    Item_create('Design');
    Item_create('Load saved values');
    Item_create('Write table');
    Item_create('Exit');
    notify_handler:=ProcessMenu;
  END;
  MenuTty:=xv_create(menu);
  WITH MenuTty^ DO BEGIN
    xv_label:='Messages:';
    item_create('clear messages');
    item_create('save messages');
    notify_handler:=TratarMenuTty;
  END;
  fmag:=xv_create(frame);
  WITH fmag^ DO BEGIN
    xv_label:='Tesla magnifier circuit parameters';
    dx:=350;
    dy:=239;
    menu_name:=MenuPrincipal;
  END;
  tr1m:=xv_create(textfield);
  WITH tr1m^ DO BEGIN
    xv_label:='Primary resistance   ';
    value_length:=20;
    field_type:=real_field;
    panel_real:=r1m0;
  END;
  tl1m:=xv_create(textfield);
  WITH tl1m^ DO BEGIN
    xv_label:='Primary inductance   ';
    y:=15;
    value_length:=20;
    field_type:=real_field;
    panel_real:=l1m0;
  END;
  tc1m:=xv_create(textfield);
  WITH tc1m^ DO BEGIN
    xv_label:='Primary capacitance  ';
    y:=30;
    value_length:=20;
    field_type:=real_field;
    panel_real:=c1m0;
  END;
  tr2m:=xv_create(textfield);
  WITH tr2m^ DO BEGIN
    xv_label:='Secondary resistance ';
    y:=45;
    value_length:=20;
    field_type:=real_field;
    panel_real:=r2m0;
  END;
  tl2m:=xv_create(textfield);
  WITH tl2m^ DO BEGIN
    xv_label:='Secondary inductance ';
    y:=60;
    value_length:=20;
    field_type:=real_field;
    panel_real:=l2m0;
  END;
  tc2m:=xv_create(textfield);
  WITH tc2m^ DO BEGIN
    xv_label:='Secondary capacitance';
    y:=75;
    value_length:=20;
    field_type:=real_field;
    panel_real:=c2m0;
  END;
  tr3m:=xv_create(textfield);
  WITH tr3m^ DO BEGIN
    xv_label:='Resonator resistance ';
    y:=90;
    value_length:=20;
    field_type:=real_field;
    panel_real:=r3m0;
  END;
  tl3m:=xv_create(textfield);
  WITH tl3m^ DO BEGIN
    xv_label:='Resonator inductance ';
    y:=105;
    value_length:=20;
    field_type:=real_field;
    panel_real:=l3m0;
  END;
  tc3m:=xv_create(textfield);
  WITH tc3m^ DO BEGIN
    xv_label:='Resonator capacitance';
    y:=120;
    value_length:=20;
    field_type:=real_field;
    panel_real:=c3m0;
  END;
  tklm:=xv_create(textfield);
  WITH tklm^ DO BEGIN
    xv_label:='Coupling coefficient ';
    y:=135;
    value_length:=20;
    field_type:=real_field;
    panel_real:=km0;
  END;
  tvc1m:=xv_create(textfield);
  WITH tvc1m^ DO BEGIN
    xv_label:='Primary peak voltage ';
    y:=150;
    value_length:=20;
    field_type:=real_field;
    panel_real:=vc1m0;
  END;
  tfile:=xv_create(textfield);
  WITH tfile^ DO BEGIN
    xv_label:='Title for files';
    y:=165;
    panel_value:='mag';
  END;
  tfatorm:=xv_create(textfield);
  WITH tfatorm^ DO BEGIN
    xv_label:='Scaling factor for currents';
    y:=180;
    value_length:=12;
    field_type:=real_field;
    panel_real:=1000;
  END;
  bacrescentar:=xv_create(button);
  WITH bacrescentar^ DO BEGIN
    xv_label:='Save';
    y:=195;
    notify_handler:=CriarMagnifier;
  END;
  fmensagens:=xv_create(frame);
  WITH fmensagens^ DO BEGIN
    xv_label:='Messages';
    dx:=288;
    dy:=480;
    x:=351;
    menu_name:=MenuPrincipal;
  END;
  tmsg:=xv_create(tty);
  tmsg^.menu_name:=MenuTty;
  fdesign:=xv_create(frame);
  WITH fdesign^ DO BEGIN
    xv_label:='Optimum design';
    x:=30; y:=290;
    dx:=288;
    dy:=160;
    menu_name:=MenuPrincipal;
  END;
  tk:=xv_create(textfield);
  WITH tk^ DO BEGIN
    xv_label:='k';
    field_type:=int_field;
    panel_int:=1;
    min_value:=1;
  END;
  tl:=xv_create(textfield);
  WITH tl^ DO BEGIN
    xv_label:='l';
    y:=15;
    field_type:=int_field;
    panel_int:=2;
    min_value:=1;
  END;
  tm:=xv_create(textfield);
  WITH tm^ DO BEGIN
    xv_label:='m';
    y:=31;
    field_type:=int_field;
    panel_int:=3;
    min_value:=1;
  END;
  tw0:=xv_create(textfield);
  WITH tw0^ DO BEGIN
    xv_label:='w0';
    y:=45;
    value_length:=20;
    field_type:=real_field;
    panel_real:=200000;
  END;
  tc10:=xv_create(textfield);
  WITH tc10^ DO BEGIN
    xv_label:='C1';
    y:=60;
    value_length:=20;
    field_type:=real_field;
    panel_real:=c1m0;
  END;
  tc30:=xv_create(textfield);
  WITH tc30^ DO BEGIN
    xv_label:='C3';
    y:=75;
    value_length:=20;
    field_type:=real_field;
    panel_real:=c3m0;
  END;
  tl30:=xv_create(textfield);
  WITH tl30^ DO BEGIN
    xv_label:='L3';
    y:=90;
    value_length:=20;
    field_type:=real_field;
    panel_real:=l3m0;
  END;
  bdesign:=xv_create(button);
  WITH bdesign^ DO BEGIN
    xv_label:='Design';
    y:=120;
    notify_handler:=Ndesign;
  END;
  stesla:=xv_create(setting);
  WITH stesla^ DO BEGIN
    xv_label:='Show Tesla coil case';
    item_create('n');
    item_create('y');
    y:=120;
    x:=80;
    exclusive:=TRUE;
    sel_setting:=2
  END;
  shz:=xv_create(setting);
  WITH shz^ DO BEGIN
    item_create('rad/s');
    item_create('Hz');
    exclusive:=TRUE;
    sel_setting:=2;
    y:=45; x:=190;
  END;
  suse:=xv_create(setting);
  WITH suse^ DO BEGIN
    xv_label:='Update';
    item_create('n');
    item_create('y');
    exclusive:=TRUE;
    sel_setting:=1;
    y:=105;
  END;
  sloss:=xv_create(setting);
  WITH sloss^ DO BEGIN
    xv_label:='Lossy';
    item_create('n');
    item_create('y');
    exclusive:=TRUE;
    sel_setting:=1;
    x:=82;
    y:=105;
  END;
  smodo:=xv_create(setting);
  WITH smodo^ DO BEGIN
    xv_label:='Use';
    item_create('w0');
    item_create('L3');
    exclusive:=TRUE;
    sel_setting:=2;
    x:=156;
    y:=105;
  END;
  ftable:=xv_create(frame);
  WITH ftable^ DO BEGIN
    xv_label:='Table';
    x:=59;
    y:=114;
    dx:=236;
    dy:=103;
    menu_name:=MenuPrincipal
  END;
  tmaxk:=xv_create(textfield);
  WITH tmaxk^ DO BEGIN
    xv_label:='Maximum k  ';
    field_type:=int_field;
    panel_int:=15;
    min_value:=1;
  END;
  tmaxlk:=xv_create(textfield);
  WITH tmaxlk^ DO BEGIN
    xv_label:='Maximum l-k';
    y:=15;
    field_type:=int_field;
    panel_int:=3;
    min_value:=1;
  END;
  tmaxml:=xv_create(textfield);
  WITH tmaxml^ DO BEGIN
    xv_label:='Maximum m-l';
    y:=30;
    field_type:=int_field;
    panel_int:=9;
    min_value:=1;
  END;
  tfileklm:=xv_create(textfield);
  WITH tfileklm^ DO BEGIN
    xv_label:='File name  ';
    y:=45;
    panel_value:='mag.klm';
  END;
  btable:=xv_create(button);
  WITH btable^ DO BEGIN
    xv_label:='Write table';
    y:=60;
    notify_handler:=WriteTable;
  END;

  Msg('Tesla magnifier designer');
  Msg('By Antonio Carlos M. de Queiroz');
  Msg('acmq@coe.ufrj.br');
  Msg('Version '+version);
  Msg(^J^M'This program computes the Laplace transforms');
  Msg('of the voltages and currents in a magnifier');
  Msg('circuit, and can also compute optimum values');
  Msg('for the lossless case, from the three');
  Msg('oscillatory modes of the circuit, at');
  Msg('frequencies k*w0, l*w0, and m*w0 rad/s.');
  Msg('k is a positive integer, l=k+1, k+3,');
  Msg('k+5, ... and m=l+1, l+3, l+5, ...');
  Msg('The resulting waveforms can be plotted in the');
  Msg('Teslasim program, from the Laplace');
  Msg('transforms computed by this program.');
  Msg('(Use impulse inputs in Teslasim.)');
  Msg('It can also plot a table with component');
  Msg('values for a specified range of modes.');
  open_window(fdesign);
  xv_main_loop(fmag);
  {Exit}
  RestoreCrtMode;
END.
