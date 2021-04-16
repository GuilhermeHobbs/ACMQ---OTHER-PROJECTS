/*
Programa de demonstracao de analisa nodal modificada no tempo
Por Antonio Carlos M. de Queiroz acmq@ufrj.br
Modificado a partir do programa MNA1
Utiliza o metodo "backward" de Euler.
As fontes sao provisorias
Versao 0.0 de 13/5/2003 C, L e K funcionando.
Versao 0.1 de 15/5/2003 Diodos funcionando.
Versao 0.2 de 17/5/2003 Transistor NMOS funcionando.
Versao 1.0 de 18/5/2003 Transistor PMOS funcionando.
Versao 1.0a de 19/5/2003 Entrada por linha de comando e alguns testes.
Versao 1.0b de 03/7/2003 Corrigidos "elses" faltando.
*/

/*
Elementos aceitos e linhas do netlist:

Resistor:  R<nome> <no+> <no-> <resistencia>
VCCS:      G<nome> <io+> <io-> <vi+> <vi-> <transcondutancia>
VCVC:      E<nome> <vo+> <vo-> <vi+> <vi-> <ganho de tensao>
CCCS:      F<nome> <io+> <io-> <ii+> <ii-> <ganho de corrente>
CCVS:      H<nome> <vo+> <vo-> <ii+> <ii-> <transresistencia>
Fonte I:   I<nome> <io+> <io-> <tipo de fonte>
Fonte V:   V<nome> <vo+> <vo-> <tipo de fonte>
Amp. op.:  O<nome> <vo1> <vo2> <vi1> <vi2>
Capacitor: C<nome> <no+> <no-> <capacitancia> [IC=<tensao inicial>]
Indutor:   L<nome> <no+> <no-> <indutancia> [IC=<corrente inicial>]
Ind. mutua:K<nome> <L1> <L2> <coeficiente de acoplamento>
Diodo:     D<nome> <no+> <no->
Trans. MOS:M<nome> <nod> <nog> <nos> <nob> <tipo> L=<comprimento> W=<largura>

As fontes F e H tem o ramo de entrada em curto.
O amplificador operacional ideal tem a saida suspensa.
Os nos podem ser nomes.
O acoplamento entre indutores (K) deve ser entre indutores ja declarados.
O diodo conduz 1 mA com v=0.6 V.
Tipos de fonte:
DC <valor>
SIN (<nivel continuo> <amplitude> <frequencia>)
PULSE (<amplitude inicial> <amplitude final> <atraso>)
Tipo de transistor MOS: NMOS ou PMOS
Os transistores MOS tem |Vt|=1 V, K=0.0001(W/L), e Lambda=0.05
*/
/*
#define TESTE
*/
#define versao "1.0b - 3/7/2003"
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#define MAX_NOME 11
#define MAX_ELEM 50
#define MAX_NOS 50
#define TOLG 1e-9
#define TOLE 1e-7
#define MAX_IT 100
#define VT 25e-3
#define IS 3.7751345e-14
#define K0 0.0001
#define LAMBDA 0.05
#define VT0 1

typedef struct elemento { /* Elemento do netlist */
  char nome[MAX_NOME];
  double p1,p2,p3; /* Tres parametros */
  int a,b,c,d,x,y; /* Nos e correntes */
} elemento;

elemento netlist[MAX_ELEM]; /* Netlist */
int
  ne, /* Elementos */
  nv, /* Variaveis */
  nn, /* Nos */
  nao_linear, /* 1 se ha elementos nao lineares */
  iteracoes, /* Contador de iteracoes */
  max_iter,  /* Maximo numero de iteracoes que ocorreu */
  i,j,k,
  naoleu;
long int
  n,
  npontos, /* Passos no grafico */
  npassos, /* Passos entre pontos mostrados */
  ntotal;  /* Total de passos */
char
  nomearquivo[81],
  tipo,
  na[MAX_NOME],nb[MAX_NOME],nc[MAX_NOME],nd[MAX_NOME],
  lista[MAX_NOS+1][MAX_NOME+2], /* Tem que caber jx antes do nome */
  txt[81];
FILE *arquivo;
double
  g,ex,v,gds,source,drain,gate,vgs,vds,Km,id,
  Yn[MAX_NOS][MAX_NOS], /* Sistema nodal */
  et[MAX_NOS], /* e(t), ultima solucao */
  en[MAX_NOS], /* en(t+dt), aproximacao atual */
  tempo,t,dt,dt1,
  erro,erro_max,
  t_max_iter,
  ci;

/* Resolucao de sistema de equacoes pelo metodo de Gauss-Jordan
   com condensacao pivotal */
void resolversistema(void)
{
  int i,j,l, a;
  double t, p;

  for (i=1; i<=nv; i++) {
    t=0.0;
    a=i;
    for (l=i; l<=nv; l++) {
      if (fabs(Yn[l][i])>fabs(t)) {
	a=l;
	t=Yn[l][i];
      }
    }
    if (i!=a) {
      for (l=1; l<=nv+1; l++) {
	p=Yn[i][l];
	Yn[i][l]=Yn[a][l];
	Yn[a][l]=p;
      }
    }
    if (fabs(t)<TOLG) {
      printf("Sistema singular\n");
      exit(1);
    }
    for (j=nv+1; j>i; j--) {  /* Basta j>i */
      Yn[i][j] /= t;
      p=Yn[i][j];
      for (l=1; l<=nv; l++) {
	if (l!=i)
	  Yn[l][j]-=Yn[l][i]*p;
      }
    }
  }
}

/* Rotina que atribui numeros as tensoes e correntes do sistema */
int numero(char *nome)
{
  int i,achou;

  i=0; achou=0;
  while (!achou && i<=nv)
    if (!(achou=!strcmp(nome,lista[i]))) i++;
  if (!achou) {
    if (nv==MAX_NOS) {
      printf("O programa so aceita ate %d nos\n",nv);
      exit(1);
    }
    nv++;
    strcpy(lista[nv],nome);
    return nv; /* novo no*/
  }
  else {
    return i; /* no ja conhecido */
  }
}

int main(int argc,char* argv[])
{
  clrscr();
  printf("Programa demonstrativo de analise nodal modificada no tempo\n");
  printf("Por Antonio Carlos M. de Queiroz - acmq@coe.ufrj.br\n");
  printf("Versao %s\n",versao);
  naoleu=1;
 denovo:
  ne=0; nv=0; strcpy(lista[0],"0");
  if (argc>1 && naoleu) strcpy(nomearquivo,argv[1]);
  else {
    printf("A linha de comando poderia incluir os parametros:\n<arquivo com o netlist> <tempo> <intervalos na tabela> <passos internos>\n");
    printf("Nome do arquivo com o netlist (ex: mna2[.net]): ");
    scanf("%50s",nomearquivo);
  }
  if (strchr(nomearquivo,'.')==NULL) strcat(nomearquivo,".net");
  arquivo=fopen(nomearquivo,"r");
  if (arquivo==0) {
    printf("Arquivo %s inexistente\n",nomearquivo);
    naoleu=0;
    goto denovo;
  }
  printf("Lendo netlist %s:\n",nomearquivo);
  nao_linear=0;
  fscanf(arquivo,"%*[^\n]"); /* Ignora a primeira linha */
  while (fscanf(arquivo,"%10s",txt)!=EOF) {
    ne++; /* Nao usa o netlist[0] */
    if (ne>MAX_ELEM) {
      printf("O programa so aceita ate %d elementos\n",MAX_ELEM);
      exit(1);
    }
    txt[0]=toupper(txt[0]);
    tipo=txt[0];
    strcpy(netlist[ne].nome,txt);
    if (tipo=='R') {
      fscanf(arquivo,"%10s%10s%lg%*[^\n]",na,nb,&netlist[ne].p1);
      printf("%s %s %s %g\n",netlist[ne].nome,na,nb,netlist[ne].p1);
      netlist[ne].a=numero(na);
      netlist[ne].b=numero(nb);
    }
    else if (tipo=='C' || tipo=='L') {
      netlist[ne].p2=0;
      /* O espaco no %*[IC= ] e necessario */
      fscanf(arquivo,"%10s%10s%lg%*[IC= ]%lg%*[^\n]",na,nb,&netlist[ne].p1,&netlist[ne].p2);
      printf("%s %s %s %g IC=%g\n",netlist[ne].nome,na,nb,netlist[ne].p1,netlist[ne].p2);
      netlist[ne].a=numero(na);
      netlist[ne].b=numero(nb);
    }
    else if (tipo=='I' || tipo=='V') {
      /* Aceita: DC valor, SIN (nivel amplitude frequencia), PULSE (nivel1 nivel2 atraso) */
      netlist[ne].p2=0; netlist[ne].p3=0;
      fscanf(arquivo,"%10s%10s%1s%*[^0-9.+-]%lg%lg%lg%*[^\n]",na,nb,txt,&netlist[ne].p1,&netlist[ne].p2,&netlist[ne].p3);
      printf("%s %s %s %s (%g %g %g)\n",netlist[ne].nome,na,nb,txt,netlist[ne].p1,netlist[ne].p2,netlist[ne].p3);
      netlist[ne].a=numero(na);
      netlist[ne].b=numero(nb);
      txt[0]=toupper(txt[0]);
      if (strchr("DSP",txt[0])==NULL) {
        printf("Fonte invalida\n");
        exit(1);
      }
      netlist[ne].y=txt[0]; /* Aproveita o campo y para o tipo de fonte */
    }
    else if (tipo=='G' || tipo=='E' || tipo=='F' || tipo=='H') {
      fscanf(arquivo,"%10s%10s%10s%10s%lg%*[^\n]",na,nb,nc,nd,&netlist[ne].p1);
      printf("%s %s %s %s %s %g\n",netlist[ne].nome,na,nb,nc,nd,netlist[ne].p1);
      netlist[ne].a=numero(na);
      netlist[ne].b=numero(nb);
      netlist[ne].c=numero(nc);
      netlist[ne].d=numero(nd);
    }
    else if (tipo=='O') {
      fscanf(arquivo,"%10s%10s%10s%10s%*[^\n]",na,nb,nc,nd);
      printf("%s %s %s %s %s\n",netlist[ne].nome,na,nb,nc,nd);
      netlist[ne].a=numero(na);
      netlist[ne].b=numero(nb);
      netlist[ne].c=numero(nc);
      netlist[ne].d=numero(nd);
    }
    else if (tipo=='K') {
      fscanf(arquivo,"%10s%10s%lg%*[^\n]",na,nb,&netlist[ne].p1);
      printf("%s %s %s %g\n",netlist[ne].nome,na,nb,netlist[ne].p1);
      netlist[ne].a=0;
      netlist[ne].b=0;
      for (i=1; i<ne; i++) {
        if (strcmp(na,netlist[i].nome)==0) netlist[ne].a=i;
        if (strcmp(nb,netlist[i].nome)==0) netlist[ne].b=i;
      }
      if (netlist[ne].a==0 || netlist[ne].b==0) {
        printf("Indutor nao encontrado.");
        exit(1);
      }
      /* a e b apontam para as linhas do netlist com os indutores */
      printf("(%s esta na linha %d, %s esta na linha %d)\n",na,netlist[ne].a,nb,netlist[ne].b);
      /* Guarda M em p2 */
      netlist[ne].p2=netlist[ne].p1*
        sqrt(netlist[netlist[ne].a].p1*netlist[netlist[ne].b].p1);
    }
    else if (tipo=='D') {
      fscanf(arquivo,"%10s%10s%*[^\n]",na,nb);
      printf("%s %s %s\n",netlist[ne].nome,na,nb);
      netlist[ne].a=numero(na);
      netlist[ne].b=numero(nb);
      nao_linear=1;
    }
    else if (tipo=='M') {
      fscanf(arquivo,"%10s%10s%10s%10s%10s%*[L= ]%lg%*[W= ]%lg%*[^\n]",na,nb,nc,nd,txt,&netlist[ne].p1,&netlist[ne].p2);
      printf("%s %s %s %s %s %s L=%g W=%g\n",netlist[ne].nome,na,nb,nc,nd,txt,netlist[ne].p1,netlist[ne].p2);
      netlist[ne].a=numero(na);
      netlist[ne].b=numero(nb);
      netlist[ne].c=numero(nc);
      netlist[ne].d=numero(nd);
      txt[0]=toupper(txt[0]);
      if (strchr("NP",txt[0])==NULL) {
        printf("Transistor invalido\n");
        exit(1);
      }
      netlist[ne].y=txt[0]; /* Guarda o tipo em y */
      nao_linear=1;
    }
    else if (tipo=='*') { /* Comentario comeca com "*" */
      fscanf(arquivo,"%*[^\n]");
      ne--;
    }
    else {
      printf("Elemento desconhecido: %s\n",txt);
      getch();
      exit(1);
    }
  }
  fclose(arquivo);
  /* Organiza as variaveis de corrente */
  nn=nv;
  for (i=1; i<=ne; i++) {
    tipo=netlist[i].nome[0];
    if (tipo=='V' || tipo=='E' || tipo=='F' || tipo=='O' || tipo=='L') {
      nv++;
      if (nv>MAX_NOS) {
        printf("As correntes extra excederam o numero de variaveis permitido (%d)\n",MAX_NOS);
        exit(1);
      }
      strcpy(lista[nv],"j"); /* Tem espaco para mais dois caracteres */
      strcat(lista[nv],netlist[i].nome);
      netlist[i].x=nv;
    }
    else if (tipo=='H') {
      nv=nv+2;
      if (nv>MAX_NOS) {
        printf("As correntes extra excederam o numero de variaveis permitido (%d)\n",MAX_NOS);
        exit(1);
      }
      strcpy(lista[nv-1],"jx"); strcat(lista[nv-1],netlist[i].nome);
      netlist[i].x=nv-1;
      strcpy(lista[nv],"jy"); strcat(lista[nv],netlist[i].nome);
      netlist[i].y=nv;
    }
    else if (tipo=='K') {
      netlist[i].x=netlist[netlist[i].a].x;
      netlist[i].y=netlist[netlist[i].b].x;
      /* x e y apontam para as correntes */
    }
  }
  printf("Toque uma tecla...\n");
  getch();
  printf("Variaveis: \n");
  for (i=0; i<=nv; i++)
    printf("%d %s\n",i,lista[i]);
#ifdef TESTE
  getch();
  printf("Netlist interno final\n");
  for (i=1; i<=ne; i++) {
    tipo=netlist[i].nome[0];
    if (tipo=='R') {
      printf("%s %d %d %g",netlist[i].nome,netlist[i].a,netlist[i].b,netlist[i].p1);
    }
    else if (tipo=='L' || tipo=='C' || tipo=='I' || tipo=='V') {
      printf("%s %d %d %g %g",netlist[i].nome,netlist[i].a,netlist[i].b,netlist[i].p1,netlist[i].p2);
    }
    else if (tipo=='G' || tipo=='E' || tipo=='F' || tipo=='H') {
      printf("%s %d %d %d %d %g",netlist[i].nome,netlist[i].a,netlist[i].b,netlist[i].c,netlist[i].d,netlist[i].p1);
    }
    else if (tipo=='O') {
      printf("%s %d %d %d %d",netlist[i].nome,netlist[i].a,netlist[i].b,netlist[i].c,netlist[i].d);
    }
    else if (tipo=='D') {
      printf("%s %d %d",netlist[i].nome,netlist[i].a,netlist[i].b);
    }
    else if (tipo=='M') {
      printf("%s %d %d %d %d %c %g %g",netlist[i].nome,netlist[i].a,netlist[i].b,netlist[i].c,netlist[i].d,netlist[i].y,netlist[i].p1,netlist[i].p2);
    }
    if (tipo=='V' || tipo=='E' || tipo=='F' || tipo=='O' || tipo=='L')
      printf(" jx:%d\n",netlist[i].x);
    else if (tipo=='H')
      printf(" jx:%d, jy:%d\n",netlist[i].x,netlist[i].y);
    else if (tipo=='K')
      printf("%s: jx:%d (%s), jy:%d (%s)\n",netlist[i].nome,netlist[i].x,netlist[netlist[i].a].nome,netlist[i].y,netlist[netlist[i].b].nome);
    else printf("\n");
  }
  getch();
#endif
  printf("O circuito tem %d nos, %d variaveis e %d elementos\n",nn,nv,ne);
  if (argc==5) {
    sscanf(argv[2],"%lg",&tempo);
    sscanf(argv[3],"%ld",&npontos);
    sscanf(argv[4],"%ld",&npassos);
  }
  else {
    printf("Tempo total de analise (ex: 20e-6): ");
    scanf("%lg",&tempo);
    printf("Numero de intervalos no grafico (ex: 600): ");
    scanf("%ld",&npontos);
    printf("Numero de passos por intervalo (ex: 10): ");
    scanf("%ld",&npassos);
  }
  printf("Tempo=%g, intervalos=%ld, passos=%ld\n",tempo,npontos,npassos);
  if (tempo<=0 || npontos<=0 || npassos<=0 || npontos>100000) {
    printf("Parametros invalidos\n");
    exit(1);
  }
  ntotal=npassos*npontos;
  dt1=tempo/ntotal;
  /* Inicializa a solucao "atual" (mas ver a montagem dos transistores) */
  for (i=0; i<=nv; i++) en[i]=0; /* "Chute" inicial, inclusive en[0] */
  t=0; dt=dt1/1000; /* primeira solucao em t=0 com dt menor */
  max_iter=0; /* Para estatistica */
  arquivo=fopen("mna2.tab","w");
  for (n=0;n<=ntotal;n++) {
    iteracoes=0;
    do {
      /* Zera sistema */
      for (i=0; i<=nv+1; i++) {
        for (j=0; j<=nv+1; j++)
          Yn[i][j]=0;
      }
      /* Monta estampas */
      for (i=1; i<=ne; i++) {
        tipo=netlist[i].nome[0];
        if (tipo=='R') {
          g=1/netlist[i].p1;
          Yn[netlist[i].a][netlist[i].a]+=g;
          Yn[netlist[i].b][netlist[i].b]+=g;
          Yn[netlist[i].a][netlist[i].b]-=g;
          Yn[netlist[i].b][netlist[i].a]-=g;
        }
        else if (tipo=='C') {
          g=netlist[i].p1/dt;
          Yn[netlist[i].a][netlist[i].a]+=g;
          Yn[netlist[i].b][netlist[i].b]+=g;
          Yn[netlist[i].a][netlist[i].b]-=g;
          Yn[netlist[i].b][netlist[i].a]-=g;
          if (n==0) ci=netlist[i].p2;
          else ci=et[netlist[i].a]-et[netlist[i].b];
          g*=ci;
          Yn[netlist[i].a][nv+1]+=g;
          Yn[netlist[i].b][nv+1]-=g;
        }
        else if (tipo=='L') {
          g=netlist[i].p1/dt;
          Yn[netlist[i].a][netlist[i].x]=1;
          Yn[netlist[i].b][netlist[i].x]=-1;
          Yn[netlist[i].x][netlist[i].a]=-1;
          Yn[netlist[i].x][netlist[i].b]=1;
          Yn[netlist[i].x][netlist[i].x]=g;
          if (n==0) ci=netlist[i].p2;
          else ci=et[netlist[i].x];
          g*=ci;
          Yn[netlist[i].x][nv+1]=g;
        }
        else if (tipo=='K') {
          /* Monta M/dt em Yn[x,y] e Yn[y,x] */
          g=netlist[i].p2/dt;
          Yn[netlist[i].x][netlist[i].y]+=g;
          Yn[netlist[i].y][netlist[i].x]+=g;
          /* Monta +M/dt jy(t0) na linha x de IS */
          if (n==0) ci=netlist[netlist[i].b].p2;
          else ci=et[netlist[netlist[i].b].x];
          Yn[netlist[i].x][nv+1]+=g*ci;
          /* Monta +M/dt jx(t0) na linha y de IS */
          if (n==0) ci=netlist[netlist[i].a].p2;
          else ci=et[netlist[netlist[i].a].x];
          Yn[netlist[i].y][nv+1]+=g*ci;
        }
        else if (tipo=='G') {
          g=netlist[i].p1;
          Yn[netlist[i].a][netlist[i].c]+=g;
          Yn[netlist[i].b][netlist[i].d]+=g;
          Yn[netlist[i].a][netlist[i].d]-=g;
          Yn[netlist[i].b][netlist[i].c]-=g;
        }
        else if (tipo=='I') {
          switch (netlist[i].y) {
            case 'D': g=netlist[i].p1; break;
            case 'S': g=netlist[i].p1+netlist[i].p2*sin(2*M_PI*netlist[i].p3*t); break;
            case 'P': g=(t<netlist[i].p3)?netlist[i].p1:netlist[i].p2;
          }
          Yn[netlist[i].a][nv+1]-=g;
          Yn[netlist[i].b][nv+1]+=g;
        }
        else if (tipo=='V') {
          Yn[netlist[i].a][netlist[i].x]=1;
          Yn[netlist[i].b][netlist[i].x]=-1;
          Yn[netlist[i].x][netlist[i].a]=1;
          Yn[netlist[i].x][netlist[i].b]=-1;
          switch (netlist[i].y) {
            case 'D': g=netlist[i].p1; break;
            case 'S': g=netlist[i].p1+netlist[i].p2*sin(2*M_PI*netlist[i].p3*t); break;
            case 'P': g=(t<netlist[i].p3)?netlist[i].p1:netlist[i].p2;
          }
          Yn[netlist[i].x][nv+1]=g;
        }
        else if (tipo=='E') {
          g=netlist[i].p1;
          Yn[netlist[i].a][netlist[i].x]=1;
          Yn[netlist[i].b][netlist[i].x]=-1;
          Yn[netlist[i].x][netlist[i].a]=1;
          Yn[netlist[i].x][netlist[i].b]=-1;
          Yn[netlist[i].x][netlist[i].c]=-g;
          Yn[netlist[i].x][netlist[i].d]=g;
        }
        else if (tipo=='F') {
          g=netlist[i].p1;
          Yn[netlist[i].a][netlist[i].x]=g;
          Yn[netlist[i].b][netlist[i].x]=-g;
          Yn[netlist[i].c][netlist[i].x]=1;
          Yn[netlist[i].d][netlist[i].x]=-1;
          Yn[netlist[i].x][netlist[i].c]=1;
          Yn[netlist[i].x][netlist[i].d]=-1;
        }
        else if (tipo=='H') {
          g=netlist[i].p1;
          Yn[netlist[i].a][netlist[i].x]=1;
          Yn[netlist[i].b][netlist[i].x]=-1;
          Yn[netlist[i].c][netlist[i].y]=1;
          Yn[netlist[i].d][netlist[i].y]=-1;
          Yn[netlist[i].x][netlist[i].a]=1;
          Yn[netlist[i].x][netlist[i].b]=-1;
          Yn[netlist[i].y][netlist[i].c]=1;
          Yn[netlist[i].y][netlist[i].d]=-1;
          Yn[netlist[i].x][netlist[i].y]=-g;
        }
        else if (tipo=='O') {
          Yn[netlist[i].a][netlist[i].x]=1;
          Yn[netlist[i].b][netlist[i].x]=-1;
          Yn[netlist[i].x][netlist[i].c]=1;
          Yn[netlist[i].x][netlist[i].d]=-1;
        }
        else if (tipo=='D') {
          v=en[netlist[i].a]-en[netlist[i].b];
          if (v>0.9) v=0.9; /* Da erro com 1 */
          ex=exp(v/VT);
          g=(IS/VT)*ex;
          Yn[netlist[i].a][netlist[i].a]+=g;
          Yn[netlist[i].b][netlist[i].b]+=g;
          Yn[netlist[i].a][netlist[i].b]-=g;
          Yn[netlist[i].b][netlist[i].a]-=g;
          g=IS*(ex-1)-g*v;
          Yn[netlist[i].a][nv+1]-=g;
          Yn[netlist[i].b][nv+1]+=g;
        }
        else if (tipo=='M') {
          if (netlist[i].y=='N') { /* NMOS */
            if (en[netlist[i].a]>en[netlist[i].c]) {
              drain=netlist[i].a;
              source=netlist[i].c;
            }
            else {
              drain=netlist[i].c;
              source=netlist[i].a;
            }
            gate=netlist[i].b;
            if (n==0 && iteracoes==0) vgs=2; /* Para garantir que comeca conduzindo */
            else vgs=en[gate]-en[source];
            if (vgs>VT0) {
              vds=en[drain]-en[source];
              Km=K0*netlist[i].p2/netlist[i].p1;
              if (vds>vgs-VT0) {
                g=2*Km*(vgs-VT0)*(1+LAMBDA*vds);
                gds=Km*(vgs-VT0)*(vgs-VT0)*LAMBDA;
                id=Km*(vgs-VT0)*(vgs-VT0)*(1+LAMBDA*vds);
              }
              else {
                g=Km*2*vds*(1+LAMBDA*vds);
                gds=Km*(2*(vgs-VT0)-2*vds+4*LAMBDA*(vgs-VT0)*vds-3*LAMBDA*vds*vds);
                id=Km*(2*(vgs-VT0)*vds-vds*vds)*(1+LAMBDA*vds);
              }
              id=id-g*vgs-gds*vds;
              Yn[drain]   [gate]+=g;
              Yn[source][source]+=g;
              Yn[drain] [source]-=g;
              Yn[source]  [gate]-=g;
              Yn[drain]  [drain]+=gds;
              Yn[source][source]+=gds;
              Yn[drain] [source]-=gds;
              Yn[source] [drain]-=gds;
              Yn[drain] [nv+1]-=id;
              Yn[source][nv+1]+=id;
            }
          }
          else { /* PMOS: Trata vgs e vds positivos. Diferencas marcadas */
            if (en[netlist[i].a]<en[netlist[i].c]) {  /***/
              drain=netlist[i].a;
              source=netlist[i].c;
            }
            else {
              drain=netlist[i].c;
              source=netlist[i].a;
            }
            gate=netlist[i].b;
            if (n==0 && iteracoes==0) vgs=2; /* Para garantir que comeca conduzindo */
            else vgs=-(en[gate]-en[source]); /***/
            if (vgs>VT0 || (n==0 && iteracoes==0)) {
              vds=-(en[drain]-en[source]); /***/
              Km=K0*netlist[i].p2/netlist[i].p1;
              if (vds>vgs-VT0) {
                g=2*Km*(vgs-VT0)*(1+LAMBDA*vds);
                gds=Km*(vgs-VT0)*(vgs-VT0)*LAMBDA;
                id=Km*(vgs-VT0)*(vgs-VT0)*(1+LAMBDA*vds);
              }
              else {
                g=Km*2*vds*(1+LAMBDA*vds);
                gds=Km*(2*(vgs-VT0)-2*vds+4*LAMBDA*(vgs-VT0)*vds-3*LAMBDA*vds*vds);
                id=Km*(2*(vgs-VT0)*vds-vds*vds)*(1+LAMBDA*vds);
              }
              id=-(id-g*vgs-gds*vds);  /***/
              Yn[drain]   [gate]+=g;
              Yn[source][source]+=g;
              Yn[drain] [source]-=g;
              Yn[source]  [gate]-=g;
              Yn[drain]  [drain]+=gds;
              Yn[source][source]+=gds;
              Yn[drain] [source]-=gds;
              Yn[source] [drain]-=gds;
              Yn[drain] [nv+1]-=id;
              Yn[source][nv+1]+=id;
            }
          }
        }
#ifdef TESTE
        printf("Tempo= %5.4f Sistema apos a estampa de %s\n",t,netlist[i].nome);
        for (k=1; k<=nv; k++) {
          for (j=1; j<=nv+1; j++)
            if (Yn[k][j]!=0) printf("%+3.1f ",Yn[k][j]);
            else printf(" ... ");
          printf("\n");
        }
        getch();
#endif
      }
      resolversistema();
#ifdef TESTE
      printf("Iteracao %d Sistema resolvido:\n",iteracoes);
      for (i=1; i<=nv; i++) {
          for (j=1; j<=nv+1; j++)
            if (Yn[i][j]!=0) printf("%+3.1f ",Yn[i][j]);
            else printf(" ... ");
          printf("\n");
        }
      getch();
#endif
      /* Testa convergencia */
      erro_max=0;
      iteracoes+=1;
      if (iteracoes>MAX_IT) {
        printf("Nao convergiu em t=%g\n",t);
        exit(1);
      }
      for (i=1; i<=nv; i++) {
        erro=fabs(en[i]-Yn[i][nv+1]);
        if (erro>erro_max) erro_max=erro;
        en[i]=Yn[i][nv+1];
      }
    }
    while (nao_linear && erro_max>TOLE);
    if (iteracoes>max_iter) {
      max_iter=iteracoes;
      t_max_iter=t;
    }
#ifdef TESTE
    printf("Tempo= %6.4f Solucao:\n",t);
    strcpy(txt,"Voltagem");
    for (i=1; i<=nv; i++) {
      if (i==nn+1) strcpy(txt,"Corrente");
      printf("%s %s: %g\n",txt,lista[i],en[i]);
    }
    getch();
#endif
    for (i=1; i<=nv; i++) et[i]=en[i];
    if (n % npassos==0) {
      fprintf(arquivo,"%g",t);
      for (i=1; i<=nv; i++) fprintf(arquivo," %g",et[i]);
      fprintf(arquivo,"\n");
    }
    dt=dt1;
    t+=dt;
  }
  fclose(arquivo);
  printf("Numero maximo de iteracoes: %d, em t=%g\n",max_iter,t_max_iter);
  printf("Terminado. Resultados salvos em mna2.tab.\n");
  return 0;
}

