#ifdef __GNUC__
#include <pc.h>
#include <std.h>
#include <grx.h>
#include <bccgrx.h>
#define random RANDOM_TP
#else
#include <conio.h>
#include <stdlib.h>
#include <graphics.h>
#endif
#include <assert.h>

#define nlines 60

typedef struct figstruct {              /* structue for images */
  int blocks;                           /* number of image blocks -1 */
  int fdx, fdy;                         /* dimensions */
  unsigned int blocksize,lastblocksize; /* size of the blocks */
  void *v[2001];                        /* allocated until "blocks" */
} figstruct;
typedef struct figstruct *ptrfig;       /* imagem pointer */

ptrfig fig1,fig2,fig3,fig4;
int i,j,left,top;
int board,mode;

void getfiguremem(ptrfig *p,int dw, int dh)
{
  int i,nb;

  nb=dh/nlines;
  *p=(ptrfig)malloc(5*sizeof(int)+sizeof(void*)*(nb+1));
  assert(*p!=NULL);
  (*p)->blocks=nb;
  (*p)->blocksize=imagesize(0,1,dw,nlines);
  (*p)->fdx=dw;
  (*p)->fdy=dh;
  for (i=0; i<nb; i++) {
    (*p)->v[i]=malloc((*p)->blocksize);
    assert((*p)->v[i]!=NULL);
  }
  (*p)->lastblocksize=imagesize(0,0,dw,dh%nlines);
  (*p)->v[nb]=malloc((*p)->lastblocksize);
  assert((*p)->v[nb]!=NULL);
}

void freefiguremem(ptrfig p)
{
  int i;

  for (i=p->blocks; i>=0; i--) free(p->v[i]);
  free(p);
}

void getfigure(int x1,int y1, int x2,int y2,ptrfig p)
{
  int i,v1,v2;

  v1=y1; v2=y1+nlines-1;
  for (i=0; i<=p->blocks; i++) {
    if (v2>y2) v2=y2;
    getimage(x1,v1,x2,v2,p->v[i]);
    v1=v2+1; v2=v1+nlines-1;
  }
}

void putfigure(int x,int y, ptrfig p, unsigned int bitblt)
{
  int i,v;

  v=y;
  for (i=0; i<=p->blocks; i++) {
    putimage(x,v,p->v[i],bitblt);
    v+=nlines;
  }
}

void movefigure(ptrfig p1,ptrfig p2)
/* Copies an image in memory. Used to reduce heap fragmentation */
/* Does not work with djgpp ? */
{
  int i;

  for (i=0; i<p1->blocks; i++) memmove(p2->v[i],p1->v[i],p1->blocksize);
  i=p1->blocks;
  memmove(p2->v[i],p1->v[i],p1->lastblocksize);
}

void main(void)
{
  board=0;
  initgraph(&board,&mode,getenv("TCBGI"));
  for (i=1; i<=200; i++) {
    setcolor(random(15));
    left=random(getmaxx()); top=random(getmaxy());
	 for (j=1; j<=20; j++) line(left,top,left+random(100)-50,top+random(100)-50);
  }
  getfiguremem(&fig1,200,200);
  getfiguremem(&fig2,200,200);
  getfiguremem(&fig3,200,200);
  getfiguremem(&fig4,200,200);
  getfigure(0,0,200,200,fig1);
  getfigure(200,250,400,450,fig2);
  getfigure(10,10,210,210,fig3);
  getfigure(20,20,220,220,fig4);
  getch();
  putfigure(300,0,fig1,COPY_PUT);
  putfigure(300,250,fig2,COPY_PUT);
  getch();
  movefigure(fig2,fig1);
  putfigure(300,0,fig1,COPY_PUT);
  putfigure(300,250,fig2,COPY_PUT);
  getch();
  movefigure(fig3,fig1);
  putfigure(300,0,fig1,COPY_PUT);
  putfigure(300,250,fig2,COPY_PUT);
  getch();
  movefigure(fig3,fig2);
  putfigure(300,0,fig1,COPY_PUT);
  putfigure(300,250,fig2,COPY_PUT);
  getch();
}

