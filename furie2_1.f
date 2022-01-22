C フーリエ変換する,parameter音声データの読み込みたいデータ、文字と数字を対応させる
      parameter(n=88064)
      
C    fxc,fxsはそれぞれ関数の定義   
      external fxc,fxs
      
C     fqはそれぞれの高さ、h幅一定の、sumは出力用合計値、PIはπ、dataは時間(横軸)、Nは分割数,fは周波数
C     maxは横軸tの最大値、minは最低値,
      real fqs,fqc,t,f,h,PI,max,min,sum,sumim,sumre
 
C     voは読み込み用
      real vo(N)
      
      integer x,inf,it
      complex sk,sumcom,sekibun
      
      common PI,f,vo
      
C データ読み込み
      open(20,file='水中-地上.csv')
      
      do i=1,N
      
      read(20,*)vo(i)
      end do
      
C      do i=1,N,100
      
C      write(6,*)i,vo(i)
      
C      end do
C      stop

      PI=4.0*atan(1.0)
      
      min =0.0

C     50というのは周波数の始まり      
      f0=50.0

C    サンプリング周期ごとに      
      h = 1.0/44100.0
      max = min+h*(N-1)
      
      df=1.0
      
      do inf=0,1000
      
C     周波数がどんどん増えてそれぞれの周波数に対応して積分を解いていく
      f=f0+float(inf)*df
      sumim=0
      sumre=0
  
C     t0imは虚数,t0reは実数      
      t0im=fxs(min,1)
      t0re=fxc(min,1)
      tnim=fxs(max,N)
      tnre=fxc(max,N)

C 台形を作るうえで必要のないところ  
    
      sk=(cmplx(t0re,t0im)+cmplx(tnre,tnim))/2.0
      
C itは間隔のこと
      
      do 10 it=1,N-2

C tは始まりの座標を表す
            t=min+float(it)*h
            sumim=sumim+fxs(t,it)
            sumre=sumre+fxc(t,it)
            
C      今やったのは時間ごとの 実部と虚部においての足し算           
      
   10 continue  
    
C     全部の足し算が終わった！sumreとsumimは現在それぞれ実数、これを2つ合わせた複素数変換しよう
      sumcom=cmplx(sumre,sumim)
      sekibun=(sumcom+sk)*h
      sum=abs(sekibun)
      
      open(30,file='水中-地上フーリエ変換.csv')
      write(30,*)f,',',real(sekibun),',',Imag(sekibun),',',sum
      
      write(6,*)f,',',real(sekibun),',',Imag(sekibun),',',sum
      
      end do
      
      stop
      end
      
      
C     cmplxは複素数変換するためのもの
      
C e-jwtの計算
      function fxc(x,it)
      parameter(n=88064)
      real vo(N)
      common PI,f,vo
      fxc=cos(2*PI*f*x)*vo(it)
      return
      end   
      
      function fxs(x,it)
      parameter(n=88064)
      real vo(N)
      common PI,f,vo
      fxs =sin(2.0*PI*f*x)*vo(it)
      return
      end
         
      