C �t�[���G�ϊ�����,parameter�����f�[�^�̓ǂݍ��݂����f�[�^�A�����Ɛ�����Ή�������
      parameter(n=88064)
      
C    fxc,fxs�͂��ꂼ��֐��̒�`   
      external fxc,fxs
      
C     fq�͂��ꂼ��̍����Ah�����́Asum�͏o�͗p���v�l�API�̓΁Adata�͎���(����)�AN�͕�����,f�͎��g��
C     max�͉���t�̍ő�l�Amin�͍Œ�l,
      real fqs,fqc,t,f,h,PI,max,min,sum,sumim,sumre
 
C     vo�͓ǂݍ��ݗp
      real vo(N)
      
      integer x,inf,it
      complex sk,sumcom,sekibun
      
      common PI,f,vo
      
C �f�[�^�ǂݍ���
      open(20,file='����-�n��.csv')
      
      do i=1,N
      
      read(20,*)vo(i)
      end do
      
C      do i=1,N,100
      
C      write(6,*)i,vo(i)
      
C      end do
C      stop

      PI=4.0*atan(1.0)
      
      min =0.0

C     50�Ƃ����͎̂��g���̎n�܂�      
      f0=50.0

C    �T���v�����O�������Ƃ�      
      h = 1.0/44100.0
      max = min+h*(N-1)
      
      df=1.0
      
      do inf=0,1000
      
C     ���g�����ǂ�ǂ񑝂��Ă��ꂼ��̎��g���ɑΉ����Đϕ��������Ă���
      f=f0+float(inf)*df
      sumim=0
      sumre=0
  
C     t0im�͋���,t0re�͎���      
      t0im=fxs(min,1)
      t0re=fxc(min,1)
      tnim=fxs(max,N)
      tnre=fxc(max,N)

C ��`����邤���ŕK�v�̂Ȃ��Ƃ���  
    
      sk=(cmplx(t0re,t0im)+cmplx(tnre,tnim))/2.0
      
C it�͊Ԋu�̂���
      
      do 10 it=1,N-2

C t�͎n�܂�̍��W��\��
            t=min+float(it)*h
            sumim=sumim+fxs(t,it)
            sumre=sumre+fxc(t,it)
            
C      ��������͎̂��Ԃ��Ƃ� �����Ƌ����ɂ����Ă̑����Z           
      
   10 continue  
    
C     �S���̑����Z���I������Isumre��sumim�͌��݂��ꂼ������A�����2���킹�����f���ϊ����悤
      sumcom=cmplx(sumre,sumim)
      sekibun=(sumcom+sk)*h
      sum=abs(sekibun)
      
      open(30,file='����-�n��t�[���G�ϊ�.csv')
      write(30,*)f,',',real(sekibun),',',Imag(sekibun),',',sum
      
      write(6,*)f,',',real(sekibun),',',Imag(sekibun),',',sum
      
      end do
      
      stop
      end
      
      
C     cmplx�͕��f���ϊ����邽�߂̂���
      
C e-jwt�̌v�Z
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
         
      