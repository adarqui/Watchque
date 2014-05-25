test_resque:
	make cabal
	make dirs
	./dist/dist-*/build/watchque-hs/watchque-hs 127.0.0.1:6379 class1:queue1:C:/tmp/wq/C class2:queue2:cN:/tmp/wq/CN class3:queue3:d:/tmp/wq/d class4:queue4:a:/tmp/wq/a class5:queue5:u:/tmp/wq/u class6:queue6:C:/tmp/wq/CWorking:'Working/*' class7:queue7:A:/tmp/wq/A class8:queue8:I:/tmp/wq/I class9:queue9:O:/tmp/wq/O class10:queue10:Z:/tmp/wq/Z class11:queue11:D:/tmp/wq/D class12:queue12:r:/tmp/wq/r class13:queue13:c:/tmp/wq/c +RTS -V0

test_local:
	make cabal
	make dirs
	make local_dirs
	./dist/dist-*/build/watchque-hs/watchque-hs /tmp/wq/bin class1:queue1:C:/tmp/wq/C class2:queue2:cN:/tmp/wq/CN class3:queue3:d:/tmp/wq/d class4:queue4:a:/tmp/wq/a class5:queue5:u:/tmp/wq/u class6:queue6:C:/tmp/wq/CWorking:'Working/*' class7:queue7:A:/tmp/wq/A class8:queue8:I:/tmp/wq/I class9:queue9:O:/tmp/wq/O class10:queue10:Z:/tmp/wq/Z class11:queue11:D:/tmp/wq/D class12:queue12:r:/tmp/wq/r class13:queue13:c:/tmp/wq/c +RTS -V0

dirs:
	bash -c "mkdir -p /tmp/wq/{a,c,u,d,D,r,C,A,M,U,I,O,Z,CN,CWorking}"

local_dirs:
	bash -c "mkdir -p /tmp/wq/bin/"
	bash -c 'for i in `seq 1 13`; do mkdir -p /tmp/wq/bin/class$$i; done'
	bash -c 'for i in `seq 1 13`; do printf "#!/bin/bash\necho im $$i \$$@\n" > /tmp/wq/bin/class$$i/queue$$i && chmod 755 /tmp/wq/bin/class$$i/queue$$i; done'

cabal:
	cabal install
