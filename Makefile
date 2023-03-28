# $Id: Makefile,v 1.21 2023/03/23 08:30:36 leavens Exp leavens $
# Makefile for lexer in COP 3402

# Add .exe to the end of target to get that suffix in the rules
COMPILER = compiler
VM = vm
CC = gcc
# on Linux, the following can be used with gcc:
# CFLAGS = -fsanitize=address -static-libasan -g -std=c17 -Wall
CFLAGS = -g -std=c17 -Wall
MV = mv
RM = rm -f
SUBMISSIONZIPFILE = submission.zip
ZIP = zip -9
SOURCESLIST = sources.txt
TESTS = hw3-asttest*.pl0 hw3-parseerrtest*.pl0 hw3-declerrtest*.pl0 hw4-asttest*.pl0 hw4-parseerrtest*.pl0 hw4-declerrtest*.pl0
VMTESTS = hw4-vmtest*.pl0
EXPECTEDOUTPUTS = `echo $(TESTS) | sed -e 's/\\.pl0/.out/g'`
EXPECTEDVMINPUTS = `echo $(VMTESTS) | sed -e 's/\\.pl0/.vmi/g'`
EXPECTEDVMOUTPUTS = `echo $(VMTESTS) | sed -e 's/\\.pl0/.vmo/g'`

$(COMPILER): *.c *.h
	$(CC) $(CFLAGS) -o $(COMPILER) `cat $(SOURCESLIST)`

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

.PHONY: clean
clean:
	$(RM) *~ *.o *.myo '#'*
	$(RM) $(COMPILER).exe $(COMPILER)
	$(RM) *.stackdump core
	$(RM) $(SUBMISSIONZIPFILE)

.PRECIOUS: %.myo
%.myo: %.pl0 $(COMPILER)
	./$(COMPILER) $< > $@ 2>&1

%.myvi: %.pl0 $(COMPILER)
	./$(COMPILER) $< > $@

%.myvo: %.myvi $(VM)/$(VM)
	$(VM)/$(VM) $< > $@ 2>&1

.PHONY: check-outputs check-vm-outputs
check-outputs: check-vm-outputs

check-vm-outputs: $(COMPILER) $(VMTESTS) $(VM)/$(VM)
	DIFFS=0; \
	for f in `echo $(VMTESTS) | sed -e 's/\\.pl0//g'`; \
	do \
		echo compiling "$$f.pl0" ...; \
		$(RM) "$$f.vmi"; \
		./$(COMPILER) "$$f.pl0" > "$$f.myvi"; \
		echo running "$$f.myvi" in the VM ...; \
		vm/vm "$$f.myvi" > "$$f.myvo" 2>&1; \
		diff -w -B "$$f.vmo" "$$f.myvo" && echo 'passed!' || DIFFS=1; \
	done; \
	if test 0 = $$DIFFS; \
	then \
		echo 'All tests passed!'; \
	else \
		echo 'Test(s) failed!'; \
	fi

# Automatically generate the submission zip file
$(SUBMISSIONZIPFILE): $(SOURCESLIST) *.c *.h *.myo *.myvo
	$(ZIP) $(SUBMISSIONZIPFILE) $(SOURCESLIST) *.c *.h *.myo *.myvo Makefile

# Automatically regenerate the sources.txt file
.PRECIOUS: $(SOURCESLIST)
$(SOURCESLIST):
	echo *.c > $(SOURCESLIST)


# developer's section below...

.PRECIOUS: %.out
%.out: %.pl0 $(COMPILER)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should NOT use the target $@,'; \
		echo 'as using this target ($@) will invalidate a test'; \
		exit 1; \
	fi
	./$(COMPILER) -u $< > $@ 2>&1

.PRECIOUS: %.vmi
%.vmi: %.pl0 $(COMPILER)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should NOT use the target $@,'; \
		echo 'as using this target ($@) will invalidate a test'; \
		exit 1; \
	fi
	./$(COMPILER) $< > $@

.PRECIOUS: %.vmo
%.vmo: %.vmi $(VM)/$(VM)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should NOT use the target $@,'; \
		echo 'as using this target ($@) will invalidate a test'; \
		exit 1; \
	fi
	$(VM)/$(VM) $< > $@ 2>&1

.PHONY: create-outputs create-vm-outputs
create-outputs: $(COMPILER) $(TESTS) $(VMTESTS) 
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should use the target check-outputs,' ;\
		echo 'as using this target (create-outputs) will invalidate the tests!' ; \
		exit 1; \
	fi
	$(MAKE) create-non-vm-outputs create-vm-outputs

create-non-vm-outputs: $(COMPILER) $(TESTS) 
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should use the target check-non-vm-outputs,'; \
		echo 'as using this target (create-non-vm-outputs) will invalidate the tests!' ; \
		exit 1; \
	fi
	for f in `echo $(TESTS) | sed -e 's/\\.pl0//g'`; \
	do \
		echo running "$$f.pl0"; \
		$(RM) "$$f.out"; \
		./$(COMPILER) -u "$$f.pl0" >"$$f.out" 2>&1; \
	done; \
	echo 'done creating non-VM test outputs!'

create-vm-outputs: $(COMPILER) $(VMTESTS)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should use the target check-vm-outputs,'; \
		echo 'as using this target (create-vm-outputs) will invalidate the tests!' ; \
		exit 1; \
	fi
	for f in `echo $(VMTESTS) | sed -e 's/\\.pl0//g'`; \
	do \
		echo compiling "$$f.pl0"; \
		$(RM) "$$f.vmi"; \
		./$(COMPILER) "$$f.pl0" > "$$f.vmi"; \
		echo running "$$f.vmi"; \
		vm/vm "$$f.vmi" > "$$f.vmo" 2>&1; \
	done
	echo 'done creating VM test outputs!'

.PHONY: digest
digest digest.txt: 
	for f in `ls $(TESTS) | sed -e 's/\\.pl0//g'`; \
        do cat $$f.pl0; echo " "; cat $$f.out; echo " "; echo " "; \
        done >digest.txt

.PHONY: vmdigest
vmdigest vmdigest.txt: 
	for f in `ls $(VMTESTS) | sed -e 's/\\.pl0//g'`; \
        do cat $$f.pl0; echo " "; cat $$f.vmo; echo " "; echo " "; \
        done >vmdigest.txt

# don't use develop-clean unless you want to regenerate the expected outputs
.PHONY: develop-clean
develop-clean: clean
	$(RM) $(EXPECTEDOUTPUTS) $(EXPECTEDVMOUTPUTS) digest.txt vmdigest.txt
	cd $(VM); $(MAKE) clean

TESTSZIPFILE = ~/WWW/COP3402/homeworks/hw4-tests.zip
PROVIDEDFILES = compiler_main.c \
		token.[ch] lexer_output.[ch] utilities.[ch] lexer.[ch] \
		reserved.[ch] \
		ast.[ch] parser.[ch] parserInternal.h \
		unparser.[ch] unparserInternal.h \
		file_location.[ch] id_attrs.[ch] id_use.[ch] \
		scope.[ch] scope_check.[ch] symtab.[ch] \
		code.[ch] instruction.[ch] machine_types.h \
		label.[ch] lexical_address.[ch] gen_code.[ch]

.PHONY: stubs
stubs: gen_code_stubs.c
	test -f gen_code_actual.c || $(MV) gen_code.c gen_code_actual.c
	$(MV) gen_code_stubs.c gen_code.c

.PHONY: gen_code_restore
restore: gen_code_actual.c
	test -f gen_code_stubs.c || $(MV) gen_code.c gen_code_stubs.c
	$(MV) gen_code_actual.c gen_code.c
	chmod u+w *

.PHONY: zip
zip hw4-tests.zip: create-vm-outputs stubs $(TESTSZIPFILE) 

$(TESTSZIPFILE): $(TESTS) $(VMTESTS) Makefile $(PROVIDEDFILES)
	test -f gen_code_actual.c || exit 1
	$(RM) $(TESTSZIPFILE)
	chmod 444 $(VMTESTS) $(PROVIDEDFILES) $(EXPECTEDVMINPUTS) $(EXPECTEDVMOUTPUTS) Makefile
	chmod u+w gen_code.c Makefile 
	$(ZIP) $(TESTSZIPFILE) $(VMTESTS) $(EXPECTEDVMINPUTS) $(EXPECTEDVMOUTPUTS) $(PROVIDEDFILES) Makefile
	$(ZIP) $(TESTSZIPFILE) -x '$(VM)/RCS' -x '$(VM)/RCS/*' -x '$(VM)/$(VM).exe' -x '$(VM)/*.out' -x '$(VM)/*.myo' -x '$(VM)/*.tex' -x '$(VM)/*.aux' -x '$(VM)/$(VM)/*.vmi' -r $(VM)

.PHONY: check-separately
check-separately:
	$(CC) $(CFLAGS) -c *.c
