# Set deault to the help menu
.DEFAULT_GOAL := help

# Compile with gcc, change this to clang if you prefer
COMPILER = gcc

# The C flags to pass to gcc
C_FLAGS = -Wall -Wextra -g

# Ending to ensure that <math> and the for loops work
END = -lm -std=c99

#Targets for cleaning
TARGETS = NetTest DNetTest test.txt DNet.txt

#Data file input
FILE = data.txt

FILE2 = testData.csv

#Rule to fix .o file creation
%.o:%.c
	gcc $(CFLAGS) $< -o $@

# Written commands to call for full tests
NetTestFull: NetTest NetTestRun

DNetTestFull: DNetTest DNetTestRun 

DNetTestImport: DNetTest DNetTestRunI

#Run the compiled files
NetTestRun:
	./NetTest $(FILE)
	
DNetTestRun:
	./DNetTest $(FILE)

DNetTestRunI:
	./DNetTest $(FILE) $(FILE2)
	
# Compile network test
NetTest: .FORCE
	$(COMPILER) $(C_FLAGS) -o NetTest NetworkTest.c NetworkGen.c $(END)
	
# Compile code simulation
DNetTest: .FORCE
	$(COMPILER) $(C_FLAGS) -o DNetTest DNetTest.c DNet.c NetworkGen.c $(END)
	
.FORCE:
	
#Remove extra files
clean:
	rm -f $(TARGETS) *.o *~

.PHONY: help
help: 
	@echo ' ---------------------HELP MENU--------------------'
	@echo ' TERM - MEANING'
	@echo ' Net - Just Network'
	@echo ' DNet - Simulation [Dynamic Network]'
	@echo ' Import - Import a network when running'
	@echo ' '
	@echo ' TASK - COMMAND'
	@echo ' Compile and Run - NetTestFull, DNetTestFull, and DNetTestImport'
	@echo ' Compile - NetTest and DNetTest'
	@echo ' Run - NetTestRun and DNetTestRun'
	@echo ' Remove Files - clean'
	@echo ' '
	@echo ' To input file, type FILE="filename" at end '
