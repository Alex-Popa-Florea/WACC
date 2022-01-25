# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
all:
	sbt compile assembly

# clean up all of the compiled files
clean:
	sbt clean

.PHONY: all clean
