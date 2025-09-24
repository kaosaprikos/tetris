.PHONY: run test report clean

OUT  = out
RPT  = test_report_lab3.txt
EXE  = tetris
CHK  = check.sh
SRC  = Shapes.hs Tetris.hs ConsoleGUI.hs GameInterface.hs 

$(EXE): $(SRC)
	ghc --make -main-is Tetris -hidir $(OUT) -odir $(OUT) -o tetris Tetris.hs

run: $(EXE)
	./$(EXE)

test:
	./$(CHK)

report:
	@rm -f $(RPT)
	@./$(CHK) > $(RPT)
	@echo "Report written to $(RPT)"

clean:
	rm -fr $(OUT) $(RPT) $(EXE)
