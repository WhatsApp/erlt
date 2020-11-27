../erltc/priv/sterlang.jar: $(shell find . -name "*.scala")
	sbt assembly
	find . -name sterlang.jar
	mkdir -p ../erltc/priv
	find . -name sterlang.jar | tail -n 1 | xargs -I _ cp _ ../erltc/priv
	cd ../erltc/priv && pwd && ls

clean:
	rm -f ../erltc/priv/sterlang.jar

