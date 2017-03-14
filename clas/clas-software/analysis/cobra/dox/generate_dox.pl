#!/usr/bin/perl

# run doxygen
system("doxygen tutorials.dox");
system("cp output/html/files.html output/html/tutorial_files.html");
system("doxygen cobra_tutorials.dox");
system("doxygen cobra.dox > temp.log");
system("rm -f temp.log");


# copy stuff into html directory
system("cp cobra.css output/html/.");
system("cp style.css output/html/.");
system("cp index.html output/html/index.html");
system("cp main.html output/html/main.html");
system("cp toc.html output/html/toc.html");
system("cp install.html output/html/install.html");
system("cp start.html output/html/start.html");
system("cp tutorials.html output/html/tutorials.html");

# copy in some gif's
system("cp gifs/*.gif output/html/.");
