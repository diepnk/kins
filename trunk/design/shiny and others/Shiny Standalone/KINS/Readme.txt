1.Require:
- .Net framework is required

2.How to run:
Double click on InsuranceSimulator.exe

Files/Folders structure:

ShinyDemoV1/
+- Browser/					[Description: a tiny Windows Application - loads Browser using .Net framework]
+- Logs/					[Description: Contains log files]
+- R-Portable/					[Description: a portable of R with fully required library which used in shiny app]
+- shiny/					[Description: R shiny app]
   +- ui.R
   +- server.R
+- run.vbs					[Description: vb script starts R-Portable and Shiny app]
+- runShinyApp.R				[Description: install required lib if any, and start Shiny app]
+- readme.txt					[Description: More information about the example]
+- InsuranceSimulator.exe			[Description: Executor]
