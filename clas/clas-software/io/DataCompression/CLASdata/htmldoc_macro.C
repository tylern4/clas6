{
// load necessary libs
gSystem.Load("libPhysics");
gSystem.Load("./libCLASdata.so");
// make sure env variable is set right to link to ROOT page
gEnv->SetValue("Root.Html.Root","http://root.cern.ch/root/html");
//gEnv->SetValue("Root.Html.Author","Mike Williams");
gEnv->SetValue("Root.Html.Footer","footer.html");
gEnv->SetValue("Root.Html.OutputDir: htmldoc");
THtml html;
html.MakeAll(kTRUE,"*");
}
