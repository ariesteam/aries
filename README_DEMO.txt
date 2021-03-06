Load Eclipse: click on eclipse logo in right-hand application launcher

To update RiskWiz:

	1. Right click on RiskWizCVars project, select Team->Update, make sure no errors (red symbols) come up in package explorer.
	2. Open project in explorer, right click on riskwiz.jardesc, Create Jar; this will update the jar in dist/;
	3. Copy the riskwiz.jar (ctrl-c) and paste it into thinklab/plugins/org.integratedmodelling.thinklab.riskwiz/lib (expand the plugin folder 
		below, not the package).

To run thinklab, use the app launcher Thinklab under the green "play" button menu in the toolbar. Simplest way to run everything is:

	0. put all of your Clojure driver code into 
		aries/plugins/org.integratedmodelling.aries.core/bindings/applications/demo.clj. 
	1. pload aries.core (this brings in all the needed plugins and compiles all the clojure bindings,
		 including the gssm code)
	2. run aries-demo  (this will run the demo.clj above after setting all the classpaths properly).

If you need to change the bindings being loaded or the files being run in (2), all the settings are in the plugin.xml file in  aries/plugins/org.integratedmodelling.aries.core.
Any change you make should be reflected instantaneously, no need to recompile or anything, just stop the
application (red stop button in the console window) and relaunch it. Avoid running apps on top of each
other (although it works fine) to save heap.

Semantic annotations for the data are in  aries/plugins/org.integratedmodelling.aries.core/demo/kbox 
(all xml files in there are loaded automatically; the ones in the subdir are ignored);

Bayesian networks are read from aries/plugins/org.integratedmodelling.aries.core/demo/bn; the name is the main observable concept (that you pass as a "benefit"
to gssm-interface) with the colon substituted by an underscore;

Dependency trees are generated for each main observable by reading the files in aries/plugins/org.integratedmodelling.aries.core/demo/models; they are simply 
text files with several concepts listed per row; the first is the "root", the others are dependents. All the concepts mentioned after the first one represent observables
for which observations will be looked up in the data, so they will end up in the state map for the BN unless data are not available. The binding is done by matching
the observables in the data kbox, i.e.

		<observation:hasObservable>
			<aestheticService:Park />
		</observation:hasObservable>
		
TO CHANGE THE REGION OF INTEREST:

The ROI is the result of aries/select-region-of-interest and it must be a ShapeValue. The default 
binding is generated from the Java task 
org.integratedmodelling.aries.core.task.prioritization.SelectRegionOfInterest. In the source code you'll
find strings that specify the WKT definition for the ROI (a polygon; the last point must coincide with
the first.) The default value is a portion of king county.

If you want to override that without touching Java (more efficient if you're testing in Clojure) the
way to do it is

(defn select-region-of-interest 
	""
	[]
	(tl/lit 'geospace:SpatialRecord "POLYGON((-122.2 47.6,-122.1 47.6,-122.1 47.5,-122.1 47.5,-122.2 47.6))"))
	
Obviously you're on your own selecting the polygon, but this should work as long as the WKT is
correct. Otherwise you can add your region as a constant in the Java task and substitute the constant
in the call to validateLiteral in run() - your choice.
