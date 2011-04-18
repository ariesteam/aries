package org.integratedmodelling.aries.webapp.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.apache.commons.lang.StringUtils;
import org.geotools.geometry.jts.JTS;
import org.integratedmodelling.aries.webapp.ARIESWebappPlugin;
import org.integratedmodelling.aries.webapp.visualization.WebZKVisualization;
import org.integratedmodelling.corescience.context.ObservationContext;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.corescience.interfaces.internal.IContextTransformation;
import org.integratedmodelling.corescience.interfaces.internal.IDatasourceTransformation;
import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.geospace.extents.ArealExtent;
import org.integratedmodelling.geospace.literals.ShapeValue;
import org.integratedmodelling.modelling.ModellingPlugin;
import org.integratedmodelling.modelling.context.Context;
import org.integratedmodelling.modelling.context.FilteredTransformation;
import org.integratedmodelling.modelling.interfaces.IModel;
import org.integratedmodelling.modelling.model.Model;
import org.integratedmodelling.modelling.model.ModelFactory;
import org.integratedmodelling.modelling.model.Scenario;
import org.integratedmodelling.modelling.storyline.ModelStoryline;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.modelling.visualization.storyline.StorylineTemplate;
import org.integratedmodelling.olmaps.OLmaps;
import org.integratedmodelling.olmaps.event.FeatureAddedEvent;
import org.integratedmodelling.olmaps.layer.GoogleMapsLayer;
import org.integratedmodelling.olmaps.layer.VectorLayer;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.http.application.ThinklabWebApplication;
import org.integratedmodelling.thinklab.http.geospace.zk.OLMAPS;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.literals.BooleanValue;
import org.integratedmodelling.thinklab.webapp.TC;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.ZK.ZKComponent;
import org.integratedmodelling.thinklab.webapp.view.components.Ribbon;
import org.integratedmodelling.thinklab.webapp.view.components.ThinkcapComponent;
import org.integratedmodelling.utils.NameGenerator;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.Path;
import org.integratedmodelling.utils.Triple;
import org.integratedmodelling.utils.image.ColorMap;
import org.opengis.referencing.operation.TransformException;
import org.zkoss.zhtml.Text;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;
import org.zkoss.zk.ui.event.SelectEvent;
import org.zkoss.zul.Button;
import org.zkoss.zul.Div;
import org.zkoss.zul.Listbox;
import org.zkoss.zul.Listitem;
import org.zkoss.zul.Textbox;
import org.zkoss.zul.Toolbarbutton;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Scenario editor. 
 * 
 * TODO: make it a bit less aries-dependent; use Thinkcap component for area selection (tie to context).
 * 
 * @author Ferdinando
 *
 */
public class ScenarioEditor extends ThinkcapComponent {

	private static final long serialVersionUID = 8048071165550994145L;
	private int _height;
	private int _width;
	AriesBrowser _browser;
	private int leftW;
	private int rightW;
	private int topH;
	private int botH;
	private int midH;
	
	private int mapState = 0;
	private IModel model = null;
	private Scenario scenario;
	
	Toolbarbutton scaddpolyg = null;
	Toolbarbutton scsubpolyg = null;
	Toolbarbutton scresetdraw = null;
	Textbox scname = null;
	
	private OLmaps layermap;
	private GoogleMapsLayer scgoogle;
	private VectorLayer     scvector;
	private int picmode;
	private ShapeValue roi = null;
	private IContext context;
	private ModelStoryline storyline;
	private ArrayList<IConcept> editables = new ArrayList<IConcept>();
	private Collection<Scenario> scenarios = new ArrayList<Scenario>();

	private Div layereditor;
	private Ribbon layerselector;
	
	class Transformation {
		ShapeValue roi = null;
		IConcept   obs = null;
		Object[]   sel = null;
		Object     ret = null;
	}
	
	ArrayList<Transformation> transformations = new ArrayList<ScenarioEditor.Transformation>();
	private String scenarioId;
	private Pair<Integer,Integer> xy = null;
	
	public ScenarioEditor(AriesBrowser browser, int width, int height) {

		this._width = width;
		this._height = height;
		this._browser = browser;
		this.setSclass(STYLE.SCENARIO_WINDOW);
		this.setHeight(_height+"px");
		this.setWidth(_width+"px");
		
		// relative window sizes
		this.midH = height - 100;
		this.leftW = _width*23/100;
		this.rightW = _width*73/100;
		this.topH = (midH*33/100);
		this.botH = (midH*63/100)+9;
	}
	
	@Override
	public void initialize() {
	}

	/**
	 * Recover the scenario created
	 * @return
	 * @throws ThinklabException
	 */
	public Triple<String, Scenario, IContext> getScenarioContext() throws ThinklabException {
		
		IContext ret = context.cloneExtents();
		for (Transformation t : transformations) {

			// create and insert transformations into context
			FilteredTransformation tr = new FilteredTransformation(t.obs, t.ret);
			if (t.sel != null) {
				for (Object o : t.sel)
					tr.addFilter(o);
			}
//			((ObservationContext)ret).addTransformation(tr);
			
		}
		return new Triple<String, Scenario,IContext>(this.scenarioId, this.scenario, ret);
	}
	
	public void setStoryline(ModelStoryline storyline) {
		
		if (storyline.getStatus() == Storyline.COMPUTED) {
			
			this.context = storyline.getContext();
			this.storyline = storyline;
			
			/*
			 * compute possible scenarios and list of editable
			 * concepts
			 */
			this.editables  = new ArrayList<IConcept>();
			for (IState s : this.context.getStates()) {
				String ed = 
					s.getObservableClass().getAnnotation(ModellingPlugin.EDITABLE_ANNOTATION);
				if (ed != null && BooleanValue.parseBoolean(ed)) {
					this.editables.add(s.getObservableClass());
				}
			}
			
			this.scenarios  =
				ModelFactory.get().getApplicableScenarios((Model) storyline.getModel());
			
		}
		
		setup();
	}
	
	public void setup() {

		clear();
		
		setContent(
			ZK.vbox(
					
				ZK.separator(false).height(8),

				// top bar
				ZK.hbox(
					ZK.div(
						ZK.label("Scenario editor").sclass(STYLE.TEXT_LARGE_BOLD_RED)
					).align("left"),
					ZK.div(
						ZK.hbox(
							ZK.label("Scenario name: "),
							ZK.textbox().width(200).id("scname")
						)
					).width("100%").align("right")
				).width("100%"),
				
				// main area
				ZK.window(
					ZK.hbox(
					   ZK.vbox(
					      ZK.groupbox("Global scenarios",
					          createPredefinedEditor(model)
					      ).width(leftW).height(topH),
					      ZK.groupbox("Editable parameters",
					    	  createParameterEditor()
					      ).width(leftW).height(botH)
					   ),
					   ZK.groupbox("Policy options editor",
							 ZK.vbox(
									 createSpaceSelector(),
									 createLayerSelector(),
									 createLayerEditor().hide())
					   ).width(rightW).height(midH)
					)
				),
				
				// bottom bar
				ZK.div(
					ZK.hbox(
						ZK.button("Save").
							listener("onClick", new EventListener() {
								@Override
								public void onEvent(Event arg0) throws Exception {
									String sname = scname.getValue();
									if (sname == null || sname.equals("")) {
										alert("Please enter a name for the scenario");
									} else {
										_browser.addScenario(context);
									}
								}
							}),
						ZK.button("Cancel").
							listener("onClick", new EventListener() {
								@Override
								public void onEvent(Event arg0) throws Exception {
									_browser.addScenario(null);
								}
							})
					)
				).align("right")
			).width("100%")
		);
		
		afterCompose();
		
	}
	
	public void setLocation(int zoom, ArealExtent iExtent) {
		
		Coordinate p1 = null, p2 = null;
		try {
			p1 = JTS.transform(
					new Coordinate(iExtent.getWest(), iExtent.getSouth()), null, ARIESWebappPlugin.get().geoToGoogleTransform);
			p2 = 
				JTS.transform(
					new Coordinate(iExtent.getEast(), iExtent.getNorth()), null, ARIESWebappPlugin.get().geoToGoogleTransform);
		} catch (TransformException e) {
			throw new ThinklabRuntimeException(e);
		}

		layermap.setZoom(zoom);
		layermap.setCenter(p1.x + (p2.x-p1.x)/2, p1.y + (p2.y-p1.y)/2);
	}
	
	protected void mergeScenario(Scenario rid) {
		this.scenario.merge(rid);
		// TODO redisplay
	}

	private ZKComponent createLayerSelector() {
		
		if (this.xy  == null) {
			this.xy = 
				VisualizationFactory.get().getPlotSize(58, 58, storyline.getContext());
		}
		
		ArrayList<ZK.ZKComponent> components = new ArrayList<ZK.ZKComponent>();
		WebZKVisualization visualization =
			(WebZKVisualization) storyline.getVisualization();

		for (final IConcept c : editables) {
		
			StorylineTemplate.Page p = storyline.getTemplate().getPage(c);
			if (p == null)
				continue;
			
			String img = visualization.getStateUrl(p.getConcept(), VisualizationFactory.PLOT_GEOSURFACE_2D);
			if (img != null) {
				components.add(
					ZK.div(
						ZK.vbox(
							ZK.image(img).width(xy.getFirst()).height(xy.getSecond()),
							ZK.label(StringUtils.abbreviate(p.getName(),24)).sclass(STYLE.TEXT_VERYSMALL).align("center").fillx())).
						align("center").
						width(120).
						tooltip(p.getRunningHead()).
						hand().listener("onClick", new EventListener() {
						
						@Override
						public void onEvent(Event arg0) throws Exception {
							editLayer(c);
						}
					}).tmargin(64 - xy.getSecond()));
			}
		}
				
		return ZK.ribbon(rightW -6, 64, 48, components.toArray(new ZK.ZKComponent[components.size()])).id("layerselector");

	}

	protected void editLayer(IConcept c) throws ThinklabException {
		// TODO Auto-generated method stub		
		IState state = context.getState(c);
		StorylineTemplate.Page page = storyline.getTemplate().getPage(c);
		String img = 
			((WebZKVisualization)(storyline.getVisualization())).
				getStateUrl(page.getConcept(), VisualizationFactory.PLOT_GEOSURFACE_2D);

		Boolean continuous = (Boolean) state.getMetadata().get(Metadata.CONTINUOUS);
		HashMap<IConcept, Integer> ranks = Metadata.getClassMappings(state.getMetadata());

		ZK.ZKComponent selector = null;
		int rwid = rightW - 6 - 328;
		
		if (continuous != null && continuous) {
			
			/*
			 * class selector for continuous
			 * Image w/labels 
			 * Separator
			 * Range selector <All><Range><Greater than><Lower than><Equal to>
			 * 		Set To
			 * 		<value> (show range)
			 * Separator
			 * In <Chosen poly><Area>
			 * Separator
			 * <OK><Cancel>
			 */
			selector = 
				ZK.hbox();
			
			
		} else if (ranks != null) {

			ColorMap cmap = (ColorMap) state.getMetadata().get(Metadata.COLORMAP);
			HashMap<IConcept, String> legend = VisualizationFactory.get().getClassLegend(state);
			
			/*
			 * create class selector for layer editor
			 * Image (w/ labels)
			 * Separator
			 * Range Selector <All><Only xxx - multiple selection>
			 * Set to: all classes (ribbon) - check out Legend for code
			 * Separator
			 * In: <chosen polygon><whole area> enable/disable according to selection
			 * Separator
			 * <ok> <cancel>
			 */
			selector = 
				ZK.hbox(
					ZK.separator(true),
					ZK.div(ZK.image(img).width(xy.getFirst()).height(xy.getSecond())).
						width(64).align("center"),
					ZK.separator(true),
					// ribbon 
					ZK.div().width(rwid),
					ZK.separator(true),
					// logical filter
					ZK.div().width(128),
					ZK.separator(true),
					// space filter
					ZK.div().width(82),
					ZK.vbox(
						ZK.separator().height(8),
						ZK.button("  OK  ").listener("onClick", new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								/*
								 * collect transformation data from fields
								 */
								layereditor.setVisible(false);
								layerselector.setVisible(true);
							}
						}).width(46),
						ZK.button("Cancel").listener("onClick", new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								layereditor.setVisible(false);
								layerselector.setVisible(true);
							}
						}).width(46)
					).width(48),
					ZK.separator(true)
				);
			
			
		} else {
			// WTF? Probably unnecessary - scenarios on categorizations or probabilities?
		}
		
		ZK.resetComponent(layereditor);
		layereditor.appendChild(selector.get());
		
		layerselector.setVisible(false);
		layereditor.setVisible(true);
		
	}

	
	private ZKComponent createLayerEditor() {
		return ZK.div().id("layereditor");
	}

	private ZKComponent createSpaceSelector() {
		
		ThinklabWebApplication application = _browser.application;
		
		return 
		 	ZK.vbox(
		 		ZK.hbox(
		 			ZK.div(
		 			).align("left"),
		 			ZK.div(
		 				ZK.hbox(
							ZK.imagebutton(TC.url(application, "/images/icons/edit.png"))
								.id("scaddpolyg").
								listener("onClick", new EventListener() {
									@Override
									public void onEvent(Event arg0) throws Exception {
										picmode = AriesBrowser.ADD;
										scvector.setEditControl("polygon");
										painting(false);
									}
								})
								.tooltip("Draw a shape and add it to the selection"),
							ZK.imagebutton(TC.url(application, "/images/icons/cut_disabled.png"))
								.enable(false)
								.id("scsubpolyg").
								listener("onClick", new EventListener() {
									@Override
									public void onEvent(Event arg0) throws Exception {
										picmode = AriesBrowser.SUBTRACT;
										scvector.setEditControl("polygon");
										painting(true);
									}
								})
								.tooltip("Draw a shape and subtract it from the selection"),
							ZK.imagebutton(TC.url(application, "/images/icons/delete_disabled.png"))
								.enable(false)
								.id("scresetdraw").
								listener("onClick", new EventListener() {
									@Override
									public void onEvent(Event arg0) throws Exception {
										picmode = AriesBrowser.IDLE;
										scvector.clearFeatures();
										roi = null;
										idle(false);
									}
								})
								.tooltip("Clear all selections made so far"),
							ZK.image(TC.url(application, "/images/icons/separator.png")),
							ZK.imagebutton(TC.url(application, "/images/icons/world.png")).
								listener("onClick", new EventListener() {
									@Override
									public void onEvent(Event arg0) throws Exception {
										mapState = (mapState + 1) % 4;
										scgoogle.setMapType(AriesBrowser.mapStates[mapState]);
									}
								})
								.tooltip("Cycle through map views"))
							)
						).align("right").fillx(),
		 		OLMAPS.map(
		 			OLMAPS.googlelayer().maptype("physical").id("scgoogle"), 
		 			OLMAPS.vectorlayer().drawcontrols(true).id("scvector")
		 		).zoom(2).id("layermap").width(rightW -6).height(midH-120)
			);
	}

	private ZKComponent createParameterEditor() {

		// TODO obviously need to be real, not hard-coded, when I finished
		// demonstrating.
		return ZK.vbox(
			ZK.separator(false).height(8),
			ZK.label("Sequestration relevance threshold"),
			ZK.hbox(
				ZK.label("0"),
				ZK.slider().width(180),
				ZK.label("100 tons C/ha/yr")
				),
			ZK.separator(true).height(8),
			ZK.label("Use relevance threshold"),
			ZK.hbox(
				ZK.label("0"),
				ZK.slider().width(180),
				ZK.label("100 tons C/ha/yr")
				),
			ZK.separator(true).height(8),
			ZK.label("Sink relevance threshold"),
			ZK.hbox(
					ZK.label("0"),
					ZK.slider().width(180),
					ZK.label("100 tons C/ha/yr")
					),
			ZK.separator(true));
		
	}

	private ZK.ZKComponent createPredefinedEditor(IModel model) {
		
		ZK.ZKComponent ret = null;

		if (scenarios.size() > 0) {
			
			String desc = null;
			String label = null;
			
			ZK.ZKComponent[] lst = new ZK.ZKComponent[scenarios.size()];
			int i = 0;
			for (Scenario s : scenarios) {
				if (i == 0) {
					desc = s.getDescription();
					label = Path.getLast(s.getId()).toUpperCase().replaceAll("-", " ");
				}
				lst[i++] = 
					ZK.listitem(Path.getLast(s.getId()).toUpperCase().replaceAll("-", " "), s);
			}
			
			if (desc == null || desc.equals("")) {
				desc = "No description was given for this scenario.";
			}
			
			ret = 
				ZK.vbox(
					ZK.listbox(lst).nrows(1).selected(0).mold("select").id("pdef_chooser").
						listener("onSelect",
							new EventListener() {
								@Override
								public void onEvent(Event event) throws Exception {
									Listitem li = (Listitem)
										((SelectEvent)event).getSelectedItems().iterator().next();
									Scenario rid = (Scenario) li.getValue();
									((Text)(get("pdef_desc").getFirstChild())).
										setValue(rid.getDescription());
									((Button)get("pdef_merge")).
										setLabel("Merge " + Path.getLast(rid.getName()).toUpperCase().replaceAll("-", " "));
												
								}
							}).width(leftW-4),
							ZK.text(desc).id("pdef_desc").width(leftW-4).height(topH-60),
							ZK.button("Merge " + label).id("pdef_merge").width(leftW-4).
								listener("onClick", new EventListener() {
									
									@Override
									public void onEvent(Event arg0) throws Exception {
										Listitem li = (Listitem)
											((Listbox)get("pdef_chooser")).getSelectedItems().iterator().next();
										mergeScenario((Scenario) li.getValue());
									}
								})
						);
			
		} else {
			ret = ZK.label("No applicable scenarios");
		}			
		return ret;
	}

	public Scenario getScenario() {
		return scenario;
	}
	
	public void onFeatureAdded$scvector(Event e) {

		boolean userDrawn = (this.picmode == AriesBrowser.ADD || this.picmode == AriesBrowser.SUBTRACT);
		FeatureAddedEvent ev = (FeatureAddedEvent) e;
		
		if (userDrawn) {
			scvector.setEditControl("navigate");
		}
		
		try {
			
			boolean redraw = false;
			ShapeValue shape = 
				new ShapeValue(layermap.getProjectionId() + " " + ev.getFeatureWKT());
			
			/*
			 * happens when user draws too fast, especially with slow
			 * browsers
			 */
			if (!shape.isValid()) {
				if (getRegionOfInterest() == null)
					scvector.clearFeatures();
				return;
			}
			
			if (this.picmode == AriesBrowser.ADD) {
				redraw = addRegionOfInterest(shape);
			} else if (this.picmode == AriesBrowser.SUBTRACT) {
				redraw = true;
				subtractRegionOfInterest(shape);
			}
			
			this.picmode = AriesBrowser.IDLE;
			
			if (redraw) {
				scvector.clearFeatures();
				scvector.addFeature(
						NameGenerator.newName("scsh"), 
						getRegionOfInterest().getWKT());
			}
			
		} catch (ThinklabException e1) {
			throw new ThinklabRuntimeException(e1);
		}
		
		if (userDrawn) {
			idle(roi != null);
		}
	}

	public ShapeValue getRegionOfInterest() throws ThinklabException {
		return roi;
	}

	public void subtractRegionOfInterest(ShapeValue region) throws ThinklabException {
		
		if (roi != null)
			roi = roi.difference(region);
	}
	
	public boolean addRegionOfInterest(ShapeValue region) throws ThinklabException {
		boolean ret = roi != null;
		roi = roi == null ? region : roi.union(region);
		return ret;
	}

	public void resetRegionOfInterest(ShapeValue region) throws ThinklabException {
		roi = region; 	
	}
	
	// set the icons in "painting" mode
	void painting(boolean isScissors) {
		
		scaddpolyg.setImage(TC.url(_browser.application, 
				isScissors ? 
						"/images/icons/edit.png" :
						"/images/icons/edit_active.png"));
		scsubpolyg.setImage(TC.url(_browser.application, 
				isScissors ? 
						"/images/icons/cut_active.png" :
						"/images/icons/cut_disabled.png"));
		
		scresetdraw.setImage(TC.url(_browser.application, 
				"/images/icons/cancel_disabled.png"));
	}
	
	void idle(boolean hasSelection) {

		scaddpolyg.setImage(TC.url(_browser.application, "/images/icons/edit.png"));
		scsubpolyg.setImage(TC.url(_browser.application, 
				hasSelection? "/images/icons/cut.png" : "/images/icons/cut_disabled.png"));
		scresetdraw.setImage(TC.url(_browser.application, 
				hasSelection? "/images/icons/delete.png" : "/images/icons/delete_disabled.png"));
				
		scsubpolyg.setDisabled(!hasSelection);
		scresetdraw.setDisabled(!hasSelection);
		
	}


}
