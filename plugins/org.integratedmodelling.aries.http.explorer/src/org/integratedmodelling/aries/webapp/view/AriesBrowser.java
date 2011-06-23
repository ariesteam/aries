package org.integratedmodelling.aries.webapp.view;

import java.io.FileNotFoundException;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.integratedmodelling.aries.webapp.ARIESWebappPlugin;
import org.integratedmodelling.aries.webapp.model.ARIESUserModel;
import org.integratedmodelling.aries.webapp.model.ARIESUserModel.Location;
import org.integratedmodelling.aries.webapp.model.AriesModule;
import org.integratedmodelling.aries.webapp.model.STATUS;
import org.integratedmodelling.aries.webapp.view.storyline.ModelStorylineView;
import org.integratedmodelling.aries.webapp.view.storyline.ModuleStorylineView;
import org.integratedmodelling.aries.webapp.view.storyline.StorylineControlPanel;
import org.integratedmodelling.aries.webapp.view.storyline.StorylineView;
import org.integratedmodelling.aries.webapp.view.storyline.UserStorylineView;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.extents.ArealExtent;
import org.integratedmodelling.geospace.implementations.observations.RasterGrid;
import org.integratedmodelling.geospace.literals.ShapeValue;
import org.integratedmodelling.modelling.storyline.ModelStoryline;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.olmaps.OLmaps;
import org.integratedmodelling.olmaps.event.FeatureAddedEvent;
import org.integratedmodelling.olmaps.layer.GoogleMapsLayer;
import org.integratedmodelling.olmaps.layer.VectorLayer;
import org.integratedmodelling.searchengine.exceptions.ThinklabInvalidQueryException;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabIOException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.http.ThinkWeb;
import org.integratedmodelling.thinklab.http.ThinklabWebSession;
import org.integratedmodelling.thinklab.http.application.ThinklabWebApplication;
import org.integratedmodelling.thinklab.http.geospace.zk.OLMAPS;
import org.integratedmodelling.thinklab.interfaces.query.IQueryResult;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.view.components.Sidebar;
import org.integratedmodelling.thinklab.webapp.view.components.SidebarModule;
import org.integratedmodelling.thinklab.webapp.view.components.ThinkcapCometComponent;
import org.integratedmodelling.thinklab.webapp.view.components.ThinkcapComponent;
import org.integratedmodelling.utils.NameGenerator;
import org.integratedmodelling.utils.Pair;
import org.opengis.referencing.operation.TransformException;
import org.zkoss.zk.ui.Sessions;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;
import org.zkoss.zul.AbstractListModel;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Filedownload;
import org.zkoss.zul.Hbox;
import org.zkoss.zul.Image;
import org.zkoss.zul.ListModel;
import org.zkoss.zul.ListSubModel;
import org.zkoss.zul.SimpleListModel;
import org.zkoss.zul.Toolbarbutton;
import org.zkoss.zul.Vbox;
import org.zkoss.zul.Window;

import com.vividsolutions.jts.geom.Coordinate;

public class AriesBrowser extends ThinkcapCometComponent {

	private static final long serialVersionUID = -5040020286520380177L;

	// modes for polygon drawing
	static int IDLE = 0;
	static int ADD = 1;
	static int SUBTRACT = 2;
	
	private int mapState = 0;
	
	/*
	 * 0 = still selecting; 1 = selection made, storylines shown
	 */
	private int browserState = 0;
	
	static final String[] mapStates = {"physical", "normal", "hybrid", "satellite"};

	public static final int MAPWIDTH = 1000;
	
	// the corresponding view (may be null)
	private ArrayList<SidebarModule> moduleViews = new ArrayList<SidebarModule>();

	// automatically wired components
	Hbox browser   = null;            // the whole browser hbox
	GoogleMapsLayer google = null;    // the google maps layer
	private VectorLayer     vector = null;    // vector layer that user draws on
	VectorLayer     coverage = null;  // vector layer to show coverage and other info
	OLmaps          map = null;       // the main map containing the layers
	Sidebar         sidebar = null;   // container for module views
	ARIESStatusBar  statusbar = null; // status bar
	Combobox		gazetteer = null; // gazetteer search bar
	ScenarioEditor  sceditor = null;  // scenario editor
	Window          tableview = null; // view data as numbers
	Window          storyline = null; // container for storylines
	Vbox			mapwindow = null; // entire map selection window with toolbars
	
	// buttons
	Toolbarbutton addpolyg = null;
	Toolbarbutton subpolyg = null;
	Toolbarbutton resetdraw = null;
	Toolbarbutton run = null;
	Toolbarbutton view = null;
	Image         statusimg = null;
	
	ThinklabWebSession session = null;
	ThinklabWebApplication application = null;
	ARIESUserModel userModel = null;
	
	private int picmode = IDLE;
	private SelectionBar selectionBar;

	private Storyline currentStoryline = null;	
	private HashMap<Storyline,StorylineView> storylineViews = 
		new HashMap<Storyline, StorylineView>();

	private AriesBanner banner;

	boolean shapeShown;
	
	/*
	 * list model for gazetteer auto-complete functionality
	 */
	class GazModel extends AbstractListModel implements ListSubModel {
		
		private static final long serialVersionUID = -6800680505557782481L;
		private List<ARIESUserModel.Location> defaultValues = null;
		
		GazModel(List<ARIESUserModel.Location> defaultLocations) {
			this.defaultValues = defaultLocations;
		}
		
		@Override
		public Object getElementAt(int index) {
			return this.defaultValues.get(index);
		}

		@Override
		public int getSize() {
			return this.defaultValues.size();
		}

		@Override
		public ListModel getSubModel(Object value, int nRows) {
			
			final String name = value == null ? "" : value.toString().trim();
			if (nRows < 0) nRows = 20;
			
			ArrayList<ARIESUserModel.Location> ret = new ArrayList<ARIESUserModel.Location>();
			
			if (name.isEmpty() && defaultValues != null) {
				ret.addAll(defaultValues);
			} else if (!name.isEmpty()) {
				try {					
					IQueryResult qr = null;
					try {
						qr = Geospace.get().lookupFeature(name);
					} catch (ThinklabInvalidQueryException e) {
						// this will catch query compile errors if users write
						// strange characters or are in between writing complex
						// queries with parentheses
					}
					if (qr != null) {
						for (int i = 0; i < nRows && i < qr.getTotalResultCount(); i++) {
							
							ARIESUserModel.Location r = new ARIESUserModel.Location();
							r.id          = qr.getResultField(i, "id");
							r.label       = qr.getResultField(i, "label");
							r.description = qr.getResultField(i, "description");
							ret.add(r);
						}
					}
				} catch (ThinklabException e) {
					throw new ThinklabRuntimeException(e);
				}
			}			
			ListModel rt = new SimpleListModel(ret);
			return rt;
		}		
	}
	
	
	/**
	 * The bar that gives access to modeling, export and other functions after the case study area
	 * is defined.
	 * @author Ferdinando
	 */
	public class SelectionBar extends ThinkcapComponent {

		private static final long serialVersionUID = -8887549561186071008L;
		
		@Override
		public void initialize() {
			
			this.setContent(ZK.hbox(
					
					ZK.div(
						ZK.hbox(
							ZK.combobox().
								listmodel(new GazModel(userModel.getDefaultLocations())).
								listener(new EventListener() {
									@Override
									public void onEvent(Event arg0) throws Exception {
										if (((Combobox)arg0.getTarget()).getSelectedItem() != null) {
											ARIESUserModel.Location chosen = 
												(Location) ((Combobox)arg0.getTarget()).getSelectedItem().getValue();
											setRegionFromGazetteer(chosen.id.toString());
										} else {
											/*
											 * see if we put in a value and were too fast to have it searched
											 */
											String s = ((Combobox)arg0.getTarget()).getValue().trim();
											if (s != null && !s.isEmpty()) {
												setRegionFromGazetteer(s);												
											}
										}
									}
								}).
								id("gazetteer").
								width(280)
							/*,
							 * 
							 * TODO
							 * these should be re-enabled when they work.
							 * 
							ZK.imagebutton("/images/icons/folder_open.png")
								.tooltip("load a previously saved configuration"),
							ZK.imagebutton("/images/icons/save.png")
								.tooltip("save the current selection for future reference") */
						)
					).align("left"),	
					ZK.div(
						ZK.hbox(
							ZK.image("/images/icons/separator.png"),
							ZK.imagebutton("/images/icons/edit.png")
								.id("addpolyg")
								.tooltip("Draw a shape and add it to the selection"),
							ZK.imagebutton("/images/icons/cut_disabled.png")
								.enable(false)
								.id("subpolyg")
								.tooltip("Draw a shape and subtract it from the selection"),
							ZK.imagebutton("/images/icons/delete_disabled.png")
								.enable(false)
								.id("resetdraw")
								.tooltip("Clear all selections made so far"),
								ZK.image("/images/icons/separator.png"),
							ZK.imagebutton("/images/icons/play_disabled.png")
								.enable(false)
								.id("run")
								.tooltip("Accept selection and continue to assessment"))
					).align("right").width("100%")	
				).width(MAPWIDTH).height(24).sclass(STYLE.MAP_TOOLBAR));
		}
		
		// set the icons in "painting" mode
		void painting(boolean isScissors) {
			
			addpolyg.setImage( 
					isScissors ? 
							ZK.fixUrl("/images/icons/edit.png") :
							ZK.fixUrl("/images/icons/edit_active.png"));
			subpolyg.setImage(
					isScissors ? 
							ZK.fixUrl("/images/icons/cut_active.png") :
							ZK.fixUrl("/images/icons/cut_disabled.png"));
			
			resetdraw.setImage(ZK.fixUrl("/images/icons/cancel_disabled.png"));
			
		}
		
		void idle(boolean hasSelection) {

			addpolyg.setImage(ZK.fixUrl("/images/icons/edit.png"));
			subpolyg.setImage(
					hasSelection? 
							ZK.fixUrl("/images/icons/cut.png") : 
							ZK.fixUrl("/images/icons/cut_disabled.png"));
			resetdraw.setImage( 
					hasSelection? 
							ZK.fixUrl("/images/icons/delete.png") : 
							ZK.fixUrl("/images/icons/delete_disabled.png"));
			
			run.setImage(
					(hasSelection) ? 
							ZK.fixUrl("/images/icons/play.png") : 
							ZK.fixUrl("/images/icons/play_disabled.png"));
			
			subpolyg.setDisabled(!hasSelection);
			resetdraw.setDisabled(!hasSelection);
			run.setDisabled(!hasSelection);
			
		}
	}
	
	public AriesBrowser() {
		
		super();
		// get these while we are in the main thread.
		session = ThinkWeb.getThinkcapSession(Sessions.getCurrent());
		application = session.getApplication();
		userModel = (ARIESUserModel)session.getUserModel();
	}
	
	public ThinklabWebSession getSession() {
		return session;
	}
	
	public AriesBanner getBanner() {
		
		if (banner == null) {
			banner = (AriesBanner) ZK.getComponentById(this.getParent(), "top");
		}
		return banner;
	}
	
	public void addModuleView(AriesModuleView view) {
		
		moduleViews.add(view);
		
		if (view != null) {
			sidebar.addModule(view);
		}
	}
	
	public OLmaps getMap() {
		return map;
	}
	
	@Override
	public void initialize() {
		
		this.selectionBar = new SelectionBar();
		
		selectionBar.afterCompose();
		
		/*
		 * build basic UI, same for all usages.
		 */
		this.setContent(
			ZK.div(
  			  ZK.hbox(
				ZK.sidebar(270,700).
					headersclass(STYLE.MODULE_HEADER).
					labelsclass(STYLE.MODULE_TITLE, STYLE.MODULE_TITLE_DISABLED).
					headersize(24).id("sidebar"),
				ZK.separator(false).width(10).height(700).sclass(STYLE.SEPARATOR),
				ZK.vbox(
					ZK.c(selectionBar),
					OLMAPS.map(
						OLMAPS.googlelayer().maptype("physical").id("google"),
						OLMAPS.vectorlayer().drawcontrols(true).id("vector")
					).zoom(2).center(1073500.73, 4482706.85).height(700).width(MAPWIDTH).id("map"),
					ZK.window().width(MAPWIDTH).height(700).bgcolor("#cccccc").color("#000000").
								scroll().id("tableview").hide(),
					ZK.c(new ARIESStatusBar(STATUS.get(), this)).
						id("statusbar").height(28).width("100%"),
					ZK.imagebutton("/aries/images/world48.png")
						.id("view")
						.tooltip("Toggle world views").sclass("mapbutton")
				).spacing(0).id("mapwindow"),
				
				/*
				 * container for storylines, to be shown when we have made our selection
				 */
				ZK.window().width(MAPWIDTH).height(760).id("storyline").hide()

			).id("browser").spacing(0),
			
			/*
			 * container for scenario editor - FIXME this should be just in the storyline window
			 */
			ZK.c(new ScenarioEditor(this,1270,760)).id("sceditor").hide()));
	}
	
	public void onClick$view(Event e) {
		mapState = (mapState + 1) % 4;
		google.setMapType(mapStates[mapState]);
	}
	
	public void onClick$reset(Event e) {
		try {
			resetCaseStudy();
		} catch (ThinklabException e1) {
			throw new ThinklabRuntimeException(e1);
		}
	}
	
	public void onClick$run(Event e) {

		getVectorLayer().clearFeatures();
		shapeShown = false;
		
		selectionBar.setVisible(false);
		
		/*
		 * 	1. prepare info page for case study with area picture, list of possible storylines with explanations,
		 * 	   instructions for use, and pointers to web site where it counts. Also: form to save area for later,
		 * 	   disabled if not logged in.
		 * 
		 *  2. Fill in enabled modules with storylines. Only show enabled ones with option to show those that are
		 *     not enabled.
		 *     
		 *  3. Each storyline badge should bring up main page for storyline when clicked. That should have a main area
		 *     for the ES and one for the storyline-specific info. Model should look for the components and get
		 *     the descriptions from the concepts. Links to web site wherever necessary. Page has "start" etc on
		 *     it. Instead of list of pages, have "Computation is ongoing" or "Computation is waiting" or 
		 *     "Computation is not active (activate)". When computed. the area becomes the index for the result
		 *     pages.
		 *     
		 *  4. All possible downloads should be clearly marked on storyline page.
		 */

		try {
			userModel.startAnalysis();
		} catch (ThinklabException e1) {
			throw new ThinklabRuntimeException(e1);
		}
		
		browserState = 1;

		/* 
		 * create view for all storylines and collect in storylineViews map
		 * 
		 * show all module storylines in their modules, activate those with > 0 storylines, 
		 * and show each model storyline in each module.
		 */
		getBanner().setStoryline(userModel.getStoryline());
		
		for (SidebarModule view : moduleViews) {
			view.display();
		}
		
		/*
		 * create view for user storyline and display it. The map window becomes invisible until
		 * session reset.
		 */
		ZK.resetComponent(this.storyline);
		storylineViews.clear();
		currentStoryline = null;
		
		addStorylineView(
				userModel.getStoryline(),
				new UserStorylineView(userModel.getStoryline(), this));
		
		this.mapwindow.setVisible(false);
		this.storyline.setVisible(true);
		showStoryline(userModel.getStoryline(), null);
	}

	public boolean isContextSet() {
		return browserState == 1;
	}
	
	/*
	 * Called through reflection to refresh all components that have to do with the 
	 * associated storyline.
	 */
	public void refreshStoryline(String c) {
		
		Storyline ss = userModel.getStoryline().findStoryline(c);
		
		sidebar.redisplay();
		if (currentStoryline != null && ss.equals(currentStoryline)) {
			currentStoryline = null;
			showStoryline(ss, null);		
		}
	}
	
	/**
	 * Show an arbitrary shape on the world map. Only one shape can be seen at a
	 * time. If the passed shape is null, just remove whatever shape is shown.
	 * 
	 * @param sh
	 */
	public void showRegion(ShapeValue sh) {
		
		if (shapeShown) {
			getVectorLayer().removeFeature("selection");
			shapeShown = false;
		}
		
		if (sh != null) {
			try {
				sh = sh.transform(Geospace.get().getGoogleCRS());
			} catch (ThinklabException e) {
				throw new ThinklabRuntimeException(e);
			}
		
			moveMapTo(map, sh);
			
			getVectorLayer().clearFeatures();
			getVectorLayer().addFeature(
					"selection", 
					sh.getWKT());
		
			this.shapeShown = true;
		}
	}
	
	public void setRegionFromGazetteer(String s) {
		
		if (s != null && !s.equals("")) {
			
			try {
				ShapeValue sh = 
					Geospace.get().retrieveFeature(s);
				if (sh != null) {

					sh = sh.transform(Geospace.get().getGoogleCRS());
					
					userModel.resetRegionOfInterest(sh);
					selectionBar.idle(true);
					moveMapTo(map, sh);
					
					getVectorLayer().clearFeatures();
					getVectorLayer().addFeature(
						"sel", 
						sh.getWKT());					
				}
			} catch (ThinklabException e1) {
				throw new ThinklabRuntimeException(e1);
			}
		}
		
	}
		
	public void onClick$addpolyg(Event e) {
		
		selectionBar.painting(false);
		this.picmode = ADD;
		setStatus(STATUS.DRAWING_ADD_MODE);
		getVectorLayer().setEditControl("polygon");
	}
	
	private void createModuleViews() {
		
		for (AriesModule module : userModel.getAllModules()) {		
			
			/*
			 * use the class specified in the module storyline if any is
			 * given; if not, create a stock AriesModule.
			 */
			AriesModuleView mod = null;
			String scl = 
				module.getStoryline().getTemplate().
					getDefault("module-sidebar-view-class", 
							   AriesModuleView.class.getCanonicalName());
			try {
				Class<?> amc = Class.forName(scl, true, ARIESWebappPlugin.get().getClassLoader());
	            Constructor<?> ct = amc.getConstructor(AriesModule.class, AriesBrowser.class);
				mod = (AriesModuleView) ct.newInstance(module, this);
			} catch (Exception e) {
				throw new ThinklabRuntimeException(e);
			}

			addModuleView(mod);
		}

	}
	
	public void resetCaseStudy() throws ThinklabException {
		
		storylineViews.clear();
		currentStoryline = null;
		
		/*
		 * reset ROI and all modules
		 */
		userModel.clearAnalysis();
		
		/*
		 * ask the user model to give us all the modules we have access to.
		 */
		userModel.initializeModules();
		
		moduleViews.clear();
		sidebar.resetModules();
		createModuleViews();
		
		/*
		 * restore interface
		 */
		selectionBar.setVisible(true);
		selectionBar.idle(false);
		gazetteer.setValue("");
		getBanner().setStoryline(null);
		
		ZK.resetComponent(this.storyline);
		
		this.storyline.setVisible(false);
		this.mapwindow.setVisible(true);

		browserState = 0;
		sidebar.redisplay();
		
	}
	
	public void onClick$subpolyg(Event e) {
		
		selectionBar.painting(true);
		this.picmode = SUBTRACT;
		setStatus(STATUS.DRAWING_SUBTRACT_MODE);
		getVectorLayer().setEditControl("polygon");
	}

	public void onClick$resetdraw(Event e) {

		selectionBar.idle(false);
		
		this.picmode = IDLE;
		setStatus(STATUS.IDLE);

		getVectorLayer().clearFeatures();
		try {
			userModel.resetRegionOfInterest(null);
		} catch (ThinklabException e1) {
			throw new ThinklabRuntimeException(e1);
		}
	}
	
	public void onFeatureAdded$vector(Event e) {

		boolean userDrawn = (this.picmode == ADD || this.picmode == SUBTRACT);
		FeatureAddedEvent ev = (FeatureAddedEvent) e;
		
		if (userDrawn) {
			selectionBar.idle(true);
			getVectorLayer().setEditControl("navigate");
		}
		
		try {
			
			boolean redraw = false;
			ShapeValue shape = 
				new ShapeValue(map.getProjectionId() + " " + ev.getFeatureWKT());
			
			/*
			 * happens when user draws too fast, especially with slow
			 * browsers
			 */
			if (!shape.isValid()) {
				setStatus(STATUS.INVALID_SHAPE);
				if (userModel.getRegionOfInterest() == null)
					getVectorLayer().clearFeatures();
				return;
			}
			
			if (this.picmode == ADD) {
				redraw = userModel.addRegionOfInterest(shape);
			} else if (this.picmode == SUBTRACT) {
				redraw = true;
				userModel.subtractRegionOfInterest(shape);
			}
			
			this.picmode = IDLE;
			
			if (redraw) {
				getVectorLayer().clearFeatures();
				getVectorLayer().addFeature(
						NameGenerator.newName("sh"), 
						userModel.getRegionOfInterest().getWKT());
			}
			
		} catch (ThinklabException e1) {
			throw new ThinklabRuntimeException(e1);
		}
		
		if (userDrawn)
			setStatus(STATUS.IDLE);
	}

	public void setStatus(String status) {
		statusbar.set(status);
	}

	@Override
	public void postInitialize() {
		
		userModel.initializeModules();
		createModuleViews();
		sidebar.afterCompose(); 
		sidebar.redisplay();
		statusbar.initialize();
		start(); // get ready to process messages
	}
	
	public void moveMapTo(OLmaps mp, RasterGrid grid) {
		
		Coordinate p1 = null, p2 = null;
		try {
			p1 = JTS.transform(
					new Coordinate(grid.getLeft(), grid.getBottom()), null, ARIESWebappPlugin.get().geoToGoogleTransform);
			p2 = 
				JTS.transform(
					new Coordinate(grid.getRight(), grid.getTop()), null, ARIESWebappPlugin.get().geoToGoogleTransform);
			
		} catch (TransformException e) {
			// shouldn't happen
			throw new ThinklabRuntimeException(e);
		}
		
		mp.setBounds(p1.x, p1.y, p2.x, p2.y);
	}
	
	public static void moveMapTo(OLmaps mp, ShapeValue value) {
		
		Coordinate p1 = null, p2 = null;
		ShapeValue env = new ShapeValue(value.getEnvelope());
		try {
			env = env.transform(Geospace.get().getStraightGeoCRS());
		} catch (ThinklabException e2) {
			throw new ThinklabRuntimeException(e2);
		}
		ReferencedEnvelope e = env.getEnvelope();
		
		try {
			p1 = JTS.transform(
					new Coordinate(e.getMinX(), e.getMinY()), null, ARIESWebappPlugin.get().geoToGoogleTransform);
			p2 = 
				JTS.transform(
					new Coordinate(e.getMaxX(), e.getMaxY()), null, ARIESWebappPlugin.get().geoToGoogleTransform);
			
		} catch (TransformException e1) {
			// shouldn't happen
			throw new ThinklabRuntimeException(e1);
		}
		
		mp.setBounds(p1.x, p1.y, p2.x, p2.y);
	}

	public ThinklabWebApplication getApplication() {
		return application;
	}

	public ARIESUserModel getUserModel() {
		return userModel;
	}

	public VectorLayer getVectorLayer() {
		return vector;
	}
	
	public StorylineView createStorylineView(Storyline sl, StorylineControlPanel badge) {
		
		StorylineView view = null;
		
		/*
		 * create storyline view appropriately. User view is already there.
		 */
		Class<? extends StorylineView> moduleViewClass = 
			ARIESWebappPlugin.get().
		 		getViewClass(sl.getObservable());

		if (moduleViewClass != null) {
			try {
				view = moduleViewClass.newInstance();
			} catch (Exception e) {
				throw new ThinklabRuntimeException(e);
			}
			if (view instanceof ModelStorylineView)
				((ModelStorylineView)view).initialize(sl, this, badge);
			else
				view.initialize(sl, this);
		}

		if (view == null) {
			if (sl instanceof ModelStoryline) {
				view = new ModelStorylineView(sl, this, badge);			
			} else {
				view = new ModuleStorylineView(sl, this);
			}
		}
		
		addStorylineView(sl, view);
		
		return view;
	}
	
	/**
	 * Called by other components to show, and if necessary create, a view for the storyline
	 * corresponding to a given concept.
	 * 
	 * @param storyline
	 */
	public void showStoryline(Storyline storyline, StorylineControlPanel badge) {
		
		if (currentStoryline != null && storyline.equals(currentStoryline)) {
			return;
		}

		if (currentStoryline != null)
			storylineViews.get(currentStoryline).setVisible(false);
		
		if (!storylineViews.containsKey(storyline)) {
			createStorylineView(storyline, badge);
		}
		
		storylineViews.get(storyline).setVisible(true);		
		currentStoryline = storyline;
		ZK.resetComponent(this.storyline);
		this.storyline.appendChild(storylineViews.get(storyline));
	}
	
	private void addStorylineView(Storyline storyline, StorylineView sl) {
		
		sl.setVisible(false);
		storylineViews.put(storyline, sl);	
	}
	
	public void showCoverage(ShapeValue shape) {
		showRegion(shape);
	}

	public void exportKML() throws ThinklabException {
		
		Pair<String, String> uncu = application.getNewResourceUrl(".kmz", session);
		userModel.getStoryline().export(uncu.getFirst());
		try {
			Filedownload.save(uncu.getSecond(), "application/vnd.google-earth.kmz");
		} catch (FileNotFoundException e) {
			throw new ThinklabIOException(e);
		}
	}
	
	public void createScenario(ModelStoryline storyline) throws ThinklabException {
		
		browser.setVisible(false);
		sceditor.setVisible(true);
		sceditor.setStoryline(storyline);
		sceditor.setLocation(
				map.getZoom(), 
				(ArealExtent) storyline.getContext().getSpace());
	}
	
	// temp - this should be passed what the editor created, or null
	public void addScenario(IContext editor) throws ThinklabException {
		
		/*
		 * if editor is not null, do something
		 */
		sceditor.setVisible(false);
		browser.setVisible(true);
	}	
} 
