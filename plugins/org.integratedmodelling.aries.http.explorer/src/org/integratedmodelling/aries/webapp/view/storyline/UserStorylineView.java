package org.integratedmodelling.aries.webapp.view.storyline;

import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.util.ArrayList;

import org.integratedmodelling.aries.webapp.view.AriesBrowser;
import org.integratedmodelling.aries.webapp.view.STYLE;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.ZK.ZKComponent;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.image.ImageUtil;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;

/**
 * Describe a case study (full user session) in its general aspects: show the area, the
 * allowed other storylines below this one, etc. 
 * 
 * @author Ferdinando
 *
 */
public class UserStorylineView extends StorylineView {

	private static final long serialVersionUID = 1453718315283730905L;

	private String covUrl = null;

	int patch;
	
	public UserStorylineView(Storyline s, AriesBrowser browser) {
		super(s, browser);
		try {
			render();
		} catch (ThinklabException e) {
			throw new ThinklabRuntimeException(e);
		}
	}

//	protected String getImage() throws ThinklabException {
//		
//		if (covUrl == null) {
//			
//			Pair<String, String> zr =
//				browser.getApplication().getNewResourceUrl(".png", browser.getSession());
//
//			Pair<String, String> wr =
//				browser.getApplication().getNewResourceUrl(".png", browser.getSession());
//			/*
//			 * create context image and world locator
//			 */
//			ShapeValue space = ((ArealExtent) storyline.getContext().getSpace()).getShape();
//		
//			Pair<Integer,Integer> xy = 
//				VisualizationFactory.get().getPlotSize(VIEWPORT_X-2, VIEWPORT_Y-2, storyline.getContext());
//			
//			ImageUtil.saveImage(
//				GeoImageFactory.get().getImagery(space, xy.getFirst(), xy.getSecond(), 0),
//				zr.getFirst());
//			
//			patch = (VIEWPORT_Y - 2 - xy.getSecond())/2;
//			
//			covUrl = zr.getSecond();
//
//		}
//		return covUrl;
//	}

	protected String getImage() throws ThinklabException {
		
		if (covUrl == null) {
			
			Pair<Integer,Integer> xy = 
				VisualizationFactory.get().getPlotSize(vx-2, vy-2, storyline.getContext());
			patch = (vy - 2 - xy.getSecond())/2;
			
			Pair<String, String> zr =
				browser.getApplication().getNewResourceUrl(".png", browser.getSession());

			/*
			 * create coverage image
			 */
			BufferedImage bim = storyline.getCoverageMap(xy.getFirst(), xy.getSecond());
			if (bim != null) {
				ImageUtil.saveImage(bim, zr.getFirst());
			}
			
			patch = (vy - 2 - xy.getSecond())/2;
			covUrl = zr.getSecond();

		}
		return covUrl;
	}

	@Override
	protected ZKComponent getRibbon(int width, int height) {
		
		ArrayList<ZKComponent> badges = new ArrayList<ZK.ZKComponent>();
		
		for (int i = 0; i < storyline.getChildCount(); i++) {
			final Storyline m = (Storyline) storyline.getChildAt(i);
			if (storyline.getContext() != null && m.isCovered())
				badges.add(getBadge(220,
						"/images/icons/globe48c.png", 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"There are models and data available to compute " +
							m.getTemplate().getTitle() + 
							" in this region. Click to switch to its information page.",
						new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								browser.showStoryline(m, null);
							}
						}).tmargin(6));
		}
		for (int i = 0; i < storyline.getChildCount(); i++) {
			Storyline m = (Storyline) storyline.getChildAt(i);
			if (storyline.getContext() == null || !m.isCovered())
				badges.add(getBadge(220,
						"/images/icons/globe48.png", 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"There is not enough information to compute " + 
							m.getTemplate().getTitle() + 
							" in this region.",
						null).tmargin(6));
		}
		return ZK.ribbon(width, height, 220, badges.toArray(new ZKComponent[badges.size()]));
	}


	@Override
	protected ZKComponent getImageArea(int width, int height) throws ThinklabException {
		
		String img = getImage();
		Pair<Double, Double> aa = storyline.getCoverageStatistics();
		DecimalFormat df = new DecimalFormat("#.#");

		float atot = 0.0f, perc = 0.0f;
		if (aa != null) {
			atot = (float)(aa.getFirst()/1000000.0);
			perc = aa.getSecond().floatValue();
		}
		
		return 
			ZK.div(
				ZK.vbox(
					ZK.spacer(12),
					ZK.label(
						"  Total area: " + 
						df.format(atot) + 
						" km\u00B2. Covered by models: " +
						df.format(perc) +
						"%").sclass(STYLE.TEXT_VERYSMALL), 
					ZK.separator().height(patch),
					ZK.image(img))).
				align("center").
				sclass(STYLE.TEXTURED_SUNK).
				width(width).
				height(height);
	}

}
