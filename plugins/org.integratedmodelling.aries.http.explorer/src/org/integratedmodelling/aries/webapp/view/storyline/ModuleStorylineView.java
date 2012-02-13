/*
Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)

This file is part of ARIES.

ARIES is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License,
or (at your option) any later version.

ARIES is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
*/

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

public class ModuleStorylineView extends StorylineView {

	private static final long serialVersionUID = 916338709034161292L;

	int patch;
	String covUrl = null;
	
	public ModuleStorylineView(Storyline s, AriesBrowser browser) {
		super(s, browser);
		try {
			render();
		} catch (ThinklabException e) {
			throw new ThinklabRuntimeException(e);
		}
	}


	@Override
	protected ZKComponent getRibbon(int width, int height) {
		
		ArrayList<ZKComponent> badges = new ArrayList<ZK.ZKComponent>();
		
		for (int i = 0; i < storyline.getChildCount(); i++) {
			
			final Storyline m = (Storyline) storyline.getChildAt(i);
			if (m.getStatus() == Storyline.IDLE) {
				badges.add(getBadge(220,
						ZK.fixUrl("/aries/images/icons/check48.png"), 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"The " +
							m.getTemplate().getTitle() + 
							" storyline is covered and ready to be computed in this region. Click to show it.",
						new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								browser.showStoryline(m, null);
							}
						}));
			} else if (m.getStatus() == Storyline.COMPUTING) {
				badges.add(getBadge(220,
						ZK.fixUrl("/aries/images/icons/spinner48.png"), 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"The " +
							m.getTemplate().getTitle() + 
							" storyline is computing. Please wait for it to finish.",
						new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								browser.showStoryline(m, null);
							}
						}));
			}  else if (m.getStatus() == Storyline.PENDING) {
				badges.add(getBadge(220,
						ZK.fixUrl("/aries/images/icons/spinner48.png"), 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"The " +
							m.getTemplate().getTitle() + 
							" storyline has been activated and is waiting for its turn to be computed.",
						new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								browser.showStoryline(m, null);
							}
						}));
			} else if (m.getStatus() == Storyline.ERROR) {
				badges.add(getBadge(220,
						ZK.fixUrl("/aries/images/icons/warning48.png"), 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"The " +
							m.getTemplate().getTitle() + 
							" storyline has finished computing with errors.",
						null));
			} else if (m.getStatus() == Storyline.DISABLED) {
				badges.add(getBadge(220,
						ZK.fixUrl("/images/icons/boh48.png"), 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"The " +
							m.getTemplate().getTitle() + 
							" storyline is not covered in this region.",
						null));
			}  else if (m.getStatus() == Storyline.COMPUTED) {
				badges.add(getBadge(220,
						ZK.fixUrl("/aries/images/icons/check48_green.png"), 
						m.getTemplate().getTitle(), 
						m.getTemplate().getRunningHead(), 
						"The " +
							m.getTemplate().getTitle() + 
							" storyline has computed successfully. Click to show it.",
						new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								browser.showStoryline(m, null);
							}
						}));
			} 
			
			badges.get(badges.size()-1).tmargin(6);
		}
		
		return ZK.ribbon(width, height, 220, badges.toArray(new ZKComponent[badges.size()]));

	}

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
