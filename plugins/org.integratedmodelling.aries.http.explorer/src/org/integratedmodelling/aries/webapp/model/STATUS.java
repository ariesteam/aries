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

package org.integratedmodelling.aries.webapp.model;

import java.util.Hashtable;

import org.integratedmodelling.utils.Pair;

public class STATUS  {
	
	public static String IDLE = "idle";
	public static String INVALID_SHAPE = "invShape";
	public static String DRAWING_ADD_MODE = "addShape";
	public static String DRAWING_SUBTRACT_MODE = "subShape";
	
	private static final long serialVersionUID = -4655379959274919317L;
	static Hashtable<String, Pair<String,String>> _map =
			new Hashtable<String, Pair<String,String>>();
	
	static {
		_map.put(IDLE, 
			new Pair<String,String>
				("Select the region of interest using the map and the drawing toolbar. Click continue when done.", 
				 "/aries/images/status/check.png"));
		_map.put(INVALID_SHAPE, 
				new Pair<String,String>
					("The last shape drawn had some invalid data in it. Please draw it again slowly.", 
					 "/aries/images/status/edit_no.png"));
		_map.put(DRAWING_ADD_MODE, 
				new Pair<String,String>
					("Draw a shape to be added to the selection. Shift-click to draw straight lines.", 
					 "/aries/images/status/edit.png"));
		_map.put(DRAWING_SUBTRACT_MODE, 
				new Pair<String,String>
					("Draw a shape to be subtracted from the selection. Shift-click to draw straight lines.", 
					 "/aries/images/status/edit.png"));
	}
	
	public static Hashtable<String, Pair<String,String>> get() {
		return _map;
	}

}
