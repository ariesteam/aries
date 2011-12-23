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

package org.integratedmodelling.aries;

import java.io.File;

import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESPlugin extends ThinklabPlugin {

    public static final String PLUGIN_ID = "org.integratedmodelling.aries.aries";

    public static ARIESPlugin get() {
        return (ARIESPlugin) getPlugin(PLUGIN_ID);
    }

    @Override
    protected void load(KnowledgeManager km) throws ThinklabException {

        /*
         * official gazetteers
         */
        Geospace.get().loadGazetteersFromDirectory(getScratchPath());

        /*
         * load any personal gazetteers. Use env variable THINKLAB_GAZETTEERS_HOME to change default
         * of user home.
         */
        String ghome = System.getenv("ARIES_GAZETTEERS_HOME");
        String gdir  = System.getProperty("user.home");
        if (ghome != null)
            gdir = ghome;

        Geospace.get().loadGazetteersFromDirectory(new File(gdir));

    }

    @Override
    protected void unload() throws ThinklabException { }

}
