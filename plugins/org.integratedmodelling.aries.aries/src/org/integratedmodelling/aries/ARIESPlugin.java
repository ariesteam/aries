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
