package hotel_nlp.externals;

import java.util.Map;
import java.util.HashMap;
import org.uimafit.component.JCasAnnotator_ImplBase; 
import org.apache.uima.analysis_engine.AnalysisEngineProcessException;
import org.apache.uima.resource.ResourceInitializationException;
import org.apache.uima.jcas.JCas;
import org.apache.uima.UimaContext;
import org.uimafit.descriptor.ConfigurationParameter;
//import clojure.lang.DynamicClassLoader;
import clojure.lang.IFn;

public class UIMAProxy extends JCasAnnotator_ImplBase{

  private UimaContext context;
  public static final Map<String, Object> resultMap = new HashMap<String, Object>();
  public static final String PARAM_POSTFN = "postfn-parameter";
  public static final String PARAM_ANNFN = "annfn-parameter";
  public static final String PARAM_EXTFN = "extfn-parameter";

  @ConfigurationParameter(name = PARAM_POSTFN) //the namespace string
  private String postfn;

  @ConfigurationParameter(name = PARAM_EXTFN) //the input-extractor-fn-class string
  private String extfn;

  @ConfigurationParameter(name = PARAM_ANNFN) //the annotator-fn-class string
  private String annfn;

 
  @Override
  public void initialize(final UimaContext context) throws ResourceInitializationException {
    super.initialize(context);
    this.context = context;
  }    


  @Override 
  public void process(JCas aJCas) throws AnalysisEngineProcessException{

      try{ doActualWork(extfn, annfn, postfn, context, aJCas); }
      catch (Exception e){
           throw new AnalysisEngineProcessException(e);
        }
        
  }
  //helper method to do all the interop stuff
   private void doActualWork (String strextfn, String strannfn, String strpostfn, UimaContext context, JCas jcas){
    IFn extfn = null; 
    IFn annfn = null;
    IFn postfn = null;
    ClassLoader dcl = Thread.currentThread().getContextClassLoader();  //should return Clojure's dynamic-classloader

     try{  
         extfn =  (IFn)Class.forName(strextfn,  true, dcl).newInstance();
         postfn = (IFn)Class.forName(strpostfn, true, dcl).newInstance();
         annfn =  (IFn)Class.forName(strannfn,  true, dcl).newInstance();
    }
    catch (Exception e) {
      throw new RuntimeException(e);
    }

   Object trueInput = extfn.invoke(jcas, context); //extractor should be a function that accepts at least a JCas object - context can be null
   Object result = annfn.invoke(trueInput); //we've now sepearted our component completely from UIMA
   
   resultMap.put("result", result); //for testing
   postfn.invoke(jcas, result, trueInput);

  }
}
