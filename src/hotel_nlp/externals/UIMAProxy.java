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
  public ClassLoader dcl = null; 

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
    dcl = Thread.currentThread().getContextClassLoader();  //should return Clojure's dynamic-classloader
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
    Object trueInput = null;
    

    try{  
        extfn =  (IFn)Class.forName(strextfn,  true, dcl).newInstance();
        postfn = (IFn)Class.forName(strpostfn, true, dcl).newInstance();     
    }
    catch (Exception e) {System.err.println("Exception ignored! extractor/post fns remain null...hopefully the annotator can handle extracting from and writing to the CAS.");} 


     try{ annfn =  (IFn)Class.forName(strannfn,  true, dcl).newInstance(); } //this is the important one - cannot be null
     catch (Exception e) {
      throw new RuntimeException(e);
    }
   
   Object result = null;
   if (extfn != null){
     trueInput = extfn.invoke(jcas, context); //extractor should be a function that accepts at least a JCas object - context can be null
     result = annfn.invoke(trueInput); //we've now sepearted our component completely from UIMA - it only expects trueInput
    }
    else 
      result = annfn.invoke(jcas);  //assuming consumer has bundled up all the functionality in his ann-fn
   
   resultMap.put("result", result); //for testing

   if (extfn != null)
   postfn.invoke(jcas, result, trueInput);

  }
}
