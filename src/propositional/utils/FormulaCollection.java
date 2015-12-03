package propositional.utils;

import propositional.Atom;
import propositional.Formula;

import java.util.*;


import scala.collection.JavaConversions;
import scala.collection.JavaConversions.*;
import scala.collection.JavaConverters;
import scala.collection.JavaConverters.*;

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 29.07.13
 * Time: 14:58
 * To change this template use File | Settings | File Templates.
 */
public class FormulaCollection {

    private LinkedHashMap<Integer,Set<Formula>> frms;

    private HashMap<Formula,List<Integer>> atomDict;
    
    private HashMap<Formula,Boolean> inferredValues;

    private Integer maxKey=0;
    
    private boolean hasEmptyFormula = Atom.toScalaBool(new java.lang.Boolean(false));


    public FormulaCollection(List<scala.collection.immutable.Set<Formula>> frms){
        this.frms=new LinkedHashMap<Integer,Set<Formula>>();
        this.inferredValues=new HashMap<Formula, Boolean>();
        maxKey=0;
        for(scala.collection.immutable.Set<Formula> f: frms){
            if(f.size()==0) hasEmptyFormula = true;
            this.frms.put(maxKey++,scala.collection.JavaConverters.setAsJavaSetConverter(f).asJava());
        }


        this.atomDict=buildAtomsDict();
        unitPropagation();
    }

    /**
     * Konstruktor kopiujacy
     * @param toCopy
     */
    private FormulaCollection(FormulaCollection toCopy){
        this.frms=new LinkedHashMap<Integer, Set<Formula>>(toCopy.frms.size()+1);
        for(Map.Entry<Integer,Set<Formula>> e: toCopy.frms.entrySet()){
            this.frms.put(e.getKey(),JavaConverters.setAsJavaSetConverter(Atom.duplicate(e.getValue())).asJava());
        }


        this.atomDict=new HashMap<Formula, List<Integer>>(toCopy.atomDict.size());

        for(Map.Entry<Formula,List<Integer>> e: toCopy.atomDict.entrySet()){
            this.atomDict.put(e.getKey(),new ArrayList<Integer>(e.getValue()));
        }

        this.inferredValues=new HashMap<Formula, Boolean>();

        for(Map.Entry<Formula,Boolean> e: toCopy.inferredValues.entrySet()){
            this.inferredValues.put(Atom.duplicate(e.getKey()),e.getValue());
        }

        this.maxKey=toCopy.maxKey;
        this.hasEmptyFormula=toCopy.hasEmptyFormula;
    }
    
    private void addToDict(Integer key,Set<Formula> f){
        for(Formula atom: f){
            if(atom instanceof Atom || Atom.isNegatedAtom(atom)){
                if(atomDict.containsKey(atom))
                    atomDict.get(atom).add(key);
                else{
                    ArrayList<Integer> v=new ArrayList<Integer>();
                    v.add(key);
                    atomDict.put(atom,v);
                }
            }
        }
    }

    public Map<Formula,Boolean> getInferredValues(){
        return inferredValues;
    }

    public Formula selectLiteral(){
        Formula maxAt=null;
        Integer maxCount=0;
        for(Map.Entry<Formula,List<Integer>> e: atomDict.entrySet()){
           if(e.getValue().size()>maxCount){
               maxAt=e.getKey();
               maxCount=e.getValue().size();
           }
        }

        return maxAt;
    }

    public FormulaCollection addUnit(Formula unit){
        Set<Formula> f=new HashSet<Formula>();
        f.add(unit);

        FormulaCollection dpl=new FormulaCollection(this);

        dpl.frms.put(dpl.maxKey++, f);
        dpl.addToDict(dpl.maxKey - 1, f);
        dpl.frms=dpl.eliminateUnit(unit, dpl.frms);
        dpl.unitPropagation();

        return dpl;
    }
    
    public boolean isEmpty(){
        return frms.isEmpty();
    }
    
    public boolean hasEmptyFormula(){
         return hasEmptyFormula;
    }

    private LinkedHashMap<Integer,Set<Formula>> eliminateUnit(Formula unit,LinkedHashMap<Integer,Set<Formula>> fMap){
        if(unit instanceof Atom){
            inferredValues.put(unit,true);
        }
        else{
            inferredValues.put(unit,false);
        }
        if(atomDict.containsKey(unit)){
            for(Integer formulaKey: atomDict.get(unit)){
                fMap.remove(formulaKey);
            }
            atomDict.remove(unit);
        }
        
        if(atomDict.containsKey(Atom.negate(unit))){
            for(Integer formulaKey: atomDict.get((Atom.negate(unit)))){

                if(fMap.containsKey(formulaKey)){
                    Set<Formula> f=fMap.get(formulaKey);
                    f=Atom.filterFrom(Atom.negate(unit), f);
                    if(f.size()==0) hasEmptyFormula = true;
                    fMap.put(formulaKey, f);
                }
            }
        }

        return fMap;
    }

    
    private void unitPropagation(){

        frms=propagationProc(0,frms);
    } 


    private LinkedHashMap<Integer,Set<Formula>> propagationProc(Integer k,LinkedHashMap<Integer,Set<Formula>> fMap){
        if(k>=maxKey) return fMap;

        Set<Formula> f= frms.get(k);

        if(f!=null && f.size()==1){
           return propagationProc(k+1,eliminateUnit(f.iterator().next(), fMap));
        }
        else{
           return propagationProc(k+1,fMap);
        }
    }

    private HashMap<Formula,List<Integer>> buildAtomsDict(){
        HashMap<Formula,List<Integer>> ret=new HashMap<Formula, List<Integer>>();

        for(Map.Entry<Integer,Set<Formula>> e: frms.entrySet()){
            for(Formula atom: e.getValue()){
                if(atom instanceof Atom || Atom.isNegatedAtom(atom)){
                    if(ret.containsKey(atom))
                        ret.get(atom).add(e.getKey());
                    else{
                        ArrayList<Integer> v=new ArrayList<Integer>();
                        v.add(e.getKey());
                        ret.put(atom,v);
                    }
                }
            }
            
        }

        return ret;
    }
}
