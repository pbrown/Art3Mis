package org.samples.java;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Utility {

    public static List<Integer> map(List<Integer> list, MapFunction<Integer> mapFunction){
        List<Integer> myNewList = new ArrayList<Integer>();
        int size = list.size();
        for (int i=0;i<size;i++){
            Integer value = mapFunction.map(list.get(i));
            myNewList.add(value);
        }
        return myNewList;
    }


    public static void main(String[] args){
        List<Integer> myList = Arrays.asList(1,2,3,4);

        List<Integer> myNewList = Utility.map(myList, new MapFunction<Integer>() {
            public Integer map(Integer integer) {
                return integer.intValue()+1;
            }
        });
        System.out.println(myNewList);
    }

}
