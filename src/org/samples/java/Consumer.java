package org.samples.java;

/**
 * Created by IntelliJ IDEA.
 * User: pbrown
 * Date: 2/28/12
 * Time: 9:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class Consumer extends Thread {

   Fridge fridge;

   Consumer(Fridge fridge){
       this.fridge = fridge;
   }

    public void run(){

       fridge.setItemToStore("apple");
       fridge.setItemToStore("bread");
    }

    public static void main(String args[]){
        Fridge fridge = new Fridge();
        fridge.start();
        Consumer consumer = new Consumer(fridge);
        consumer.start();
        System.out.println("Fridge State: "+ fridge.getFridge().toString());
    }
}
