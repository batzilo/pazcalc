
Scripts για τον έλεγχο του μεταγλωττιστή σας.

Υποθέσεις:

  0. Τρέχετε τα πάντα σε linux με στημένο bash (προφανώς) και dosbox.
     Αν όχι, αυτά τα scripts δεν σας κάνουν (εκτός αν τα αλλάξετε).

  1. Στο directory Support/ έχετε τοποθετήσει το συμβολομεταφραστή
     (masm) και το συνδέτη (link).  Μπορείτε να τα κατεβάσετε από
     τη σελίδα του μαθήματος.  Λειτουργεί τόσο με την έκδοση 5.10
     (support.zip) όσο και με την έκδοση 6.11 (support-611d.zip).

  2. Στα directories Correct/ και Wrong/ υπάρχει το testsuite σας,
     αποτελούμενο αντίστοιχα από προγράμματα που είναι σωστά και
     προγράμματα που είναι λάθος.

  3. Η κατάληξη των προγραμμάτων στην αρχική σας γλώσσα είναι "paz"
     και ο μεταγλωττιστής σας λέγεται "pazcalc".  Αν όχι, τροποποιήστε
     τις μεταβλητές MYLANG και MYCOMP αντίστοιχα, στο πάνω μέρος των
     scripts.

  4. Στο αρχείο "paz.lib" (προκύπτει από τη μεταβλητή MYLIB στο
     πάνω μέρος του script "run.bat") βρίσκεται η βιβλιοθήκη χρόνου
     εκτέλεσης.

  5. Δεν έχετε κάτι χρήσιμο σε αρχεία που ονομάζονται "a.*" και
     "*.asm" στο directory απ' όπου θα τρέξετε αυτά τα scripts.
     (You have been warned!)
     
Τρόπος χρήσης:

  1. Για ένα πρόγραμμα:

     ./do.sh wherever/whatever.paz

  2. Για όλο το testsuite:

     ./doall.sh

  3. Για όλο το υπόλοιπο testsuite, ξεκινώντας από το επόμενο
     πρόγραμμα μετά το "wherever/whatever.paz".  (Αυτό είναι
     χρήσιμο αν σταματήσετε το "doall.sh" και θελήσετε να το
     ξεκινήσετε ξανά από εκεί που μείνατε.)

     ./doall.sh wherever/whatever.paz
