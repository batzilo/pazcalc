
Διαδικασία μεταγλώττισης ενός προγράμματος:

1. Μεταγλώττιση χρησιμοποιώντας το μεταγλωττιστή σας για να πάρετε
   το αρχείο "program.asm".

2. Κλήση του συμβολομεταφραστή (assembler) και του συνδέτη (linker):

      ml /AT /Cx /nologo /Zm program.asm /link paz.lib

   όπου "paz.lib" το όνομα της βιβλιοθήκης χρόνου εκτέλεσης.

   Εναλλακτικά, μπορείτε να κάνετε ξεχωριστά τη συμβολομετάφραση
   και τη σύνδεση:

   2α. Κλήση του συμβολομεταφραστή (assembler):

       masm /Mx /t program.asm;

   2β. Κλήση του συνδέτη (linker):

       link /tiny /noignorecase /nologo program.obj,program.com,nul,paz.lib;
