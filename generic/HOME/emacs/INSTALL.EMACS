###############################################
##  PLEASE FOLLOW THE INSTRUCTIONS CAREFULLY ##
###############################################

1) Create a new emacs directory in your HOME directory.

   % cd ~/

   % mkdir emacs


2) Copy the emacs.tar.gz file into emacs directory and untar the file.

   % cd ~/emacs

   % tar -zxvf emacs.tar.gz


3) Move the current '.emacs' file in your HOME directory to a backup 
   file '.emacs.bak'. Copy the 'my.emacs' file from emacs directory to
   your HOME directory as '.emacs'.

   % mv ~/.emacs ~/.emacs.bak

   % cp ~/emacs/my.emacs ~/.emacs


4) Run the following command in your HOME directory, after replacing 
   vsundare with your userid in the command.

   % perl -i -p -e 's/LOGIN/vsundare/g' .emacs

   or, Open the new ~/.emacs file and change the following path to point 
   to where you untarred the distribution i.e. replace LOGIN with your userid.

   (defun USER-load-file (f) (load-file
                             (concat "/home/LOGIN/emacs/customizations/" f)))

5) Log out and log back in.