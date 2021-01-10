# Resubmission of maintainance release

Original issue: 

XGBoost is used as "suggested" package for the main vignette. Since currently XGBoost is not available for all systems, Prof. Ripley kindly asked me to catch this problem via "requireNamespace".


Resubmission due to two moved urls as detected by the checks of Uwe:

"Thanks, we see:

   Found the following (possibly) invalid URLs:
     URL: http://arxiv.org/abs/1612.08468 (moved to 
https://arxiv.org/abs/1612.08468)
       From: inst/doc/flashlight.html
       Status: 200
       Message: OK
     URL: http://arxiv.org/abs/1801.01489 (moved to 
https://arxiv.org/abs/1801.01489)
       From: inst/doc/flashlight.html
       Status: 200
       Message: OK
     URL: http://arxiv.org/abs/1903.11420 (moved to 
https://arxiv.org/abs/1903.11420)
       From: inst/doc/flashlight.html
       Status: 200
       Message: OK

Please change http --> https, add trailing slashes, or follow moved 
content as appropriate.

Please fix and resubmit.

Best,
Uwe Ligges""


