% {Show 'Hello World!'}
% 
% {Browse 'Hello World'}
% 
% declare W H
% {Browse foo(width:W height:H surface:thread W*H end)}

% local A B in 
%    A = 3
%    proc {B}
%       {Show A + 'Tinman'}
%    end 
%    {B 7}
% end
    
declare fun {Steiner N}
   case 
      N mod 6 == 1 orelse N mod 6 == 3            
   then 
      proc {$ Ss}  
         {FS.var.list.upperBound (N*(N-1)) div 6 [1#N] Ss}  
         {ForAll Ss proc {$ S} {FS.card S 3} end}    
          
         {ForAllTail Ss                              
          proc {$ S1|Sr}                             
             {ForAll Sr                              
              proc {$ S2} S3 in 
                 S3 = {FS.intersect S1 S2}           
                 {FS.cardRange 0 1 S3}               
              end}
          end}
          
         {FS.distribute naive Ss}
      end 
   else proc {$ _} fail end 
   end 
end 
 
 