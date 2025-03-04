
% this is a comment 
% advent of code 2015 day 1 
declare F SIZE FC
F={New Open.file init(name:'../input' flags:[read])}

% 
SIZE = 7000
{F read(list:FC size:SIZE)}
{Browse FC}
{Browse {List.length FC}}

declare
proc {Santa XS R S}
   case XS of nil then S=R
   [] H |T then
      if H == &( then
	 {Santa T (R+1) S}     
      else if H == &) then
	      {Santa T (R-1) S}
	   else
	      {Exception.'raise' foobar}
	   end	 
      end
   end   
end
local X in
   {Santa FC 0 X}
   {Browse X}
end

% results in 280 in mozart browser

% what position P causes santa to enter basement
declare
proc {Santa2 P XS R S}
   case XS of nil then S=nil
   [] H |T then
      if H == &( then
	 {Santa2 (P+1) T (R+1) S}     
      else if H == &) then
	      if (R - 1) < 0 then S = P
	      else		 
		 {Santa2 (P+1) T (R-1) S}
	      end
	   else
	      {Exception.'raise' foobar}
	   end	 
      end
   end   
end
local POS FLOOR X in
   POS = 1
   FLOOR = 0
   {Santa2 POS FC FLOOR X}
   {Browse X}
end


% results in 1797 in mozart browser



% % uppercase procedure names 
% declare
% proc {Santa XS R S}
%    case XS of nil then R=0
%    [] &( |T then
%       {Santa T R (S+1)}     
%    [] &) |T then
%       {Santa T R (S-1)}
%    end   
% end
% local X in
%    {Santa FC 0 X}
%    {Browse X}
% end


% % [Prototyper]={Module.link ["x-oz://system/wp/Prototyper.ozf"]}
% % {Prototyper.run}

% % character in mozart oz
% % character defined prefix with ampersand &

% % [] is not the empty list , nil is the empty list !
% % {Browse nil}
% % {Browse 1 | nil }
% % nil == nil


% declare
% proc {Add X Y Z}
%    Z = X + Y
% end
% local A in
%    {Add 2 3 A}
%    {Browse A}
% end

% declare
% proc {LEN XS R}
%    case XS of nil then R=0
%    [] H|T then
%       S in 
%       {LEN T S}
%       R = S + 1 
%    end   
% end
% local X in
%    {LEN [a b c] X}
%    {Browse X}
% end


% declare
% proc {Count XS}
%    H T in
%    if XS == nil then 0
%    else 
%       H | T = XS
%       1 + {Count T}   
%    end
% end
% local F in
%    F = {Count [1 2 3]}
%    {Browse F}
% end




% declare
% proc {Count XS FLOOR}
%    if XS == nil then FLOOR
%    else local H T in
% 	   XS = H | T
% 	   {Count T (FLOOR + 1)}
% 	end
%    end
% end
% local F in
%    {Count [1 2 3] F}
%    {Browse F}
% end


% declare
% proc {Count XS FLOOR}
%    local H T in 
%       if XS == (H | T) then FLOOR=1
%       else FLOOR = 0
%       end
%    end
% end

% local F in
%    {Count [1 2 3] F}
%    {Browse F}
% end

% {Browse [1 2 3]}

   


% declare
% proc {Count XS ?FLOOR}
%    local H T in
%    if XS = [] then FLOOR
%    elseif H|T = XS then
%       if H = &( then Count T (FLOOR + 1)
%       else if H = &) then Count T (FLOOR - 1)
% 	   else error "foo"
% 	   end
%       end
%    end
%    end


% % check if open parens is a character    
% local X in   
%    {Char.is &( X}
%    {Browse X}
% end

