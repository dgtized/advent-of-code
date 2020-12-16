function parseTicket(line)
   ticket = {}
   for elem in line:gmatch "(%d+)" do
      table.insert(ticket, elem)
   end
   return ticket
end

function parseInput(filename)
   local file = io.input(filename)
   local field_rules = {}
   local your_ticket
   local nearby_tickets = {}
   local mode = 0

   while true do
      local line = io.read()
      if line == nil then
         break
      end

      if line == "" then
         mode = mode + 1
      elseif mode == 0 then
         local i = string.find(line, ":")
         local field = string.sub(line, 0, i-1)
         local values = string.sub(line, i+1)

         local rules = {}
         for lower,upper in values:gmatch "(%d+)-(%d+)" do
            table.insert(rules, {lower, upper})
         end

      elseif mode == 1 then
         if(string.find(line, ",")) then
            your_ticket = parseTicket(line)
         end
      elseif mode == 2 then
         if(string.find(line, ",")) then
            table.insert(nearby_tickets, parseTicket(line))
         end
      end
   end
end

parseInput("example")
