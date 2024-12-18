Point = {}
function Point:new(x, y)
    self.__index = self
    return setmetatable({
        x = x,
        y = y
    }, self)
end

Grid = {}
function Grid:new(width, height, init)
    self.__index = self

    local m = {
        grid = {},
        width = width,
        height = height,
        visited = {}
    }

    for x = 1, m.width do
        m.grid[x] = {}
        m.visited[x] = {}
        for y = 1, m.height do
            m.grid[x][y] = init or '▫'
            m.visited[x][y] = false
        end
    end

    return setmetatable(m, self)
end

function Grid:__index(x)
    if type(x) == "number" then
        return self.grid[x]
    else
        return Grid[x]
    end
end

function Grid:clearVisited()
    for x = 1, self.width do
        for y = 1, self.height do
            self.visited[x][y] = false
        end
    end
end

function Grid:print()
    for y = 1, self.height do
        for x = 1, self.width do
            io.write(self.grid[x][y])
        end
        io.write("\n")
    end
end

function Grid:printWithPath(path)
    local directionToChar = {
        ["up-right"] = "╔",
        ["up-left"] = "╗",
        ["down-right"] = "╚",
        ["down-left"] = "╝",
        ["right-up"] = "╝",
        ["right-down"] = "╗",
        ["left-up"] = "╚",
        ["left-down"] = "╔",
        ["up-up"] = "║",
        ["down-down"] = "║",
        ["left-left"] = "═",
        ["right-right"] = "═"
    }

    local function getDirection(from, to)
        if from.x < to.x then
            return "right"
        elseif from.x > to.x then
            return "left"
        elseif from.y < to.y then
            return "down"
        elseif from.y > to.y then
            return "up"
        else
            error("Invalid move: from and to points are the same")
        end
    end

    local tempGrid = {}
    for k, v in ipairs(self.grid) do
        tempGrid[k] = {}
        for k2, v2 in ipairs(v) do
            tempGrid[k][k2] = v2
        end
    end
    for i = 3, #path do
        local from = path[i - 1]
        local preFrom = path[i - 2]
        local to = path[i]
        local prevDirection = getDirection(preFrom, from)
        local currDirection = getDirection(from, to)
        local char = directionToChar[prevDirection .. "-" .. currDirection]
        if char then
            tempGrid[from.x][from.y] = "\27[31m" .. char .. "\27[0m"
        end
    end

    tempGrid[path[1].x][path[1].y] = ""
    tempGrid[path[#path].x][path[#path].y] = "\27[31m●\27[0m"
    for y = 1, self.height do
        for x = 1, self.width do
            io.write(tempGrid[x][y])
        end
        io.write("\n")
    end
end

function Grid:corrupt(point)
    self.grid[point.x + 1][point.y + 1] = '█'
end

function readPointsFromFile(filename)
    local points = {}
    for line in io.lines(filename) do
        if line == "" then
            break
        end
        local x, y = line:match("(%d+),(%d+)")
        x = tonumber(x)
        y = tonumber(y)
        table.insert(points, Point:new(x, y))
    end
    return points
end

function Grid:isValidMove(x, y)
    return x > 0 and y > 0 and x <= self.width and y <= self.height and self.grid[x][y] == '▫' and
               not self.visited[x][y]
end

function Grid:visit(x, y)
    self.visited[x][y] = true
end

function Grid:findShortestPath(kwargs)
    kwargs = kwargs or {}

    local directions = {{
        dx = 1,
        dy = 0
    }, {
        dx = -1,
        dy = 0
    }, {
        dx = 0,
        dy = 1
    }, {
        dx = 0,
        dy = -1
    }}

    self:clearVisited()

    local queue = {{
        x = 1,
        y = 1,
        dist = 0,
        parent = nil
    }}

    self:visit(1, 1)

    local reversePath = {}

    while #queue > 0 do
        local current = table.remove(queue, 1)
        if current.x == self.width and current.y == self.height then
            local result = current.dist
            if kwargs.andPrint then
                while current do
                    table.insert(reversePath, Point:new(current.x, current.y))
                    current = current.parent
                end
                self:printWithPath(reversePath)
            end
            return result
        end

        for _, direction in ipairs(directions) do
            local newX = current.x + direction.dx
            local newY = current.y + direction.dy
            if self:isValidMove(newX, newY) then
                self:visit(newX, newY)
                table.insert(queue, {
                    x = newX,
                    y = newY,
                    dist = current.dist + 1,
                    parent = current
                })
            end
        end
    end

    return -1
end

local points = readPointsFromFile("data/task.data")
local grid = Grid:new(71, 71)

for i = 1, 1024 do
    grid:corrupt(points[i])
end

-- first task
local shortestPathLength = grid:findShortestPath{
    andPrint = true
}

if shortestPathLength == -1 then
    print("No path found")
else
    print("Shortest path length: " .. shortestPathLength)
end

-- second task
for byte = 1025, #points do
    grid:corrupt(points[byte])
    shortestPathLength = grid:findShortestPath()
    if shortestPathLength == -1 then
        print("Obstacle at " .. points[byte].x .. "," .. points[byte].y .. " on iteration " .. byte)
        break
    end
end
