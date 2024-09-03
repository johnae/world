-- You can define your global state here
local main_ratio = 0.65
local gaps = 10
local smart_gaps = false
local layout = "main-side-stack"
local tag_layouts = {}

function dump(o)
	if type(o) == "table" then
		local s = "{ "
		for k, v in pairs(o) do
			if type(k) ~= "number" then
				k = '"' .. k .. '"'
			end
			s = s .. "[" .. k .. "] = " .. dump(v) .. ","
		end
		return s .. "} "
	end
	return tostring(o)
end

-- This layout function is a main window with a side window below which a stack of windows resides
function main_side_stack(args)
	local retval = {}
	if args.count == 1 then
		if smart_gaps then
			table.insert(retval, { 0, 0, args.width, args.height })
		else
			table.insert(retval, { gaps, gaps, args.width - gaps * 2, args.height - gaps * 2 })
		end
	elseif args.count == 2 then
		local main_w = (args.width - gaps * 3) * main_ratio
		local side_w = (args.width - gaps * 3) - main_w
		local main_h = args.height - gaps * 2
		table.insert(retval, {
			gaps,
			gaps,
			main_w,
			main_h,
		})
		table.insert(retval, {
			(main_w + gaps * 2),
			gaps,
			side_w,
			main_h,
		})
	elseif args.count > 2 then
		local main_w = (args.width - gaps * 3) * main_ratio
		local side_w = (args.width - gaps * 3) - main_w
		local main_h = args.height - gaps * 2
		table.insert(retval, {
			gaps,
			gaps,
			main_w,
			main_h,
		})
		table.insert(retval, {
			main_w + (gaps * 2),
			gaps,
			side_w,
			main_h * main_ratio,
		})
		for i = 0, (args.count - 3) do
			local x = (main_w + gaps * 2) + (i * gaps)
			local y = (main_h * main_ratio) + (gaps * 2) + (i * gaps)
			local width = side_w - (i * gaps) - (((args.count - 3) - i) * gaps)
			local height = main_h - (main_h * main_ratio) - gaps - (i * gaps)
			table.insert(retval, {
				x,
				y,
				width,
				height,
			})
		end
	end
	return retval
end

function monocle(args)
	local retval = {}
	local main_w = args.width
	local main_h = args.height
	table.insert(retval, {
		0,
		0,
		main_w,
		main_h,
	})
	if args.count > 1 then
		for i = 0, (args.count - 2) do
			table.insert(retval, {
				args.width + i + 1,
				args.height + i + 1,
				0,
				0,
			})
		end
	end
	return retval
end

-- This layout function emulates the default rivertile layout
function main_stack(args)
	local retval = {}
	if args.count == 1 then
		if smart_gaps then
			table.insert(retval, { 0, 0, args.width, args.height })
		else
			table.insert(retval, { gaps, gaps, args.width - gaps * 2, args.height - gaps * 2 })
		end
	elseif args.count > 1 then
		local main_w = (args.width - gaps * 3) * main_ratio
		local side_w = (args.width - gaps * 3) - main_w
		local main_h = args.height - gaps * 2
		local side_h = (args.height - gaps) / (args.count - 1) - gaps
		table.insert(retval, {
			gaps,
			gaps,
			main_w,
			main_h,
		})
		for i = 0, (args.count - 2) do
			table.insert(retval, {
				main_w + gaps * 2,
				gaps + i * (side_h + gaps),
				side_w,
				side_h,
			})
		end
	end
	return retval
end

function main_bottom_stack(args)
	local retval = {}
	if args.count == 1 then
		if smart_gaps then
			table.insert(retval, { 0, 0, args.width, args.height })
		else
			table.insert(retval, { gaps, gaps, args.width - gaps * 2, args.height - gaps * 2 })
		end
	elseif args.count == 2 then
		local main_h = (args.height - gaps * 3) * main_ratio
		local side_h = (args.height - gaps * 3) - main_h
		local main_w = args.width - gaps * 2
		table.insert(retval, {
			gaps,
			gaps,
			main_w,
			main_h,
		})
		table.insert(retval, {
			gaps,
			(main_h + gaps * 2),
			main_w,
			side_h,
		})
	elseif args.count > 2 then
		local main_h = (args.height - gaps * 3) * main_ratio
		local side_h = (args.height - gaps * 3) - main_h
		local main_w = args.width - gaps * 2
		table.insert(retval, {
			gaps,
			gaps,
			main_w,
			main_h,
		})
		table.insert(retval, {
			gaps,
			(main_h + gaps * 2),
			main_w * main_ratio,
			side_h,
		})
		for i = 0, (args.count - 3) do
			local y = (main_h + gaps * 2) + (i * gaps)
			local x = (main_w * main_ratio) + (gaps * 2) + (i * gaps)
			local height = side_h - (i * gaps) - (((args.count - 3) - i) * gaps)
			local width = main_w - (main_w * main_ratio) - gaps - (i * gaps)
			table.insert(retval, {
				x,
				y,
				width,
				height,
			})
		end
	end
	return retval
end

-- The most important function - the actual layout generator
--
-- The argument is a table with:
--  * Focused tags (`args.tags`)
--  * Window count (`args.count`)
--  * Output width (`args.width`)
--  * Output height (`args.height`)
--  * Output name (`args.output`)
--
-- The return value must be a table with exactly `count` entries. Each entry is a table with four
-- numbers:
--  * X coordinate
--  * Y coordinate
--  * Window width
--  * Window height

function handle_layout(args)
	local resolved_layout = tag_layouts[args.tags] or layout
	if resolved_layout == "main-side-stack" then
		return main_side_stack(args)
	elseif resolved_layout == "main-bottom-stack" then
		return main_bottom_stack(args)
	elseif resolved_layout == "monocle" then
		return monocle(args)
	end
	return main_stack(args)
end

-- This optional function returns the metadata for the current layout.
-- Currently only `name` is supported, the name of the layout. It get's passed
-- the same `args` as handle_layout()
function handle_metadata(args)
	return { name = layout }
end

-- IMPORTANT: User commands send via `riverctl send-layout-cmd` are treated as lua code.
-- Active tags are stored in `CMD_TAGS` global variable.
-- Output name is stored in `CMD_OUTPUT` global variable.

-- Here is an example of a function that can be mapped to some key
-- Run with `riverctl send-layout-cmd luatile "toggle_gaps()"`
local gaps_alt = 0
function toggle_gaps()
	print("Toggling gaps")
	local tmp = gaps
	gaps = gaps_alt
	gaps_alt = tmp
end

function set_main_ratio(ratio)
	print("Setting main ratio to " .. ratio)
	main_ratio = ratio
end

function adjust_main_ratio_by(val)
	print("Adjust main ratio by " .. val)
	main_ratio = main_ratio + val
end

function set_gaps(gap)
	print("Setting gaps to " .. gap)
	gaps = gap
end

function set_smart_gaps(smart)
	print("Setting smart gaps to " .. smart)
	smart_gaps = smart
end

function set_layout(name)
	print("Setting layout to " .. name)
	layout = name
end

function set_tag_layout(name)
	print("Setting layout to " .. name)
	tag_layouts[CMD_TAGS] = name
end

function next_layout()
	if layout == "main-side-stack" then
		set_layout("monocle")
	elseif layout == "monocle" then
		set_layout("main-stack")
	elseif layout == "main-stack" then
		set_layout("main-bottom-stack")
	else
		set_layout("main-side-stack")
	end
end

function next_tag_layout()
	local resolved_layout = tag_layouts[CMD_TAGS] or layout
	if resolved_layout == "main-side-stack" then
		set_tag_layout("monocle")
	elseif resolved_layout == "monocle" then
		set_tag_layout("main-stack")
	elseif resolved_layout == "main-stack" then
		set_tag_layout("main-bottom-stack")
	else
		set_tag_layout("main-side-stack")
	end
end
