
function GetChannel(name)
  for i = 0, reaper.GetNumAudioInputs() - 1 do
    local channelName = reaper.GetInputChannelName(i)
    if channelName == name then return i
    end
  end
  return -1
end  

function CreateFolder(index, name)
  reaper.InsertTrackAtIndex(index, false)
  folder = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(folder, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( folder, 'I_FOLDERDEPTH',1)
  reaper.SetMediaTrackInfo_Value(folder, 'I_FOLDERCOMPACT', 0)
end

function CreateTrack(index, name, channel, lastInFolder)
  local ch = GetChannel(channel)
  local folderDepth = 0
  if lastInFolder then folderDepth = -1 end
  
  reaper.InsertTrackAtIndex(index, false)
  tr = reaper.GetTrack(0,index)
  reaper.GetSetMediaTrackInfo_String(tr, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECARM',1)
  reaper.SetMediaTrackInfo_Value( tr, 'I_RECINPUT',1024 + ch)
  reaper.SetMediaTrackInfo_Value( tr, 'I_FOLDERDEPTH',folderDepth)
end

CreateFolder(0, "Group 1")
CreateTrack(1, "Track 1", "Input 1 (BlackHole 64ch)", false)
CreateTrack(2, "Track 2", "Input 3 (BlackHole 64ch)", true)

CreateFolder(3, "Group 2")
CreateTrack(4, "Track 3", "Input 5 (BlackHole 64ch)", false)
CreateTrack(5, "Track 4", "Input 7 (BlackHole 64ch)", false)
