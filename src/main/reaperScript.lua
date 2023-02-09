
function CreateFolder(index, name)
  reaper.InsertTrackAtIndex(index, false)
  folder = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(folder, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( folder, 'I_FOLDERDEPTH',1)
  reaper.SetMediaTrackInfo_Value(folder, 'I_FOLDERCOMPACT', 0)
end

function ImportAudio(index, channel, trackName, fileName, lastInFolder)
  local folderDepth = 0
  if lastInFolder then folderDepth = -1 end

  reaper.SetEditCurPos(0, false, false)
  reaper.InsertTrackAtIndex(index, false)
  tr = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(tr, 'P_NAME', trackName, true)
  reaper.SetMediaTrackInfo_Value( tr, 'I_FOLDERDEPTH',folderDepth)
  reaper.SetOnlyTrackSelected(tr, true)
  reaper.InsertMedia(fileName, 0)
  item = reaper.GetTrackMediaItem(tr, 0)
  take = reaper.GetActiveTake(item)
  reaper.SetMediaItemTakeInfo_Value(take, "I_CHANMODE", channel + 64 + 2)
end

audioFile = "/Users/danielstahl/Documents/Music/Pieces/Ambient Music/Ambient Music 6/stage/ambientMusic6Score.caf"

CreateFolder(0, "Long parts")
ImportAudio(1, 1, "Long 1", audioFile, false)
ImportAudio(2, 3, "Long 1 effect", audioFile, false)
ImportAudio(3, 5, "Long 2", audioFile, false)
ImportAudio(4, 7, "Long 2 effect", audioFile, false)
ImportAudio(5, 9, "Long 3", audioFile, false)
ImportAudio(6, 11, "Long 3 effect", audioFile, true)

CreateFolder(7, "Short part")
ImportAudio(8, 13, "Short part", audioFile, true)