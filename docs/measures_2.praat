# Praat Analysis Script
# Reads aspiration and vowel durations, and manually-entered amplitude values

# Get subject and list number from user
beginPause: "Enter Subject and List Information"
    comment: "Please enter the following information:"
    word: "Sub", ""
    word: "List", ""
endPause: "Continue", 1

# Store the input values
sub$ = sub$
list$ = list$

# Get the Sound and TextGrid objects
sound$ = selected$("Sound")
soundID = selected("Sound")
textgridID = selected("TextGrid")

# Get tier numbers
select textgridID
regionTier = 0
wordTier = 0
ampTier = 0
nTiers = Get number of tiers

for i from 1 to nTiers
    tierName$ = Get tier name: i
    if tierName$ = "region"
        regionTier = i
    elsif tierName$ = "word"
        wordTier = i
    elsif tierName$ = "amp"
        ampTier = i
    endif
endfor

# Check if tiers were found
if regionTier = 0 or wordTier = 0 or ampTier = 0
    exitScript: "Error: Could not find 'region', 'word', and/or 'amp' tiers"
endif

# Print header with comma separators including Sub and List#
writeInfoLine: "Sub,List#,word,asp_dur,vowel_dur,MaxHi,F1_amp,HiDiff"

# Get number of intervals in word tier
select textgridID
nWordIntervals = Get number of intervals: wordTier

# Loop through each word interval
for iWord from 1 to nWordIntervals
    select textgridID
    word$ = Get label of interval: wordTier, iWord
    
    # Skip empty intervals
    if word$ <> ""
        # Get word boundaries
        wordStart = Get start time of interval: wordTier, iWord
        wordEnd = Get end time of interval: wordTier, iWord
        
        # Find region "1" and "2" within this word
        asp_dur = undefined
        vowel_dur = undefined
        
        # Search for region intervals within word boundaries
        nRegionIntervals = Get number of intervals: regionTier
        
        for iRegion from 1 to nRegionIntervals
            regionLabel$ = Get label of interval: regionTier, iRegion
            regionStart = Get start time of interval: regionTier, iRegion
            regionEnd = Get end time of interval: regionTier, iRegion
            
            # Check if this region is within the current word
            if regionStart >= wordStart and regionEnd <= wordEnd
                if regionLabel$ = "1"
                    asp_dur = regionEnd - regionStart
                elsif regionLabel$ = "2"
                    vowel_dur = regionEnd - regionStart
                endif
            endif
        endfor
        
        # Find amp values within this word (first two intervals)
        maxHi = undefined
        f1_amp = undefined
        ampCount = 0
        
        nAmpIntervals = Get number of intervals: ampTier
        
        for iAmp from 1 to nAmpIntervals
            ampLabel$ = Get label of interval: ampTier, iAmp
            ampStart = Get start time of interval: ampTier, iAmp
            ampEnd = Get end time of interval: ampTier, iAmp
            
            # Check if this amp interval is within the current word
            if ampStart >= wordStart and ampEnd <= wordEnd and ampLabel$ <> ""
                ampCount = ampCount + 1
                if ampCount = 1
                    maxHi = number(ampLabel$)
                elsif ampCount = 2
                    f1_amp = number(ampLabel$)
                endif
            endif
        endfor
        
        # Only proceed if both regions and both amp values were found
        if asp_dur <> undefined and vowel_dur <> undefined and maxHi <> undefined and f1_amp <> undefined
            
            # Calculate difference
            hiDiff = f1_amp - maxHi
            
            # Round all values to 2 decimal places
            asp_dur_rounded = round(asp_dur * 100) / 100
            vowel_dur_rounded = round(vowel_dur * 100) / 100
            maxHi_rounded = round(maxHi * 100) / 100
            f1_amp_rounded = round(f1_amp * 100) / 100
            hiDiff_rounded = round(hiDiff * 100) / 100
            
            # Print results with comma separators, including Sub and List# at the beginning
            appendInfoLine: sub$, ",", list$, ",", word$, ",", fixed$(asp_dur_rounded, 2), ",", fixed$(vowel_dur_rounded, 2), ",", 
                ... fixed$(maxHi_rounded, 2), ",", fixed$(f1_amp_rounded, 2), ",", fixed$(hiDiff_rounded, 2)
        endif
    endif
endfor

# Final selection
select soundID
plus textgridID

appendInfoLine: ""
appendInfoLine: "Analysis complete!"