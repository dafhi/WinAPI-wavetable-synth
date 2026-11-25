/' -- WinAPI wavetable synth - 2026 Nov 24  by dafhi

    - credit -
    
  synth engine  dafhi     99 %
  scopes        deepseek  99 %
  knobs         deepseek  90 %
  API + UI      dafhi     90 %
  ideas         dafhi     80 %
  inspiration   Hermes    40 %,  Qwen / Gemini 10 %

    unique:
  variable size Tone buffers (sine, square..) relatively small at 110.
  You can hear a bit of 'wind' at quieter tones b/c of that smallness
  
  Sometimes the audio will initialize incorrectly - restart or increase WinAPI buffer
 
  - changes Nov 24
  . removed unused f_a256()
  . tone buf len
  . FM ratio lbound 0.001 -> 0.000

   ToDo:
   
  ADSR
  note input
  Try to work out a more compelling noise readout
  
  license:  modify, profit

'/


dim as single  tDemoSeconds = 600

print "demo will quit after"; tDemoSeconds; " seconds  (eco friendly)"


'#include "synth.bas"
/' -- wavetable synth - 2026 Nov 20

    concept and code: dafhi (99%)

    initial presentation of wavetype morph:  Google Gemini (possibly inspired by Fl Studio's Sytrus plugin)

'/

'#include "knobs.bas"
'#include "winaudio.bas"
#ifndef winaudio_h
    #define winaudio_h

#include once "windows.bi"      '' Audio class by D.J.Peters
#include once "win/mmsystem.bi"

dim shared as integer  nSamplesPerBuffer = 60 '' if audio skips, raise either
dim shared as integer  nBuffers          = 34

const      as integer  rate              = 44100


declare sub custom_signal(as short ptr)   '' 2026 Oct 10  dafhi


#define DEBUG
#ifdef DEBUG
#define DPRINT(txt) OPEN ERR FOR OUTPUT AS #99:print #99,txt:close #99
#else
#define DPRINT(txt) :
#endif

type WAVEOUT_DEVICE
  declare constructor(DeviceIndex as integer=-1)
  declare destructor
  private:
  declare static sub WaveOutProc(hDevice as HWAVEOUT, DriverMessage as uinteger, _
                                 pDevice as WAVEOUT_DEVICE ptr, pWaveHeader as PWAVEHDR, _
                                 Param2 as DWORD)
  declare sub PrepareBuffer(pBuffer as PWAVEHDR)
  as WAVEFORMATEX     wfex
  as WAVEOUTCAPS      Caps
  as HWAVEOUT         hDevice
  as MMRESULT         LastResult
  as PWAVEHDR ptr     Buffers
  as integer          IsAccurate, IsOpen, IsRunning
  as integer          nPlayedBuffers
end type

constructor WAVEOUT_DEVICE(DeviceIndex as integer)
  LastResult = waveOutGetDevCaps(DeviceIndex, @Caps, sizeof(WAVEOUTCAPS))
  if LastResult = MMSYSERR_NOERROR then ISAccurate = (Caps.dwSupport and WAVECAPS_SAMPLEACCURATE)
  with wfex
    .wFormatTag      = WAVE_FORMAT_PCM
    .nSamplesPerSec  = rate
    .nChannels       = 2
    .wBitsPerSample  = 16
    .nBlockAlign     = (.wBitsPerSample shr 3) * .nChannels
    .nAvgBytesPerSec = .nBlockAlign * .nSamplesPerSec
    .cbSize          = 0
  end with
  LastResult = waveOutOpen(@hDevice, DeviceIndex, cptr(LPCWAVEFORMATEX,@wfex), _
                           cast(uinteger,@WaveOutProc), cast(uinteger,@this), CALLBACK_FUNCTION)
  
  if LastResult = MMSYSERR_NOERROR then '' 2026 Nov 16 - DeepSeek
  'if IsOpen then
    dim as integer size = wfex.nBlockAlign * nSamplesPerBuffer
    Buffers = callocate(nBuffers * sizeof(PWAVEHDR))
    for i as integer = 0 to nBuffers-1
      Buffers[i] = callocate(sizeof(WAVEHDR))
      with *Buffers[i]
        .lpData         = callocate(size)
        .dwBufferLength = size
        .dwUser         = i
        .dwFlags        = 0
      end with
      LastResult = waveOutPrepareHeader(hDevice, Buffers[i], sizeof(WAVEHDR))
    next
    for i as integer = 0 to nBuffers-1
      waveOutWrite(hDevice, Buffers[i], sizeof(WAVEHDR))
    next
    IsRunning = 1
  end if
end constructor

destructor WAVEOUT_DEVICE
  if hDevice <> NULL then
    if IsOpen <> 0 then
      if IsRunning <> 0 then
        IsRunning = 0
        waveOutReset hDevice
        sleep 1100
      end if
      if Buffers <> NULL then
        for i as integer = 0 to nBuffers-1
          if Buffers[i] <> NULL then
            if Buffers[i]->dwFlags and WHDR_PREPARED then
              waveOutUnprepareHeader(hDevice, Buffers[i], sizeof(WAVEHDR))
            end if
            if Buffers[i]->lpData then deallocate Buffers[i]->lpData
            deallocate Buffers[i]
          end if
        next
        deallocate Buffers
      end if
      waveOutClose hDevice
    end if
  end if
  dprint("WAVEOUT_DEVICE~")
end destructor

sub WAVEOUT_DEVICE.WaveOutProc(hDevice as HWAVEOUT, DriverMsg as uinteger, _
                               pDevice as WAVEOUT_DEVICE ptr, pBuffer as PWAVEHDR, _
                               Param2 as DWORD)
  select case as const DriverMsg
  case WOM_DONE
    pDevice->nPlayedBuffers += 1
    if pDevice->IsRunning then
      pDevice->PrepareBuffer(pBuffer)
    else
      DPRINT("WOM_DONE")
    end if
  case WOM_OPEN  : pDevice->IsOpen  = 1
  case WOM_CLOSE : pDevice->IsOpen  = 0
  end select
end sub

sub WAVEOUT_DEVICE.PrepareBuffer(pBuffer as PWAVEHDR)
  waveOutUnprepareHeader(hDevice, pBuffer, sizeof(WAVEHDR))
  pBuffer->dwFlags = 0
  custom_signal cast(short ptr, pBuffer->lpdata)
  if IsRunning <> 0 then
    waveOutPrepareHeader(hDevice, pBuffer, sizeof(WAVEHDR))
    waveOutWrite(hDevice, pBuffer, sizeof(WAVEHDR))
  end if
end sub

#endif ' winaudio

'#include "boilerplate.bas"
    #ifndef min
function min( a as double, b as double ) as double: if a<b then return a
return b :  end function
function max( a as double, b as double ) as double: if a>b then return a
return b :  end function
#endif

function clamp(in as double, hi as double = 1, lo as double = 0) as double
  return min(max(in, lo), hi)
end function

const pi  = 4*atn(1)
const tau = 8*atn(1)

' ------------------------------ IMAGE HELPERS ------------------------------
type imvars
  as long     w, h, bypp, pitch
  as any ptr  pixels, im
  as long     wm, hm
end type

sub _gfx_release(byref im as any ptr)
  if im <> 0 then if imageinfo(im) = 0 then imagedestroy im
  im = 0
end sub

sub fill_imvars(byref i as imvars, im as any ptr = 0, free_prev as boolean = true)
  if free_prev then _gfx_release i.im
  if im = 0 then
    ScreenInfo i.w, i.h, , i.bypp, i.pitch : i.pixels = screenptr
  else
    ImageInfo im, i.w, i.h, i.bypp, i.pitch, i.pixels : i.im = im
  end if
  i.wm = i.w-1 : i.hm = i.h-1
end sub

#Macro Alpha256(ret, back, fore, a256)
  scope
    dim as long _a = (a256)
    ret = (((fore and &Hff00ff) * _a + (back and &Hff00ff) * (256-_a) + &H800080) and &Hff00ff00 Or _
           ((fore and &H00ff00) * _a + (back and &H00ff00) * (256-_a) + &H008000) and &H00ff0000) Shr 8
  end scope
#EndMacro

type t_draw_area field = 2
  dim as short x0, y0, x1, y1
end type

' ------------------------------ AA_DOT ------------------------------------
namespace aa_dot
  dim as imvars _im
  dim as long   scan_x0, scan_x1, final_a
  dim as single xm5, ym5, xp5, rp5_sq, alpha_sa, edge_iRSq, dySq, dx, dy, rSq
  dim as t_draw_area rc

  sub render_target(im as any ptr = 0)
    fill_imvars _im, im, false
  end sub

  sub _cliprect(x0 as single, y0 as single, x1 as single, y1 as single)
    rc.x0 = max(0, int(x0)) : rc.y0 = max(0, int(y0))
    rc.x1 = min(_im.wm, int(x1)) : rc.y1 = min(_im.hm, int(y1))
  end sub

  sub _precalcs(x as single, y as single, c as ulong, r as single, edge as single)
    _cliprect x - r, y - r, x + r, y + r
    alpha_sa  = min(1, edge*r) * 256.499 * (c shr 24) / 255
    rSq       = r^2
    edge_irSq = edge * alpha_sa / rSq
    xm5 = x - .5 : xp5 = x + .5 : ym5 = y - .5 : rp5_sq = (r + .5)^2
  end sub

  sub draw(x as single = 0, y as single = 0, c as ulong = -1, rad as single = 5, edge as single = 1)
    _precalcs x, y, c, rad, edge
    for iy as long = rc.y0 to rc.y1
      dy = iy - ym5
      dx = sgn(rp5_sq - dy^2) * sqr(abs(rp5_sq - dy^2))
      scan_x0 = max(int(xp5 - dx), rc.x0)
      scan_x1 = min(int(xm5 + dx), rc.x1)
      dySq = dy * dy
      var pixel = cast(ulong ptr, _im.pixels + iy * _im.pitch)
      for ix as long = scan_x0 to scan_x1
        dx = ix - xm5
        dim as single invert = rSq - (dx*dx + dySq)
        final_a = min(alpha_sa, edge_irSq * max(0, invert))
        alpha256(pixel[ix], pixel[ix], c, final_a)
      next
    next
  end sub
end namespace

' ------------------------------ HSV ---------------------------------------
function triwave(i as single) as single
  return abs(i - int(i) - .5)' - .25 '' 2026 Nov 1
end function

function _cchsv(h as single, s as single, v as single) as ubyte
  dim as single wave_hgt = s * v
  return 255.499 * (wave_hgt * (clamp((triwave(h)- .25)*6+.5)-1) + v)
end function

function hsv(h as single=0, s as single=1, v as single=1, a as ubyte = 255) as ulong
  return rgba(_cchsv(h + 0/3, s,v), _cchsv(h + 2/3, s,v), _cchsv(h + 1/3, s,v), a)
end function

function round(in as double, places as ubyte = 2) as string
  dim as long mul = 10 ^ places
  return str(csng(int(in * mul + .5) / mul))
end function

function f_quant( t as double, m as double ) as single
    return m * int( t / m )
End Function
    '
    ' ----- boilerplate.bas


namespace knobs  /'

     - Credits -
    
     dafhi (90% ideas / 33% code)
    
    .. ideas
    .bulb + aura hsv sequence, knob panels & screws       - dafhi
    .mouse micro adjust via vertical direction change
    .knob group (tKnobs) template as a tKnob extension    2026 Nov 19
    .knob as synth param                                  ..
    .modified bulb hsv sequence                           - DeepSeek V4 Flash
    .knob & bulb highlights
    .code sample demonstrating new knob api               2026 Nov 19
    
    .. code
    .mouse micro adjust                                   - dafhi
    .tweaks, cleanups
    .tosses
    .initial knob subs/funcs, beautifully crafted screws  - DeepSeek
    .extrapolation & code fills / replacement             - DeepSeek, Hermes  2026 Nov 19

'/

  const PANEL_HI   = &hFF45403A
  const PANEL_LO   = &hFF1A1714
  const KNOB_BODY  = &hFF1E1B18
  const KNOB_HI    = &hFF3A342E
  const KNOB_RIM   = &hFF0A0908
  const SCREW_BASE = &hFF3D3833
  const SCREW_HI   = &hFF5A544C
  const SCREW_SLOT = &hFF15120F
  const TICK_COLOR = &hFF6B6357


  type tKnob '' (new)
    declare operator cast as single
    as single       lo, hi = 1, inc
    as single       _val   = 0
    as long         x, y, size', index '' might make life easier.  i like to see what i can get away with
    as single       hue
    as string       label
    as long         dirty
  End Type
  
      operator tKnob.cast as single
        dim as single f = lo + (hi - lo) * _val
        dim as single mst = max( inc, .001)
        return mst * int(f / mst + .5)
      End Operator


  type tKnobs extends tKnob '' ad-hoc mutate and re-argument
  
    as long   cols = 1, rows = 1, gap_x = 10, gap_y = 10
    as single hue_spread = 0.1
    as string group_label
    as string suffixes
  
'    as string  label
    as long    first, n       '' inclusive knob index range
    as long    render_type = 0
    as single  hue = 0
  End Type
  
  ' ---- derived geometry  impossible to desync from iw/ih ----
  #define knob_cx(t) ( (t).size * 0.5 )
  #define knob_cy(t) ( (t).size * 0.5 )

sub knob_init(byref t as tknob,  _
              sHi as single, _
              sVal as single, _
              size as single = 140, sLabel as string = "", _
              sLo as single = 0, sStep as single = 0, x as single = 0, y as single = 0)
  t.lo    = sLo    : t.hi  = sHi : t.inc = sStep
  t.size = size
  t._val  = clamp( (sVal - sLo) / max(0.000, sHi - sLo) )
  if sLabel <> "" then t.label = sLabel
  if x <> 0 then t.x = x
  if y <> 0 then t.y = y
end sub  

sub init_knobs( a() as tKnob, byref k as tKnobs, index as long, size as long = 0, gap_x as long = 0, gap_y as long = 0 )
  
  if size > 0 then k.size = size
  if gap_x > 0 then k.gap_x = gap_x
  if gap_y > 0 then k.gap_y = gap_y
  
  dim as long n = k.cols * k.rows
  k.first = index
  k.n     = n

  '' split suffixes ("base;vari;LFO") into parts
  dim as string parts(0 to 31)
  dim as long   nparts = 0
  dim as long   p = 1
  do while p <= len(k.suffixes) andalso nparts <= 31
    dim as long q = instr(p, k.suffixes, ";")
    if q = 0 then q = len(k.suffixes) + 1
    parts(nparts) = mid(k.suffixes, p, q - p)
    nparts += 1 : p = q + 1
  loop

  dim as long pitch_x = k.size + k.gap_x
  dim as long pitch_y = k.size + k.gap_y
  dim as long idx = index

  for row as long = 0 to k.rows - 1
    for col as long = 0 to k.cols - 1
      if idx > ubound(a) then exit for, for
      dim as tKnob ptr kn = @a(idx)

      kn->x    = k.x + col * pitch_x
      kn->y    = k.y + row * pitch_y
      kn->size = k.size
      kn->hue  = k.hue + col * k.hue_spread   '' spread is per-column
      kn->dirty = true

      '' suffix label (skip if k.suffixes empty  keep existing label from kinit)
      if col < nparts andalso len(parts(col)) > 0 then
        if k.render_type = 0 then
          kn->label = parts(col)                        '' group heading carries the prefix
        else
          kn->label = k.group_label & " " & parts(col)  '' inline, no heading
        end if
      end if

      idx += 1
    next
  next
end sub

  function knob_valstr(byref t as tKnob) as string
    if t.inc >= 1 then return round( t, 1 )
    return round(t,3)
  end function

        '' allow micro adjust AND full knob traversal one sweep
        ''
      dim as long c_sgn '' consecutive direction

      sub _microadjust_via_mousedir_change( byref t as tKnob, dy as single )
        dim as long i = sgn(dy) : c_sgn += i
        if i*sgn(c_sgn) < 1 then c_sgn = 0
        dim as single samedir_curve = (abs(c_sgn) ^ 0.5 / 16) ^ 2.5
        dim as single knob_DY_curve = (abs(dy) ^ 2.0 / 300) ^ 2.5   '' 2026 Nov 13
        t._val = clamp( t._val - i * (samedir_curve ^ 2.75 + knob_DY_curve) )
      End Sub

sub knob_set_from_drag(byref t as tKnob, byref startY as single, curY as single)
  _microadjust_via_mousedir_change t, curY - startY
  t.dirty = true
  startY = curY
end sub

function mousedown__knob_hit(byref t as tKnob, mx as long, my as long) as boolean
  c_sgn = 0 '' micro adjust reset
  return mx >= t.x and mx <= t.x + t.size and my >= t.y and my <= t.y + t.size
end function

  sub draw_arc( byref t as tKnob, cx as single, cy as single, _
               a0 as single, a1 as single, col as ulong, thick as single, rad as single )
    rad *= t.size * 0.3
    for a as single = a0 to a1 step 1.4
      dim as single q = a * 3.1416 / 180
      aa_dot.draw cx + cos(q)*rad, cy + sin(q)*rad, col, thick, 1
    next
  end sub

  sub draw_screw(cx as single, cy as single, r as single)
    
    '' divot
    aa_dot.draw cx,          cy,          &hFF0B0A09, 12, 1
    
    aa_dot.draw cx, cy, &h80000000, r+1.5, 1
    aa_dot.draw cx, cy, SCREW_BASE, r, 1
    aa_dot.draw cx - r*0.25, cy - r*0.25, SCREW_HI, r*0.55, 1
    dim as single ang = -35 * 3.14159265 / 180
    dim as single dx = cos(ang)*r*0.75, dy = sin(ang)*r*0.75
    for s as single = -1 to 1 step 0.08
      aa_dot.draw cx + dx*s, cy + dy*s, SCREW_SLOT, 0.9, 1
    next
    
  end sub

  sub draw_knob_body(cx as single, cy as single, r as single)
    aa_dot.draw cx, cy, KNOB_RIM, r+1.2, 1
    aa_dot.draw cx, cy, KNOB_BODY, r, 5
    'aa_dot.draw cx - r*0.28, cy - r*0.30, KNOB_HI, r*0.62, 1
    aa_dot.draw cx + r*0.3, cy + r*0.3, KNOB_HI, r*0.5, 1
  end sub

  sub draw_lamp(cx as single, cy as single, v as single, hue as single )
    v = clamp(v)
    
    dim as single lRad = 3.0 + v*2.0
    dim as single va = 0.4, vb = 1 - va, sat_lamp, sat_aura, val_aura, val_lamp
        
    sat_aura = iif( v < 0.5, 0, (v - 0.5) * 2)
    val_aura = clamp( v * 2 )
    aa_dot.draw cx, cy, hsv( hue, sat_aura, val_aura ), lRad*1.35, 0.8
    
    sat_lamp = (.5 - triwave(v)) * 2
    val_lamp = clamp(va + 1.5 * v * vb)
    aa_dot.draw cx, cy, hsv( hue, sat_lamp, val_lamp ), lRad, 1
    
    '' specular highlight idea by DeepSeek
    aa_dot.draw cx - lRad*0.3, cy - lRad*0.3, , lRad*0.38, 1 '' sharpness
    
  end sub

    
    sub _knob_background_bake( byref des as imvars, byref reference as tKnob, r as single = 1 )
      
      if des.w = reference.size or des.h = reference.size then exit sub
      fill_imvars des, imagecreate( reference.size, reference.size )
      
      '' plate
      for y as long = 0 to des.hm
        dim as ulong ptr p = cast(ulong ptr, des.pixels + y*des.pitch)
        dim as ulong c_result
        dim as single f = y / des.hm
        Alpha256( c_result, PANEL_HI, PANEL_LO, 256.499 * f )
        for x as long = 0 to des.wm : p[x] = c_result : next
      next

      aa_dot.render_target des.im
      
      dim as single si = 11, sr = 3
      draw_screw si,            si,            sr
      draw_screw des.w-1-si,     si,            sr
      draw_screw si,            des.h-1-si,     sr
      draw_screw des.w-1-si,     des.h-1-si,     sr

      dim as single ve = reference._val
      dim as single cx = knob_cx(reference), cy = knob_cy(reference)
      draw_arc reference, cx, cy, 135, 405, TICK_COLOR, .7, 1.0

      draw_knob_body cx, cy, reference.size * 0.25 * r

    End Sub

        dim as imvars   imv_knob_background
        dim as imvars   imv_knob_complete


sub knob_bake(byref t as tKnob, r as single = 1)

  if imv_knob_complete.w <> t.size or imv_knob_complete.h <> t.size then _
  fill_imvars imv_knob_complete, imagecreate( t.size, t.size )
 
  _knob_background_bake imv_knob_background, t
 
  put imv_knob_complete.im, (0,0), imv_knob_background.im, PSET
  aa_dot.render_target imv_knob_complete.im
  
  dim as single cx = knob_cx(t), cy = knob_cy(t)     ' local from macro
  draw string imv_knob_complete.im, (cx-0 - len(t.label)*4, 09), t.label, hsv(t.hue, 0.10, 0.1, 255)
  draw string imv_knob_complete.im, (cx-1 - len(t.label)*4, 11), t.label, hsv(t.hue, 0.10, 0.1, 255)
  draw string imv_knob_complete.im, (cx+1 - len(t.label)*4, 10), t.label, hsv(t.hue, 0.10, 0.91, 255)
  draw string imv_knob_complete.im, (cx+0 - len(t.label)*4, 10), t.label, hsv(t.hue, 0.70, 0.99, 255)
  
  dim as single ve = t._val
  draw_arc t, cx, cy, 135, 135 + ve*270, hsv(t.hue,0.4,0.8), 1.1, 1.0
  draw_lamp cx, cy - t.size * 0.18 * r, ve, t.hue

  t.dirty = false
end sub

sub knob_putdown(byref t as tKnob)
  if t.dirty then knob_bake(t)
  
  put (t.x, t.y), imv_knob_complete.im, pset
  dim as single cx = knob_cx(t)
  
  dim as string vs = knob_valstr(t)
  draw string (t.x + cx - len(vs)*4, t.y + t.size - 18), vs, hsv(t.hue, 0.3, 0.75, 255)
end sub

sub knob_images_cleanup
    _gfx_release imv_knob_background.im
    _gfx_release imv_knob_complete.im
End Sub

' ---- group panels -------------------------------------------------------
' render_type 0 : rectangle background + heading centered at top; knob labels are suffixes
' render_type 1 : no visual grouping; knob labels combine group label + suffix

function group_knob_label( byref g as tKnobs ) as string
  if g.render_type = 0 then return g.suffixes
  if g.label = "" then return g.suffixes
  return g.label + " " + g.suffixes
end function

sub apply_group_labels(a() as tKnob, byref g as tKnobs, i as long, n as long = 0 )
  if n = 0 then n = g.n
  g.n = n
  for j as long = i to i + n - 1
    a(j).label = group_knob_label(g)
    a(j).dirty = true
  next
end sub

sub draw_group_panel(a() as tKnob, byref g as tKnobs)
  if g.render_type <> 0 then exit sub

  dim as long x0 = a(g.first).x - 8
  dim as long x1 = a(g.first + g.n - 1).x + a(g.first + g.n - 1).size + 8
  dim as long y0 = a(g.first).y - 26
  dim as long y1 = a(g.first).y + a(g.first).size + 8

  line (x0, y0)-(x1, y0 + 20), PANEL_HI, B   ' heading strip
  line (x0, y0+20)-(x1, y1), PANEL_HI, B
  dim as long tx = x0 + (x1 - x0) / 2 - len(g.group_label) * 4
  draw string (tx, y0 + 7), g.group_label, hsv(g.hue, 0.30, 0.85, 255)
end sub

end namespace ' -- knobs

namespace my_wavetables

  dim as long     wt_len_tone    = 110
  dim as long     wt_samp_valmax = 65536 * 3
  
  dim as long     wt_sine(), wt_tri(), wt_saw(), wt_square(), wt_noise()', wt_wave()
  dim as long ptr pWave(0 to 3)
  dim as long     wt_len_noise
  dim as single   virtual_len_noise

  function f_idx_tonal(t as single) as long
    return (wt_len_tone) * (t - int(t)) - 0.5 '' 2026 Nov 23
  end function

function wSine(t as single)  as long  : return wt_sine( f_idx_tonal(t) )   : end function
function wTri(t as single)   as long  : return wt_tri( f_idx_tonal(t) )    : end function
function wSaw(t as single)   as long  : return wt_saw( f_idx_tonal(t) )    : end function
function wSquare(t as single) as long : return wt_square( f_idx_tonal(t) ) : end function

function wNoise(t as single) as long  : return wt_noise( int(t) mod virtual_len_noise ): end function

sub fill_wavetables(u as long = -1)
    if u > -1 then wt_len_tone = u + 1
    u = wt_len_tone - 1
    if u <> ubound(wt_sine) then
        redim wt_sine(u) : redim wt_tri(u) : redim wt_saw(u) : redim wt_square(u)
    end if
    for i as long = 0 to wt_len_tone - 1
    wt_sine(i)   = sin(tau * i / wt_len_tone)               * wt_samp_valmax / 2 - 0.5 '' freebasic
    wt_tri(i)    = (abs(i / wt_len_tone - 0.5) - 0.25)      * wt_samp_valmax * 2 - 0.5
    wt_saw(i)    = (i / wt_len_tone - 0.5)                  * wt_samp_valmax - 0.5
    wt_square(i) = (i \ (wt_len_tone / 2) - 0.5)            * wt_samp_valmax - 0.5
    next
    redim wt_noise(rate * 1.75 - 1)
    wt_len_noise = ubound(wt_noise) + 1
    for i as long = 0 to wt_len_noise - 1
        wt_noise(i) = (rnd - 0.5) * wt_samp_valmax - 0.5
    next
    if virtual_len_noise < 0.001 then virtual_len_noise = wt_len_noise
    pWave(0) = @wt_sine(0) :  pWave(1) = @wt_tri(0) :  pWave(2) = @wt_saw(0) :  pWave(3) = @wt_square(0)
end sub

    function f_wavemorph( morph_val as single, samp_index as long ) as long
        morph_val = clamp(morph_val) * (ubound(pwave) - 0.001)
        dim as long k = int(morph_val) '' 0 1 or 2
        dim as long samp__wtable_A = pwave(k)[samp_index]
        dim as long samp__wtable_B = pwave(k+1)[samp_index]
        return samp__wtable_A + frac(morph_val) * ( samp__wtable_B - samp__wtable_A )
    End function
  
end namespace

using my_wavetables

' ------------------------------ LngLR / buffer ----------------------------
union LngLR
  as long sval
  type
    as long L, R
  end type
end union

dim shared as LngLR precision_intbuf()
redim precision_intbuf(nSamplesPerBuffer - 1)

#Macro LimitThenCopy(pdest, pSrc)
  pSrc = pSrc / _master_vol_divideval
  if pSrc >  32767 then pSrc =  32767
  if pSrc < -32767 then pSrc = -32767
  pdest = pSrc
#EndMacro

' ------------------------------ SYNTH BACKEND -----------------------------
using knobs

namespace mySynthBackend

dim as single  Base_Note_Index = 60
dim as single  master_vol      = 0.2
dim as long    c_notesplaying
dim as long    _master_vol_divideval

'' ceil comes live from the Base knob's hi; flo is fixed per group (Hermes) (human comment: group flo may lack flexibility)
type tLFOParams
  as tKnob ptr pBase, pVari, pRate
  as single    flo
end type

sub set_LFO(byref p as tLFOParams, a() as tKnob, i as long, mflo as single = 0.0)
  p.pBase = @a(i) : p.pVari = @a(i+1) : p.pRate = @a(i+2) : p.flo = mflo
end sub

type pos_dbl
  as double t
end type

type t_SynthPatch
  as tLFOParams LFO_FMAmt
  as tLFOParams LFO_q
  as tLFOParams LFO_wavemorph
end type

dim as t_SynthPatch synth_patch

type notevars
  as pos_dbl        chanL, chanR
  as double         mod_phase        '' one phase for all 3 groups (time base)
  as tLFOParams ptr pFM_Amp_LFO
  as tLFOParams ptr pWavemorph_LFO
  as tLFOParams ptr pQuant_LFO
  as single         velo      = 0.6
  as single         vol_noise = 0.7
  as single         fm_ratio  = 1.4
  as single         inc
end type

dim as notevars   shared_notes()

function myfunc( nvars as notevars,_
                t as single,_
                morph_val as single,_
                fm_amt as single,_
                noise_quant as single ) as long
                
  dim as single fm  = fm_amt * wSine( t * nvars.fm_ratio ) / wt_samp_valmax
  dim as long wave  = f_wavemorph( morph_val, f_idx_tonal(t + fm) )
  dim as long noise = wnoise( f_quant(t*8, noise_quant) )
  
  return ( wave + nvars.vol_noise * (noise - wave) ) * nvars.velo
end function

    function lfo_sample(byref p as tLFOParams, phase as double) as single
      dim as single w = wsine(phase * *p.pRate) / wt_samp_valmax
      return clamp( *p.pBase + w * *p.pVari, p.pBase->hi, p.flo )
    end function

'' i initially created LUTs to match wavetables' size; now they match audio main
dim as single     LFO_wavemorphLUT()
dim as single     LFO_FMAmtLUT()
dim as single     LFO_qLUT()

'' fill per-buffer modulation tables from a note's mod_phase (sample-accurate)
sub fill_mod_luts(byref nv as notevars, n as long)
  for i as long = 0 to n - 1
    dim as double ph = nv.mod_phase
    LFO_FMAmtLUT(i)     = lfo_sample(*nv.pFM_Amp_LFO, ph)
    LFO_wavemorphLUT(i) = lfo_sample(*nv.pWavemorph_LFO, ph)
    LFO_qLUT(i)         = lfo_sample(*nv.pQuant_LFO, ph)
    nv.mod_phase += 1.0 / rate
  next
end sub


    #Macro SampleLogic(nvars, pChan, pDest)
      k = myfunc(nvars, pChan.t, LFO_wavemorphLUT(i), LFO_FMAmtLUT(i), LFO_qLUT(i) )
      pChan.t += nvars.inc
      pDest += k
    #EndMacro

sub AddNoteToSignal(a() as LngLR, byref nvars as notevars)
  dim as long k
  for I as long = 0 to ubound(a)
    SampleLogic( nvars, nvars.chanL, a(I).L )
    SampleLogic( nvars, nvars.chanR, a(I).R )
  next
end sub

'' one-shot setup: size LUTs, master scale.  call before audio callback.
sub synth_init()
  fill_wavetables
  redim LFO_wavemorphLUT( nSamplesPerBuffer-1 )
  redim LFO_FMAmtLUT( nSamplesPerBuffer-1 )
  redim LFO_qLUT( nSamplesPerBuffer-1 )
  _master_vol_divideval = wt_samp_valmax / 65536
end sub

sub SetNote(note as single, velo as single = 1.0)
    if c_notesplaying > ubound(shared_notes) then exit sub
    
    dim byref as notevars nvars = shared_notes(c_notesplaying)
    nvars.velo            = velo * master_vol
    nvars.inc             = 440 * 2 ^ ((note - 57) / 12) / rate
    dim as single _phase_offs     = 0.5
    nvars.chanL.t         = Rnd * _phase_offs
    nvars.chanR.t         = Rnd * _phase_offs
    nvars.mod_phase       = 0
    nvars.pFM_Amp_LFO     = @synth_patch.LFO_FMAmt
    nvars.pQuant_LFO      = @synth_patch.LFO_q
    nvars.pWavemorph_LFO  = @synth_patch.LFO_wavemorph
    c_notesplaying += 1
end sub

end namespace ' --- mySynthBackend

    using mySynthBackend


' ---- AUDIO CALLBACK
'
sub custom_signal(pSamples as short ptr)
  
  '' reset the main signal
  for i as long = 0 to nSamplesPerBuffer - 1
    precision_intbuf(i).L = 0
    precision_intbuf(i).R = 0
  next
  
  for J as long = 0 to c_NotesPlaying - 1
    '' fill per-sample modulation tables for this note (sample-accurate), then render
    fill_mod_luts shared_notes(J), nSamplesPerBuffer
    AddNoteToSignal(precision_intbuf(), shared_notes(J))
  next
  
  for I as long = 0 to nSamplesPerBuffer - 1
    dim as long i2 = i * 2
    LimitThenCopy(pSamples[i2 + 0], precision_intbuf(I).L)
    LimitThenCopy(pSamples[i2 + 1], precision_intbuf(I).R)
  next
end sub
  '
  ' ------- synth.bas


'' ============================================================
''  scope
'' idea: show waveshape  (dafhi)
'' code: DeepSeek 95%
'' debug: (dafhi) 5%
'' idea + code: DeepSeek
''    wave modes:  0 = carrier, 1 = carrier+FM, 2 = carrier+FM+noise
''    TAB cycles wave mode
'' idea: noise detail (not implimented) (dafhi)
'' ============================================================

namespace scopes
  const as long WAVE_W = 280
  const as long WAVE_H = 100
  const as long LFO_W  = 280
  const as long LFO_H  = 110
  const as long PAD    = 6
  const as long COL_W  = (LFO_W - PAD*4) \ 3
  const as long COL_H  = LFO_H - PAD*2 - 20

  dim as long     _x = 490, _y = 10
  dim as long     _wave_mode = 2
  dim as any ptr  wave_im = 0
  dim as any ptr  lof_im  = 0

  dim as string*20 titles(2) => { "Carrier", "FM", "Output" }
  
  sub init(x as long = -1, y as long = -1)
    if x > -1 then _x = x
    if y > -1 then _y = y
    if wave_im = 0 then wave_im = imagecreate(WAVE_W, WAVE_H)
    if lof_im  = 0 then lof_im  = imagecreate(LFO_W,  LFO_H)
  end sub

  sub close_bufs()
    if wave_im <> 0 then imagedestroy wave_im : wave_im = 0
    if lof_im  <> 0 then imagedestroy lof_im  : lof_im  = 0
  end sub

  sub next_wave_mode()
    _wave_mode = (_wave_mode + 1) mod 3
  end sub

  '' ---- wave reveal ----
  function sample_wave(phase as single) as single

    dim as single fm_amt = 0
    dim as single morph  = 0.5
    dim as single q_val  = 8
    
    if c_notesplaying > 0 then
      dim as notevars ptr nv = @shared_notes(0)
      dim as double ph = nv->mod_phase
      fm_amt = lfo_sample(*nv->pFM_Amp_LFO,    ph)
      morph  = lfo_sample(*nv->pWavemorph_LFO, ph)
      q_val  = lfo_sample(*nv->pQuant_LFO,     ph)
    end if

    dim as single fm  = 0
    if _wave_mode >= 1 then
      dim as single fm_ratio = 1.4
      if c_notesplaying > 0 then fm_ratio = shared_notes(0).fm_ratio
      fm = fm_amt * wSine(phase * fm_ratio) / wt_samp_valmax
    end if

    dim as long   idx  = f_idx_tonal(phase + fm)
    dim as long   wave = f_wavemorph(morph, idx)

    if _wave_mode < 2 then return wave / wt_samp_valmax

    dim as single vol_noise = 0
    if c_notesplaying > 0 then vol_noise = shared_notes(0).vol_noise
    dim as long   noise = wnoise( f_quant(phase * 8, q_val) )
    return (wave + vol_noise * (noise - wave)) / wt_samp_valmax
  end function

  sub draw_wave()
    if wave_im = 0 then exit sub
    if ScreenPtr = 0 then exit sub

    line wave_im, (0,0)-(WAVE_W-1, WAVE_H-1), rgb(8,8,12), BF
    line wave_im, (2, 20)-(WAVE_W-3, WAVE_H-3), rgb(40,40,52), B
    line wave_im, (2, (20 + WAVE_H-3)\2)-(WAVE_W-3, (20 + WAVE_H-3)\2), rgb(30,30,38)

    static as ulong  cols(0 to 2)   = { rgb(120,200,255), rgb(255,180,120), rgb(160,255,160) }
    draw string wave_im, (5, 4), titles(_wave_mode), cols(_wave_mode)

    dim as single prev_y = -1
    for px as long = 0 to WAVE_W - 1
      dim as single phase = px / (WAVE_W - 1)
      dim as single v = sample_wave(phase)
      v = clamp(v * 1.8, 1, -1)
      dim as single y = (20 + (WAVE_H - 3 - 20)\2) - v * ((WAVE_H - 3 - 20) * 0.45)
      if prev_y >= 0 then line wave_im, (px, prev_y)-(px + 1, y), cols(_wave_mode)
      prev_y = y
    next

    line wave_im, (0,0)-(WAVE_W-1, WAVE_H-1), rgb(80,80,100), B
    put (_x, _y), wave_im, pset
  end sub

  '' ---- LFO strip ----
  sub trace_lfo(byref p as tLFOParams, cx as long, cy as long, cw as long, ch as long, _
                col as ulong, playhead_phase as double)
    dim as single rate = *p.pRate
    if rate < 0.0001 then exit sub

    dim as long   N = cw - 1
    dim as single prevY = -1
    for i as long = 0 to N
      dim as single phase = i / N * (1.0 / rate)
      dim as single v  = lfo_sample(p, phase)
      dim as single lo = p.flo
      dim as single hi = p.pBase->hi
      dim as single mi = (hi + lo) * 0.5
      dim as single hs = (hi - lo) * 0.5
      if hs < 0.0001 then hs = 1
      dim as single norm = clamp((v - mi) / hs, 1, -1)
      dim as single y = (cy + ch*0.5) - norm * (ch * 0.45)
      if prevY >= 0 then line lof_im, (cx + i - 1, prevY)-(cx + i, y), col
      prevY = y
    next

    dim as single rel = (playhead_phase * rate) - int(playhead_phase * rate)
    dim as long   px  = cx + rel * N
    dim as single v   = lfo_sample(p, playhead_phase)
    dim as single lo  = p.flo, hi = p.pBase->hi
    dim as single mi  = (hi + lo) * 0.5
    dim as single hs  = (hi - lo) * 0.5
    if hs < 0.0001 then hs = 1
    dim as single py  = (cy + ch*0.5) - ((v - mi) / hs) * (ch * 0.45)
    line lof_im, (px - 3, py)-(px + 3, py), rgb(255,255,255)
    line lof_im, (px, py - 3)-(px, py + 3), rgb(255,255,255)
  end sub

  sub draw_lfo()
    if lof_im = 0 then exit sub
    if ScreenPtr = 0 then exit sub

    line lof_im, (0,0)-(LFO_W-1, LFO_H-1), rgb(8,8,12), BF

    dim as double ph = 0
    if c_notesplaying > 0 then ph = shared_notes(0).mod_phase

    dim as tLFOParams ptr lfos(0 to 2)
    lfos(0) = @synth_patch.LFO_wavemorph
    lfos(1) = @synth_patch.LFO_FMAmt
    lfos(2) = @synth_patch.LFO_q

    static as string*20 names(0 to 2) = { "Morph", "FM", "Quant" }
    dim as ulong cols(0 to 2) = { hsv(0.06,0.7,0.9), hsv(0.10,0.7,0.9), hsv(0.30,0.7,0.9) }

    for k as long = 0 to 2
      dim as long cx = PAD + k * (COL_W + PAD)
      dim as long cy = PAD + 20
      line lof_im, (cx, cy)-(cx + COL_W - 1, cy + COL_H - 1), rgb(40,40,52), B
      line lof_im, (cx, cy + COL_H\2)-(cx + COL_W - 1, cy + COL_H\2), rgb(30,30,38)
      dim as string ttl = names(k) & "  " & round(*lfos(k)->pRate, 1) & " Hz"
      draw string lof_im, (cx + 2, PAD + 4), ttl, cols(k)
      trace_lfo *lfos(k), cx + 1, cy + 1, COL_W - 2, COL_H - 2, cols(k), ph
    next

    line lof_im, (0,0)-(LFO_W-1, LFO_H-1), rgb(80,80,100), B
    put (_x, _y + WAVE_H + 5), lof_im, pset
  end sub

  '' ---- public ----
  sub key_poll()
    static as boolean tab_dn = false
    if multikey(15) then  '' Tab scancode
      if not tab_dn then
        tab_dn = true
        next_wave_mode
      end if
    else
      tab_dn = false
    end if
  end sub

  sub draw_it()
    draw_wave
    draw_lfo
  end sub
end namespace ' --- scopes

    using scopes


randomize

synth_init


dim shared as tKnob myk(0 to 11)

sub kinit( index as long, startV as single, hi as single, _
           lbl as string = "", lo as single = 0.0, x as single = 0, y as single = 0, ste as single = 0.0, size as long = 0 )
  dim byref as tKnob t = myk(index)
  if size < 1 then size = t.size
  knob_init t, hi, startV, size, lbl, lo, ste, x, y
end sub

' -- this ui section is a bit of a hack
'
dim shared as tKnobs  groups(0 to 3)

dim as long _
                      index = 0, _
                      size = 120, _
                      gap_x = 20, _
                      gap_y = 20, _
                      y0 = 55

'' --- Group 0: Morph (3 cols) ---
with groups(0)
  .cols = 3 : .rows = 1
  .x = 40 : .y = y0
  .hue = 0.06 : .hue_spread = 0.06
  .group_label = "Morph"
  .suffixes = "Cen;Amp;LFO"
  .render_type = 0
end with
init_knobs myk(), groups(0), index, size, gap_x, gap_y
kinit  0,   1/4,   1.0
kinit  1,   0.0,   2.0
kinit  2,   9,   5000
set_LFO synth_patch.LFO_wavemorph, myk(), index

with groups(1)
  .cols = 3 : .rows = 1
  .x = 40 : .y = y0 + (155 + gap_y)
  .hue = 0.10 : .hue_spread = 0.04
  .group_label = "FM Amp"
  .suffixes = "Cen;Amp;LFO"
  .render_type = 0
end with
index = 3 : size = 130
init_knobs myk(), groups(1), index, size, gap_x, gap_y
kinit  3,   0.5,   2.5
kinit  4,   0.0,   2.5
kinit  5,   4.0,   5000
myk(6) = myk(5)
kinit  6,  0.001, 2.5,  "FM Ratio", 0.000, groups(1).x + 3 * (size + gap_x), groups(1).y, 0.0 '' step

set_LFO synth_patch.LFO_FMAmt,   myk(), index, -myk(6).hi '' DeepSeek suggestion

with groups(2)
  .cols = 3 : .rows = 1
  .x = 40 : .y = y0 + (155 + gap_y) * 2
  .hue = 0.30 : .hue_spread = 0.06
  .group_label = "Noise Quant"
  .suffixes = "Cen;Amp;LFO"
  .render_type = 0
end with
index = 7
init_knobs myk(), groups(2), index, size, gap_x, gap_y
kinit  7,  4.0,  20.0,  "", 0.001
kinit  8,  0.0,  20.0
kinit  9,  8.0,  24.0
set_LFO synth_patch.LFO_q,      myk(), index, 0.001 '' floor

with groups(3)
  .cols = 2 : .rows = 1
  .x = 40 + (size + gap_x) * 3 : .y = y0 + (155 + gap_y) * 2
  .hue = 0.65 : .hue_spread = 0.10
  .group_label = "Noise"
  .suffixes = "Vol;BufLen"
  .render_type = 1
end with
index = 10
init_knobs myk(), groups(3), index, size, gap_x, gap_y
kinit 10,       0.0,    1.0,  "", 0.0 '' lo
kinit 11, 2500, wt_len_noise, "", 2.5
  '
  ' --- ui hack

' ---- map knobs -> synth params ----
sub apply_knobs()
  for i as long = 0 to c_notesplaying - 1
    dim byref as notevars nv = shared_notes(i)
    nv.fm_ratio  = myk(6)
    nv.vol_noise = myk(10)
  next
  virtual_len_noise = myk(11)
end sub

  redim shared_notes(0 to 31)


dim as single velo = 0.7
SetNote Base_Note_Index, velo
SetNote Base_Note_Index+4, velo

apply_knobs

' ------------------------------ STARTUP -----------------------------------
if waveOutGetNumDevs() < 1 then
  ? "sorry no active WAVE output device !"
  beep
end if

screenres 800, 600, 32

if ScreenPtr <> 0 then scopes.init '490, 10

for i as long = 0 to ubound(groups) : draw_group_panel myk(), groups(i) : next
for i as long = 0 to ubound(myk)    : knob_putdown myk(i) : next


dim as WAVEOUT_DEVICE WaveOut

' ------------------------------ MAIN LOOP --------------------------------
dim as long     activeKnob = -1, mx, my, mb, prevMb, prevMy
dim as single   dragStartY

dim as single   t0 = Timer

locate 2,2: print "Key TAB for top scope mode"

do
  getmouse mx, my, , mb
  if (inkey = chr(27)) or (timer - t0 > tDemoSeconds) then exit do

  if mb andalso (prevMb = 0) then
    for i as long = 0 to ubound( myk )
      if mousedown__knob_hit( myk(i), mx, my) then
        activeKnob   = i
        dragStartY   = my
        exit for
      end if
    next
  elseif mb andalso prevMy<>my andalso activeKnob >= 0 then
    knob_set_from_drag myk(activeKnob), dragStartY, my
    knob_putdown myk(activeKnob)
    apply_knobs
  elseif (mb = 0) andalso prevMb then
    activeKnob = -1
  end if
  prevMb = mb
  prevMy = my
  scopes.key_poll
  scopes.draw_it
  sleep 8
loop

knob_images_cleanup
scopes.close_bufs

print "demo finished.  exiting .."
sleep 1500

