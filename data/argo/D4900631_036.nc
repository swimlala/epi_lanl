CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       MEDS   source        
Argo float     history       2015-05-22T20:17:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment              user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  8   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8\   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9    JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�����h�        9$   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9,   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        90   LATITUDE            	   	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y      	reference         WGS84      coordinate_reference_frame        urn:ogc:crs:EPSG::4326          98   	LONGITUDE               	   	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X      	reference         WGS84      coordinate_reference_frame        urn:ogc:crs:EPSG::4326          9@   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9H   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9L   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9T   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9X   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9\   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :`   PRES         
         	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z      coordinate_reference_frame        urn:ogc:crs:EPSG::5113          :d   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ;�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      X           ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        =4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        >T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ?t   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature           ?�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  @�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        A$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        BD   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  Cd   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity          C�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  D�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        E   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  F4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    F�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    L�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    R�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        T  X�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    X�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    X�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    X�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    X�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  X�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Y8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    YH   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    YL   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Y\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         Y`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        Yd   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    YhArgo profile    3.1 1.2 19500101000000  20150522201712  20150522201712  4900631 Canadian Argo                                                   Blair Greenan                                                   PRES            TEMP            PSAL               $A   ME  49006319766PF                   2C+ D   APEX-SBE                        1997                            n/a                             846 @�.��O�1   @�.��O�@;���   �d9�    1   ARGOS   A   A   A   Primary sampling: discrete                                                                                                                                                                                                                                          @�  A��A�  A�  B��BE��Bm��B�33B�ffB�ffBƙ�Bڙ�B���CffCffCffC� C)ffC333C=L�CF�fCP��C[L�CeL�CoffCx�fC�s3C��3C�� C�� C�� C�� C���C���C���C���C��3C�s3Cǳ3C�3C���D	33D�fD"L�D.��D;ffDG� DT@ D`� DmL�Dy�fD�0 D�ffD�� D��D�0 D�c3D���D�� D�,�D�l�D�� D��D��D�s3Dڣ3D�� D�&fD�Y�D�D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111@���AfgA�ffA�ffB  BB��Bj��B���B�  B�  B�34B�34B�fgC �3C
�3C�3C��C(�3C2� C<��CF33CP�CZ��Cd��Cn�3Cx33C��C�Y�C�ffC�ffC�ffC�ffC�@ C�33C�33C�33C�Y�C��C�Y�C�Y�C�@ D	fD��D"  D.��D;9�DG�3DT3D`�3Dm  Dy��D��D�P D���D��4D��D�L�D��4D�ɚD�gD�VgD���D��4D�4D�\�Dڌ�D�ɚD� D�C4D�4D�ɚD�� 111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aӝ�Aө�AӸRAӧ�A�`BA�bNA���A�JA�%A��A�K�A��hA���A��A�bNA��A��HA���A�^Ay��Au��At=qApM�Al�yAf��AdffAahsA\��AXv�AT��AS�AP��AJ��AF�yA@~�A<�A7oA/�A'�AoA/@��@�M�@�1@�v�@�Ĝ@�t�@��@�&�@u��@ol�@f��@`Ĝ@V@L��@H1'@B�@=`B@3t�@.$�@%�@"^5@ȴ@�@��@
=@
�\@��@S�@ b?�1?���111111111111111111111111111111111111111111111111111111111111111111111111Aӝ�Aө�AӸRAӧ�A�`BA�bNA���A�JA�%A��A�K�A��hA���A��A�bNA��A��HA���A�^Ay��Au��At=qApM�Al�yAf��AdffAahsA\��AXv�AT��AS�AP��AJ��AF�yA@~�A<�A7oA/�A'�AoA/@��@�M�@�1@�v�@�Ĝ@�t�@��@�&�@u��@ol�@f��@`Ĝ@V@L��@H1'@B�@=`B@3t�@.$�@%�@"^5@ȴ@�@��@
=@
�\@��@S�@ b?�1?���111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBjBiyBhsBaHBVB��B��B��B}�B>wB-B��B�BB�=B�B
�B
��B
�B
s�B
cTB
[#B
T�B
;dB
2-B
�B
bB
B	�mB	��B	�^B	�^B	�B	�B	cTB	6FB	"�B	B�fB�qB�PBQ�B9XB-B:^B`BB�7B��B�B	�B	XB	v�B	��B	�wB	�BB	��B	��B
PB
�B
/B
6FB
C�B
I�B
O�B
XB
bNB
ffB
l�B
p�B
v�B
{�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111BjBi	BhB`�BU�B�vBӃB��B}�B>B,�B�xB��B��B2B
�OB
�vB
��B
s[B
b�B
Z�B
T�B
;B
1�B
KB
B
�B	�B	ϑB	�B	�B	��B	��B	cB	6B	"�B	�B�.B�=B�#BQ�B96B,�B:?B`B�B˝B�pB	xB	W�B	v�B	��B	�9B	�B	��B	��B
B
iB
.�B
6 B
CQB
IrB
O�B
W�B
bB
f B
lCB
p]B
v�B
{�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            Pcorrected = Praw(n) - surface_pres_offset(n)+5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 PSAL_ADJUSTED is calculated from a conductivity multiplicative adjustment term r.                                                                                                                                                                               PRES_ADJUSTED=PRES + coefficient (see procedure 3.2.1 in Argo DMQC manual v2.4)                                                                                                                                                                                                                                                                                                                                                                                                                                                 PSAL_ADJUSTED is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                     surface_pres_offset = 5.8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       COEFFICIENT r FOR CONDUCTIVITY IS 0.99987, +/- 0.0001709585                                                                                                                                                                                                     ADDITIVE COEFFICIENT FOR PRESSURE ADJUSTMENT IS -0.7db                                                                                                                                                                                                                                                                                                                                                                                                                                                                          r=0.999983, � 0.0001322821                                                                                                                                                                                                                                      Calibration error is manufacturers specified PRES accuracy at time of lab calibration                                                                                                                                                                           Calibration error is manufacturers specified TEMP accuracy at time of lab calibration                                                                                                                                                                           Adjusted salinity to climatology according to WJO(2003). Ref. Data are WOD 2001.                                                                                                                                                                                PRES_ADJUSTED is calculated following the 3.2.1 procedure in the Argo Quality Control Manual version 2.5. No significant pressure drift was detected.Pressure evaluation done on 24-Aug-2010 16:03:44                                                           No approved method for delayed-mode qc on TEMP is available                                                                                                                                                                                                     Sensor drift detected. Adjusted salinity to WJO statistical recommendation with CTD&BOTTLE_2008V1 as reference database. Mapping scales used are 56/52 (lon) 52/50 (lat). Visual piecewise linear fit done upon inspection of profiles 1 to 184. 4 breakpoints. 200903051539032007020203281020070202032810201008241707212010082417072120100824170721ME  RFMT    1.0                                                                 20060730000000  CR  RCRD            G�O�G�O�G�O�                ME  ARDP    1.0                                                                 20060730000000  CR  RCRD            G�O�G�O�G�O�                ME  ARGQ    1.0                                                                 20100824170721  QCP$RCRD            G�O�G�O�G�O�0007FBCE        ME  ARGQ    1.0                                                                 20060730000000  QCF$RCRD            G�O�G�O�G�O�0               ME  ARUP    1.0                                                                 20060730000000  UP  RCRD            G�O�G�O�G�O�                ME  ARCAWJO 1.0 WOD01 WITH MIN_MAP_ERR = -1                                     20061002000000  CV  CNDC            ?�  ?�  ?�                  ME  ARUP    1.0                                                                 20061002000000  UP  RCRD            G�O�G�O�G�O�                CI  ARSQWJO 2.0bWOD01 WITH MIN_MAP_ERR = -1                                     20070912000000  CR  PSAL            G�O�G�O�G�O�                ME  ARDU    1.0                                                                 20071024000000  UP  RCRD            G�O�G�O�G�O�                ME  ARSQWJO 2.0bWOD01 WITH MIN_MAP_ERR = -1                                     20081016000000  QC  PSAL            G�O�G�O�G�O�                ME  ARDU    1.0                                                                 20090305000000  UP  RCRD            G�O�G�O�G�O�                ME  ARSQWJO 2.0bCTD&BOTTLE_2008V1                                               20100824170721  QCCV                G�O�G�O�G�O�                