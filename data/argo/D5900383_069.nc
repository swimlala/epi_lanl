CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   I   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-04-28T00:51:29Z creation;2011-02-14T00:43:20Z update;2015-06-08T23:39:54Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        $  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       $  ;   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  <4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  <�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  ?   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  @8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  @�   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  C   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  D<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  D�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  E�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   F<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   O<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   X<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  a<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    b   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         b0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         b4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        b8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b<Argo profile    3.1 1.2 19500101000000  5900383 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA  20050428005129  20150614012510  A4_24957_069                    2C  D   APEX                            921                             072602                          846 @ӻu�%*�1   @ӻv����@6��v��e$r� Ĝ1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��A�ffA���B33BDffBn  B���B�  B���B���B���B�  CL�C  C� C  C)33C333C=L�CG33C[ffCo�C�� C���C���C��3C���C��fC�s3Cǌ�Cѳ3Cۙ�C�3C�fC���D��D��D��DٚDٚDٚD� D$��D)� D.��D3�3D8�3D=ٚDB��DG�fDN�DT` DZ�fD`�3Dg  DmS3Ds�3Dy� D�)�D�i�D��fD��fD�,�D�p D�ٚD�c3D���D�i�D���D�S3D���D�\�1111111111111111111111111111111111111111111111111111111111111111111111111   @�ffA��A�ffA���B33BDffBn  B���B�  B���B���B���B�  CL�C  C� C  C)33C333C=L�CG33C[ffCo�C�� C���C���C��3C���C��fC�s3Cǌ�Cѳ3Cۙ�C�3C�fC���D��D��D��DٚDٚDٚD� D$��D)� D.��D3�3D8�3D=ٚDB��DG�fDN�DT` DZ�fD`�3Dg  DmS3Ds�3Dy� D�)�D�i�D��fD��fD�,�D�p D�ٚD�c3D���D�i�D���D�S3D���D�\�2222222222222222222222222222222222222222222222222222222222222222222222222   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AƟ�AƑhAƁA�\)A�7LA�r�A��\A���A��-A�(�A���A�1'A�1A�z�A�&�A��mA�1A�ƨA�~�A���A�5?A��ArffAe�AY\)AL�9AC�TA7��A-�A#`BA�DA��AA�A	%AV@���@�+@ׅ@�
=@î@���@�\)@���@��h@�;d@�j@���@��@��@���@��@�r�@�^5@�ƨ@��w@��m@�w@~�+@yX@w�w@q�@h�`@\�@St�@K��@?�@3o@'�@I�@�F@33@��@��1111111111111111111111111111111111111111111111111111111111111111111111111   AƟ�AƑhAƁA�\)A�7LA�r�A��\A���A��-A�(�A���A�1'A�1A�z�A�&�A��mA�1A�ƨA�~�A���A�5?A��ArffAe�AY\)AL�9AC�TA7��A-�A#`BA�DA��AA�A	%AV@���@�+@ׅ@�
=@î@���@�\)@���@��h@�;d@�j@���@��@��@���@��@�r�@�^5@�ƨ@��w@��m@�w@~�+@yX@w�w@q�@h�`@\�@St�@K��@?�@3o@'�@I�@�F@33@��@��2222222222222222222222222222222222222222222222222222222222222222222222222   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B�sB�sB�B�B�^B}�B�B��B�TB�}B�-B�-B��B�+BW
B:^B
ĜB
"�B	��B	�1B	M�B	{B�`BĜB��B��B�=Bz�Bv�BiyBn�Bp�B� BiyBo�B�oB��B��B�mB�B��B	+B	bB	�B	'�B	<jB	N�B	ffB	�JB	�B	�RB	��B	�
B	�#B	�B
  B
DB
�B
)�B
0!B
=qB
G�B
S�B
aHB
jB
r�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��BB��B�B�yB�)B�)B�jB�B�B��B�`B��B�-B�3B��B�7BXB<jB
ƨB
$�B	��B	�=B	N�B	�B�fBŢB��B��B�DB{�Bw�Bk�Bo�Bq�B�BiyBp�B�oB��B��B�mB�B��B	+B	bB	�B	'�B	<jB	N�B	ffB	�JB	�B	�RB	��B	�
B	�#B	�B
  B
DB
�B
)�B
0!B
=qB
G�B
S�B
aHB
jB
r�B
z�B
z�2222222222222222222222222222222222222222222222222222222222222222222222222   <#�
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
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200505090000002005050900000020050509000000200701261121412007012611214120070126112141200506090000002005060900000020050609000000  JA  ARFMfmtp2.2                                                                 20050428005129  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050428005129  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050428005646                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050501205243  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050501205244  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrelo2.1                                                                 20050501205244  CV  TIME            G�O�G�O�F�۸                JA  ARGQrelo2.1                                                                 20050501205244  CV  LAT$            G�O�G�O�A�x�                JA  ARGQrelo2.1                                                                 20050501205244  CV  LON$            G�O�G�O��)#�                JA  ARUP                                                                        20050501205802                      G�O�G�O�G�O�                JM  ARGQJMQC                                                                    20050429000000  CV  DAT$            G�O�G�O�F�۴                JM  ARGQJMQC                                                                    20050429000000  CV  LAT$            G�O�G�O�A�p�                JM  ARGQJMQC                                                                    20050429000000  CV  LON$            G�O�G�O��)�                JM  ARCAJMQC                                                                    20050509000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050509000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050609000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060131021350  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060202051826                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312114616  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318055454  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318055947                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110208022113  CV  JULD            G�O�G�O�F�۲                JM  ARCAJMTM1.0                                                                 20070126112141  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20110208022113  CF  PRES_ADJUSTED_QC@�ffD�\�G�O�                JM  AREQREJM1.0                                                                 20110208022113  CF  TEMP_ADJUSTED_QC@�ffD�\�G�O�                JM  AREQREJM1.0                                                                 20110208022113  CF  PSAL_ADJUSTED_QC@�ffD�\�G�O�                JA  RFMTcnvd2.1                                                                 20110214004127  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110214004320                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608233950                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614012510                      G�O�G�O�G�O�                