CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   history       d2012-03-19T08:11:56Z creation; 2013-03-22T15:15:56Z updated; 2021-01-17T20:39:22Z converted from 2.2   title         Argo float vertical profile    institution       AOML   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   axis      T      
resolution        >�E�vQ�        8   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        8   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8$   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8,   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    80   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    88   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8<   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8@   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           8D   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9d   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        ;   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        <4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  =T   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ?   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        @$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  AD   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        B�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    DD   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    GD   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    JD   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  MD   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    O$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    O4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         OH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         OL   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        OP   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OT   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    Mp   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        Np   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     Nt   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     N�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     N�Argo profile    3.1 1.2 19500101000000  5903828 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  20120319081156  20210117204137  4213_108689_007                 2C  D   846 @�0n�c�1   @�0PD� @5�Ƨ�e$��
=q1   ARGOS   A   A   A   @���A$��A�ffA���A�ffB��B333BPffBm33B�ffB�  B���B�ffB�33B�ffB晚B���C33C33C� C� C#� C+� C4� C>33CH�CRL�C]��Ch�3CtffC�L�C�L�C��C��C��3C�@ C��fC�&fC�� CɦfCә�C޳3C�&fC�@ DL�D�D�DffD�D&S3D.��D7��DA�DK  DU�3D`� Dl�Dx3D�i�D�,�D�,�D��3D�9�D��fD�fD�&fD£3D̆fD�fD�0 D�fD�s3111111111111111111111111111111111111111111111111111111111111111111111111@���A#33A���A���A���B34B2��BP  Bl��B�33B���B���B�33B�  B�33B�fgB�fgC�C�CffCffC#ffC+ffC4ffC>�CH  CR33C]� Ch��CtL�C�@ C�@ C��C��C��fC�33C���C��C�s3Cə�Cӌ�CަfC��C�33DFgD4D4D` DgD&L�D.�gD7�gDA4DK�DU��D`��DlgDx�D�fgD�)�D�)�D�� D�6gD��3D�3D�#3D  D̃3D�3D�,�D�3D�p 111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AÝ�Aå�Aé�Aé�Aá�Aç�Aá�AÝ�AÕ�AÏ\AÃA�hsA�  A¸RA���A�Q�A��#A��A���A�O�A���A�1A��#A��+A��A��jA�`BA�?}A�5?A�|�AuS�AgO�A[p�AQ��AI�A<�DA3l�A)`BA&A�wAXA
{Ahs@�1@�ȴ@�
=@�?}@���@�|�@�@�@���@���@���@�V@��@��^@�5?@��T@|z�@s@e�@]�T@UV@JJ@@�@7|�@,�D@�w@ȴ@�T@��111111111111111111111111111111111111111111111111111111111111111111111111AÝ�Aå�Aé�Aé�Aá�Aç�Aá�AÝ�AÕ�AÏ\AÃA�hsA�  A¸RA���A�Q�A��#A��A���A�O�A���A�1A��#A��+A��A��jA�`BA�?}A�5?A�|�AuS�AgO�A[p�AQ��AI�A<�DA3l�A)`BA&A�wAXA
{Ahs@�1@�ȴ@�
=@�?}@���@�|�@�@�@���@���@���@�V@��@��^@�5?@��T@|z�@s@e�@]�T@UV@JJ@@�@7|�@,�D@�w@ȴ@�T@��111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBW
BXBXBVBVBVBT�BVBT�BVBW
BZB[#BYBXBJ�BE�B;dB$�B�B�XB�JBaHB0!B�B�}BP�B
��B
�%B
0!B	��B	�=B	W
B	)�B��B�NBȴB�jB��B�Bm�BcTBYB`BBu�B��B�B��B��B	oB	K�B	s�B	��B	�wB	��B	�yB	��B
  B
hB
�B
%�B
,B
1'B
:^B
A�B
I�B
P�B
[#B
bNB
l�B
v�111111111111111111111111111111111111111111111111111111111111111111111111BXBWBX BX$BVBVBVBUBVBUBV%BWPBZVB[�BZBX�BK_BG]B<�B'YB�'B�uB�#BcKB2�B�UB��BT�B
�B
��B
2�B	�]B	�0B	X�B	,B�WB�B�jB��B�B�aBn�Bd�BZ�BaRBv}B�8B��B�'B�3B	�B	LB	s�B	��B	��B	�7B	�B	�B
 4B
�B
�B
&B
,9B
1^B
:�B
A�B
I�B
QB
[LB
bvB
l�B
v�111111111111111111111111111111111111111111111111111111111111111111111111<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.1 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201303111202102013031112021020130311120210  Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                APEX                            5834                            122707                          AO  ARGQ                                                                        20120319081156  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20120319081156  QCF$                G�O�G�O�G�O�0               AO  ARCAADJP                                                                    20120319081156    IP                G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20130311120210  QC  PRES            @���D�s3                    PM  ARSQCTM V1.1                                                                20130311120210  QC  PSAL            @���D�s3                    PM  ARSQOWGUV1.0                                                                20210117204137  IP                  G�O�G�O�G�O�                