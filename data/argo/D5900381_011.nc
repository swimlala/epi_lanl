CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   I   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       ~2003-09-24T06:48:15Z creation;2013-11-06T02:32:24Z update;2015-06-08T22:50:18Z conversion to V3.1;2019-05-08T06:15:42Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8�   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        =���   standard_name         sea_water_pressure       $  ;,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  <P   PRES_ADJUSTED_ERROR          
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
resolution        :�o   standard_name         sea_water_temperature        $  ?0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  @T   TEMP_ADJUSTED_ERROR          
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
resolution        :�o   standard_name         sea_water_salinity       $  C4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  DX   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  D�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  E�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   FX   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   OX   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   XX   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  aX   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    b(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         bL   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         bP   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        bT   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    bXArgo profile    3.1 1.2 19500101000000  5900381 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20030924064815  20190630171514  A4_19567_011                    2C  D   APEX                            784                             072602                          846 @�*��΀1   @�*)�� @/~vȴ9X�d��`A�71   ARGOS   A   A   A   Primary sampling: discrete [1 Hz CTD subsampled]                                                                                                                                                                                                                   @���A  A���A陚B33BC33Bn��B�ffB�  B�  B�  B�33B�  C� C  CffCL�C)ffC333C=�CG�C[�3Co33C�s3C�� C��fC���C���C�ٚC��fCǙ�C�L�Cۙ�C� C�s3C��fD�3DٚD�3DٚD� D�fD�3D$� D)ٚD.��D4fD8ٚD=� DB� DG�fDM�fDT� DZs3D`��Dg,�DmY�Ds�3Dy� D�  D�VfD���D���D�L�D�VfD�� D�I�D�� D�y�D�fD�C3D�� D�C31111111111111111111111111111111111111111111111111111111111111111111111111   @���A  A���A陚B33BC33Bn��B�ffB�  B�  B�  B�33B�  C� C  CffCL�C)ffC333C=�CG�C[�3Co33C�s3C�� C��fC���C���C�ٚC��fCǙ�C�L�Cۙ�C� C�s3C��fD�3DٚD�3DٚD� D�fD�3D$� D)ٚD.��D4fD8ٚD=� DB� DG�fDM�fDT� DZs3D`��Dg,�DmY�Ds�3Dy� D�  D�VfD���D���D�L�D�VfD�� D�I�D�� D�y�D�fD�C3D�� D�C32222222222222222222222222222222222222222222222222222222222222222222222222   A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�v�A��TAް!Aީ�A�Aޡ�AޅAדuA��TAΟ�A�%A���A�`BA�+A�ZA�  A���A�=qA�l�A�\)A�  A�A�Ax�`AY�#A;XA-�A&  AC�A�RA
=A��A��@���@�7L@���@�^5@�|�@���@܃@���@���@с@·+@�S�@�@�\)@��9@��@�bN@� �@���@�(�@� �@�x�@�@���@���@���@���@��
@�^5@�C�@yhs@m/@b�!@O�@>@1�@%p�@�@dZ@1@ƨ1111111111111111111111111111111111111111111111111111111111111111111111111   A�v�A��TAް!Aީ�A�Aޡ�AޅAדuA��TAΟ�A�%A���A�`BA�+A�ZA�  A���A�=qA�l�A�\)A�  A�A�Ax�`AY�#A;XA-�A&  AC�A�RA
=A��A��@���@�7L@���@�^5@�|�@���@܃@���@���@с@·+@�S�@�@�\)@��9@��@�bN@� �@���@�(�@� �@�x�@�@���@���@���@���@��
@�^5@�C�@yhs@m/@b�!@O�@>@1�@%p�@�@dZ@1@ƨ2222222222222222222222222222222222222222222222222222222222222222222222222   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB?}B?}BE�Bm�B{�B�PB�B�BƨBM�B�B�;B�B��B�B��BB�B&�B��BuB
<jB	~�B	+B�B	YB	7LB	P�B	VB	r�B	��B	��B	��B	�^B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B
  B
B
%B
1B
B
+B
DB
\B
hB
�B
�B
�B
 �B
%�B
-B
5?B
<jB
D�B
O�B
YB
bNB
m�B
w�B
~�B
�+B
�11111111111111111111111111111111111111111111111111111111111111111111111111   B>wB?}B?}BE�Bm�B{�B�bB�1B�BȴBO�B�B�NB�B  B�B��BŢB�1B)�B��B�B
@�B	�B		7B�B	[#B	8RB	Q�B	W
B	r�B	��B	��B	��B	�^B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B
  B
B
%B
1B
B
+B
DB
\B
hB
�B
�B
�B
 �B
%�B
-B
5?B
<jB
D�B
O�B
YB
bNB
m�B
w�B
~�B
�+B
�12222222222222222222222222222222222222222222222222222222222222222222222222   <#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281126202007062811262020070628112620200706282348152007062823481520070628234815201905080615432019050806154320190508061543  JA      rqcp1.3e                                                                20030924064815  QCP$RCRD            G�O�G�O�G�O�  11110110111111JA  RFMTcnvp2.0                                                                 20040209020110  IP                  G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628112620  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628112620  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070628234815  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001043740  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001044646                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312114453  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318055839  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318060214                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20131031000000  CV  JULD            G�O�G�O�F�P                 JM  AREQREJM1.0                                                                 20131031000000  CF  PRES_ADJUSTED_QC@���D�C3G�O�                JM  AREQREJM1.0                                                                 20131031000000  CF  TEMP_ADJUSTED_QC@���D�C3G�O�                JM  AREQREJM1.0                                                                 20131031000000  CF  PSAL_ADJUSTED_QC@���D�C3G�O�                JA  RFMTcnvd2.1                                                                 20131106023119  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20131106023224                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608225008                      G�O�G�O�G�O�                JA  ARDU                                                                        20150617170515                      G�O�G�O�G�O�                JM  RENCREJM3.0                                                                 20190508061542  ED                  G�O�G�O�G�O�                JA  ARDU                                                                        20190630171514                      G�O�G�O�G�O�                