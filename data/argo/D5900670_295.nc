CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2013-03-14T18:00:08Z creation; 2013-03-14T18:00:08Z updated; 2015-08-09T15:59:41Z converted from 2.2   
references        (http://www.argodatamgt.org/Documentation   comment              user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7D   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8    	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8(   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8H   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8h   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   axis      T      units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8l   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8t   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8x   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   axis      Y      units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      axis      X      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   axis      Z      	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        ;   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        <|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ?   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  @$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        @l   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        B�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        D\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  E|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    E�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    H�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    K�   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  N�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    O(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    O8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         OL   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         OP   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        OT   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OXArgo profile    3.1 1.2 19500101000000  5900670 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL              'A   AO  20130314180008  20151031163114  0969_34292_295                  2C  D   APEX                            1597                            040704                          846 @֊�VG�w1   @֊���@1��-�dtI�^5?1   ARGOS   Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                A   B   B   @�ff@�  A4��Ax  A�  A�33A���A���B��B  B1��BE33BY��Bn  B���B�ffB���B�  B�33B�ffB�ffB�  B�  B�  B���B���B���CL�C33C��CL�C#ffC+L�C433C>ffCHffCR� C]� ChffCtL�C�&fC��C�&fC�L�C���C��3C�� C��C�s3CɦfCә�CަfC��C�L�D` DfD�DY�DfD&S3D.�3D7��DA3DK�DU��D`�3Dl�Dx�D�c3D�fD�3D�)�111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�  A,��Ap  A�  A�33A���A���B
��B  B/��BC33BW��Bl  B33B�ffB���B�  B�33B�ffB�ffB�  B�  B�  B���B���B���C��C�3C�C��C"�fC*��C3�3C=�fCG�fCR  C]  Cg�fCs��C��C���C��fC��C�Y�C��3C�� C�ٚC�33C�ffC�Y�C�ffC�ٚC��D@ D�fD��D9�D�fD&33D.�3D7��D@�3DJ��DUl�D`s3Dk��Dw��D�S3D�fD�3D��111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜAƸRA���A���A���A��
A��#A���Aƕ�A�|�A�S�A�p�AƓuAƍPA�z�A�\)A��A��A�+A�I�A�I�A�I�A�9XA�&�A�oA��A�M�A��#A���A�{A��A�p�A���A�{A��A�XA�/A���Ay33Alz�AZ��AJbNAD��A:�A2�uA,��A'�PA!G�A��Av�A~�Av�@�&�@�\)@�C�@ج@���@��@�|�@°!@��w@���@��@���@���@�ȴ@���@�x�@�33@���@��w@{C�111111111111111111111111111111111111111111111111111111411111111111111111A�ĜAƸRA���A���A���A��
A��#A���Aƕ�A�|�A�S�A�p�AƓuAƍPA�z�A�\)A��A��A�+A�I�A�I�A�I�A�9XA�&�A�oA��A�M�A��#A���A�{A��A�p�A���A�{A��A�XA�/A���Ay33Alz�AZ��AJbNAD��A:�A2�uA,��A'�PA!G�A��Av�A~�Av�@�&�@�\)G�O�@ج@���@��@�|�@°!@��w@���@��@���@���@�ȴ@���@�x�@�33@���@��w@{C�111111111111111111111111111111111111111111111111111111411111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�?B�9B�9B�9B�?B�3B�}B��B�B�B'�B9XB;dB:^B6FB33B5?B?}BK�BO�BQ�BP�BO�BM�BL�BJ�B~�BbNBPB��BÖB{�BJ�B�B\)B
�B
�uB
A�B	�?�;dB	;dB	�B�B�5B��BŢB�?B��    B�VBȴB��B	-B	1    B	8RB	ZB	x�B	��B	�!B	ŢB	�
B	�B	��B
B
{B
�B
�B
%�B
0!B
:^111111111111111111111111111111111111114444111114444114444111111111111111B��B��B��B��B��B��B��B�3B�BbBhB�B,B.B.B)�B%�B'�B2-B>wBB�BD�BC�BB�BA�B@�B@�Br�BYBB�B�^Bp�BA�B�yBS�B
�TB
v�G�O�G�O�G�O�G�O�B	\B�mB��BĜB�G�O�G�O�G�O�G�O�B�qB�wG�O�G�O�G�O�G�O�B	M�B	l�B	�7B	��B	�XB	��B	�;B	�B	��B
1B
bB
oB
�B
#�B
.111111111111111111111111111111111111114444111114444114444111111111111111<#�
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
<�oG�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<D��G�O�G�O�G�O�G�O�<#�
<#�
G�O�G�O�G�O�G�O�<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.5 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9997 (+/-0), vertically averaged dS = -0.013 (+/-0)                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201508271532552015082715325520150827153255  AO  ARGQ                                                                        20130314180008  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20130314180008  QCF$                G�O�G�O�G�O�4A40            AO  ARCAADJP                                                                    20130314180008    IP                G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20150824161156  QC  PRES            @�ffD�)�G�O�                PM  ARSQCTM V1.1                                                                20150824161156  QC  PSAL            @�ffD�)�G�O�                PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20150824165047  CF  TEMP            Cg�fCs��@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20150824165047  CF  TEMP            C�ٚC�33@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20150824165047  CF  TEMP            C��C��@�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20150827152201  IP                  G�O�G�O�G�O�                