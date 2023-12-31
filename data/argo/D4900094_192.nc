CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   <   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       +2015-Mar-13 converted from 2.2 to 3.1 at UW    
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  6�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7<   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7l   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7p   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7t   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7x   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8    JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8$   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8,   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            80   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           88   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8@   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8H   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8L   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8T   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9T   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9X   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9\   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9`   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z         �  9d   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  :T   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  :�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  ;�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  ;�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  <�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  =�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  =�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  >�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  ?   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  ?�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  @�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  A    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  B   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  BL   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  C<   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Cl   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Fl   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Il   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  Ll   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    L�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    L�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    L�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    L�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  L�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    L�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    L�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    L�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         M   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         M   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        M   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    MArgo profile    3.1 1.2 19500101000000  4900094 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  20081016090915  20150323113419  0274_18441_192                  2C  D   APEX                            633                             050302                          846 @��d\(��1   @��v�?��@7lI�  �e	���1   ARGOS   Primary sampling: discrete []                                                                                                                                                                                                                                      C   C   C   @�  AffA�33A�33B��BE33E��D\�3EQ��B���G�O�G�O�G�O�G�O�G�O�CL�C33C)L�C3  C<� CG�CP�fC[�Cd�fCn�fD�FfE��E&� C�� C�s3E<�fDڼ�C�s3C�L�C�Y�EU��D?��DffCǳ3C���D��fE7��Et�D.�fD.�3E|i�D��DTL�D`�3DmY�E.�fE�p C�� E�8�D��3D�,�D�ffD���D��D�33333333444499999333333333344444444334444344444444344444443333@�  @���A�33A�33B��B933G�O�G�O�G�O�B���G�O�G�O�G�O�G�O�G�O�CL�C33C&L�C0  C9� CD�CM�fCX�Ca�fCk�fG�O�G�O�G�O�C�  C��3G�O�G�O�C��3C���C�ٚG�O�G�O�G�O�C�33C��G�O�G�O�G�O�G�O�G�O�G�O�G�O�DS��D`3Dl��G�O�G�O�G�O�G�O�D��3D���D�fD�<�D���D��3111111444199999111111111144411441114441144444441114444111111@��@��@��@��@��@��G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�@��@��G�O�G�O�@��@��@��G�O�G�O�G�O�@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��G�O�G�O�G�O�G�O�@��@��@��@��@��@��A�A�A���A���A��-A�hs��5?BP�A�T@�^G�O�G�O�G�O�G�O�G�O�A���A�JA��A��A� �A�/A��A�A�1A� �Bn.A�^5ArbAj�/Ac�^BFȴB[#AV��AS�mAKXB(�}By(�A7A1VAQ�B	7LB(��A؍P@�ȴ@�oA��@���@�33@���@�v�BP��A\)B�?A�7@`b@V5?@M��@I7L@EV@>5?333333444499999333333344444444444444444444444444444444333333A�A�A���A���A��-A�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�JA��A��A� �A�/A��A�A�1A� �G�O�G�O�G�O�Aj�/Ac�^G�O�G�O�AV��AS�mAKXG�O�G�O�G�O�A1VAQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�33@���@�v�G�O�G�O�G�O�G�O�@`b@V5?@M��@I7L@EV@>5?111111444499999111111111144411441114441144444441114444111111;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�;o;oG�O�G�O�;o;o;oG�O�G�O�G�O�;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;oG�O�G�O�G�O�G�O�;o;o;o;o;o;oBq�Bq�Bq�Bq�Bq�Bx�BW
B��B/�dBL�G�O�G�O�G�O�G�O�G�O�BP�B7LB��B��B��Bs�BN�B8RB�B
�BB�A���BuB
JB	�A�G�@���B	��B	�1B	^5A�n�Bp�BB�TB�-A�FB#S�B9��B��B�9B��B��B	M�B	�JB	�'B-}�B�HBT��B&�RB
hB
�B
(�B
8RB
B�B
G�333333444499999433333334444444444444444444444444444444433333Br�Br�Br�Br�Br�By�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BQ�B8RB��B��B��Bt�BO�B9XB�B
�G�O�G�O�G�O�B
PB	�G�O�G�O�B	��B	�7B	_;G�O�G�O�G�O�B�ZB�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	O�B	�VB	�3G�O�G�O�G�O�G�O�B
uB
�B
+B
:^B
D�B
I�111111444499999111111111144411441114441144444441114444111111<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�<#�
<#�
G�O�G�O�<#�
<#�
<#�
G�O�G�O�G�O�<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =3 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        200911021823512009110218235120091102182352  AO  ARGQ                                                                        20081016090915  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20081016090915  QCF$                G�O�G�O�G�O�8CA40           UW  ARSQUWQCV1.0WOD & nearby Argo as visual check                               20091102182352  IP                  G�O�G�O�G�O�                