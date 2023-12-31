CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:02Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               )A   AO  20111130140005  20190522121825  1727_5046_041                   2C  D   APEX                            2143                            040306                          846 @�J�Ƞ��1   @�J�m�?�@7O��-V�c�V�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dx�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�  A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BX  B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C   C  C�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/��C1�fC3�fC5�fC7�fC9�fC<  C=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFs3DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_s3D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dx��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̝�A̧�Ạ�A̮A̴9A̶FA̶FA̸RA̸RA̶FA̶FA̶FA̶FA̶FA̸RA̸RA̴9A̰!A̩�A̧�A̧�A̟�A̝�A̓uA̍PA�|�A�ZA��A�~�AʶFA�C�A�%A�v�A�VA�ƨA��hA��TA��`A�ȴA���A��A�G�A��A�$�A�A�A��+A�ZA�ffA���A��PA�5?A�A�A���A�=qA�Q�A�A��A�v�A��A���A���A���A���A�
=A���A�%A�G�A���A�&�A�l�A���A��A�A���A�l�A��/A�l�A���A��A�z�A�dZA��A��jA�JA���A�G�A�bA�"�A�~�A���A���A�VA�=qA�XA�oA�A�A��A�x�A��wA��DA�K�A�{AS�A|�`A{�Ax��Av �Au��Au��Aup�As�Aq�An��AmdZAm&�Al�Aj{Ag�PAd��Ab��Aa�TAa��Aa�7A`�+A_33A]��A^bA]�TA]p�A\��A[�AZ�AZ^5AY�AY��AX5?AU��AT��AT{AS�#ARVAQp�AP�`APJAO7LAN��AMO�AKAKAJ�RAJ5?AI33AH�DAFĜAE�hAEC�AEADM�AC�^AC%ABJA@��A?�-A>��A<�A:�!A9x�A8��A8�uA7��A5�A5?}A4��A4v�A3�A1�
A0�RA/�A/��A/dZA.�RA-`BA,�RA,-A, �A+��A*ȴA)�A(�!A't�A&z�A%��A%?}A$�jA$�uA#x�A#A"A�A!K�A �yA �A`BA(�A\)A��A�A�jA/A�A�A=qA��A�/AAG�A�jA$�Ap�A�9A�#A�;A�+A
�A	?}Av�A��A"�A��A�A�mA�A��A�A �H@�5?@�r�@���@��@��D@��-@�ȴ@�j@��@�E�@��-@�9X@�@���@�bN@�dZ@���@䛦@�A�@��@�V@ᙚ@�I�@�@݁@�Ĝ@�b@���@ٺ^@���@�
=@�{@�hs@�Ĝ@��m@���@�@�z�@Ͼw@��@̬@�Q�@˝�@�~�@��#@��`@���@�33@�v�@���@�r�@Å@�S�@�^5@��/@�"�@���@�O�@�%@���@� �@�K�@�ff@���@���@�1'@���@�-@���@�hs@��@�9X@��m@�ƨ@��P@��@�n�@��@�1'@�+@�+@��@�
=@���@��+@�^5@��`@��F@��H@�5?@�@��D@���@�o@��@��@�Q�@��
@�C�@��@�ȴ@�J@�&�@���@���@��@�|�@�ȴ@�$�@��@��`@�b@���@��;@�1'@��@�dZ@�
=@��@�K�@�o@���@���@�X@�G�@�7L@��`@�r�@�1@�ƨ@��@�;d@�ȴ@�v�@�ff@�M�@��@�@�^5@�M�@���@��@�V@��`@��/@���@��j@���@�1@�C�@�x�@�/@���@�A�@�C�@��R@��!@��H@��-@���@�9X@���@���@�
=@�5?@�$�@��H@���@���@��^@���@�@��@��@��@�O�@�I�@���@��@�bN@�9X@�b@���@�ƨ@��w@���@��P@�l�@�C�@��@���@��+@�5?@��@��T@���@���@�O�@��@�&�@��@�Ĝ@���@�Ĝ@��9@��u@�r�@�A�@�9X@��m@��@���@���@���@��@��@��@�S�@�ȴ@��!@�5?@���@��@���@���@��7@��@�p�@��@��@���@���@�j@�1'@�b@��;@���@�t�@�33@��@���@�^5@�$�@��@���@��^@���@��@�x�@�p�@�G�@�&�@}��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̝�A̧�Ạ�A̮A̴9A̶FA̶FA̸RA̸RA̶FA̶FA̶FA̶FA̶FA̸RA̸RA̴9A̰!A̩�A̧�A̧�A̟�A̝�A̓uA̍PA�|�A�ZA��A�~�AʶFA�C�A�%A�v�A�VA�ƨA��hA��TA��`A�ȴA���A��A�G�A��A�$�A�A�A��+A�ZA�ffA���A��PA�5?A�A�A���A�=qA�Q�A�A��A�v�A��A���A���A���A���A�
=A���A�%A�G�A���A�&�A�l�A���A��A�A���A�l�A��/A�l�A���A��A�z�A�dZA��A��jA�JA���A�G�A�bA�"�A�~�A���A���A�VA�=qA�XA�oA�A�A��A�x�A��wA��DA�K�A�{AS�A|�`A{�Ax��Av �Au��Au��Aup�As�Aq�An��AmdZAm&�Al�Aj{Ag�PAd��Ab��Aa�TAa��Aa�7A`�+A_33A]��A^bA]�TA]p�A\��A[�AZ�AZ^5AY�AY��AX5?AU��AT��AT{AS�#ARVAQp�AP�`APJAO7LAN��AMO�AKAKAJ�RAJ5?AI33AH�DAFĜAE�hAEC�AEADM�AC�^AC%ABJA@��A?�-A>��A<�A:�!A9x�A8��A8�uA7��A5�A5?}A4��A4v�A3�A1�
A0�RA/�A/��A/dZA.�RA-`BA,�RA,-A, �A+��A*ȴA)�A(�!A't�A&z�A%��A%?}A$�jA$�uA#x�A#A"A�A!K�A �yA �A`BA(�A\)A��A�A�jA/A�A�A=qA��A�/AAG�A�jA$�Ap�A�9A�#A�;A�+A
�A	?}Av�A��A"�A��A�A�mA�A��A�A �H@�5?@�r�@���@��@��D@��-@�ȴ@�j@��@�E�@��-@�9X@�@���@�bN@�dZ@���@䛦@�A�@��@�V@ᙚ@�I�@�@݁@�Ĝ@�b@���@ٺ^@���@�
=@�{@�hs@�Ĝ@��m@���@�@�z�@Ͼw@��@̬@�Q�@˝�@�~�@��#@��`@���@�33@�v�@���@�r�@Å@�S�@�^5@��/@�"�@���@�O�@�%@���@� �@�K�@�ff@���@���@�1'@���@�-@���@�hs@��@�9X@��m@�ƨ@��P@��@�n�@��@�1'@�+@�+@��@�
=@���@��+@�^5@��`@��F@��H@�5?@�@��D@���@�o@��@��@�Q�@��
@�C�@��@�ȴ@�J@�&�@���@���@��@�|�@�ȴ@�$�@��@��`@�b@���@��;@�1'@��@�dZ@�
=@��@�K�@�o@���@���@�X@�G�@�7L@��`@�r�@�1@�ƨ@��@�;d@�ȴ@�v�@�ff@�M�@��@�@�^5@�M�@���@��@�V@��`@��/@���@��j@���@�1@�C�@�x�@�/@���@�A�@�C�@��R@��!@��H@��-@���@�9X@���@���@�
=@�5?@�$�@��H@���@���@��^@���@�@��@��@��@�O�@�I�@���@��@�bN@�9X@�b@���@�ƨ@��w@���@��P@�l�@�C�@��@���@��+@�5?@��@��T@���@���@�O�@��@�&�@��@�Ĝ@���@�Ĝ@��9@��u@�r�@�A�@�9X@��m@��@���@���@���@��@��@��@�S�@�ȴ@��!@�5?@���@��@���@���@��7@��@�p�@��@��@���@���@�j@�1'@�b@��;@���@�t�@�33@��@���@�^5@�$�@��@���@��^@���@��@�x�@�p�@�G�@�&�@}��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBĜBŢBĜBǮBȴBȴBȴBǮBȴBȴBȴBȴBȴBȴBȴBɺBɺBɺBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B�5B�;B�ZB�TB�ZB�NB�NB�HB�BB�5B�B�B��B��BǮBƨBŢBÖB�wB�RB�B��B��B��B��B�uB�oB��B��B��B��B��B��B�uB�DB�Bx�Bm�BaHBZBS�BF�BA�B1'B�BoBB�ZB�BB�B��B��B�Bz�Bv�Be`BG�B6FB�B
��B
�B
�HB
�/B
�B
��B
��B
�?B
�oB
� B
p�B
hsB
W
B
Q�B
<jB
(�B
&�B
-B
-B
#�B
hB
B	��B	��B	��B	�fB	��B	B	�LB	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�{B	�bB	�JB	�7B	{�B	m�B	gmB	cTB	cTB	\)B	R�B	M�B	J�B	I�B	D�B	7LB	.B	+B	1'B	49B	1'B	,B	"�B	,B	.B	-B	(�B	%�B	#�B	!�B	�B	�B	DB��B�B�mB�`B�TB�)B��B��B��BǮB��B�dB�LB�XB�^B�XB�FB�FB�3B�-B�FB�RB�9B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�DB�+B�B�B~�B|�Bz�Bx�Bw�Bu�Bs�Bq�Bo�Bl�BhsBcTB_;BZBXBVBS�BQ�BO�BM�BK�BI�BG�BD�BB�B@�B?}B=qB:^B8RB6FB33B1'B1'B1'B0!B/B/B-B-B,B+B+B)�B(�B(�B'�B'�B'�B'�B&�B$�B$�B$�B#�B#�B$�B$�B$�B$�B$�B$�B&�B&�B'�B)�B)�B+B-B-B.B.B-B/B0!B2-B49B49B6FB7LB6FB6FB6FB7LB8RB8RB:^B;dB?}B>wB?}BA�BF�BG�BH�BI�BJ�BK�BK�BJ�BK�BN�BP�BVBZB[#B[#B[#B]/B]/B\)B`BBdZBffBgmBhsBiyBjBl�Bo�Br�Bu�Bx�B{�B{�B|�B~�B�+B�DB�JB�PB�hB�oB�oB�oB��B��B��B��B�B�!B�-B�?B�^BĜBƨBȴB��B��B�
B�
B�)B�BB�NB�ZB�`B�sB�B�B�B�B�B��B	  B	B	B	B	+B	DB	PB	VB	\B	oB	�B	�B	�B	�B	�B	!�B	$�B	+B	/B	5?B	G�B	J�B	H�B	J�B	O�B	P�B	S�B	[#B	e`B	e`B	e`B	e`B	gmB	m�B	q�B	s�B	t�B	s�B	u�B	{�B	}�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�DB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�B	�!B	�-B	�-B	�3B	�3B	�9B	�9B	�9B	�9B	�3B	�3B	�3B	�?B	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�XB	�^B	�jB	�wB	�wB	�wB	�}B	��B	B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BĜBŢBĜBǮBȴBȴBȴBǮBȴBȴBȴBȴBȴBȴBȴBɺBɺBɺBɺBɺBɺB��B��B��B��B��B��B��B��B�B�B�TB�B�B�B�mB�`B�`B�ZB�ZB�TB�/B�B��B��B��B��BɺBȴBɺBB�FB�!B��B��B��B��B��B��B��B��B��B��B��B��B�VB�B~�Bp�BcTB]/BYBH�BF�B8RB�B�B	7B�sB�)BɺB�'B�B��B�B|�B|�Bo�BN�B@�B$�BB
�B
�NB
�HB
�
B
��B
��B
�wB
��B
�B
v�B
n�B
[#B
W
B
C�B
)�B
&�B
.B
1'B
(�B
�B
B	��B	��B	��B	�B	�/B	ȴB	�^B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�oB	�VB	�VB	�B	p�B	iyB	dZB	hsB	_;B	T�B	P�B	M�B	K�B	I�B	<jB	1'B	,B	33B	8RB	49B	2-B	&�B	-B	/B	0!B	+B	(�B	'�B	&�B	!�B	�B	hB	%B�B�yB�fB�`B�HB��B��B��B��BĜB�wB�XB�^B�dB�dB�^B�RB�?B�-B�RB�dB�RB�-B�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�JB�1B�%B�B~�B}�B{�Bz�Bw�Bu�Bt�Br�Bp�Bo�BhsBe`B`BB[#BYBVBVBQ�BP�BM�BK�BJ�BF�BG�BC�B@�B?}B>wB<jB:^B6FB49B2-B2-B2-B1'B1'B/B/B/B-B,B+B+B+B)�B+B+B)�B(�B&�B&�B&�B&�B%�B%�B%�B&�B&�B&�B&�B'�B(�B)�B+B+B-B.B.B/B/B.B0!B2-B33B5?B6FB8RB:^B8RB7LB7LB8RB9XB:^B<jB=qB@�B?}B@�BC�BG�BH�BI�BJ�BK�BK�BL�BK�BL�BP�BR�BXBZB[#B[#B\)B]/B^5B_;BbNBffBgmBhsBjBjBl�Bn�Bq�Bs�Bv�By�B|�B|�B~�B�B�1B�JB�PB�VB�oB�uB�oB�uB��B��B��B��B�B�'B�3B�?B�^BŢBǮB��B��B��B�
B�B�/B�HB�TB�`B�fB�yB�B�B�B�B�B��B	  B	B	B	B	+B	DB	PB	VB	\B	uB	�B	�B	�B	�B	 �B	#�B	%�B	+B	.B	1'B	G�B	L�B	I�B	J�B	P�B	R�B	S�B	ZB	ffB	gmB	e`B	e`B	gmB	m�B	q�B	s�B	u�B	u�B	u�B	{�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�JB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�?B	�3B	�9B	�3B	�?B	�RB	�XB	�XB	�XB	�XB	�dB	�^B	�XB	�^B	�qB	�}B	�wB	�wB	��B	��B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446482012010314464820120103144648  AO  ARGQ                                                                        20111130140005  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140005  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144648  IP                  G�O�G�O�G�O�                