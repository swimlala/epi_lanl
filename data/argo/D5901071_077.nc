CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:12Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               MA   AO  20111130140859  20190522121826  1727_5046_077                   2C  D   APEX                            2143                            040306                          846 @�y/�@1   @�y/�Q�@8G�z�H�dn��P1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DUy�DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @&ff@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC  C�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe��Cg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DD  DDy�DD��DEy�DE��DFy�DG  DG� DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT�3DUs3DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY�3DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Dds3Dd�3Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Ds� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�JA�VA�JA�
=A�JA�1A�%A���A���AȺ^A��A�n�Aď\A��yA�bNA�%A���A�l�A�ĜA�ZA�jA���A��A�bNA�\)A���A��A��
A��uA���A�VA�\)A�|�A�
=A���A�A��A��A��A�ZA���A�C�A���A�XA��TA�r�A���A�G�A�A���A��+A�9XA��`A�5?A�ffA��hA�(�A��DA�33A���A��A�/A�-A�+A��jA��PA�^5A��A��^A�l�A���A�|�A��A�|�A���A��uA�|�A�JA�XA���A��mA��-A���A���A��;A� �A���A���A�"�A���A�ĜA���A�S�A��HA�z�A�/A��A��A�1A��A���A�oA�/A��A�
=A��A�S�A�z�A��A��\A��
A���A�=qAx�A~�!A~-A}K�A}�A|�A{�hAy�^AyVAul�As\)Ar��AqC�Ao\)An�uAnI�Al~�Aj�Ai�wAhz�Af�\Ae�AdJAbA`(�A_�FA^��A[p�AX�\AW��AV��AV�AU�7AUoAS�^AR�`ARJAP��AO�
AO33AM�AK�mAK7LAJ��AJQ�AIx�AH�DAH  AF��ADv�AA�A@v�A@A?�mA?�TA?�hA>�HA>�uA=x�A<�A;A;XA:A�A9l�A9VA8�jA7��A7S�A6A4�HA3��A1�#A0�jA0ffA0A/�#A//A-��A,��A,M�A+��A*�\A)|�A(��A(ZA'p�A&1'A%;dA$�A#dZA"v�A!�mA!?}A AĜA��Av�AI�A{A��A��A�PAx�A�yAJA%A5?A1'A��A��A�\A=qA�FAK�A��A~�AM�A{A7LA�`A��Av�AffA|�A�uA��A
9XA	7LA	VA	%A�A��AVAG�AQ�A �@��@���@�(�@���@��@�1'@�~�@�bN@�R@�7L@�t�@�b@�@���@�K�@�  @۝�@�~�@١�@�9X@׍P@�"�@֧�@�O�@��@��/@ԃ@�S�@�/@�  @�5?@͡�@�`B@���@�z�@�I�@���@ɉ7@�O�@�G�@�G�@�O�@�O�@�z�@��
@�K�@���@�@�V@��^@�dZ@�5?@��#@���@�V@��w@�"�@��R@�n�@�E�@�@�bN@��R@�$�@��#@��-@��h@�p�@�@�n�@��R@�ȴ@�5?@���@�p�@�V@�9X@��w@�t�@��@��R@�5?@�hs@�Ĝ@��;@��@�n�@�-@�{@���@� �@��@�M�@���@�`B@��@���@���@�1'@���@�S�@�33@�
=@�J@�V@�1'@��`@�o@�;d@���@�E�@��@�I�@��`@��h@�t�@�v�@�Z@�l�@��@�@�Ĝ@�(�@�b@��@��
@�C�@��@���@��@�t�@�o@���@���@��@���@��-@�hs@�?}@�&�@��@�%@�%@�%@���@��@��`@��9@�I�@��@�dZ@�33@�"�@��@�7L@��`@�Q�@��
@���@�\)@���@�@��@��#@��u@�Q�@���@��@�I�@��@��P@��H@��@�G�@�z�@��@�\)@���@�{@���@��D@��@��;@���@�;d@��@�C�@���@��H@�|�@�+@�~�@���@���@��^@��-@�`B@��9@�j@�A�@� �@���@��@���@�C�@�V@���@��`@��@�l�@�K�@�ȴ@���@���@�n�@�-@�{@�J@��#@��@�X@�7L@��@��`@��9@��@��@��D@�z�@�r�@�z�@�z�@�Q�@�t�@��@�ȴ@�ȴ@��y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�JA�VA�JA�
=A�JA�1A�%A���A���AȺ^A��A�n�Aď\A��yA�bNA�%A���A�l�A�ĜA�ZA�jA���A��A�bNA�\)A���A��A��
A��uA���A�VA�\)A�|�A�
=A���A�A��A��A��A�ZA���A�C�A���A�XA��TA�r�A���A�G�A�A���A��+A�9XA��`A�5?A�ffA��hA�(�A��DA�33A���A��A�/A�-A�+A��jA��PA�^5A��A��^A�l�A���A�|�A��A�|�A���A��uA�|�A�JA�XA���A��mA��-A���A���A��;A� �A���A���A�"�A���A�ĜA���A�S�A��HA�z�A�/A��A��A�1A��A���A�oA�/A��A�
=A��A�S�A�z�A��A��\A��
A���A�=qAx�A~�!A~-A}K�A}�A|�A{�hAy�^AyVAul�As\)Ar��AqC�Ao\)An�uAnI�Al~�Aj�Ai�wAhz�Af�\Ae�AdJAbA`(�A_�FA^��A[p�AX�\AW��AV��AV�AU�7AUoAS�^AR�`ARJAP��AO�
AO33AM�AK�mAK7LAJ��AJQ�AIx�AH�DAH  AF��ADv�AA�A@v�A@A?�mA?�TA?�hA>�HA>�uA=x�A<�A;A;XA:A�A9l�A9VA8�jA7��A7S�A6A4�HA3��A1�#A0�jA0ffA0A/�#A//A-��A,��A,M�A+��A*�\A)|�A(��A(ZA'p�A&1'A%;dA$�A#dZA"v�A!�mA!?}A AĜA��Av�AI�A{A��A��A�PAx�A�yAJA%A5?A1'A��A��A�\A=qA�FAK�A��A~�AM�A{A7LA�`A��Av�AffA|�A�uA��A
9XA	7LA	VA	%A�A��AVAG�AQ�A �@��@���@�(�@���@��@�1'@�~�@�bN@�R@�7L@�t�@�b@�@���@�K�@�  @۝�@�~�@١�@�9X@׍P@�"�@֧�@�O�@��@��/@ԃ@�S�@�/@�  @�5?@͡�@�`B@���@�z�@�I�@���@ɉ7@�O�@�G�@�G�@�O�@�O�@�z�@��
@�K�@���@�@�V@��^@�dZ@�5?@��#@���@�V@��w@�"�@��R@�n�@�E�@�@�bN@��R@�$�@��#@��-@��h@�p�@�@�n�@��R@�ȴ@�5?@���@�p�@�V@�9X@��w@�t�@��@��R@�5?@�hs@�Ĝ@��;@��@�n�@�-@�{@���@� �@��@�M�@���@�`B@��@���@���@�1'@���@�S�@�33@�
=@�J@�V@�1'@��`@�o@�;d@���@�E�@��@�I�@��`@��h@�t�@�v�@�Z@�l�@��@�@�Ĝ@�(�@�b@��@��
@�C�@��@���@��@�t�@�o@���@���@��@���@��-@�hs@�?}@�&�@��@�%@�%@�%@���@��@��`@��9@�I�@��@�dZ@�33@�"�@��@�7L@��`@�Q�@��
@���@�\)@���@�@��@��#@��u@�Q�@���@��@�I�@��@��P@��H@��@�G�@�z�@��@�\)@���@�{@���@��D@��@��;@���@�;d@��@�C�@���@��H@�|�@�+@�~�@���@���@��^@��-@�`B@��9@�j@�A�@� �@���@��@���@�C�@�V@���@��`@��@�l�@�K�@�ȴ@���@���@�n�@�-@�{@�J@��#@��@�X@�7L@��@��`@��9@��@��@��D@�z�@�r�@�z�@�z�@�Q�@�t�@��@�ȴ@�ȴ@��y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB[#B[#B[#B[#B[#B[#BZBZBXBS�BH�B;dB.B&�B%�B(�B)�B8RB6FB1'B)�B�B\BPB+B��B��B��B��B�B�B�HB��B��B��B��B��B��B�B�5B�/B�)B�/B�#B�B�B��B��B��BȴBŢBŢBŢB�wB�LB��B��B��B�oB�7B�By�Bs�BiyBbNB_;BZBO�B:^B�B1B��B��B�B�fB�BB�5B�B��BȴBÖB��B�wB�^B�B��B��B�{B�7B� Br�BbNBP�BH�B8RB33B.B&�B�BB
�B
�mB
�B
��B
ǮB
�dB
�3B
��B
��B
��B
�oB
�7B
� B
z�B
v�B
s�B
o�B
m�B
k�B
bNB
XB
Q�B
@�B
7LB
33B
+B
 �B
�B
�B
oB
+B
B	��B	�B	�B	�TB	�#B	��B	��B	ƨB	�?B	��B	��B	��B	��B	��B	��B	�uB	�\B	�7B	�%B	~�B	z�B	q�B	jB	ffB	dZB	aHB	[#B	W
B	R�B	O�B	>wB	33B	/B	.B	.B	.B	-B	(�B	%�B	!�B	�B	�B	�B	uB	bB	VB	JB	1B	B��B��B�B�B�yB�sB�`B�ZB�BB�#B�B�B��B��B��BȴBŢBB�qB�^B�LB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�JB�1B�+B�B�B�B�B� B~�B}�B}�B{�Bz�By�Bx�Bw�Bv�Bs�Bp�Bk�BhsBffBffBe`BdZBbNB^5BXBVBS�BR�BO�BL�BI�BI�BG�BE�BE�BH�BI�BH�BI�BG�BE�BE�BJ�BJ�BI�BG�BD�BB�BB�BB�BD�BF�BF�BE�BB�B@�B>wB>wB>wB>wB=qB=qB=qB=qB@�BB�BC�BC�BD�BD�BE�BF�BF�BI�BN�BO�BN�BR�BW
BXBXBXB\)B]/B^5B_;B^5B^5BaHBffBiyBk�Bm�Bo�Bq�Bv�B�B�1B�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�FB�wB�^B�RB�XB�^B�dB�jB�jB�qB��BĜBƨB��B��B��B��B�/B�B��B��B��B�B�yB�B��B��B		7B	�B	%�B	!�B	�B	"�B	"�B	"�B	#�B	%�B	&�B	'�B	,B	49B	7LB	9XB	:^B	;dB	?}B	F�B	H�B	I�B	L�B	N�B	N�B	O�B	P�B	P�B	P�B	P�B	Q�B	Q�B	R�B	VB	VB	VB	VB	T�B	P�B	Q�B	VB	\)B	]/B	\)B	]/B	[#B	[#B	\)B	\)B	YB	[#B	_;B	iyB	jB	jB	m�B	m�B	l�B	l�B	p�B	p�B	o�B	n�B	n�B	o�B	q�B	v�B	w�B	z�B	{�B	}�B	�B	�B	�%B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�3B	�9B	�FB	�LB	�LB	�LB	�^B	�dB	�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B[#B[#B[#B[#B[#B[#BZBZBYBXBO�BD�B1'B)�B'�B)�B,B;dB8RB6FB.B#�B�BoBVBB��B��B��B�B�B�`B�
B��B�B�B�B��B�#B�BB�HB�BB�5B�5B�/B�B��B��B��B��BǮBǮBȴB��B�qB�3B��B��B��B�PB�B~�Bx�Bk�BcTB`BB]/BVBF�B#�BDBB��B�B�sB�HB�HB�/B��B��BĜB��B�}B�wB�-B��B��B��B�JB�By�BiyBS�BO�B9XB49B/B(�B$�B	7B
��B
�B
�)B
��B
��B
�wB
�LB
�B
��B
��B
��B
�PB
�B
|�B
x�B
u�B
p�B
n�B
o�B
gmB
[#B
\)B
F�B
9XB
8RB
1'B
#�B
�B
!�B
�B
DB
%B
B	��B	�B	�yB	�BB	��B	��B	��B	�dB	�B	��B	��B	��B	��B	��B	��B	�hB	�JB	�1B	�B	~�B	u�B	l�B	gmB	e`B	cTB	^5B	YB	W
B	VB	F�B	6FB	0!B	.B	.B	/B	/B	)�B	)�B	$�B	�B	�B	�B	�B	hB	\B	\B	
=B	1B	B��B��B�B�B�yB�fB�mB�`B�5B�B�B�
B��B��B��BȴBƨB��B�wB�XB�LB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�hB�=B�7B�%B�B�B�B�B�B~�B~�B~�B{�Bz�By�Bx�By�Bv�Bu�Bs�Bk�BgmBffBffBe`BdZBgmBaHB[#BXBT�BS�BP�BP�BK�BJ�BH�BH�BK�BL�BL�BK�BI�BJ�BH�BK�BK�BJ�BI�BE�BC�BC�BD�BE�BG�BG�BG�BE�BB�BA�B?}B?}B?}B>wB>wB@�B?}BA�BB�BC�BC�BD�BE�BF�BG�BJ�BI�BN�BP�BR�BT�BXBYBYBZB]/B^5B_;B_;B_;BaHBdZBgmBjBk�Bm�Bo�Bq�Bu�B�B�1B�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�3B�LB��B�jB�XB�^B�dB�jB�jB�jB�wB��BŢBƨB��B��B��B��B�)B�mB��B	  B��B�B�B�B��B��B	B	�B	'�B	$�B	 �B	$�B	#�B	"�B	#�B	%�B	'�B	'�B	+B	49B	8RB	:^B	:^B	<jB	@�B	F�B	H�B	J�B	L�B	N�B	N�B	O�B	P�B	P�B	P�B	P�B	Q�B	R�B	S�B	VB	VB	VB	VB	W
B	Q�B	R�B	W
B	]/B	]/B	]/B	^5B	\)B	[#B	]/B	^5B	YB	ZB	_;B	jB	k�B	jB	n�B	n�B	m�B	m�B	q�B	q�B	o�B	n�B	o�B	q�B	r�B	v�B	x�B	{�B	{�B	}�B	�B	�B	�B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�3B	�9B	�LB	�XB	�RB	�LB	�^B	�dB	�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447012012010314470120120103144701  AO  ARGQ                                                                        20111130140859  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140859  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144701  IP                  G�O�G�O�G�O�                