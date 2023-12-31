CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:55Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135557  20190522121825  1727_5046_017                   2C  D   APEX                            2143                            040306                          846 @�->ʶ�1   @�-@�߻@7�"��`B�c�l�C��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D.��D/� D0  D0y�D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?33BG��BO��BW33B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC��C�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%��C'��C)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+� D+��D,y�D,��D-y�D-��D.y�D.�3D/y�D/��D0s3D0��D1s3D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DA� DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di�3Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��Aϥ�A�=qA�A��yA���A�ƨAΩ�AΉ7A�1A��A�XA���A̙�A���Aˣ�A�M�A���A�A�  A�  A��HA�|�AÉ7A�JA��A���A�ƨA���A��A��RA�&�A��A��A�A�A��PA�^5A�$�A�$�A�?}A�+A�$�A�%A�~�A��A��A�|�A�JA�t�A�A�ĜA�{A�ĜA�t�A�A�A�jA�XA�VA���A�XA�-A�A�C�A��A��A���A�=qA�Q�A���A��^A���A���A���A�l�A�v�A��\A���A��A���A�?}A���A�9XA���A�5?A�bNA�+A��A��/A���A�+A�M�A��TA�hsA���A���A��7A��A�TA~�A~r�A}�#A}/A{l�AyƨAwl�AvVAu;dAsl�Aq�
Ao�FAo33An�uAnjAnAlz�Ak�AjI�Ai��Ah��Af��Ae�FAe`BAeVAdffAb��AbA_��A]�A[?}AZ�AZ�uAZ=qAY�AX�HAW�AU�FAU�AR��AO�AN�\AM��AM
=AL$�AK%AJ~�AIx�AH�/AG�wAF�/AE%ADE�AC�7AB��AA�A>�uA=�A<��A;�TA:Q�A9VA8�+A7��A6A4r�A3�
A3S�A2ȴA2z�A2ffA1��A0�A/�A.��A.�A.9XA,ĜA+��A+33A*�uA)�PA(n�A'�7A&v�A%��A$��A#��A#33A"ĜA"A�A!|�A�
A��AbA\)A�FA�HAZA��A�-A��A��A�\A�A��Ax�AI�A��A�AA�AƨAdZA/A�A&�A
�uA
  A	��A	?}A��A`BA�A�mA��AVA5?AK�A�
AS�A �A Z@�~�@�/@��P@�
=@�G�@��w@���@�=q@��h@���@�P@��#@�E�@�t�@�z�@�%@���@�&�@��@�9@�ƨ@�\@���@�j@�x�@�33@��@�w@�C�@�@�S�@�R@��`@�{@�dZ@�$�@�@ٲ-@�x�@�33@�n�@�@�O�@У�@��@�C�@���@�V@��#@�X@��`@�O�@�@���@���@�9X@�J@�  @�V@��@�K�@�ƨ@��u@�C�@�I�@��@���@�ff@��P@���@���@���@�Ĝ@�A�@�9X@�S�@��j@�O�@���@�5?@�l�@�$�@��@�&�@���@��@��+@�dZ@���@�+@�|�@�@��T@��@��H@��h@��7@��@�ƨ@�S�@�@�V@�&�@���@���@�b@���@���@���@�`B@�Ĝ@�I�@��w@�@�E�@��@��T@��@�%@�A�@��D@��@�j@��D@���@��
@�z�@�O�@��@��@�bN@��@��!@���@�X@��/@��@�l�@�
=@���@�^5@�E�@���@�M�@���@�Z@�$�@���@��!@���@��@�^5@��@�/@���@��D@��u@�1'@�ƨ@��@�"�@���@��!@���@�n�@�M�@�$�@���@��@�(�@��;@���@��F@���@��@�dZ@�@�n�@�E�@�E�@�@��7@�G�@�V@��/@��9@��D@�A�@�b@�  @��m@��
@���@�dZ@�C�@�@�v�@�@���@��T@��@���@�J@�@���@�@�x�@��9@�bN@� �@�t�@���@�E�@��@���@��/@��@�j@�Z@�A�@�(�@� �@� �@��@�b@���@��@��F@�t�@��y@��@�
=@��@�o@�@��y@���@�ff@�X@���@�%@�?}@�/@�Z@��
@�;d@��y@�~�@�@��T@�$�@���@��R@�=q@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��Aϥ�A�=qA�A��yA���A�ƨAΩ�AΉ7A�1A��A�XA���A̙�A���Aˣ�A�M�A���A�A�  A�  A��HA�|�AÉ7A�JA��A���A�ƨA���A��A��RA�&�A��A��A�A�A��PA�^5A�$�A�$�A�?}A�+A�$�A�%A�~�A��A��A�|�A�JA�t�A�A�ĜA�{A�ĜA�t�A�A�A�jA�XA�VA���A�XA�-A�A�C�A��A��A���A�=qA�Q�A���A��^A���A���A���A�l�A�v�A��\A���A��A���A�?}A���A�9XA���A�5?A�bNA�+A��A��/A���A�+A�M�A��TA�hsA���A���A��7A��A�TA~�A~r�A}�#A}/A{l�AyƨAwl�AvVAu;dAsl�Aq�
Ao�FAo33An�uAnjAnAlz�Ak�AjI�Ai��Ah��Af��Ae�FAe`BAeVAdffAb��AbA_��A]�A[?}AZ�AZ�uAZ=qAY�AX�HAW�AU�FAU�AR��AO�AN�\AM��AM
=AL$�AK%AJ~�AIx�AH�/AG�wAF�/AE%ADE�AC�7AB��AA�A>�uA=�A<��A;�TA:Q�A9VA8�+A7��A6A4r�A3�
A3S�A2ȴA2z�A2ffA1��A0�A/�A.��A.�A.9XA,ĜA+��A+33A*�uA)�PA(n�A'�7A&v�A%��A$��A#��A#33A"ĜA"A�A!|�A�
A��AbA\)A�FA�HAZA��A�-A��A��A�\A�A��Ax�AI�A��A�AA�AƨAdZA/A�A&�A
�uA
  A	��A	?}A��A`BA�A�mA��AVA5?AK�A�
AS�A �A Z@�~�@�/@��P@�
=@�G�@��w@���@�=q@��h@���@�P@��#@�E�@�t�@�z�@�%@���@�&�@��@�9@�ƨ@�\@���@�j@�x�@�33@��@�w@�C�@�@�S�@�R@��`@�{@�dZ@�$�@�@ٲ-@�x�@�33@�n�@�@�O�@У�@��@�C�@���@�V@��#@�X@��`@�O�@�@���@���@�9X@�J@�  @�V@��@�K�@�ƨ@��u@�C�@�I�@��@���@�ff@��P@���@���@���@�Ĝ@�A�@�9X@�S�@��j@�O�@���@�5?@�l�@�$�@��@�&�@���@��@��+@�dZ@���@�+@�|�@�@��T@��@��H@��h@��7@��@�ƨ@�S�@�@�V@�&�@���@���@�b@���@���@���@�`B@�Ĝ@�I�@��w@�@�E�@��@��T@��@�%@�A�@��D@��@�j@��D@���@��
@�z�@�O�@��@��@�bN@��@��!@���@�X@��/@��@�l�@�
=@���@�^5@�E�@���@�M�@���@�Z@�$�@���@��!@���@��@�^5@��@�/@���@��D@��u@�1'@�ƨ@��@�"�@���@��!@���@�n�@�M�@�$�@���@��@�(�@��;@���@��F@���@��@�dZ@�@�n�@�E�@�E�@�@��7@�G�@�V@��/@��9@��D@�A�@�b@�  @��m@��
@���@�dZ@�C�@�@�v�@�@���@��T@��@���@�J@�@���@�@�x�@��9@�bN@� �@�t�@���@�E�@��@���@��/@��@�j@�Z@�A�@�(�@� �@� �@��@�b@���@��@��F@�t�@��y@��@�
=@��@�o@�@��y@���@�ff@�X@���@�%@�?}@�/@�Z@��
@�;d@��y@�~�@�@��T@�$�@���@��R@�=q@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B��B��B��B��B��B�hB�VB�7B�7B�DB�bB��B��B�'BȴB��B��B��B�B�yB�B�yB�yB�B�B�B�mB�fB�#B��B��B��B�dB�B�B��B��B��B��B��B��B�oB�\B�JB�1B�B�By�Bq�Bl�Be`B_;BQ�B<jB-B(�B!�B�B\BDBB��B�B�B��B�-B��B�bBv�BffBW
BE�B9XB/B&�B�B�BPBB
��B
��B
��B
��B
�B
�mB
�NB
�#B
ȴB
��B
�B
x�B
o�B
p�B
o�B
l�B
gmB
[#B
K�B
;dB
33B
,B
�B
�B
PB
hB
�B
#�B
!�B
�B
VB
+B	��B	��B	�B	�sB	�B	�B	�B	�HB	�B	ƨB	�!B	��B	��B	��B	��B	��B	��B	��B	�PB	�%B	t�B	ffB	^5B	[#B	W
B	S�B	Q�B	O�B	K�B	H�B	E�B	>wB	7LB	33B	.B	+B	 �B	{B	bB	\B	B��B��B�B�B�;B��B��B��B��B��BɺBǮBĜBB��BȴBƨB�wB�XB�RB�-B�B�B��B��B��B��B��B��B��B��B��B�uB�\B�DB�7B�B� Bz�Bu�Bs�Bq�Bn�Bk�BiyBhsBffBe`BdZBbNBaHB`BB^5B]/BZB[#BZBZBYBXBVBT�BS�BR�BQ�BO�BN�BL�BN�BN�BM�BK�BJ�BI�BJ�BI�BI�BI�BI�BH�BG�BF�BF�BI�BS�BVBiyBo�Bs�B|�B�DB�hB�VB�DB�+B�B{�Bt�Bq�Bq�Bu�B{�Bz�Bx�Bt�Bq�Bk�BhsBhsBhsBgmBbNB`BBgmBgmBgmBgmBgmBhsBhsBhsBiyBl�Bt�Bz�B�B�B�B{�Bx�B�B�=B�DB�Bx�Bu�Bp�B}�B�7B�oB��B��B�B��B��B��B��B��B��B��B�\B�uB�bB�hB��B�B��B��B��B��B��B��B��B��B��B��B�'B�-B�9B�9B�-B�'B�!B�B�B�B�B�B�B�B�B�!B�3B�?B�RB�XB�^B�dB�jB�wBƨB��B��B��B��B�B�)B�5B�mB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B		7B	�B	6FB	A�B	E�B	E�B	E�B	G�B	H�B	J�B	J�B	M�B	P�B	R�B	T�B	VB	VB	W
B	XB	XB	XB	YB	[#B	[#B	ZB	ZB	`BB	e`B	ffB	gmB	hsB	iyB	iyB	jB	m�B	o�B	q�B	u�B	w�B	x�B	x�B	y�B	y�B	z�B	{�B	{�B	{�B	{�B	{�B	{�B	{�B	z�B	z�B	z�B	� B	�B	�B	�B	�%B	�+B	�1B	�JB	�PB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�9B	�9B	�9B	�FB	�dB	�wB	��B	ĜB	ĜB	ĜB	B	��B	��B	��B	ÖB	ƨB	ɺB	��B	��B	�)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B��B��B��B��B�uB�hB�DB�DB�JB�bB��B��B�9B��B	7BBB�B�B��B�B�B�B�B�B�B�B�ZB�B�B��B��B�3B�'B��B��B�B��B��B��B��B�hB�bB�=B�+B�B}�Bt�Bp�BffBdZBYBA�B.B,B$�B�BoB\B+BB��B�NBƨB�LB�B��B|�Bl�B^5BI�B=qB2-B+B �B�BoBB  B
��B
��B
��B
��B
�yB
�ZB
�5B
��B
��B
�B
|�B
q�B
q�B
q�B
n�B
l�B
_;B
Q�B
>wB
6FB
1'B
!�B
�B
\B
uB
�B
%�B
&�B
�B
hB
	7B
B	��B	�B	�yB	�B	�B	�B	�ZB	�BB	��B	�?B	��B	��B	��B	��B	��B	��B	��B	�\B	�PB	|�B	jB	`BB	]/B	YB	W
B	R�B	R�B	M�B	K�B	G�B	C�B	9XB	5?B	0!B	0!B	&�B	�B	oB	oB	
=B��B��B��B�B�ZB�
B��B��B��B��B��B��BɺBŢB��B��B��B��B�dB�^B�FB�'B�!B�B�B��B��B��B��B��B��B��B��B�oB�VB�VB�1B�B� Bx�Bv�Bs�Bq�Bn�Bk�Bk�BiyBgmBffBdZBcTBaHB_;B_;B^5B]/B\)B[#B[#BZB[#BXBVBS�BS�BR�BQ�BQ�BP�BP�BO�BO�BL�BL�BK�BL�BK�BK�BJ�BI�BH�BH�BI�BI�BXBT�BhsBo�Bs�B|�B�JB�uB�bB�PB�7B�%B~�Bw�Bs�Br�Bu�B|�B{�B{�Bx�Bu�Bm�BiyBhsBiyBk�BiyBaHBhsBhsBhsBhsBhsBiyBiyBiyBjBl�Bt�Bz�B�B�B�%B~�Bw�B�B�PB�hB�%B{�Bz�Bo�B|�B�1B�bB��B��B�!B�B��B��B�B�B��B��B�bB��B�oB�JB��B�B�B��B��B��B��B��B��B��B��B��B�3B�-B�?B�FB�3B�-B�'B�!B�B�B�!B�'B�B�!B�B�!B�9B�FB�XB�^B�^B�jB�jB�}BǮB��B��B��B��B�B�5B�/B�fB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	%B	bB	33B	@�B	E�B	E�B	E�B	H�B	J�B	K�B	J�B	M�B	P�B	S�B	VB	VB	W
B	XB	XB	XB	XB	YB	[#B	\)B	[#B	\)B	aHB	e`B	ffB	gmB	hsB	iyB	jB	k�B	m�B	o�B	r�B	v�B	x�B	y�B	x�B	y�B	y�B	{�B	{�B	{�B	{�B	{�B	{�B	|�B	{�B	{�B	{�B	{�B	� B	�B	�B	�B	�%B	�+B	�7B	�JB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�9B	�?B	�FB	�LB	�dB	�wB	��B	ƨB	ŢB	ŢB	ÖB	B	��B	��B	ÖB	ŢB	ɺB	��B	��B	�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446392012010314463920120103144639  AO  ARGQ                                                                        20111130135557  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135557  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144639  IP                  G�O�G�O�G�O�                