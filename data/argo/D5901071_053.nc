CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:05Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               5A   AO  20111130140301  20190522121826  1727_5046_053                   2C  D   APEX                            2143                            040306                          846 @�Z&��	1   @�Z'O���@7*~��"��c����l�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dy` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC  C  C�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCN  CO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  D� D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D(  D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5s3D5��D6y�D6��D7s3D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB�3DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dms3Dm��Dny�Dn��Doy�Do��Dpy�Dp�3Dqy�Dq��Dry�Dr��Dsy�DyY�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��#A���A��/A��TA��HA��HA��`A��`A��mA��mA��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A��TA��HA��HA��HA��;A��/A��/A��#A��A���A��DA�ĜA��A�%A��A���A�n�A���A��
A�l�A�oA���A��A�E�A���A���A��+A��;A��RA��7A�?}A�{A�E�A���A���A�bA��A�9XA���A���A��+A���A�\)A���A���A�I�A��wA�VA�;dA�;dA�+A�G�A�ĜA��A���A��+A�G�A��HA�C�A�jA��!A��A�&�A��A�5?A��PA��A�O�A��`A�-A�A�7LA��HA�?}A��A��^A�jA�ƨA��TA�  A���A�XA���A�  A~�A{�AwAu�As7LAp��AoAm;dAk�FAi|�AeC�Ab�DA`5?A`�DA`9XA^z�A[��AZ9XAZ��A]�FA^$�A\�A[;dAW�7AUXASVAQAO;dAM�AKl�AF��AEO�AE�AE%AEG�AD5?ACl�AC
=ABr�AB^5AC�AC"�AA��A?��A>z�A=�A<�jA:�!A9+A6ZA5�A4ȴA4M�A3�FA3S�A3/A3
=A2��A2r�A2A1��A133A0�HA0��A0�uA/�A.��A,ȴA,��A+|�A*�jA)�
A'��A&��A&=qA%�7A#��A"$�AO�A�A�RA��A��A�/A�+A��A��A|�A�HA^5A�hA�+A��A��AM�A�TAXA�`A�A~�AG�A	�A	K�A��A��AM�A �A�-AK�A��A�A�AK�A��Al�AK�A?}A��AdZA ~�@��@���@�K�@�=q@�7L@��P@�^5@�hs@��u@�@��`@�V@��#@��D@�@��y@��@�\)@���@�j@���@�/@�r�@��@�l�@���@�J@��m@�?}@�Ĝ@��@���@��@�1@�5?@�ƨ@��@��@��T@���@Ͼw@͑h@�t�@�%@��`@�1@Ǖ�@�K�@�
=@�ȴ@��@�"�@�33@�o@���@���@�~�@��@�;d@î@��@���@ɩ�@�ff@�C�@�dZ@�$�@�E�@��@��/@��@å�@Ý�@���@�1'@�M�@��@�1'@�S�@���@�v�@�{@���@���@�z�@��+@��@��F@�hs@�`B@��@��@�dZ@�M�@��@�@�x�@�Q�@��R@�$�@��-@�/@���@���@�?}@��^@��@�5?@���@��y@�V@���@��#@���@���@��@��7@�ƨ@��!@��@�S�@�E�@���@�X@��j@��@�r�@��@��@��m@��m@�  @�(�@�9X@��@��m@��P@���@���@���@���@���@���@�K�@���@�v�@�n�@�V@�J@��T@��^@��h@�`B@���@��h@�?}@��/@�Ĝ@���@���@�z�@�Q�@��m@�\)@��+@�M�@�J@��^@�p�@��@��@�r�@�9X@�Q�@�?}@��^@�X@�/@��@�/@�7L@�?}@�%@�b@�1@�  @��
@�+@��\@�v�@���@�J@��@�Q�@�  @��m@���@��F@�K�@�
=@���@�J@��@��@���@�p�@��@�bN@�I�@�b@���@�1@���@��
@�  @�  @���@�l�@�+@��@�"�@�33@�;d@�C�@�C�@�C�@��!@��#@��^@���@��7@�x�@�/@���@���@��@��D@��@�z�@�j@�I�@�1'@�1'@�1'@�1'@�1@�ƨ@���@�|�@�dZ@�x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��/A��#A���A��/A��TA��HA��HA��`A��`A��mA��mA��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A��TA��HA��HA��HA��;A��/A��/A��#A��A���A��DA�ĜA��A�%A��A���A�n�A���A��
A�l�A�oA���A��A�E�A���A���A��+A��;A��RA��7A�?}A�{A�E�A���A���A�bA��A�9XA���A���A��+A���A�\)A���A���A�I�A��wA�VA�;dA�;dA�+A�G�A�ĜA��A���A��+A�G�A��HA�C�A�jA��!A��A�&�A��A�5?A��PA��A�O�A��`A�-A�A�7LA��HA�?}A��A��^A�jA�ƨA��TA�  A���A�XA���A�  A~�A{�AwAu�As7LAp��AoAm;dAk�FAi|�AeC�Ab�DA`5?A`�DA`9XA^z�A[��AZ9XAZ��A]�FA^$�A\�A[;dAW�7AUXASVAQAO;dAM�AKl�AF��AEO�AE�AE%AEG�AD5?ACl�AC
=ABr�AB^5AC�AC"�AA��A?��A>z�A=�A<�jA:�!A9+A6ZA5�A4ȴA4M�A3�FA3S�A3/A3
=A2��A2r�A2A1��A133A0�HA0��A0�uA/�A.��A,ȴA,��A+|�A*�jA)�
A'��A&��A&=qA%�7A#��A"$�AO�A�A�RA��A��A�/A�+A��A��A|�A�HA^5A�hA�+A��A��AM�A�TAXA�`A�A~�AG�A	�A	K�A��A��AM�A �A�-AK�A��A�A�AK�A��Al�AK�A?}A��AdZA ~�@��@���@�K�@�=q@�7L@��P@�^5@�hs@��u@�@��`@�V@��#@��D@�@��y@��@�\)@���@�j@���@�/@�r�@��@�l�@���@�J@��m@�?}@�Ĝ@��@���@��@�1@�5?@�ƨ@��@��@��T@���@Ͼw@͑h@�t�@�%@��`@�1@Ǖ�@�K�@�
=@�ȴ@��@�"�@�33@�o@���@���@�~�@��@�;d@î@��@���@ɩ�@�ff@�C�@�dZ@�$�@�E�@��@��/@��@å�@Ý�@���@�1'@�M�@��@�1'@�S�@���@�v�@�{@���@���@�z�@��+@��@��F@�hs@�`B@��@��@�dZ@�M�@��@�@�x�@�Q�@��R@�$�@��-@�/@���@���@�?}@��^@��@�5?@���@��y@�V@���@��#@���@���@��@��7@�ƨ@��!@��@�S�@�E�@���@�X@��j@��@�r�@��@��@��m@��m@�  @�(�@�9X@��@��m@��P@���@���@���@���@���@���@�K�@���@�v�@�n�@�V@�J@��T@��^@��h@�`B@���@��h@�?}@��/@�Ĝ@���@���@�z�@�Q�@��m@�\)@��+@�M�@�J@��^@�p�@��@��@�r�@�9X@�Q�@�?}@��^@�X@�/@��@�/@�7L@�?}@�%@�b@�1@�  @��
@�+@��\@�v�@���@�J@��@�Q�@�  @��m@���@��F@�K�@�
=@���@�J@��@��@���@�p�@��@�bN@�I�@�b@���@�1@���@��
@�  @�  @���@�l�@�+@��@�"�@�33@�;d@�C�@�C�@�C�@��!@��#@��^@���@��7@�x�@�/@���@���@��@��D@��@�z�@�j@�I�@�1'@�1'@�1'@�1'@�1@�ƨ@���@�|�@�dZ@�x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB7LB7LB8RB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB8RB9XB9XB:^B:^B:^B;dB:^B:^B;dB<jB>wB`BB�RB��B�TB�fB�HB��B�jB�LB�B��B��B�hB�PB�=B�{B��B��B��B��B��B��B�\B�7B�B�B�B�B�B�B}�Bx�Bv�Br�Bn�BhsBbNBO�B?}B+B�B��B�sB��B��BŢB�dB�B��B�VB� Bo�B^5BR�BL�BdZB[#BR�BG�BF�B/B�B
��B
�yB
�ZB
�/B
��B
�'B
��B
�7B
}�B
m�B
?}B
�B	��B	�;B	��B	ŢB	ÖB	�RB	�B	��B	�hB	n�B	\)B	O�B	_;B	^5B	W
B	H�B	B�B	bNB	��B	��B	��B	�VB	jB	T�B	G�B	:^B	"�B	�B	DB�B�NB��B��B	PB	B	B	B	B	
=B	�B	!�B	�B	oB	\B	DB��B�B�sB�B��B��B��B��B��B��B��B��BȴBƨBŢBĜBB��B��B�jB�LB�9B�-B�'B�'B�!B�B��B��B��B��B��B��B�oB�bB�PB�7B�+B�B~�Bv�Bt�Bs�Bq�Bm�Bk�BiyBgmBffBffBffBdZBcTBaHB]/B\)B[#BZBYBXBW
BT�BT�BT�BR�BP�BM�BL�BL�BK�BJ�BG�BE�BD�BD�BC�BB�BA�B@�B>wB=qB;dB9XB8RB7LB7LB5?B33B1'B1'B.B.B-B+B.B.B.B/B0!BL�Bo�BjB� B� By�B�B�JB�DB�Bs�BYBI�BL�B[#BQ�BG�BC�BI�BJ�BI�BL�BM�BM�BO�BR�BVBXB\)BaHBbNBjBk�Bo�Bz�B�uB��B��B�B�9B�3B�B��B��B��B��B��B��B��B��B��By�BgmBiyBn�Bs�Bu�B~�B�1B�+B�B�B�B�B�B�B�B~�B�B�B�B�B�7B�PB�VB�\B�hB��B��B��B��B��B�B�?B�jB�wB��BBBBĜBɺB��BȴBȴBȴBȴBȴB��B��B��B��B�
B�HB�ZB�fB�sB�B�B�B��B��B��B	  B	B	B	B	B	B	%B		7B	JB	VB	oB	uB	{B	�B	�B	%�B	'�B	-B	/B	2-B	49B	5?B	7LB	9XB	9XB	9XB	8RB	9XB	;dB	<jB	=qB	?}B	@�B	B�B	F�B	I�B	T�B	XB	[#B	_;B	aHB	e`B	gmB	hsB	gmB	hsB	n�B	o�B	q�B	s�B	s�B	v�B	z�B	x�B	v�B	w�B	w�B	w�B	y�B	{�B	}�B	~�B	}�B	� B	�B	�B	�B	�B	�+B	�=B	�DB	�JB	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�9B	�FB	�LB	�LB	�XB	�^B	�jB	�}B	�}B	�}B	�}B	�}B	��B	��B	��B	B	B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B7LB7LB8RB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB8RB9XB9XB:^B:^B:^B;dB:^B:^B;dB=qBB�B_;B�RB��B�ZB�mB�fB�/BĜBɺB�3B��B��B�{B�{B��B��B��B��B��B��B��B��B�hB�JB�%B�B�B�B�%B�B� Bz�Bw�Bs�Bp�BjBgmBT�BD�B0!B"�BB�B��B��BǮB�}B�'B��B�oB�%Bw�BcTBW
BO�BiyB^5BW
BI�BK�B49B�B
��B
�B
�`B
�TB
ĜB
�?B
��B
�DB
�B
y�B
K�B
�B
B	�ZB	�B	��B	ɺB	�qB	�!B	�B	��B	v�B	bNB	N�B	`BB	cTB	_;B	L�B	A�B	ZB	��B	��B	��B	��B	q�B	\)B	L�B	B�B	(�B	�B	�B��B�TB��B��B	hB	1B	B	B	B	1B	�B	%�B	�B	�B	bB	VB	B��B�B�)B��B��B��B��B��B��B��B��BɺBǮBǮBŢBÖBBB��B�qB�?B�?B�3B�9B�FB�B�B�B�B��B��B��B��B�uB�hB�=B�1B�%B�Bz�Bu�Bt�Bs�Bp�Bm�Bk�BiyBgmBhsBgmBe`BdZBe`BbNB]/B\)B[#BZBYBYBW
BVBVBT�BS�BR�BM�BM�BK�BK�BL�BH�BF�BE�BD�BD�BC�BC�B@�B?}B=qB;dB;dB;dB8RB7LB8RB5?B33B2-B0!B0!B/B/B/B/B0!B.BG�Bq�BiyB�B�By�B� B�JB�oB�B{�B^5BI�BK�B`BBT�BJ�BG�BI�BL�BJ�BM�BN�BN�BO�BR�BVBXB]/BaHBcTBm�Bk�Bn�Bt�B�\B��B��B�B�9B�FB�3B��B��B��B��B��B��B��B��B��B�BiyBjBo�Bs�Bu�B}�B�JB�DB�B�7B�7B�B�+B�B�B�B�B�B�B�B�JB�PB�\B�bB�oB��B��B��B��B��B�B�?B�qB�}B��BBBÖBĜB��B��B��B��B��BɺBɺB��B��B��B��B�
B�HB�ZB�fB�sB�B�B�B��B��B��B	  B	B	B	B	B	B	%B		7B	JB	\B	oB	uB	{B	�B	�B	%�B	(�B	.B	/B	2-B	49B	5?B	8RB	:^B	:^B	;dB	9XB	:^B	<jB	=qB	>wB	@�B	A�B	C�B	F�B	G�B	S�B	YB	[#B	_;B	aHB	e`B	gmB	hsB	hsB	hsB	n�B	o�B	r�B	t�B	s�B	u�B	{�B	y�B	w�B	x�B	w�B	w�B	y�B	|�B	~�B	� B	~�B	� B	�B	�B	�B	�B	�1B	�=B	�JB	�JB	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�FB	�LB	�LB	�XB	�^B	�jB	�}B	�}B	�}B	�}B	�}B	��B	��B	��B	B	B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<#�
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
<D��<D��<#�
<49X<#�
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
<49X<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446522012010314465220120103144652  AO  ARGQ                                                                        20111130140301  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140301  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144652  IP                  G�O�G�O�G�O�                