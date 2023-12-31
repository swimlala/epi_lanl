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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               LA   AO  20111130140845  20190522121826  1727_5046_076                   2C  D   APEX                            2143                            040306                          846 @�w側��1   @�w�)���@8��Q��d�7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B@  BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC��C�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC6  C8  C9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8�3D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>�3D?y�D?��D@y�D@��DA� DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY�3DZy�DZ��D[y�D[��D\s3D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AŁAŇ+AŋDAōPAŋDAŅA�|�A�r�A�E�A���Aħ�A�7LAá�A�E�A��A�ƨA��FA���A��\A�9XA���A���A��hA�A�A�v�A��A�`BA��FA��+A�p�A��;A�{A�  A��`A�?}A��
A��7A�=qA���A��HA�p�A��A��hA��A�t�A��A�ZA���A���A��!A���A�A�A��A��A�7LA��yA��A�;dA��^A�;dA���A�I�A���A��A�VA���A��A�G�A��A��wA�dZA��A���A�M�A��A�hsA���A�~�A��!A���A��mA���A�p�A��A��FA�v�A�{A��A�t�A���A��\A�
=A�{A�dZA�A���A�z�A��A�&�A���A�(�A���A��FA���A�VA��RA���A�bA�l�A}�-A|v�A{��Az��Az�uAy�#AwhsAu�hAs�FArJAp�ApffAoVAm\)Al{Ai�;AhVAf�Af�+AfZAe�
Aep�AdZAb��A^�/A]oA\v�A[�
A[S�A[&�AY�-AY&�AX��AX��AWK�AVE�AUG�AUATȴAT�\AS�AR�uAQ��AN��AM��AMO�AK�
AJ�RAJZAJ�AJ1AI��AI��AI��AH�!AF��AF1'AEAE\)AD�ACx�AAC�A?��A>jA=��A=/A<5?A;�A:n�A9x�A8��A8{A7�A7dZA5/A3��A2r�A0�DA/l�A.�RA.ZA-��A-��A-�PA-A+?}A)��A(�jA'O�A&��A%��A%�A%&�A$-A#C�A!��A VA�TA�wA�A�A�/A�TAE�A�A�-A�HA5?An�A��A33AAhsA�!A1A�-AdZA�A��A��A1'A��AbA�hA�Ax�A?}A
�HA
�A	�
A��AA��AA�A�AG�A�+A�AC�AVA ~�@��R@�;d@�M�@���@�?}@��@�1@�~�@�+@���@�@�E�@��/@� �@�
=@���@�S�@�ƨ@���@�@��@�=q@ݑh@ܛ�@���@�33@���@�5?@�Ĝ@֧�@Ցh@Չ7@��@�33@�~�@��@�?}@�j@��@ϕ�@ΰ!@�@˾w@ʧ�@�V@ɉ7@�/@�Ĝ@��
@���@Ǿw@���@�@�z�@��7@�Q�@��m@�dZ@���@��^@�O�@�G�@�9X@�K�@�`B@��@���@�&�@�%@��/@��9@��@���@�r�@�9X@�+@���@�~�@���@���@�v�@�^5@�-@�@��T@���@��/@��@�bN@�Q�@�A�@�  @���@��@�?}@��@�33@��R@�~�@��+@�~�@�=q@��#@�X@��@�I�@��w@�\)@��@���@�=q@��h@��@��@�(�@���@�t�@��@�v�@��#@��h@�p�@�O�@���@��F@��y@�ȴ@�~�@�M�@�{@��T@��h@��7@���@�&�@���@�1'@��m@��;@��w@���@��w@��@�33@��@�{@�X@�9X@�A�@��9@��w@�dZ@�"�@���@��\@�ff@�$�@��T@��@��@�o@���@�33@���@�K�@��@�
=@�\)@�K�@�n�@���@�x�@�?}@��@���@�b@��@���@�=q@�n�@��\@��\@�v�@��@�$�@�@��@��-@��-@�Q�@���@���@��7@�G�@�&�@�V@�j@�  @��P@��@���@�ȴ@���@���@���@���@���@���@�^5@�5?@�@���@��-@���@��#@��@��@��T@��^@���@�p�@�O�@�?}@��`@���@�z�@�r�@�j@�Q�@�(�@��@��@���@��@�K�@�"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AŁAŇ+AŋDAōPAŋDAŅA�|�A�r�A�E�A���Aħ�A�7LAá�A�E�A��A�ƨA��FA���A��\A�9XA���A���A��hA�A�A�v�A��A�`BA��FA��+A�p�A��;A�{A�  A��`A�?}A��
A��7A�=qA���A��HA�p�A��A��hA��A�t�A��A�ZA���A���A��!A���A�A�A��A��A�7LA��yA��A�;dA��^A�;dA���A�I�A���A��A�VA���A��A�G�A��A��wA�dZA��A���A�M�A��A�hsA���A�~�A��!A���A��mA���A�p�A��A��FA�v�A�{A��A�t�A���A��\A�
=A�{A�dZA�A���A�z�A��A�&�A���A�(�A���A��FA���A�VA��RA���A�bA�l�A}�-A|v�A{��Az��Az�uAy�#AwhsAu�hAs�FArJAp�ApffAoVAm\)Al{Ai�;AhVAf�Af�+AfZAe�
Aep�AdZAb��A^�/A]oA\v�A[�
A[S�A[&�AY�-AY&�AX��AX��AWK�AVE�AUG�AUATȴAT�\AS�AR�uAQ��AN��AM��AMO�AK�
AJ�RAJZAJ�AJ1AI��AI��AI��AH�!AF��AF1'AEAE\)AD�ACx�AAC�A?��A>jA=��A=/A<5?A;�A:n�A9x�A8��A8{A7�A7dZA5/A3��A2r�A0�DA/l�A.�RA.ZA-��A-��A-�PA-A+?}A)��A(�jA'O�A&��A%��A%�A%&�A$-A#C�A!��A VA�TA�wA�A�A�/A�TAE�A�A�-A�HA5?An�A��A33AAhsA�!A1A�-AdZA�A��A��A1'A��AbA�hA�Ax�A?}A
�HA
�A	�
A��AA��AA�A�AG�A�+A�AC�AVA ~�@��R@�;d@�M�@���@�?}@��@�1@�~�@�+@���@�@�E�@��/@� �@�
=@���@�S�@�ƨ@���@�@��@�=q@ݑh@ܛ�@���@�33@���@�5?@�Ĝ@֧�@Ցh@Չ7@��@�33@�~�@��@�?}@�j@��@ϕ�@ΰ!@�@˾w@ʧ�@�V@ɉ7@�/@�Ĝ@��
@���@Ǿw@���@�@�z�@��7@�Q�@��m@�dZ@���@��^@�O�@�G�@�9X@�K�@�`B@��@���@�&�@�%@��/@��9@��@���@�r�@�9X@�+@���@�~�@���@���@�v�@�^5@�-@�@��T@���@��/@��@�bN@�Q�@�A�@�  @���@��@�?}@��@�33@��R@�~�@��+@�~�@�=q@��#@�X@��@�I�@��w@�\)@��@���@�=q@��h@��@��@�(�@���@�t�@��@�v�@��#@��h@�p�@�O�@���@��F@��y@�ȴ@�~�@�M�@�{@��T@��h@��7@���@�&�@���@�1'@��m@��;@��w@���@��w@��@�33@��@�{@�X@�9X@�A�@��9@��w@�dZ@�"�@���@��\@�ff@�$�@��T@��@��@�o@���@�33@���@�K�@��@�
=@�\)@�K�@�n�@���@�x�@�?}@��@���@�b@��@���@�=q@�n�@��\@��\@�v�@��@�$�@�@��@��-@��-@�Q�@���@���@��7@�G�@�&�@�V@�j@�  @��P@��@���@�ȴ@���@���@���@���@���@���@�^5@�5?@�@���@��-@���@��#@��@��@��T@��^@���@�p�@�O�@�?}@��`@���@�z�@�r�@�j@�Q�@�(�@��@��@���@��@�K�@�"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBDBDBDBDB
=B
=B
=B	7B	7B	7B
=BDBPB\B\BhBoB�B�BuB{B�B�B�B!�B%�B8RB)�B�BBB��B��B��B��B�B�B�B�yB�`B�;B�
B��B��BȴBB�}B�^B�B��B��B��B��B��B�oB�Bs�Bl�BffBaHBZBYBQ�BD�B=qB7LB0!B+B&�B$�B"�B�B�B\B	7BB��B�B�`B�BȴB�?B�B��B��B��B��B�PB�Bz�Bt�Bl�B]/BT�BG�B+B�B
�B
��B
�wB
�^B
�RB
�FB
�9B
�B
��B
�hB
�7B
� B
q�B
jB
ffB
`BB
\)B
W
B
I�B
>wB
49B
,B
&�B
"�B
�B
uB
DB
B	��B	�B	�B	�B	�B	�sB	�HB	�B	ÖB	�jB	�jB	�^B	�XB	�LB	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�\B	�7B	�B	t�B	m�B	iyB	aHB	_;B	\)B	ZB	ZB	YB	XB	VB	R�B	M�B	I�B	F�B	E�B	A�B	:^B	1'B	(�B	$�B	!�B	�B	�B	�B	uB	bB	PB	DB		7B	B��B�B�B�mB�TB�HB�BB�5B�/B�#B�
B��B��BǮBĜB��B�}B�qB�dB�LB�3B�B�B�B�B��B��B��B��B��B��B��B�{B�bB�DB�+B�B� B}�B{�Bz�By�Bx�Bx�Bw�Bv�Bt�Bq�Bo�Bn�Bn�Bm�Bm�Bl�BjBk�BcTBbNB_;B[#BYBXBVBT�BS�BQ�BO�BL�BL�BL�BL�BK�BJ�BI�BF�BD�BD�BD�BC�BA�B@�B?}B=qB;dB@�BA�BA�BC�BD�BD�BF�BG�BH�BJ�BI�BI�BK�BL�BL�BO�BP�BQ�BQ�BR�BS�BT�BVBXBXBYBZBZB\)B]/B^5B`BB`BB_;BaHBcTBdZBiyBjBjBk�Bl�Bn�Bo�Bn�Bp�Bq�Bx�B~�B� B�DB��B��B��B��B��B�B�B�-B��BƨB��B��B��B��B��B��B��B�BB�ZB�ZB�`B�mB�sB�B�B�B�B�B��B��B��B	B	+B	1B	1B	1B	
=B	JB	VB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	'�B	)�B	/B	33B	5?B	7LB	9XB	:^B	:^B	>wB	B�B	F�B	I�B	J�B	Q�B	T�B	VB	W
B	]/B	cTB	k�B	k�B	k�B	k�B	iyB	gmB	iyB	o�B	q�B	r�B	r�B	v�B	y�B	z�B	~�B	�B	~�B	|�B	|�B	}�B	�B	�%B	�+B	�+B	�DB	�\B	�\B	�hB	�hB	�oB	�oB	�uB	�oB	�oB	�bB	�bB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	�{B	�uB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�3B	�LB	�XB	�^B	�^B	�^B	�dB	�jB	�jB	�qB	�wB	�}B	��B	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BDBDBDBDB
=B
=B
=B
=B
=B
=BJBVB\BhB{BhBoB�B�B{B�B�B�B�B#�B'�B=qB.B$�B	7B
=B��B��B��B��B��B�B�B�B�mB�ZB�)B��B��B��BŢBB�wB�'B�B��B��B��B��B��B�1Bw�Bn�BhsBdZB\)B\)BXBG�B@�B:^B5?B-B'�B&�B$�B�B�BhBPBB��B��B�B�5B��B�RB�'B��B��B��B��B�uB�B|�Bv�Bo�B_;BYBK�B.B�B
��B
��B
��B
�jB
�XB
�LB
�LB
�?B
��B
��B
�PB
�1B
t�B
k�B
iyB
aHB
^5B
^5B
N�B
C�B
9XB
/B
(�B
&�B
 �B
�B
hB
+B	��B	�B	�B	�B	�B	�B	�ZB	�BB	ǮB	�wB	�wB	�dB	�^B	�dB	�-B	�!B	�B	�B	��B	��B	��B	��B	��B	��B	�hB	�JB	�7B	w�B	o�B	m�B	dZB	`BB	]/B	ZB	ZB	ZB	YB	YB	XB	O�B	J�B	G�B	G�B	D�B	@�B	6FB	,B	%�B	$�B	!�B	�B	�B	�B	uB	\B	JB	DB	DB	  B��B�B�B�`B�NB�HB�;B�5B�/B�/B�
B��B��BǮBĜB��B�}B�wB�^B�RB�9B�B�B�B�B��B��B��B��B��B��B��B��B�\B�DB�B�B� B}�B{�Bz�Bz�By�Bx�Bx�Bv�Bv�Bq�Bn�Bn�Bn�Bn�Bm�Bl�Bo�BiyBffBdZB]/B[#B[#BXBW
BT�BS�BS�BR�BN�BM�BM�BL�BL�BL�BL�BJ�BF�BF�BF�BB�BB�BB�B?}B?}BC�BC�BC�BD�BE�BE�BG�BH�BI�BK�BK�BL�BM�BL�BN�BP�BQ�BR�BR�BS�BT�BVBXBZB[#B[#B[#B[#B]/B^5B`BB`BB`BBaHBcTBe`BiyBk�Bk�Bk�Bl�Bn�Bo�Bo�Bp�Br�Bt�B|�B�B�B�DB��B��B��B��B��B�B�B�3B��BƨB��B��B��B��B��B��B��B�NB�`B�ZB�`B�mB�yB�B�B�B�B�B��B��B��B	B	1B		7B		7B		7B	DB	PB	\B	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	$�B	%�B	'�B	(�B	,B	0!B	33B	6FB	7LB	:^B	;dB	:^B	>wB	B�B	G�B	J�B	J�B	Q�B	T�B	VB	W
B	]/B	cTB	l�B	l�B	l�B	l�B	k�B	gmB	iyB	p�B	r�B	s�B	s�B	v�B	y�B	{�B	� B	�B	� B	}�B	}�B	|�B	�B	�+B	�1B	�+B	�DB	�\B	�hB	�oB	�hB	�oB	�oB	�{B	�uB	�oB	�oB	�hB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�RB	�XB	�^B	�^B	�^B	�dB	�jB	�jB	�qB	�wB	��B	��B	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447002012010314470020120103144700  AO  ARGQ                                                                        20111130140845  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140845  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144700  IP                  G�O�G�O�G�O�                