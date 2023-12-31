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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135544  20190522121825  1727_5046_014                   2C  D   APEX                            2143                            040306                          846 @�)a?�?�1   @�)a�$ @7���"���c� ě��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Dsf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @333@y��@���@���AffA>ffA\��A~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq��Cs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:s3D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Dos3Do��Dpy�Dp��Dqy�Dq��Dry�Ds  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AˋDAˇ+A˃A˅Aˉ7Aˉ7AˋDAˍPAˍPAˏ\AˋDAˇ+AˋDAˇ+AʮAȶFAș�A�ffA�?}A�$�A�{A��A�;dA�C�A´9A�C�A��A��A��A�I�A�ȴA�v�A�/A���A�M�A���A��A���A���A�~�A�|�A�-A��-A�C�A���A�ƨA���A�5?A�bA��#A�A�A��;A�\)A�oA�ĜA���A���A�\)A���A���A��A��-A���A��A�ffA��7A�VA�r�A��
A�z�A���A�bA�"�A���A�K�A��
A�E�A��A�A��jA�A�7LA���A�"�A�ĜA�dZA�
=A��wA��A�O�A��A��+A��jA���A��
A���A�{A���A�?}A�E�A�Q�A��DA�oA���A�p�A��A��yA��jA��wA��;A�A�A� �A~��A{�A{Az�Az^5Ax�uAwXAvȴAvAtv�AsVAq��Ao�-AnjAmAk�Aj�Ai�Ag�^AfM�Ad9XAbZAa�A`=qA_%A\9XAZ�AZffAY?}AYG�AXn�AW�PAU�#AR�uAQoAPJAOAM��AK�mAI��AHAC�AB�AB9XA?��A=��A=33A<^5A;�7A9�A7XA6��A5/A4��A4v�A3l�A29XA0�`A/ƨA/l�A.�A.bNA.A-;dA,A�A+��A*�/A)�A)oA(�A&ȴA&�A%�wA%t�A$�DA#�A"�/A!�A!�PA �yA M�A $�AƨAAZAC�A�A�DAl�AbA�!A�#A%A��AVA1A�wA�AjAXA�\AM�A�7A�-A�A��An�A�A�
A
��A
VA�9AffA�mA��A1A�#A��A�A��A{At�A�A�
A �A 1'@��@��@��@��@���@���@��@���@��@���@�@�V@�G�@�z�@@�%@��@���@��y@�`B@�33@�%@�z�@�I�@��@߾w@�v�@�p�@���@�"�@֗�@�{@�p�@�V@�9X@Ӿw@�
=@��@���@�9X@ϥ�@���@���@�?}@��/@�r�@�C�@���@�ƨ@�+@Ɵ�@�J@��@Ł@�7L@���@�j@��m@�C�@�v�@�E�@�7L@�V@��@��!@�V@�9X@�
=@�V@�p�@��@�33@�$�@��j@��@��@�V@�@��T@��-@�/@�I�@�K�@���@�  @���@��@�M�@�\)@�;d@���@�$�@�-@�$�@�J@�hs@�O�@��@�1@��R@��!@�&�@��F@��@�A�@� �@���@��-@�33@�J@�x�@�`B@���@��#@�A�@��@��@�"�@��@�S�@���@��w@�ff@���@���@�@�v�@�O�@�C�@�9X@��@��@��@�V@�{@���@��h@��T@��@��@��@�@���@�5?@�O�@��m@��w@���@���@�J@��h@�G�@��`@��@���@���@�%@��@��h@�5?@��#@��@���@�bN@� �@���@��@�ff@���@�?}@���@�O�@���@��@��w@��P@��y@���@�E�@���@�`B@�Z@�ƨ@�"�@�+@�
=@���@�^5@�$�@�M�@��R@�v�@��-@���@�O�@��j@�j@�;d@��#@��@�r�@�9X@��@��@�+@��+@��^@�x�@�G�@��j@��@�Q�@�I�@��@�bN@��@�|�@�C�@�~�@�$�@�5?@�=q@�J@��#@��/@���@�b@���@�S�@�"�@��@���@��R@��R@���@���@�V@�$�@�@�@�J@���@�@���@�p�@�7L@�%@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AˋDAˇ+A˃A˅Aˉ7Aˉ7AˋDAˍPAˍPAˏ\AˋDAˇ+AˋDAˇ+AʮAȶFAș�A�ffA�?}A�$�A�{A��A�;dA�C�A´9A�C�A��A��A��A�I�A�ȴA�v�A�/A���A�M�A���A��A���A���A�~�A�|�A�-A��-A�C�A���A�ƨA���A�5?A�bA��#A�A�A��;A�\)A�oA�ĜA���A���A�\)A���A���A��A��-A���A��A�ffA��7A�VA�r�A��
A�z�A���A�bA�"�A���A�K�A��
A�E�A��A�A��jA�A�7LA���A�"�A�ĜA�dZA�
=A��wA��A�O�A��A��+A��jA���A��
A���A�{A���A�?}A�E�A�Q�A��DA�oA���A�p�A��A��yA��jA��wA��;A�A�A� �A~��A{�A{Az�Az^5Ax�uAwXAvȴAvAtv�AsVAq��Ao�-AnjAmAk�Aj�Ai�Ag�^AfM�Ad9XAbZAa�A`=qA_%A\9XAZ�AZffAY?}AYG�AXn�AW�PAU�#AR�uAQoAPJAOAM��AK�mAI��AHAC�AB�AB9XA?��A=��A=33A<^5A;�7A9�A7XA6��A5/A4��A4v�A3l�A29XA0�`A/ƨA/l�A.�A.bNA.A-;dA,A�A+��A*�/A)�A)oA(�A&ȴA&�A%�wA%t�A$�DA#�A"�/A!�A!�PA �yA M�A $�AƨAAZAC�A�A�DAl�AbA�!A�#A%A��AVA1A�wA�AjAXA�\AM�A�7A�-A�A��An�A�A�
A
��A
VA�9AffA�mA��A1A�#A��A�A��A{At�A�A�
A �A 1'@��@��@��@��@���@���@��@���@��@���@�@�V@�G�@�z�@@�%@��@���@��y@�`B@�33@�%@�z�@�I�@��@߾w@�v�@�p�@���@�"�@֗�@�{@�p�@�V@�9X@Ӿw@�
=@��@���@�9X@ϥ�@���@���@�?}@��/@�r�@�C�@���@�ƨ@�+@Ɵ�@�J@��@Ł@�7L@���@�j@��m@�C�@�v�@�E�@�7L@�V@��@��!@�V@�9X@�
=@�V@�p�@��@�33@�$�@��j@��@��@�V@�@��T@��-@�/@�I�@�K�@���@�  @���@��@�M�@�\)@�;d@���@�$�@�-@�$�@�J@�hs@�O�@��@�1@��R@��!@�&�@��F@��@�A�@� �@���@��-@�33@�J@�x�@�`B@���@��#@�A�@��@��@�"�@��@�S�@���@��w@�ff@���@���@�@�v�@�O�@�C�@�9X@��@��@��@�V@�{@���@��h@��T@��@��@��@�@���@�5?@�O�@��m@��w@���@���@�J@��h@�G�@��`@��@���@���@�%@��@��h@�5?@��#@��@���@�bN@� �@���@��@�ff@���@�?}@���@�O�@���@��@��w@��P@��y@���@�E�@���@�`B@�Z@�ƨ@�"�@�+@�
=@���@�^5@�$�@�M�@��R@�v�@��-@���@�O�@��j@�j@�;d@��#@��@�r�@�9X@��@��@�+@��+@��^@�x�@�G�@��j@��@�Q�@�I�@��@�bN@��@�|�@�C�@�~�@�$�@�5?@�=q@�J@��#@��/@���@�b@���@�S�@�"�@��@���@��R@��R@���@���@�V@�$�@�@�@�J@���@�@���@�p�@�7L@�%@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBYBYBZBYBYBYBYBYBYBYBZB[#BZBYB�JB��BBBBBBB��B�`B�;B�B��B��B�
B�5B�fB�mB�mB�B�B�ZB�`B�sB�B�sB�ZB�NB�mB�BB�5B�
B��B��B��BŢBB�wB�XB�'B��B��B��B��B��B�bB�B|�Br�Bo�Br�Br�Bo�BgmBZBF�B;dB>wB^5BZBVBP�BF�B;dB1'B,B#�B�B\BB��B��B��B�B�B�fB��B�9B��B�DB|�Bu�BjBQ�BC�B2-B.B'�B+B,B�BB
�B
��B
�PB
w�B
^5B
XB
N�B
49B
-B
-B
33B
C�B
A�B
=qB
9XB
0!B
$�B
�B
DB
B	��B	�B	�mB	�B	ɺB	�dB	��B	��B	��B	��B	�\B	w�B	s�B	u�B	t�B	�hB	�hB	�%B	q�B	XB	P�B	M�B	G�B	;dB	(�B	uB��B�/B��B��B�}B�FB�-B�B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B�{B�{B�{B�uB�{B��B��B��B�{B�uB�oB�bB�VB�JB�DB�DB�7B�1B�+B�%B�B�B�B�B� B}�By�Bw�Bv�Bu�Bt�Bs�Bs�Br�Bp�Bn�Bk�BjBiyBgmBdZBbNBbNB`BB_;B[#BZBYBT�BVBT�BR�BR�BS�BR�BR�BP�BP�BO�BM�BJ�BH�BF�BF�BD�B@�B?}B<jB:^B:^B9XB9XB8RB7LB6FB5?B5?B33B1'B0!B.B-B,B+B+B+B+B+B)�B'�B$�B'�B'�B'�B'�B&�B&�B&�B&�B%�B%�B$�B$�B$�B$�B#�B$�B%�B$�B$�B%�B%�B$�B$�B$�B%�B$�B%�B%�B%�B%�B&�B&�B&�B1'B49B33B33B6FB=qB?}BA�BC�BH�BK�BP�BP�BR�BS�BT�BW
BXBXBZB^5Be`Bk�By�B�uB��B��B�}BŢBŢBÖBŢBɺBɺBȴBǮB��B��B��B��B��B��B��B��B�B�B��B��B��BǮBɺB��B��B��BɺBĜBÖB�wBƨBǮBǮBŢBŢB��B�;B�mB�B�B�HB�B�B�HB�fB�B�B��B��B��B��B	  B	B	B	%B		7B	%B	B	1B	{B	�B	"�B	%�B	'�B	/B	1'B	2-B	33B	49B	6FB	;dB	C�B	F�B	F�B	H�B	H�B	J�B	K�B	L�B	L�B	K�B	N�B	Q�B	VB	VB	W
B	XB	XB	YB	ZB	ZB	[#B	]/B	`BB	e`B	hsB	l�B	o�B	p�B	q�B	w�B	~�B	�B	�B	�B	�%B	�+B	�=B	�DB	�1B	�B	�B	�+B	�=B	�JB	�DB	�=B	�JB	�VB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�?B	�FB	�FB	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�jB	�qB	�wB	��B	B	ĜB	ŢB	ŢB	ƨB	ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BYBYBZBYBYBYBYBYBYBYBZB[#BZB]/B��B��BBBBB%B1BB�B�B�BB�B�
B�#B�HB�sB�sB�sB�B�B�fB�mB�B�B�B�`B�ZB�yB�TB�NB�/B��B��B��BȴBĜBB��B�XB�B��B��B��B��B�{B�B�Bu�Br�Bv�Bt�Br�BjB`BBJ�B>wB>wBaHB\)BXBS�BJ�B>wB33B0!B'�B�BuB+BB��B��B�B�B�B�B�XB��B�bB~�By�Bq�BVBI�B8RB33B+B-B33B#�B	7B
�TB
��B
�hB
~�B
cTB
[#B
VB
6FB
.B
.B
8RB
F�B
C�B
@�B
>wB
49B
(�B
!�B
\B
%B	��B	�B	�B	�5B	��B	B	�!B	��B	��B	��B	��B	{�B	u�B	y�B	t�B	�{B	�{B	�JB	{�B	]/B	T�B	P�B	J�B	?}B	.B	�B	%B�BB��B��BÖB�RB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�hB�\B�VB�PB�DB�=B�1B�1B�+B�B�B�B�B�B~�B|�By�Bx�Bu�Bt�Bt�Bs�Br�Bp�Bn�Bl�BjBiyBiyBdZBcTBaHBbNB`BB]/B[#BYBW
BW
BW
BT�BT�BR�BS�BS�BR�BQ�BO�BL�BK�BH�BH�BF�BB�BA�B@�B=qB;dB:^B:^B9XB9XB8RB7LB7LB5?B5?B33B2-B0!B/B/B/B,B,B,B+B+B.B+B)�B(�B(�B'�B'�B(�B'�B&�B'�B&�B%�B%�B%�B%�B%�B&�B%�B&�B)�B'�B%�B%�B%�B%�B%�B&�B&�B&�B&�B'�B(�B-B33B49B49B6FB9XB>wBA�BB�BD�BJ�BM�BR�BR�BS�BT�BW
BXBXBYB[#B`BBgmBiyBt�B�oB�{B��B�wBŢBƨBĜBŢBɺBɺBɺBǮB��B��B��B��B��B��B��B�B�B�B��B��B��BȴBɺB��B��B��B��BƨBƨB�qBǮBȴB��BǮBĜB��B�5B�fB�B�B�fB�
B�B�BB�ZB�B�B��B��B��B��B	  B	B	B	B	DB	1B	B	%B	uB	�B	#�B	&�B	(�B	/B	1'B	2-B	33B	49B	5?B	:^B	D�B	G�B	G�B	H�B	H�B	K�B	L�B	M�B	M�B	L�B	O�B	Q�B	W
B	W
B	XB	XB	YB	ZB	[#B	[#B	\)B	_;B	aHB	ffB	hsB	l�B	p�B	q�B	q�B	w�B	~�B	�B	�B	�B	�+B	�1B	�DB	�PB	�=B	�+B	�B	�+B	�=B	�PB	�JB	�DB	�PB	�\B	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�?B	�?B	�FB	�FB	�?B	�FB	�LB	�XB	�dB	�dB	�dB	�jB	�qB	�wB	��B	ÖB	ĜB	ƨB	ƨB	ǮB	ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446382012010314463820120103144638  AO  ARGQ                                                                        20111130135544  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135544  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144638  IP                  G�O�G�O�G�O�                