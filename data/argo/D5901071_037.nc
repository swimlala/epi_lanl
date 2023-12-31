CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:01Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               %A   AO  20111130135907  20190522121825  1727_5046_037                   2C  D   APEX                            2143                            040306                          846 @�E��$�
1   @�E�P@7/��-V�c��9Xb1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh�fDifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDsff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg33Bo��Bw��B��B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC4  C5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#� D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D7  D7� D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dh  Dh� Di  Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Ds  Ds` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�5?A�7LA�=qA�A�A�A�A�C�A�E�A�C�A�5?A��A�{A�VA�JA�VA�JA�JA�bA�oA��A��A��A��A��A��A��A��A��A�$�A�+A�(�A�(�A�$�A�$�A��A�1A��Aΰ!A�-A���A�\)A�ȴA�/AÅA���A���A���A�7LA�E�A�VA�bA�E�A��wA�%A�M�A���A�ƨA�ȴA�XA��A��A�r�A�v�A��\A�G�A�&�A��A�"�A�~�A�+A��^A�{A�bA���A��A��9A�A��FA�jA��A��PA���A��A�|�A�A��TA��A�Q�A�  A���A�-A��HA��A�?}A�t�A�oA�M�A��A���A���A��hA���A�1'A��9A�jA��+A�`BA7LA|�A{
=Ay�Ay;dAwdZAt�yArz�Aq33AnĜAl�\Ak��Ak�Aj��Ai�-Ai%Ah9XAf��Ae7LAdM�Ac��Ac�Ab�!Ab��Abv�Ab�Aa��Aa`BA`��A_��A^�A]x�A\r�AZ�\AXĜAV~�AS��AQG�AQoAO�TAN�AN-AL1'AJn�AIO�AH��AH�AF~�AD�`AC��AC;dAB�`AB^5AB{A@jA?�^A?��A?oA>^5A=��A<ffA;t�A;O�A;7LA;
=A:^5A9�A9�FA9t�A9C�A8�HA8$�A5��A4-A3�A3A1��A/��A.��A,��A,9XA+��A+
=A)�A(�A'�;A'`BA'�A&�HA&VA$�\A#+A"��A"I�A"A!��A ��A��A��A�A�A��A��A�A��Ar�AjAbNA��AbNAE�AA��A�!A�\AZA  A��A�jAQ�A��A�mAXAr�A  A��A
ȴA
JA�AE�Al�A�\A�/A�A�/AE�AG�@�^5@�33@���@���@��@���@�1'@�j@�w@�n�@�bN@�t�@�ff@�?}@�l�@�J@���@��H@��@��@�Ĝ@�;d@�ff@�-@���@�?}@ם�@�V@�G�@�
=@���@�  @���@�@�J@�%@�1'@ʟ�@�z�@�J@Ĭ@Å@�@��@°!@�~�@��#@��@��@�ff@�7L@�1@�t�@��@��-@���@�bN@���@�@�X@���@��@���@�9X@�@�ff@���@�p�@��9@�(�@�  @��@�@��@��-@��/@�9X@�C�@�ȴ@�hs@�/@�&�@���@�1'@���@���@�t�@��H@�~�@�-@��@�X@�/@���@�r�@���@�33@��H@�E�@��@���@�p�@��/@�j@�I�@�9X@� �@���@�l�@���@��R@���@�^5@��#@�?}@��-@��T@�J@���@�G�@�V@�r�@�l�@�r�@���@�j@�9X@��@��;@���@�+@��m@�~�@�5?@�5?@��@���@��#@��#@��j@�"�@��u@�V@�G�@��T@�@�=q@���@���@��+@��!@��\@�V@�5?@�$�@���@���@��h@�hs@��@��@��@�(�@�b@��
@�K�@��+@�^5@�$�@��^@�x�@�G�@���@�Ĝ@�z�@�(�@��@��@�K�@�@�ȴ@��+@�-@���@��^@��7@��@�x�@�hs@�X@�O�@��@��u@�z�@�bN@�A�@��m@�|�@�\)@�S�@�C�@�o@�@�@��@��H@��@���@���@�~�@�$�@���@�@�hs@�&�@��@���@��@���@���@�A�@�  @���@�l�@�\)@�\)@�\)@��@�@�ȴ@��\@�~�@�~�@�v�@�n�@�-@���@���@��-@�`B@�/@�V@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�7LA�=qA�A�A�A�A�C�A�E�A�C�A�5?A��A�{A�VA�JA�VA�JA�JA�bA�oA��A��A��A��A��A��A��A��A��A�$�A�+A�(�A�(�A�$�A�$�A��A�1A��Aΰ!A�-A���A�\)A�ȴA�/AÅA���A���A���A�7LA�E�A�VA�bA�E�A��wA�%A�M�A���A�ƨA�ȴA�XA��A��A�r�A�v�A��\A�G�A�&�A��A�"�A�~�A�+A��^A�{A�bA���A��A��9A�A��FA�jA��A��PA���A��A�|�A�A��TA��A�Q�A�  A���A�-A��HA��A�?}A�t�A�oA�M�A��A���A���A��hA���A�1'A��9A�jA��+A�`BA7LA|�A{
=Ay�Ay;dAwdZAt�yArz�Aq33AnĜAl�\Ak��Ak�Aj��Ai�-Ai%Ah9XAf��Ae7LAdM�Ac��Ac�Ab�!Ab��Abv�Ab�Aa��Aa`BA`��A_��A^�A]x�A\r�AZ�\AXĜAV~�AS��AQG�AQoAO�TAN�AN-AL1'AJn�AIO�AH��AH�AF~�AD�`AC��AC;dAB�`AB^5AB{A@jA?�^A?��A?oA>^5A=��A<ffA;t�A;O�A;7LA;
=A:^5A9�A9�FA9t�A9C�A8�HA8$�A5��A4-A3�A3A1��A/��A.��A,��A,9XA+��A+
=A)�A(�A'�;A'`BA'�A&�HA&VA$�\A#+A"��A"I�A"A!��A ��A��A��A�A�A��A��A�A��Ar�AjAbNA��AbNAE�AA��A�!A�\AZA  A��A�jAQ�A��A�mAXAr�A  A��A
ȴA
JA�AE�Al�A�\A�/A�A�/AE�AG�@�^5@�33@���@���@��@���@�1'@�j@�w@�n�@�bN@�t�@�ff@�?}@�l�@�J@���@��H@��@��@�Ĝ@�;d@�ff@�-@���@�?}@ם�@�V@�G�@�
=@���@�  @���@�@�J@�%@�1'@ʟ�@�z�@�J@Ĭ@Å@�@��@°!@�~�@��#@��@��@�ff@�7L@�1@�t�@��@��-@���@�bN@���@�@�X@���@��@���@�9X@�@�ff@���@�p�@��9@�(�@�  @��@�@��@��-@��/@�9X@�C�@�ȴ@�hs@�/@�&�@���@�1'@���@���@�t�@��H@�~�@�-@��@�X@�/@���@�r�@���@�33@��H@�E�@��@���@�p�@��/@�j@�I�@�9X@� �@���@�l�@���@��R@���@�^5@��#@�?}@��-@��T@�J@���@�G�@�V@�r�@�l�@�r�@���@�j@�9X@��@��;@���@�+@��m@�~�@�5?@�5?@��@���@��#@��#@��j@�"�@��u@�V@�G�@��T@�@�=q@���@���@��+@��!@��\@�V@�5?@�$�@���@���@��h@�hs@��@��@��@�(�@�b@��
@�K�@��+@�^5@�$�@��^@�x�@�G�@���@�Ĝ@�z�@�(�@��@��@�K�@�@�ȴ@��+@�-@���@��^@��7@��@�x�@�hs@�X@�O�@��@��u@�z�@�bN@�A�@��m@�|�@�\)@�S�@�C�@�o@�@�@��@��H@��@���@���@�~�@�$�@���@�@�hs@�&�@��@���@��@���@���@�A�@�  @���@�l�@�\)@�\)@�\)@��@�@�ȴ@��\@�~�@�~�@�v�@�n�@�-@���@���@��-@�`B@�/@�V@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�3B�3B�3B�3B�3B�3B�3B�3B�3B�9B�9B�9B�?B�?B�FB�FB�LB�RB�^B�dB�jB�qB�qB�wB�wB�}B��BÖBŢBƨBǮBǮBȴB��B��B��B��B�B�`B��B+B
=BDB+B��B�B�mB�;B�B��BȴBB�-B��B��B��B�PB�7B�Bt�Bo�BffB^5BQ�BG�B>wB/B,B�BB��B�B�;B��BŢB�wB�^B�?B�B��B��B�VB�+B�B~�Bt�Bm�BffBQ�BM�BB�B33B#�B,B,B�B	7B
��B
�)B
�wB
�FB
�B
��B
��B
�bB
�B
w�B
hsB
\)B
T�B
M�B
?}B
2-B
&�B
�B

=B	��B
B
B
B	��B	��B	�B	�B	�`B	�BB	�/B	�#B	�/B	�5B	�5B	�/B	�/B	�#B	�B	�B	��B	ǮB	�wB	�?B	��B	��B	�%B	w�B	x�B	q�B	k�B	dZB	W
B	K�B	F�B	E�B	@�B	9XB	2-B	0!B	0!B	/B	-B	/B	&�B	+B	,B	'�B	"�B	�B	�B	uB	oB	oB	hB	VB	JB	DB	
=B	1B	B��B��B�B�sB�TB�B��B��BŢBB��B�qB�^B�LB�FB�?B�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�oB�hB�VB�JB�B~�B}�B}�B|�B{�By�Bw�Bs�Bp�Bn�Bm�Bk�BiyBgmBe`BaHB]/B[#BXBVBQ�BM�BK�BJ�BH�BC�B@�B>wB>wB=qB;dB8RB6FB6FB5?B33B33B33B2-B0!B/B.B-B.B/B2-B9XB;dB;dB;dB:^B:^B:^B7LB5?B2-B33B33B33B33B33B49B49B33B1'B2-B49B7LB9XB9XB9XB9XB8RB8RB8RB8RB9XB;dB<jB<jB?}BB�BA�BE�BG�BG�BG�BJ�BM�BO�BQ�BR�BT�BT�BYBZBYBYB\)B\)B_;BaHBbNBffBgmBn�Bo�Bp�Bu�B�B�=B�JB�PB�hB�{B�{B�{B��B��B��B��B��B��B��B��B�B�B�B�-B�FB�FB�LB�XB�qB�}B��B��B��B��BB��B�5B�HB�ZB�sB�B�B�B�B��B	  B	1B	PB	VB	�B	�B	�B	�B	#�B	&�B	&�B	(�B	+B	/B	49B	49B	/B	)�B	6FB	:^B	A�B	C�B	F�B	K�B	O�B	XB	]/B	_;B	`BB	aHB	aHB	bNB	dZB	dZB	e`B	ffB	hsB	iyB	m�B	m�B	m�B	n�B	o�B	p�B	q�B	t�B	u�B	w�B	{�B	|�B	~�B	�B	�B	�B	�1B	�=B	�JB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�?B	�FB	�LB	�^B	�dB	�qB	�wB	��B	��B	B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�)B	�/B	�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�3B�3B�3B�3B�3B�3B�3B�3B�3B�9B�9B�9B�?B�?B�FB�FB�LB�RB�^B�dB�jB�qB�qB�wB�wB�}B��BÖBŢBƨBǮBǮBȴB��B��B��B��B�/B�BB\BuB{B�BDB��B�B�ZB�/B��B��BɺB�^B�B��B��B�bB�PB�=Bw�Bt�BjBcTBVBL�BC�B1'B1'B�B+B��B�B�NB�BȴB��B�jB�RB�LB��B��B�hB�=B�B�Bw�Bp�Bo�BT�BN�BE�B9XB&�B-B/B�BVB  B
�fB
��B
�^B
�B
��B
��B
��B
�+B
}�B
m�B
_;B
W
B
R�B
G�B
9XB
+B
$�B
hB
B
B
%B
B	��B	��B	��B	�B	�sB	�NB	�;B	�)B	�/B	�;B	�;B	�5B	�;B	�/B	�/B	�B	��B	��B	ŢB	�dB	�'B	��B	�VB	x�B	{�B	s�B	m�B	iyB	[#B	N�B	G�B	G�B	D�B	=qB	49B	2-B	1'B	1'B	.B	49B	(�B	,B	.B	)�B	$�B	$�B	�B	{B	uB	uB	uB	bB	PB	JB	DB	
=B	1B	%B��B�B�B�mB�BB�
B��BȴBÖBÖBB�jB�^B�RB�FB�?B�9B�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB�uB�hB�hB�DB�B~�B~�B}�B|�Bz�Bz�Bu�Bt�Bq�Bo�Bn�Bk�BiyBhsBcTBaHB]/B[#BYBW
BQ�BN�BL�BK�BI�BE�B@�B?}B>wB=qB=qB;dB8RB7LB6FB5?B5?B49B33B2-B0!B1'B2-B49B6FB<jB=qB<jB<jB<jB=qB=qB9XB8RB5?B49B49B49B5?B5?B5?B6FB6FB5?B49B6FB8RB9XB9XB:^B:^B9XB:^B:^B:^B;dB<jB=qB>wBA�BC�BD�BG�BH�BH�BI�BM�BO�BQ�BR�BS�BVBVBZB[#BZBZB]/B^5BaHBbNBdZBgmBjBo�Bo�Bq�Bu�B�B�DB�JB�VB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�FB�FB�LB�^B�wB��BB��BBBÖB��B�5B�HB�ZB�yB�B�B�B�B��B	B	1B	PB	\B	�B	�B	�B	!�B	$�B	&�B	'�B	(�B	+B	/B	6FB	6FB	33B	(�B	6FB	9XB	A�B	C�B	F�B	K�B	P�B	XB	]/B	`BB	`BB	aHB	aHB	bNB	e`B	dZB	ffB	gmB	hsB	jB	m�B	n�B	n�B	o�B	o�B	q�B	r�B	u�B	u�B	x�B	|�B	}�B	� B	�B	�B	�%B	�7B	�DB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�LB	�RB	�dB	�dB	�qB	�wB	��B	B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<�C�<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446462012010314464620120103144646  AO  ARGQ                                                                        20111130135907  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135907  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144646  IP                  G�O�G�O�G�O�                