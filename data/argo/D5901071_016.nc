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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135553  20190522121825  1727_5046_016                   2C  D   APEX                            2143                            040306                          846 @�+�L
�1   @�+�����@7�I�^5�c�E����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� DofDo�fDp  Dp� Dq  Dq� Dr  Dr� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B  B��B��B��B'��B/��B733B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D � D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT�3DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^�3D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Dds3Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm�3Dny�Do  Do� Do��Dpy�Dp��Dqy�Dq��Dry�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A΁A�ffA�1'A� �A�bA���A���A͋DA���A�=qA��TA�ƨA�A�ƨA���A˧�A˗�A�~�A�VA�$�Aʣ�A��HA�A�A��A��HA�r�A���A��A��hA�A�jA�t�A��A�v�A�
=A�A��RA� �A��A�1A�n�A�$�A��
A�A�jA���A��A�JA��
A�l�A�jA��TA�=qA��DA���A��;A�jA��!A���A�jA��A�
=A�1'A��\A�33A���A��FA���A�\)A�(�A�K�A�
=A��
A��7A�O�A��!A�A�VA�VA���A���A�\)A��\A���A��;A�dZA�ĜA�?}A�~�A���A���A�n�A���A�M�A�9XA�M�A�M�A��A�+A��A��A�G�A��A�(�A}��A|�A{�TA{
=Ay�;Ay�;Ay��Ax�Aw�Au�PAs��As�hAs�As�Ar^5AmoAj�\AiO�Ahn�Af��AdbNAcXAb1'AbI�Aa�A`�uA^��A]/AZ��AY�AWhsAT�AS|�ARz�AR��ARE�AN��AM`BALVALA�ALE�AL�+AM/ANbNAK��AJffAI��AG��AF=qAC��AC�AC%AB�AAG�A@r�A>�+A=��A=K�A<�A<�9A=��A;A:JA7�A4M�A2M�A1�hA1p�A1O�A/l�A,�A+�A*��A)VA'�A'��A'�A%K�A#7LA"A�A"1A!x�A �A�7A
=A��A|�A��A+A�\A�A�jA(�A|�AC�AA�Ar�A  A�^A|�A��A��A�A��An�A�A7LA-A��AĜA��A+A�`A��AA�A\)A
Q�A	�A��A�A��An�A��A��AĜA1'Ap�A �!A 1'@�C�@�G�@�r�@�K�@�M�@���@��j@�ff@��@�$�@��@� �@�Q�@�|�@@�$�@�O�@�7L@�r�@�P@�~�@�-@�j@�1@�E�@�&�@�1'@���@�x�@�hs@ܴ9@ە�@�p�@�r�@���@�`B@�dZ@��@�1'@��H@ͺ^@̃@�l�@�+@ʧ�@�@ɉ7@ȃ@��@î@�C�@§�@��@�@���@���@�x�@�&�@��@��D@�n�@���@��!@���@��P@��w@�ƨ@�I�@�J@�$�@�^5@�~�@�x�@�%@��w@��@�1@�
=@�@��@�C�@�~�@�$�@��h@��@�b@�33@��@�$�@�x�@��/@��@���@��@�|�@���@��@��9@�1@�S�@�ȴ@�$�@���@�X@��j@� �@��@�b@�b@��@��@�;d@�o@�o@��@��+@��@��@�ff@��7@�I�@��m@��F@�l�@�o@��R@��#@��-@�&�@���@���@���@��@���@��@��@��P@�t�@�+@�@��y@��@���@���@��D@��`@�X@��@���@�$�@�n�@���@��@�ff@�J@���@���@��@���@�j@�Z@�ƨ@�ȴ@�E�@��@�=q@���@��h@��`@�  @��@�ƨ@���@�\)@�;d@�@��+@��@��#@�@��h@�hs@�hs@�/@�Ĝ@�r�@�I�@�|�@�dZ@�o@�$�@���@���@�ff@��T@�t�@��m@�33@��\@�^5@�n�@�M�@���@��h@��@��D@�I�@��@��@�ƨ@� �@�(�@��
@��w@�;d@��y@��H@���@���@���@���@��\@��\@��\@�^5@�@��^@���@��h@���@�`B@�?}@�G�@�X@�/@��@���@���@���@�bN@��@�
=@���@���@���@��R@�"�@���@�{@��@��#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A΁A�ffA�1'A� �A�bA���A���A͋DA���A�=qA��TA�ƨA�A�ƨA���A˧�A˗�A�~�A�VA�$�Aʣ�A��HA�A�A��A��HA�r�A���A��A��hA�A�jA�t�A��A�v�A�
=A�A��RA� �A��A�1A�n�A�$�A��
A�A�jA���A��A�JA��
A�l�A�jA��TA�=qA��DA���A��;A�jA��!A���A�jA��A�
=A�1'A��\A�33A���A��FA���A�\)A�(�A�K�A�
=A��
A��7A�O�A��!A�A�VA�VA���A���A�\)A��\A���A��;A�dZA�ĜA�?}A�~�A���A���A�n�A���A�M�A�9XA�M�A�M�A��A�+A��A��A�G�A��A�(�A}��A|�A{�TA{
=Ay�;Ay�;Ay��Ax�Aw�Au�PAs��As�hAs�As�Ar^5AmoAj�\AiO�Ahn�Af��AdbNAcXAb1'AbI�Aa�A`�uA^��A]/AZ��AY�AWhsAT�AS|�ARz�AR��ARE�AN��AM`BALVALA�ALE�AL�+AM/ANbNAK��AJffAI��AG��AF=qAC��AC�AC%AB�AAG�A@r�A>�+A=��A=K�A<�A<�9A=��A;A:JA7�A4M�A2M�A1�hA1p�A1O�A/l�A,�A+�A*��A)VA'�A'��A'�A%K�A#7LA"A�A"1A!x�A �A�7A
=A��A|�A��A+A�\A�A�jA(�A|�AC�AA�Ar�A  A�^A|�A��A��A�A��An�A�A7LA-A��AĜA��A+A�`A��AA�A\)A
Q�A	�A��A�A��An�A��A��AĜA1'Ap�A �!A 1'@�C�@�G�@�r�@�K�@�M�@���@��j@�ff@��@�$�@��@� �@�Q�@�|�@@�$�@�O�@�7L@�r�@�P@�~�@�-@�j@�1@�E�@�&�@�1'@���@�x�@�hs@ܴ9@ە�@�p�@�r�@���@�`B@�dZ@��@�1'@��H@ͺ^@̃@�l�@�+@ʧ�@�@ɉ7@ȃ@��@î@�C�@§�@��@�@���@���@�x�@�&�@��@��D@�n�@���@��!@���@��P@��w@�ƨ@�I�@�J@�$�@�^5@�~�@�x�@�%@��w@��@�1@�
=@�@��@�C�@�~�@�$�@��h@��@�b@�33@��@�$�@�x�@��/@��@���@��@�|�@���@��@��9@�1@�S�@�ȴ@�$�@���@�X@��j@� �@��@�b@�b@��@��@�;d@�o@�o@��@��+@��@��@�ff@��7@�I�@��m@��F@�l�@�o@��R@��#@��-@�&�@���@���@���@��@���@��@��@��P@�t�@�+@�@��y@��@���@���@��D@��`@�X@��@���@�$�@�n�@���@��@�ff@�J@���@���@��@���@�j@�Z@�ƨ@�ȴ@�E�@��@�=q@���@��h@��`@�  @��@�ƨ@���@�\)@�;d@�@��+@��@��#@�@��h@�hs@�hs@�/@�Ĝ@�r�@�I�@�|�@�dZ@�o@�$�@���@���@�ff@��T@�t�@��m@�33@��\@�^5@�n�@�M�@���@��h@��@��D@�I�@��@��@�ƨ@� �@�(�@��
@��w@�;d@��y@��H@���@���@���@���@��\@��\@��\@�^5@�@��^@���@��h@���@�`B@�?}@�G�@�X@�/@��@���@���@���@�bN@��@�
=@���@���@���@��R@�"�@���@�{@��@��#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBo�Bn�Bp�Bp�Bp�Bp�Br�Bv�B~�B�7B� B}�B~�B� B�B�B� B�B�1B�PB��B�XB�3B�'B�3B�^B�}B��BBŢBŢB��B��B�B��B��B��B�B�B�B�B�B�B�B�
B�/B�BB�)B�B��B��BɺBĜB�^B�'B��B��B�{Bz�Bn�BYBP�BM�BH�BC�B?}B:^B0!B�B%B��B��B�B�B�fB�B�wB�'B��B��B�JB�VB�B� B� Bt�B[#BA�B5?B&�B�B+B
��B
��B
�B
��B
�^B
�!B
��B
�DB
�1B
�B
y�B
hsB
N�B
C�B
H�B
M�B
@�B
F�B
Q�B
YB
Q�B
B�B
7LB
5?B
49B
2-B
�B	�yB	��B	B	�qB	�B	��B	��B	��B	��B	��B	��B	�=B	�B	s�B	u�B	gmB	R�B	J�B	G�B	VB	T�B	D�B	/B	+B	+B	-B	33B	A�B	ZB	J�B	E�B	<jB	&�B	�B	1B	B	PB		7B��B��B�`B�#B�
B�B�B�B�ZB�)BŢB�!B��B��B�'B�3B��B��B��B��B��B��B��B��B�oB�\B�PB�JB�=B�7B�+B�%B�B�B}�B|�Bz�Bx�Bx�Bw�Bw�Bv�By�B�B�B�B�B�7B�7B�+B�+B�%B�%B�B|�Bz�B|�B�B�%B�B�%B�B�B}�Bx�Bq�Bn�BjBbNB`BBW
BT�BYBXBVBQ�BN�BK�BJ�BJ�BJ�BJ�BM�BM�BR�BW
BW
BT�BP�BN�BO�BP�BN�BN�BN�BM�BL�BJ�BI�BJ�BL�BL�BL�BM�BJ�B@�BB�BD�BA�B@�B@�B<jB9XB6FB33B2-B2-B2-B33B33B33B49B5?B49B5?B6FB6FB7LB;dB>wBB�BD�BH�BI�BI�BI�BK�BK�BQ�BVB]/BdZBe`B^5B[#BbNBe`BiyBiyBr�Bp�BjBhsBk�Bl�Bk�BiyBjBjBk�Bm�Bp�Bu�By�Bz�B}�B� B�B�B�B�+B�=B�PB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�FB�FB�FB�LB�^B��BÖBȴB��B��B��B�B�B�B�B�)B�;B�TB�ZB�`B�ZB�mB�B�B�B��B��B��B��B	  B	bB	�B	�B	-B	9XB	C�B	G�B	M�B	P�B	P�B	Q�B	R�B	R�B	T�B	W
B	XB	XB	YB	ZB	[#B	]/B	`BB	bNB	cTB	bNB	bNB	ffB	iyB	iyB	l�B	m�B	o�B	q�B	t�B	t�B	u�B	v�B	y�B	z�B	{�B	}�B	� B	�B	� B	~�B	}�B	}�B	� B	�B	�JB	�DB	�B	�1B	�+B	�+B	�+B	�7B	�PB	�VB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�9B	�?B	�?B	�FB	�FB	�LB	�RB	�dB	�jB	�jB	�wB	�wB	�wB	�wB	�}B	�}B	�}B	�wB	��B	�}B	�}B	��B	��B	ĜB	ȴB	ǮB	ƨB	ƨB	ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bp�Bo�Bp�Bp�Bp�Bq�Bs�By�B�B�DB�B}�B~�B� B�B�B� B�B�7B�VB��BƨB��B�FB�^B�wB��BÖBĜBǮBɺB��B�B�B��B��B�
B�B�/B�/B�B�)B�5B�)B�#B�NB�TB�NB�#B�B��B��BǮB�qB�9B��B��B��B� Bt�B]/BT�BP�BJ�BE�BD�B>wB;dB�B
=B��B��B�B�B�B�`BB�?B��B��B�\B�uB�1B�B�7B~�BdZBF�B;dB-B!�BJB
��B
��B
�B
��B
�jB
�3B
��B
�VB
�JB
�B
}�B
n�B
R�B
D�B
J�B
P�B
@�B
G�B
S�B
\)B
YB
G�B
8RB
5?B
49B
6FB
.B	�B	��B	ŢB	B	�?B	��B	��B	��B	��B	��B	��B	�\B	�DB	v�B	|�B	n�B	W
B	L�B	G�B	W
B	\)B	G�B	1'B	+B	+B	-B	2-B	?}B	aHB	M�B	G�B	B�B	+B	"�B	
=B	B	\B	PB��B��B�sB�)B�B�B�B��B�yB�ZB��B�FB��B��B�-B�XB�B��B��B��B��B��B��B��B��B�oB�VB�VB�VB�JB�7B�+B�+B�B�B~�B~�B{�Bz�Bz�Bx�Bz�By�B�B�B�B�+B�PB�DB�1B�1B�+B�7B�%B~�B|�B� B�%B�+B�%B�+B�1B�%B� Bz�Bs�Bo�Bm�BdZBe`B[#BW
B[#BZBXBS�BQ�BM�BL�BL�BK�BL�BQ�BN�BR�BXB[#B\)BR�BP�BP�BR�BO�BP�BP�BO�BN�BL�BK�BM�BN�BN�BO�BP�BQ�BA�BD�BG�BB�BA�BC�B?}B;dB9XB5?B49B49B49B49B49B49B5?B7LB7LB:^B7LB7LB8RB;dB?}BC�BD�BI�BJ�BJ�BM�BQ�BK�BQ�BT�B]/Be`Bk�BbNB[#BbNBe`Bk�BjBt�Bs�Bn�BjBm�Bo�Bn�Bk�Bk�Bk�Bl�Bo�Bq�Bu�Bz�B{�B~�B�B�B�B�%B�1B�DB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�FB�LB�LB�FB�LB�jBBĜBɺB��B��B��B�B�
B�
B�B�)B�;B�TB�ZB�fB�`B�sB�B�B�B��B��B��B��B��B	\B	�B	�B	)�B	7LB	C�B	F�B	M�B	Q�B	Q�B	R�B	R�B	S�B	VB	W
B	XB	YB	ZB	[#B	\)B	]/B	aHB	cTB	dZB	cTB	bNB	gmB	iyB	jB	l�B	n�B	p�B	r�B	t�B	t�B	u�B	v�B	y�B	{�B	|�B	~�B	� B	�B	� B	� B	� B	~�B	� B	�B	�PB	�\B	�B	�7B	�1B	�+B	�+B	�7B	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�9B	�FB	�FB	�LB	�FB	�LB	�RB	�jB	�jB	�jB	�wB	�wB	�wB	�}B	�}B	�}B	��B	��B	B	�}B	�}B	��B	��B	ÖB	ɺB	ȴB	ƨB	ƨB	ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446392012010314463920120103144639  AO  ARGQ                                                                        20111130135553  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135553  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144639  IP                  G�O�G�O�G�O�                