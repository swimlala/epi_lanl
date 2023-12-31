CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:54Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135539  20190522121825  1727_5046_013                   2C  D   APEX                            2143                            040306                          846 @�(ߒ�1   @�(�m��@7�XbM��c���-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A���A�33A�33A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @&ff@y��@���@���AffA>ffA^ffA~ffA�33A�  A�ffA�ffA�33A�ffA�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC��C�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC6  C8  C9�fC;�fC=��C?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC�3DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcs3Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dl  Dly�Dl��Dmy�Dn  Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dr� Dr��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A��A���A��A��A��mA��;A��/A��/A��;A��HA��`AʾwA�%A�\)A�t�A���A�z�A�M�A�|�A�`BA�A�(�A�A� �A�hsA���A�ƨA�ffA��A���A�Q�A��#A��\A�M�A�1A��A�
=A�dZA���A�t�A�+A�G�A���A��A�r�A�&�A��FA�A�A�A�A�n�A��A��A��#A�oA�t�A���A��mA���A���A��A�S�A��A�A��A�&�A�r�A��
A�z�A���A�l�A��A�A�-A�1'A���A��DA�  A��A���A��A���A���A�%A�^5A�
=A��yA��7A�1A��;A�&�A���A��`A��HA�I�A���A��A��FA���A�bA��mA� �A��A��jA�A�A�z�A���A���A��DA�?}A�x�A��PA~A�A|�A|E�Az�Ax��AxAv�At�As�ArA�AqAo|�Al�uAk�TAkXAi�wAg|�AfJAe�Ad=qAc
=AbbAahsA`9XA^jA]�wA[�AZ  AXQ�AWdZAU��ATz�ASoAR�!AOXAN{ALv�AI"�AGƨAF��AC�A@��A@�A@r�A?oA=�A:��A9K�A8��A8I�A7"�A6�HA6��A6E�A5��A4��A49XA3�A3��A3"�A2v�A1�A1�PA1%A0A.�A,1A*M�A)ƨA)p�A(��A({A&��A%S�A$jA#A#�A#A"~�A!��A!�hA!33A �A bAC�AȴA�Al�A^5A��AO�A��AAA�hA�yA�uA�A+A��AƨA;dA�PA�/A�wA�jAA�A;dAv�A��At�A�;A
I�A	XA�A��A��A5?A�A;dA�A�h@��@�@�7L@��j@���@�~�@�p�@�A�@���@�o@�E�@��h@���@��m@�+@�@��H@��@ꗍ@�Q�@�5?@�b@���@�/@ߕ�@ݩ�@� �@�v�@���@�A�@�M�@���@��@���@���@��`@��#@Ѳ-@Гu@��H@�hs@ͺ^@�@�@�O�@�K�@�;d@�@�;d@�hs@�~�@ӝ�@Ӿw@҇+@�{@�1@ϥ�@�t�@�l�@�33@�{@��@��@��@��y@�ȴ@Ο�@ΰ!@Η�@���@�hs@�1@�o@���@���@ļj@�+@���@��/@��@�r�@���@��T@���@��@���@���@�&�@��@�Q�@��@��;@��@�33@���@���@���@���@�M�@�J@��@�1'@��
@���@�t�@�;d@�
=@���@��y@�n�@�O�@��@��D@��@� �@��@�t�@��\@���@�C�@�V@��^@�V@�9X@��F@�ȴ@�ff@�{@��@���@��`@�Z@���@�  @��@���@��@�"�@�n�@�`B@��`@��u@�Z@�1'@� �@��@���@�|�@�t�@�S�@�K�@��@�v�@�@���@���@���@��j@��@�1@��@���@�dZ@�\)@��@��^@��@�p�@��@�O�@��/@��u@��@��;@��@��@���@��!@�$�@�7L@�Ĝ@��u@��@�I�@�I�@�K�@�+@���@�~�@�E�@���@���@�p�@�?}@�%@��`@���@�x�@�1@�dZ@��y@��@��/@��R@��+@��j@�j@��@�;d@�dZ@�33@��@���@�n�@��@��-@�7L@��@�z�@�dZ@�l�@�S�@�K�@�
=@�o@���@��\@���@���@��@��7@�O�@�7L@��`@�Ĝ@��D@� �@�j@��@�r�@�bN@�Z@�Q�@� �@���@�dZ@��@���@���@���@���@�n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��A���A��A��A��mA��;A��/A��/A��;A��HA��`AʾwA�%A�\)A�t�A���A�z�A�M�A�|�A�`BA�A�(�A�A� �A�hsA���A�ƨA�ffA��A���A�Q�A��#A��\A�M�A�1A��A�
=A�dZA���A�t�A�+A�G�A���A��A�r�A�&�A��FA�A�A�A�A�n�A��A��A��#A�oA�t�A���A��mA���A���A��A�S�A��A�A��A�&�A�r�A��
A�z�A���A�l�A��A�A�-A�1'A���A��DA�  A��A���A��A���A���A�%A�^5A�
=A��yA��7A�1A��;A�&�A���A��`A��HA�I�A���A��A��FA���A�bA��mA� �A��A��jA�A�A�z�A���A���A��DA�?}A�x�A��PA~A�A|�A|E�Az�Ax��AxAv�At�As�ArA�AqAo|�Al�uAk�TAkXAi�wAg|�AfJAe�Ad=qAc
=AbbAahsA`9XA^jA]�wA[�AZ  AXQ�AWdZAU��ATz�ASoAR�!AOXAN{ALv�AI"�AGƨAF��AC�A@��A@�A@r�A?oA=�A:��A9K�A8��A8I�A7"�A6�HA6��A6E�A5��A4��A49XA3�A3��A3"�A2v�A1�A1�PA1%A0A.�A,1A*M�A)ƨA)p�A(��A({A&��A%S�A$jA#A#�A#A"~�A!��A!�hA!33A �A bAC�AȴA�Al�A^5A��AO�A��AAA�hA�yA�uA�A+A��AƨA;dA�PA�/A�wA�jAA�A;dAv�A��At�A�;A
I�A	XA�A��A��A5?A�A;dA�A�h@��@�@�7L@��j@���@�~�@�p�@�A�@���@�o@�E�@��h@���@��m@�+@�@��H@��@ꗍ@�Q�@�5?@�b@���@�/@ߕ�@ݩ�@� �@�v�@���@�A�@�M�@���@��@���@���@��`@��#@Ѳ-@Гu@��H@�hs@ͺ^@�@�@�O�@�K�@�;d@�@�;d@�hs@�~�@ӝ�@Ӿw@҇+@�{@�1@ϥ�@�t�@�l�@�33@�{@��@��@��@��y@�ȴ@Ο�@ΰ!@Η�@���@�hs@�1@�o@���@���@ļj@�+@���@��/@��@�r�@���@��T@���@��@���@���@�&�@��@�Q�@��@��;@��@�33@���@���@���@���@�M�@�J@��@�1'@��
@���@�t�@�;d@�
=@���@��y@�n�@�O�@��@��D@��@� �@��@�t�@��\@���@�C�@�V@��^@�V@�9X@��F@�ȴ@�ff@�{@��@���@��`@�Z@���@�  @��@���@��@�"�@�n�@�`B@��`@��u@�Z@�1'@� �@��@���@�|�@�t�@�S�@�K�@��@�v�@�@���@���@���@��j@��@�1@��@���@�dZ@�\)@��@��^@��@�p�@��@�O�@��/@��u@��@��;@��@��@���@��!@�$�@�7L@�Ĝ@��u@��@�I�@�I�@�K�@�+@���@�~�@�E�@���@���@�p�@�?}@�%@��`@���@�x�@�1@�dZ@��y@��@��/@��R@��+@��j@�j@��@�;d@�dZ@�33@��@���@�n�@��@��-@�7L@��@�z�@�dZ@�l�@�S�@�K�@�
=@�o@���@��\@���@���@��@��7@�O�@�7L@��`@�Ĝ@��D@� �@�j@��@�r�@�bN@�Z@�Q�@� �@���@�dZ@��@���@���@���@���@�n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBVBVBVBPBPBVBVBVBVBVBPB�B;dBYBZBJ�BjB�\B��B�?B��B��B��B��B�qB�B��B��B��B��B�B�B�B�B��B�yB�B�sB�ZB�sB�fB�HB�ZB�;B�B�B��B�B��B��B�)B�HB�TB��B��BŢB�B�TB�5B�fB�TB��B��BȴB�dB�B��B��B��B��B�!B��B~�BffBJ�B5?B&�B�B+B�B�yB�
B�dB��B�Bs�BcTB]/BYBN�BI�B8RB,B"�BoB1BBB
��B
�B
�;B
��B
��B
ȴB
�}B
�^B
�?B
��B
��B
��B
�B
m�B
jB
m�B
e`B
ZB
R�B
F�B
33B
%�B
�B
\B
B	�B	��B	��B	�B	�HB	�
B	��B	ȴB	��B	�qB	�jB	�B	��B	�oB	|�B	t�B	q�B	jB	ZB	O�B	C�B	=qB	#�B	uB	B�B�mB�mB�B��B��B��BŢB�RB�!B��B�'B�FBB��BɺBɺB��BɺBȴBƨBŢBÖB��B�wB�jB�RB�'B��B��B��B��B��B�{B�oB�\B�VB�VB�VB�VB�VB�JB�JB�DB�=B�7B�+B�%B�B�B�B�B�B�B� B~�B~�B}�B}�B{�Bz�Bz�By�Bw�Bt�Bq�Bo�Bm�Bk�BiyBgmBe`BdZBaHB^5B[#BYBW
BR�BN�BL�BK�BJ�BH�BE�BB�BA�B@�B?}B=qB<jB;dB:^B9XB8RB8RB7LB6FB5?B33B0!B,B+B'�B&�B$�B$�B"�B!�B �B�B �B�B�B�B�B�B!�B'�B0!B8RB6FB<jB8RB6FB=qBG�BL�BVB~�B�\B�hB�uB��B��B��B�B�B�!B�B��B��B��B��B��B�B�?B�qB�}BBǮBɺB��B��B��B��B��B��B��BƨB�}B�wB��B��BɺB��BȴB��B�3B�B�B�!B�'B�'B�-B�3B�9B�?B�LB�jB�wB�}B��BÖBBǮB��B��B��B��B�B�#B�5B�;B�BB�BB�`B�B�B�B�B�B��B��B	B	B	%B	1B	PB	\B	\B	hB	uB	�B	�B	{B	�B	�B	�B	 �B	 �B	$�B	$�B	#�B	"�B	!�B	!�B	)�B	+B	.B	/B	33B	33B	49B	49B	49B	7LB	<jB	>wB	@�B	B�B	C�B	C�B	C�B	G�B	J�B	I�B	J�B	M�B	R�B	Q�B	R�B	S�B	T�B	T�B	T�B	T�B	VB	XB	XB	XB	[#B	]/B	`BB	bNB	dZB	dZB	e`B	hsB	iyB	hsB	l�B	l�B	m�B	n�B	p�B	s�B	v�B	z�B	� B	�B	�B	�DB	�1B	�7B	�7B	�1B	�=B	��B	��B	�bB	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	�{B	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�?B	�LB	�RB	�RB	�RB	�LB	�LB	�XB	�dB	�jB	�qB	�wB	�}B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBVBVBVBPBPBVBVBVBVBVB\B�B?}B\)B\)B[#Bo�B�VB�B�jB��B��B��B��B�^B�B��B��B��B��B�B�B�B�B��B�B�B�B�`B�yB�B�ZB�fB�NB�#B�B�
B�#B�B�B�5B�TB�mB�B��BŢB�#B�sB�HB�yB�sB��B��B��B�wB�B��B��B��B��B�RB��B�1Bm�BO�B8RB(�B�B\B�B�B�NBĜB�B�7B{�BffB^5B]/BP�BP�B>wB1'B-B�B	7B%B%B  B
�B
�`B
��B
��B
��B
��B
�jB
�^B
�!B
��B
��B
�7B
q�B
l�B
q�B
jB
]/B
VB
L�B
7LB
)�B
�B
{B
JB	��B	��B	��B	��B	�`B	�B	��B	��B	ÖB	�}B	��B	�-B	��B	��B	�B	z�B	u�B	p�B	^5B	T�B	E�B	F�B	&�B	�B	JB�B�B�B�#B��B��B��B��B�qB�9B��B�3B�XBÖB��B��B��B��B��BɺBǮBǮBŢBB��B�wB�jB�RB�B��B��B��B��B��B��B��B�hB�bB�\B�bB�bB�VB�VB�PB�PB�DB�=B�1B�1B�%B�B�B�B�B�B� B� B�B� B~�B}�B}�B|�Bz�Bz�Bt�Bt�Bq�Bm�Bm�BjBhsBffBffBbNB^5B[#BZBXBR�BN�BL�BK�BL�BJ�BE�BB�BA�BA�B?}B>wB=qB;dB:^B9XB9XB8RB8RB6FB5?B5?B1'B.B,B+B(�B&�B%�B$�B#�B"�B#�B"�B!�B!�B �B �B"�B&�B0!B<jB6FB>wB:^B8RB=qBG�BL�BQ�B|�B�\B�hB�uB��B��B��B�B�!B�'B�'B�B��B��B��B��B�B�9B�qB�}BBǮBɺB��B��B��B��B��B��B��B��BB�}BĜB��B��B��B��B��B�FB�B�B�'B�-B�-B�3B�9B�?B�FB�RB�jB�wB�}BBĜBĜBɺB��B��B��B��B�B�#B�5B�BB�NB�HB�`B�B�B�B��B��B��B��B	B	%B	+B	
=B	VB	hB	bB	oB	uB	�B	�B	�B	�B	�B	�B	 �B	 �B	&�B	&�B	%�B	#�B	"�B	"�B	)�B	+B	.B	0!B	33B	33B	49B	49B	5?B	8RB	=qB	>wB	A�B	C�B	C�B	C�B	D�B	G�B	K�B	J�B	J�B	N�B	T�B	Q�B	R�B	S�B	T�B	VB	VB	VB	VB	XB	YB	YB	[#B	^5B	aHB	cTB	dZB	e`B	e`B	hsB	k�B	hsB	m�B	m�B	m�B	o�B	q�B	s�B	v�B	z�B	� B	�B	�B	�PB	�7B	�=B	�DB	�=B	�+B	��B	��B	�hB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�?B	�LB	�RB	�RB	�RB	�RB	�RB	�^B	�jB	�jB	�qB	�wB	�}B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446382012010314463820120103144638  AO  ARGQ                                                                        20111130135539  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135539  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144638  IP                  G�O�G�O�G�O�                