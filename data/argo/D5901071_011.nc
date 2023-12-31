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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135530  20190522121825  1727_5046_011                   2C  D   APEX                            2143                            040306                          846 @�%|�? 1   @�%}���@8��vȴ�cԼj~��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7��C9�fC;�fC=�fC?�fCA�fCD  CF  CG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C�  C��3C��3C��3C��3C�  C��3C��3C��3C��fC��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"� D"��D#� D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8s3D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=�3D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DG� DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcs3Dc�3Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djs3Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn�3Dos3Do��Dpy�Dp��Dqy�Dq��Dry�Dr��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�p�A�(�A���Aʇ+A�
=A���A�+A���AȮA�|�A�/A��AǼjA� �A�;dA�v�A���A�I�A�|�A�jA��A�A��A�bNA��mA�\)A��A�bNA��A�ffA��+A�^5A��`A�x�A�7LA�1A���A���A�\)A��A��hA��A�v�A��A��!A���A�n�A�1'A��A�(�A��A��PA��A���A���A���A� �A�hsA�~�A��mA��FA��A��A��A�
=A��jA��/A��A���A��-A�A�/A��+A��+A�1'A�S�A��A�+A��DA��A��A�5?A��A�;dA�C�A�S�A�&�A��A��A�G�A��A��FA�z�A�A�\)A���A��PA�/A�ȴA�I�A��FA�XA�C�A�dZA�G�A��/A�v�A��A�M�A���A���A~�HA~�A}�A}ƨA}hsA|��Ay��AwK�Av��Au|�As�mAr��An9XAm&�Aj  AiK�Ah=qAdĜAbE�A_�
A^^5A^1A]l�A\�HA[S�AY�TAW�-AU�TAT�ASS�ARjAQ�AP�AO?}AN(�AM|�AMO�AM7LAM"�AL�HALQ�AJ�9AH�!AG��AGAFr�AE��AEoAA�A@�A?��A?K�A>�RA=�hA<A�A:�\A8�jA8{A6�A5|�A3hsA21A1��A1/A0I�A/�TA/dZA-�
A-�A+%A*5?A)��A(A�A'�A&�+A%t�A$A�A#�PA"�/A"A!�-A!K�A Q�AA�yAz�AA33A�9A�A`BA
=A�/A  A��AI�A�^AXA+A��AM�A33A��A9XAO�AZA�A�HA�+A��A��A��A�wA33A
�DAr�A��AdZAJA��A��Ap�A?}AffAƨA�7AXAVAȴA9XAO�A Ĝ@��@���@���@��@��w@���@��-@���@��
@�33@�ȴ@�v�@���@�X@�l�@�V@�&�@���@�r�@�ƨ@��@��@���@�`B@�  @��@� �@�(�@��@�@�r�@�Q�@��;@�?}@��`@�(�@�"�@�=q@١�@���@�r�@�v�@�$�@թ�@Ӿw@�
=@��@�`B@У�@�|�@��y@ΰ!@�5?@Ͳ-@���@�ƨ@ʟ�@���@�/@�bN@�@�
=@���@��T@�b@��H@��h@�r�@��
@��@�x�@���@��P@���@��@�bN@�I�@���@�V@�K�@�bN@�+@��!@��+@���@��@�A�@��@��P@���@�&�@���@�"�@�t�@��@�X@�@�hs@�S�@�;d@���@�%@���@���@��@���@��@���@���@�p�@�Ĝ@�  @���@���@�K�@���@��@��\@�v�@���@��@���@��-@�@���@�&�@� �@��!@�Ĝ@���@�Q�@�bN@��D@��/@�1@���@���@�|�@�;d@���@�J@�@�x�@�&�@��@��`@��/@��@���@�\)@�"�@���@�E�@�J@���@�?}@�/@��7@��@�p�@�O�@�7L@�&�@���@��j@�z�@�b@��;@���@���@��@�G�@�I�@��+@��!@��@��H@�ȴ@���@��y@���@��@�ȴ@�5?@��#@���@��h@���@���@�x�@�&�@��@���@�I�@��F@�1@�1@�dZ@�J@�x�@�&�@���@�9X@�dZ@�v�@�5?@�J@��T@�O�@���@���@�z�@�Q�@� �@�+@���@�v�@�{@�@�E�@�ff@�$�@���@���@��@�C�@�33@�33@�33@�+@�+@�+@��@�
=@��@��\@�~�@�M�@�=q@�J@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�p�A�(�A���Aʇ+A�
=A���A�+A���AȮA�|�A�/A��AǼjA� �A�;dA�v�A���A�I�A�|�A�jA��A�A��A�bNA��mA�\)A��A�bNA��A�ffA��+A�^5A��`A�x�A�7LA�1A���A���A�\)A��A��hA��A�v�A��A��!A���A�n�A�1'A��A�(�A��A��PA��A���A���A���A� �A�hsA�~�A��mA��FA��A��A��A�
=A��jA��/A��A���A��-A�A�/A��+A��+A�1'A�S�A��A�+A��DA��A��A�5?A��A�;dA�C�A�S�A�&�A��A��A�G�A��A��FA�z�A�A�\)A���A��PA�/A�ȴA�I�A��FA�XA�C�A�dZA�G�A��/A�v�A��A�M�A���A���A~�HA~�A}�A}ƨA}hsA|��Ay��AwK�Av��Au|�As�mAr��An9XAm&�Aj  AiK�Ah=qAdĜAbE�A_�
A^^5A^1A]l�A\�HA[S�AY�TAW�-AU�TAT�ASS�ARjAQ�AP�AO?}AN(�AM|�AMO�AM7LAM"�AL�HALQ�AJ�9AH�!AG��AGAFr�AE��AEoAA�A@�A?��A?K�A>�RA=�hA<A�A:�\A8�jA8{A6�A5|�A3hsA21A1��A1/A0I�A/�TA/dZA-�
A-�A+%A*5?A)��A(A�A'�A&�+A%t�A$A�A#�PA"�/A"A!�-A!K�A Q�AA�yAz�AA33A�9A�A`BA
=A�/A  A��AI�A�^AXA+A��AM�A33A��A9XAO�AZA�A�HA�+A��A��A��A�wA33A
�DAr�A��AdZAJA��A��Ap�A?}AffAƨA�7AXAVAȴA9XAO�A Ĝ@��@���@���@��@��w@���@��-@���@��
@�33@�ȴ@�v�@���@�X@�l�@�V@�&�@���@�r�@�ƨ@��@��@���@�`B@�  @��@� �@�(�@��@�@�r�@�Q�@��;@�?}@��`@�(�@�"�@�=q@١�@���@�r�@�v�@�$�@թ�@Ӿw@�
=@��@�`B@У�@�|�@��y@ΰ!@�5?@Ͳ-@���@�ƨ@ʟ�@���@�/@�bN@�@�
=@���@��T@�b@��H@��h@�r�@��
@��@�x�@���@��P@���@��@�bN@�I�@���@�V@�K�@�bN@�+@��!@��+@���@��@�A�@��@��P@���@�&�@���@�"�@�t�@��@�X@�@�hs@�S�@�;d@���@�%@���@���@��@���@��@���@���@�p�@�Ĝ@�  @���@���@�K�@���@��@��\@�v�@���@��@���@��-@�@���@�&�@� �@��!@�Ĝ@���@�Q�@�bN@��D@��/@�1@���@���@�|�@�;d@���@�J@�@�x�@�&�@��@��`@��/@��@���@�\)@�"�@���@�E�@�J@���@�?}@�/@��7@��@�p�@�O�@�7L@�&�@���@��j@�z�@�b@��;@���@���@��@�G�@�I�@��+@��!@��@��H@�ȴ@���@��y@���@��@�ȴ@�5?@��#@���@��h@���@���@�x�@�&�@��@���@�I�@��F@�1@�1@�dZ@�J@�x�@�&�@���@�9X@�dZ@�v�@�5?@�J@��T@�O�@���@���@�z�@�Q�@� �@�+@���@�v�@�{@�@�E�@�ff@�$�@���@���@��@�C�@�33@�33@�33@�+@�+@�+@��@�
=@��@��\@�~�@�M�@�=q@�J@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B��B'�B-BM�BYB`BBgmBq�Bt�Bq�Bk�B_;B2-BhB\B�BF�B�1B��B�B��B��B�B�'B�B�B��B��B��B��B�B�dBBǮB��B��B��B��B��BǮB��B��B�#B�HB�yB�mB�NB�TB��BBPB�B�B�B7LBC�B8RB�B{B�BhB	7B��B�B�`B�B��B�3B��B��B�B^5B49BoB%B�NBɺB�?B��B��B��B�+Bz�Bz�By�Bu�BiyB_;B_;BS�BS�BJ�BB�B=qB=qB;dB:^B&�BB
�mB
��B
��B
�B
p�B
dZB
M�B
E�B
<jB
,B
.B
/B
/B
+B
"�B
PB	��B	��B	�B	�`B	�B	B	�LB	��B	��B	�{B	|�B	k�B	`BB	[#B	[#B	XB	W
B	R�B	F�B	<jB	49B	-B	)�B	$�B	�B	�B	�B	uB	hB	bB	\B	VB	JB		7B	B	B��B��B��B�B�B�ZB�;B�/B�B�B��B��B��B�wB�dB�LB�9B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�\B�VB�PB�JB�=B�=B�7B�1B�+B�B�B�B�B�B�B� B}�B{�By�Bx�Bw�Bw�Bu�Bs�Bp�Bo�Bm�Bl�Bk�Bl�Bk�Bk�BjBgmBffBe`BcTB`BB\)B[#BYBXBXBW
BVBS�BQ�BP�BO�BN�BM�BL�BI�BG�BE�BC�BB�BA�BC�BB�BA�B?}B?}BC�BM�BT�BW
BT�BQ�BO�BP�BR�BVB[#B\)B\)B`BBiyBgmBdZBcTBcTBdZBaHB_;B\)BXBYBYBW
BVBT�BS�BQ�BQ�BP�BR�BR�BQ�BS�BS�BVBW
BW
BZB_;BaHBe`BiyBk�Bn�Bq�Br�Bt�Bx�B�B�B�+B�B�B�B�B�B�1B�\B��B��B��B��B��B��B�B�FB�jB�?B�3B�3B�9B�XBŢB��B��B��B��B�B�)B�/B�TB�`B�ZB�ZB�B�B�mB�mB�fB�NB�NB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	PB	oB	{B	�B	�B	�B	�B	�B	"�B	&�B	)�B	+B	.B	0!B	1'B	5?B	<jB	<jB	>wB	@�B	@�B	@�B	A�B	D�B	G�B	H�B	I�B	I�B	I�B	I�B	I�B	H�B	K�B	P�B	R�B	W
B	`BB	dZB	dZB	dZB	e`B	ffB	iyB	iyB	k�B	l�B	l�B	k�B	k�B	k�B	l�B	jB	jB	l�B	m�B	n�B	p�B	v�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�+B	�+B	�+B	�+B	�1B	�1B	�1B	�7B	�VB	�bB	�\B	�hB	�hB	�oB	�uB	�uB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�FB	�LB	�LB	�LB	�LB	�XB	�RB	�XB	�RB	�RB	�XB	�XB	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�jB	�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B  B(�B0!BO�BYBaHBhsBr�Bu�Bs�Bo�Bk�BA�B{BuB�BG�B�1B�B�B��B��B�!B�3B�B�B��B��B��B��B�B�jBÖBȴB��B��B��B��B��BɺB��B��B�)B�NB�B�B�ZB�ZB��BBPB�B�B�B7LBE�B<jB �B�B�B�B\B��B�B�yB�)BƨB�FB��B��B�1BffB;dB�BVB�yB��B�XB�B��B��B�JB{�Bz�Bz�By�Bp�BffBffBW
BXBL�BD�B?}B>wB=qB@�B,B%B
�B
ɺB
��B
�DB
{�B
hsB
O�B
G�B
B�B
.B
.B
0!B
0!B
-B
+B
uB
B	��B	��B	�sB	�ZB	ƨB	��B	��B	��B	��B	�B	r�B	dZB	\)B	]/B	ZB	\)B	XB	M�B	B�B	:^B	0!B	-B	(�B	!�B	�B	�B	�B	oB	hB	bB	\B	\B	\B	
=B	B��B��B��B��B��B�mB�BB�5B�)B�#B�B��BƨB��B�wB�dB�^B�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�VB�VB�PB�JB�JB�=B�7B�1B�+B�+B�B�B�B�B�B~�B{�Bz�Bx�Bx�Bw�Bv�Br�Bp�Bp�Bo�Bm�Bn�Bl�Bm�Bl�Bm�BiyBgmBe`BffB_;B\)B]/BYBYBXBW
BW
BS�BQ�BP�BO�BN�BN�BL�BI�BH�BE�BD�BC�BE�BD�BC�B@�BA�BD�BN�BVBXBVBT�BQ�BR�BS�BW
B\)B]/B^5BaHBjBjBjBdZBcTBffBcTBaHBaHBYB]/BZBXBXBW
BT�BR�BR�BS�BS�BS�BT�BT�BVBW
BXBYB[#B`BBbNBffBjBm�Bp�Br�Bs�Bu�Bz�B�B�B�7B�1B�B�B�B�B�1B�JB��B��B��B��B��B��B�B�FB�}B�?B�?B�3B�9B�LBĜB��B��B��B��B�B�;B�/B�TB�fB�fB�TB�B�B�mB�sB�sB�TB�NB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	PB	oB	{B	�B	�B	�B	�B	�B	#�B	&�B	)�B	+B	0!B	0!B	1'B	5?B	=qB	=qB	?}B	A�B	A�B	A�B	A�B	E�B	G�B	I�B	J�B	J�B	J�B	J�B	J�B	I�B	K�B	Q�B	R�B	W
B	`BB	dZB	dZB	dZB	e`B	gmB	iyB	jB	l�B	l�B	m�B	l�B	m�B	l�B	n�B	m�B	jB	l�B	m�B	n�B	p�B	v�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�+B	�+B	�1B	�1B	�7B	�7B	�7B	�7B	�VB	�hB	�hB	�oB	�oB	�uB	�{B	��B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�LB	�RB	�XB	�LB	�RB	�XB	�RB	�XB	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�qB	�qB	�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446372012010314463720120103144637  AO  ARGQ                                                                        20111130135530  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135530  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144637  IP                  G�O�G�O�G�O�                