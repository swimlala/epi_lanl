CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:04Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               1A   AO  20111130140201  20190522121826  1727_5046_049                   2C  D   APEX                            2143                            040306                          846 @�T���?�1   @�T���	@7&$�/��c�� ě�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33Bי�B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dx��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B733B?��BH  BP  BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B�  B�  B�ffBۙ�B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCF  CG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D�3Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�DA  DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]� D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dm  Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dx�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��A��yA��yA��yA��yA��`A��mA��yA��`A��`A��mA��yA��yA��HA��;A��;A��;A��;A��;A��;A��;A��;A��HA��;A��;A��;A��;A��;A��HA��TA��`A��mA��TA��TA��TA��TA��TA��TA��TA��`A��`A��TA��;A��;A��HA��HA��
A�ȴAA��A�-A�(�A��A���A��
A�A�{A�~�A���A��A�S�A��A�ȴA�33A��uA��A���A�ffA���A�;dA��FA��A�n�A�+A�%A�`BA��uA�
=A���A��A�jA�ZA�ZA�K�A��A���A��TA�oA��A�dZA��A�1'A�x�A���A��!A��A�A�=qA��;A��\A�&�A�p�A�C�A��!A�1'A��A�"�A�`BA�&�A�ȴA���A�~�A�ƨA�33A��A��\A��A�r�A�`BA�E�A~�/A}C�A|�9A{��Ay�Ax-Av�At�/As�AshsArz�Aq��Aq�7AqG�An��Al�AjJAil�AhI�AgVAfA�Ae+Ad9XAc`BAbr�AaS�A_C�A]�A\��A[�#AY�AX��AXQ�AV��AV  ATQ�AS��AR��AO�mAM�AL�yAKhsAJjAJ$�AJ1AI�AI��AI��AH �AE�AB�AB�AAdZA@�A@A?�-A>��A>^5A=�;A<��A;�A:�A9�A8M�A7hsA7�A69XA4��A4{A2�`A1�hA09XA/;dA.ĜA.ffA.1'A,ĜA* �A)�PA(jA'�#A'��A&��A&=qA%��A%oA#��A"��A!`BAO�A  At�AK�A�A��A^5A�AC�A �A/A��A�A�7A�AK�A�+AS�A�A�^A%A�A�A�;A�A��A-Ax�AXA
�!A	t�A��AĜA�DA5?A��A�A�yAVA%A��A ��@���@�?}@�bN@���@�5?@��@� �@��H@��#@��@�E�@땁@��@�p�@�I�@�n�@��@�1'@��H@��@�V@���@�v�@���@��
@�{@�Z@�C�@և+@պ^@Դ9@��;@�33@���@җ�@�^5@���@Гu@�S�@���@�5?@�X@̬@�ƨ@�/@�l�@�5?@�Ĝ@�I�@ÍP@�@�M�@�?}@� �@���@��@�1@��y@��@�9X@�  @��w@�dZ@�ff@��7@���@�A�@��@�{@���@�b@�^5@�J@��7@�V@�  @�\)@��@��H@�n�@�=q@�?}@��`@���@��@� �@��@�+@��@�n�@���@��7@�X@��@�  @�C�@���@�V@�V@��@���@��+@�^5@�hs@��/@�(�@��;@��@��
@��F@���@�l�@��!@�E�@�-@�J@���@���@�p�@��`@��j@�r�@�1@���@��P@�+@��H@��@�M�@��\@�~�@�ff@��#@�/@�bN@�
=@�ȴ@�n�@�^5@�5?@��@��T@���@�V@�Ĝ@���@�j@�Z@�9X@���@�|�@�\)@�C�@�"�@���@���@��@��\@��\@�V@�n�@�v�@�n�@�@���@��@�E�@�v�@�v�@�-@���@��7@��@�p�@�G�@�7L@��`@��D@�A�@���@��m@��
@��@�S�@�;d@�C�@�33@�t�@��@��\@�-@��@��#@��h@�p�@���@��D@�bN@��@�1@��m@��
@��@�;d@���@��H@���@��!@�v�@�M�@��@��#@��-@���@�O�@�/@�A�@�
=@�dZ@���@��@�K�@�V@���@�"�@�K�@���@�n�@���@��H@��H@�J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A��A��yA��yA��yA��yA��`A��mA��yA��`A��`A��mA��yA��yA��HA��;A��;A��;A��;A��;A��;A��;A��;A��HA��;A��;A��;A��;A��;A��HA��TA��`A��mA��TA��TA��TA��TA��TA��TA��TA��`A��`A��TA��;A��;A��HA��HA��
A�ȴAA��A�-A�(�A��A���A��
A�A�{A�~�A���A��A�S�A��A�ȴA�33A��uA��A���A�ffA���A�;dA��FA��A�n�A�+A�%A�`BA��uA�
=A���A��A�jA�ZA�ZA�K�A��A���A��TA�oA��A�dZA��A�1'A�x�A���A��!A��A�A�=qA��;A��\A�&�A�p�A�C�A��!A�1'A��A�"�A�`BA�&�A�ȴA���A�~�A�ƨA�33A��A��\A��A�r�A�`BA�E�A~�/A}C�A|�9A{��Ay�Ax-Av�At�/As�AshsArz�Aq��Aq�7AqG�An��Al�AjJAil�AhI�AgVAfA�Ae+Ad9XAc`BAbr�AaS�A_C�A]�A\��A[�#AY�AX��AXQ�AV��AV  ATQ�AS��AR��AO�mAM�AL�yAKhsAJjAJ$�AJ1AI�AI��AI��AH �AE�AB�AB�AAdZA@�A@A?�-A>��A>^5A=�;A<��A;�A:�A9�A8M�A7hsA7�A69XA4��A4{A2�`A1�hA09XA/;dA.ĜA.ffA.1'A,ĜA* �A)�PA(jA'�#A'��A&��A&=qA%��A%oA#��A"��A!`BAO�A  At�AK�A�A��A^5A�AC�A �A/A��A�A�7A�AK�A�+AS�A�A�^A%A�A�A�;A�A��A-Ax�AXA
�!A	t�A��AĜA�DA5?A��A�A�yAVA%A��A ��@���@�?}@�bN@���@�5?@��@� �@��H@��#@��@�E�@땁@��@�p�@�I�@�n�@��@�1'@��H@��@�V@���@�v�@���@��
@�{@�Z@�C�@և+@պ^@Դ9@��;@�33@���@җ�@�^5@���@Гu@�S�@���@�5?@�X@̬@�ƨ@�/@�l�@�5?@�Ĝ@�I�@ÍP@�@�M�@�?}@� �@���@��@�1@��y@��@�9X@�  @��w@�dZ@�ff@��7@���@�A�@��@�{@���@�b@�^5@�J@��7@�V@�  @�\)@��@��H@�n�@�=q@�?}@��`@���@��@� �@��@�+@��@�n�@���@��7@�X@��@�  @�C�@���@�V@�V@��@���@��+@�^5@�hs@��/@�(�@��;@��@��
@��F@���@�l�@��!@�E�@�-@�J@���@���@�p�@��`@��j@�r�@�1@���@��P@�+@��H@��@�M�@��\@�~�@�ff@��#@�/@�bN@�
=@�ȴ@�n�@�^5@�5?@��@��T@���@�V@�Ĝ@���@�j@�Z@�9X@���@�|�@�\)@�C�@�"�@���@���@��@��\@��\@�V@�n�@�v�@�n�@�@���@��@�E�@�v�@�v�@�-@���@��7@��@�p�@�G�@�7L@��`@��D@�A�@���@��m@��
@��@�S�@�;d@�C�@�33@�t�@��@��\@�-@��@��#@��h@�p�@���@��D@�bN@��@�1@��m@��
@��@�;d@���@��H@���@��!@�v�@�M�@��@��#@��-@���@�O�@�/@�A�@�
=@�dZ@���@��@�K�@�V@���@�"�@�K�@���@�n�@���@��H@��H@�J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B%�BF�Bn�B�+B�=B�{B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B{�BgmBZBQ�BI�BB�B49B1'B49B49B49B6FB)�B�BPB  B�B�)BÖB�!B��Bx�Bk�BaHBT�BI�BD�B>wB5?B�BDB
��B
�B
�mB
�HB
��B
ƨB
�}B
�B
��B
�uB
�=B
w�B
ffB
ffB
dZB
bNB
_;B
S�B
J�B
E�B
>wB
49B
+B
!�B
�B
hB
\B
DB
+B
B
  B	�B	�TB	�B	��B	��B	ƨB	��B	�dB	�LB	�3B	�B	��B	��B	��B	��B	�VB	�1B	�B	~�B	x�B	s�B	k�B	ffB	_;B	P�B	L�B	I�B	F�B	D�B	B�B	B�B	A�B	?}B	;dB	49B	'�B	�B	�B	{B	uB	hB	\B	PB	JB	
=B		7B	B��B��B�B�B�yB�ZB�NB�5B�B�B�B�B�B�B��B��BŢBB�}B�qB�jB�XB�LB�?B�'B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�JB�=B�1B�B�B~�B|�By�Bw�Bt�Br�Bo�Bl�BjBiyBgmBgmBffBcTBaHB`BB_;B^5B\)BYBT�BQ�BM�BI�BG�BE�BD�BD�BC�BA�B>wB<jB:^B9XB8RB5?B2-B1'B0!B/B.B.B.B-B-B-B,B+B+B,B,B,B-B.B/B/B/B0!B0!B1'B1'B0!B1'B1'B1'B1'B1'B1'B0!B/B0!B2-B2-B33B33B49B6FB5?B6FB7LB9XB:^B<jB>wBB�BD�BE�BE�BE�BF�BG�BH�BI�BM�BM�BQ�BR�BW
BXBYBZB]/B_;B`BB`BBbNBbNBe`BgmBgmBgmBiyBk�Bn�Bn�Bp�Br�Bu�Bu�Bx�B|�B�B�=B�DB�JB�bB��B��B��B��B��B��B�B�!B�'B�'B�3B�FB�}BƨBǮBɺB��B��B��B�#B�/B�HB�fB�B�B��B��B��B	B		7B	DB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	$�B	%�B	'�B	)�B	+B	-B	2-B	6FB	7LB	9XB	:^B	<jB	@�B	B�B	D�B	E�B	I�B	N�B	P�B	YB	]/B	_;B	cTB	jB	m�B	n�B	p�B	u�B	x�B	z�B	{�B	}�B	� B	�B	�B	�+B	�7B	�=B	�=B	�DB	�JB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�jB	�XB	�LB	�dB	�jB	�jB	�jB	�dB	�}B	��B	ÖB	ŢB	ƨB	ǮB	ɺB	ɺB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B'�BK�Bt�B�1B�VB��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B�Bl�B]/BVBK�BI�B;dB49B49B49B5?G�O�B0!B�BoB%B�B�TBȴB�^B��B}�Bo�BdZBXBJ�BE�B?}B;dB �BhB
��B
�B
�yB
�ZB
�#B
ȴB
ŢB
�?B
��B
��B
�hB
~�B
ffB
ffB
e`B
cTB
dZB
XB
L�B
H�B
C�B
9XB
/B
'�B
�B
uB
oB
PB
1B
%B
1B	��B	�B	�B	�
B	��B	ɺB	ĜB	�wB	�^B	�FB	�'B	�B	��B	��B	��B	�uB	�=B	�B	�B	z�B	w�B	m�B	iyB	ffB	VB	O�B	M�B	I�B	E�B	B�B	B�B	B�B	@�B	@�B	;dB	0!B	�B	�B	�B	�B	oB	hB	\B	VB	PB	\B	B��B��B�B�B�B�sB�ZB�NB�5B�5B�)B�B�B�B�B��BǮBƨB��B�wB�wB�dB�XB�LB�?B�B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�DB�DB�1B�B�B~�B}�By�Bw�Bu�Bs�Bo�Bk�Bk�BiyBhsBhsBgmBcTBaHB`BB_;B^5B_;BYBS�BQ�BN�BK�BJ�BF�BF�BE�BD�BD�BA�B=qB;dB:^B:^B7LB33B2-B1'B2-B1'B0!B0!B/B/B.B.B.B.B0!B/B/B/B0!B1'B0!B1'B1'B1'B2-B1'B33B33B2-B2-B33B2-B2-B/B33B49B49B49B49B6FB7LB7LB8RB7LB9XB=qB>wBB�BC�BE�BF�BF�BG�BH�BI�BI�BL�BO�BO�BS�BVBXBYBZB\)B^5B`BBaHBaHBcTBdZBffBgmBhsBhsBjBl�Bo�Bo�Bq�Bs�Bu�Bv�By�B}�B�B�DB�PB�VB�hB��B��B��B��B��B�B�B�!B�'B�'B�3B�LB��BƨBǮB��B��B��B��B�#B�5B�NB�mB�B�B��B��B��B	B		7B	DB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	%�B	%�B	'�B	)�B	+B	.B	33B	6FB	7LB	9XB	:^B	<jB	@�B	C�B	D�B	E�B	I�B	N�B	P�B	ZB	]/B	_;B	cTB	jB	m�B	o�B	q�B	u�B	x�B	z�B	{�B	}�B	�B	�B	�%B	�1B	�7B	�=B	�=B	�JB	�JB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�?B	�LB	�RB	�XB	�^B	�jB	�jB	�wB	�dB	�FB	�dB	�jB	�qB	�wB	�^B	�wB	��B	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446512012010314465120120103144651  AO  ARGQ                                                                        20111130140201  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140201  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144651  IP                  G�O�G�O�G�O�                