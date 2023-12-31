CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-22T17:03:09Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  AH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20170922170309  20181025093510  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�(.�(�1   @�(�lV@9W
=p���c?�
=p�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|(�@��H@��HAp�A=p�A_
>A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_Bg\)Bo\)Bw\)B\)B��B��B�z�B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C!�
C#�
C%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Do]D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#|)D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DS|)DS��DTu�DT��DUu�DU��DVu�DV��DW|)DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz�)D{H�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�A���A�A�A���A��Aإ�A��A��A֝�A֙�A֙�A֋DAցA�p�A�p�A�p�A�p�A�n�A�jA�l�A�dZA��A�ZA�?}Aҝ�AѓuA�+A·+A�x�A�C�A�A�A�(�A��+A��HA��A�5?A�A��A�oA��A���A�=qA�bNA���A��A��A�I�A�ĜA��PA��wA���A��FA�A��A���A�/A���A���A�9XA���A�n�A�(�A�bNA�JA�|�A��/A��mA�+A���A��TA�JA���A�  A��A�Q�A�"�A��A�dZA���A�  A��A�p�A�9XA�Q�A�"�A�ffA��+A�%A~A|��A{�wAy�wAx9XAw+Av5?Au�As�As+Ap1'AmVAj�/Ah�yAg&�Ae��AcC�Aa\)A`��A_K�A]�mA]dZA]�FA]�#A^JA]�A\ȴA[?}AZ�/AX��AW��AV�DAV �AT�HATAR�AP  AN5?AM+AK��AJ�RAI��AG��AF�HAF(�AE�AD�jAC�wAC�#AB��AA�ABffAB5?AA�A?A>�A=;dA<9XA;�A:ĜA9A9dZA9/A8��A8�A7hsA6��A6bNA5�A5A4�+A4  A3�A333A2��A1��A1?}A0�A0�A/+A.�/A.�9A.z�A.A�A-��A-�
A-�hA,��A,n�A+��A*�A*�!A*�uA*r�A*bA)A)\)A(�A($�A'�^A&VA%"�A$~�A#�A#�FA#x�A"ĜA"JA ��AoA�AG�A�uA�-AhsA�AjA�A�HA�A�wA�/Av�A��A
=A$�AJA��A�9A;dA�FA�/AA�A��A�uA �A/A
v�A	C�A^5A�
A\)A��A�TA�A-A��A I�@���@���@��j@�\)@�Q�@�@�+@���@�@��#@��H@�E�@�p�@�j@�ƨ@���@�-@�Z@���@��T@�O�@��;@ڸR@�V@�@׾w@�ff@�V@�1@�|�@Ұ!@�@�dZ@�7L@̃@�l�@�@�?}@�Ĝ@�K�@�=q@�&�@��@�X@��@���@��/@�Z@�ƨ@�C�@��@��9@�ƨ@��@�~�@�{@��@��9@� �@��y@�O�@���@�I�@���@�p�@��@���@�r�@�1'@��@�|�@�;d@���@�@�O�@�Ĝ@���@��\@�{@���@��7@�%@��
@���@���@�E�@�X@�bN@��;@�;d@��!@�^5@���@�V@��@��@��@��
@�|�@�
=@���@�v�@��+@�=q@��^@�`B@��j@�Z@�1@�;d@�=q@�@�V@�A�@�\)@�~�@���@�X@��@��`@�Z@�  @���@��
@�"�@�~�@�V@���@���@���@�p�@��@���@���@��@�n�@��^@��@�/@���@��D@��;@��@���@�M�@��-@�O�@�A�@��@�dZ@�;d@�
=@��@��@��@��!@���@�V@�$�@�$�@���@��@�&�@��`@��D@�Z@�1@��@��R@���@�~�@�V@�J@��@���@��h@��`@��9@���@��u@�j@�1'@�w@K�@+@
=@~�y@~�@~�R@~��@~��@~V@}�h@}`B@|�@|Z@{�m@{�F@{��@{dZ@{33@{@z�H@z=q@zJ@y�7@y7L@x��@x�u@x�@x �@w�;@w�@w�P@w\)@w�@vv�@vE�@v@u�h@u?}@t�j@t(�@s�
@s�
@s��@s33@r�H@r�!@r�\@r~�@rn�@rn�@r^5@r=q@q�7@q�@p�@o�@o�w@o�@p1'@pQ�@pbN@pĜ@q�@q7L@q&�@p�@ol�@o�@o�@o�w@q��@rJ@q�^@qX@p�`@p�u@p �@o\)@n��@n�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�A���A�A�A���A��Aإ�A��A��A֝�A֙�A֙�A֋DAցA�p�A�p�A�p�A�p�A�n�A�jA�l�A�dZA��A�ZA�?}Aҝ�AѓuA�+A·+A�x�A�C�A�A�A�(�A��+A��HA��A�5?A�A��A�oA��A���A�=qA�bNA���A��A��A�I�A�ĜA��PA��wA���A��FA�A��A���A�/A���A���A�9XA���A�n�A�(�A�bNA�JA�|�A��/A��mA�+A���A��TA�JA���A�  A��A�Q�A�"�A��A�dZA���A�  A��A�p�A�9XA�Q�A�"�A�ffA��+A�%A~A|��A{�wAy�wAx9XAw+Av5?Au�As�As+Ap1'AmVAj�/Ah�yAg&�Ae��AcC�Aa\)A`��A_K�A]�mA]dZA]�FA]�#A^JA]�A\ȴA[?}AZ�/AX��AW��AV�DAV �AT�HATAR�AP  AN5?AM+AK��AJ�RAI��AG��AF�HAF(�AE�AD�jAC�wAC�#AB��AA�ABffAB5?AA�A?A>�A=;dA<9XA;�A:ĜA9A9dZA9/A8��A8�A7hsA6��A6bNA5�A5A4�+A4  A3�A333A2��A1��A1?}A0�A0�A/+A.�/A.�9A.z�A.A�A-��A-�
A-�hA,��A,n�A+��A*�A*�!A*�uA*r�A*bA)A)\)A(�A($�A'�^A&VA%"�A$~�A#�A#�FA#x�A"ĜA"JA ��AoA�AG�A�uA�-AhsA�AjA�A�HA�A�wA�/Av�A��A
=A$�AJA��A�9A;dA�FA�/AA�A��A�uA �A/A
v�A	C�A^5A�
A\)A��A�TA�A-A��A I�@���@���@��j@�\)@�Q�@�@�+@���@�@��#@��H@�E�@�p�@�j@�ƨ@���@�-@�Z@���@��T@�O�@��;@ڸR@�V@�@׾w@�ff@�V@�1@�|�@Ұ!@�@�dZ@�7L@̃@�l�@�@�?}@�Ĝ@�K�@�=q@�&�@��@�X@��@���@��/@�Z@�ƨ@�C�@��@��9@�ƨ@��@�~�@�{@��@��9@� �@��y@�O�@���@�I�@���@�p�@��@���@�r�@�1'@��@�|�@�;d@���@�@�O�@�Ĝ@���@��\@�{@���@��7@�%@��
@���@���@�E�@�X@�bN@��;@�;d@��!@�^5@���@�V@��@��@��@��
@�|�@�
=@���@�v�@��+@�=q@��^@�`B@��j@�Z@�1@�;d@�=q@�@�V@�A�@�\)@�~�@���@�X@��@��`@�Z@�  @���@��
@�"�@�~�@�V@���@���@���@�p�@��@���@���@��@�n�@��^@��@�/@���@��D@��;@��@���@�M�@��-@�O�@�A�@��@�dZ@�;d@�
=@��@��@��@��!@���@�V@�$�@�$�@���@��@�&�@��`@��D@�Z@�1@��@��R@���@�~�@�V@�J@��@���@��h@��`@��9@���@��u@�j@�1'@�w@K�@+@
=@~�y@~�@~�R@~��@~��@~V@}�h@}`B@|�@|Z@{�m@{�F@{��@{dZ@{33@{@z�H@z=q@zJ@y�7@y7L@x��@x�u@x�@x �@w�;@w�@w�P@w\)@w�@vv�@vE�@v@u�h@u?}@t�j@t(�@s�
@s�
@s��@s33@r�H@r�!@r�\@r~�@rn�@rn�@r^5@r=q@q�7@q�@p�@o�@o�w@o�@p1'@pQ�@pbN@pĜ@q�@q7L@q&�@p�@ol�@o�@o�@o�w@q��@rJ@q�^@qX@p�`@p�u@p �@o\)@n��@n�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B  B!�B8RBE�BH�BI�BJ�BJ�BI�BI�BJ�BL�BM�BO�BVBZB]/Bp�Bv�Bs�Bm�BcTBT�B:^BbB��B��B�B�\Bu�BgmBR�BK�BI�BE�BG�B@�B9XB1'B'�B!�B!�B<jB6FB>wBT�BF�B0!B(�B(�B�B+B��B�B�B�/B��B��B�^B�-B��B��B�hB�7B|�Bn�B]/B;dB(�B�B�B�B\BB
��B
�mB
�)B
ƨB
�9B
��B
�B
p�B
ffB
T�B
H�B
<jB
)�B
�B
oB

=B
B	��B	�B	��B	�LB	��B	��B	�JB	|�B	gmB	W
B	N�B	D�B	;dB	>wB	T�B	ZB	`BB	_;B	XB	N�B	N�B	D�B	:^B	1'B	/B	&�B	�B	�B	+B��B��B�B�B�mB�5B�#B�
B�B�B�B�yB�ZB�BB��B		7B	B��B��B�B�sB�TB�5B�B�B�B��B��B��B��BɺBŢB��B�wB�wB�}B�}B�dB�LB�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�VB�PB�JB�=B�+B�B~�B{�Bx�Bu�Br�Bp�Bo�Bm�BjBhsBffBe`BbNB`BB^5B[#BYBW
BT�BQ�BM�BI�BF�BD�BC�BB�B@�B?}B=qB;dB9XB7LB6FB49B2-B/B,B(�B&�B"�B�B�B�B�B�B{BhB\BPBJBDB
=B	7B	7B1B%BBBB  B��B  BBBB%BB%B+B+B1B+B
=B	7B1B+B1B	7B
=B
=BDBDBbBuB{BuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B �B!�B �B"�B$�B&�B'�B)�B+B-B0!B1'B2-B6FB9XB;dB?}BC�BD�BE�BE�BF�BH�BL�BN�BO�BS�BZB\)B^5B`BBaHBcTBhsBk�Bn�Bp�Bp�Bq�Bs�Bx�Bz�B}�B�%B�7B�DB�VB�\B�bB�{B��B��B��B��B��B��B�B�B�!B�'B�?B�RB�RB�XB�wB��BBŢBƨBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�#B�5B�TB�mB�B�B��B��B��B��B��B��B��B��B��B	B	B	B	+B	DB	PB	\B	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	'�B	'�B	(�B	)�B	+B	.B	0!B	1'B	2-B	2-B	2-B	33B	33B	33B	49B	8RB	9XB	<jB	B�B	E�B	F�B	G�B	G�B	H�B	H�B	H�B	K�B	L�B	N�B	O�B	P�B	R�B	R�B	T�B	W
B	YB	\)B	^5B	`BB	dZB	e`B	gmB	iyB	k�B	m�B	n�B	o�B	o�B	p�B	r�B	s�B	s�B	s�B	s�B	t�B	t�B	t�B	t�B	u�B	w�B	z�B	� B	�B	�+B	�1B	�7B	�7B	�DB	�PB	�VB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B��B��B��B��B�B$�B:YBE�BH�BJBJ�BKBI�BI�BJ�BL�BM�BO�BV3B[7Bc+Bt�ByBv�BpVBf�B[=BD
B�BB��B�qB�NB}=Bo�BWvBOyBN�BJuBN5BDhB=*B8wB/�B%B#�BAB9vBB]BY9BJgB4_B--B.�BB1B�nB�]B�B�B�B�B��B�B��B��B��B�vB��Bs�BfpB@�B+vB �B�B`BLBEB
�NB
��B
�4B
˃B
�RB
��B
��B
s3B
j�B
W�B
J�B
@�B
-8B
�B
�B
�B
�B	�uB	�B	�B	�B	�B	�lB	��B	�=B	k�B	X�B	Q�B	G�B	<�B	=�B	T�B	Y�B	`�B	a�B	[EB	O�B	SB	GPB	=YB	27B	2B	)B	#B	�B	AB	�B�OB��B�9B��B�"B��B�zB��B�yB��B�B��B�MB��B	�B	B�tB�B��B��B�B��B�
BخB�uB�oBҰB�dB��B�(B�B��B�B��B�rB�UB��B��B�FB�sB��B��B��B��B��B��B�mB��B�!B�B�B��B��B�(B�<B��B��B��B��B�B��B�\B��B�B��B��B��B��B��B��B�B�?B}JBzXBw�BshBq�Bp�Bo�BlBjTBg`Bg�BceBa�B`VB]wBYsBXMBWnBU�BR-BL#BIBFBD�BC�BA�BBB?qB>vB;�B8�B7�B5�B4vB5�B.`B)�B*uB$�B!B.BSB�B;B�B�BB�BB*BzB
KB
�B	�BB�B"B�B �B*B�B�B�B�BqBEB�B B�B	�B"B�B
�B
B	�B	B
6B�BBGB�B�B�B�B�BeB�B�B�B�BB�BwBvBBByB�B�B!EB!�B"�B#\B$�B%rB'gB(xB*]B+fB-�B0�B1�B3jB7,B:B<�B@�BD8BE,BE�BF[BH/BJBM+BO]BQ2BULBZ�B]B_B`�Ba�Bd�BiOBl1Bn�Bp�Bq9BreBtcBy%Bz�B~wB��B��B�MB��B��B�ZB��B�5B��B��B��B��B�B�sB�nB��B��B��B�qB��B�IB�SB��B�B��B��B�B�9B�=B��B�<BѬB��B�CB�jBӈB�oB��BٮB�B�LB�NB�&B�0B�B�AB�B�8B� B�B� B�9B�(B�~B	fB	4B	�B	�B	�B	�B	B	�B	 B	zB	�B	�B	�B	�B	*B	�B	B	?B	 �B	&HB	( B	((B	)ZB	*{B	+�B	.�B	0TB	1YB	2_B	2VB	2cB	3VB	3ZB	3�B	4�B	8�B	9�B	<�B	C	B	E�B	F�B	G�B	G�B	H�B	H�B	IQB	LB	MSB	O6B	PMB	Q1B	SB	S]B	UKB	WKB	YHB	\hB	^�B	`�B	d�B	e�B	g�B	i�B	k�B	nB	n�B	o�B	o�B	qB	r�B	s�B	s�B	s�B	s�B	t�B	t�B	t�B	uQB	v)B	xHB	{�B	�B	�B	�B	�0B	�AB	�B	�%B	�PB	�}B	��B	�FB	��B	��B	�B	�B	��B	�-B	�=B	�XB	�EB	�hB	��B	�SB	�3B	� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</v<#�
<#�
<P:�<Gjt<^��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)6�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.16 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352062018102313520620181023135206  AO  ARCAADJP                                                                    20170922170309    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170922170309  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170922170309  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135206  QC  PRES            @�33D{S3G�O�                PM  ARSQCTM V1.1                                                                20181023135206  QC  PSAL            @�33D{S3G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093510  IP                  G�O�G�O�G�O�                