CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:44Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143348  20190522121827  1727_5046_191                   2C  D   APEX                            2143                            040306                          846 @��@��1   @���m��@5�������c��1'1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�33B   BffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�A���A���B ��B	33B33B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bi33Bq33Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
L�C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CFL�CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	fD	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6�fD7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��DafDa��Db�Db�fDc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�3Dp�Dp��Dq�Dq��Dr�Dr��Ds�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��/A��;A��HA��/A��#A��HA��HA��HA��TA��TA��mA��mA��mA��mA��mA��yA��mA��HA��#A��#A��#A��
A���AþwAîAÙ�A�$�A���A�~�A�^5A���A��A���A��^A���A��jA��#A��mA�G�A�/A�A���A���A���A���A��DA�x�A�K�A�
=A�"�A�K�A��;A���A��7A�M�A�&�A���A��A�$�A�ĜA�ZA�$�A��`A��hA���A��RA��A�|�A��DA��/A��9A�A��A�VA�A�A���A��A�-A��A��A��RA��TA�1A�`BA�"�A��A�ffA��mA���A�K�A���A�;dA��A��;A�C�A�I�A�A��RA�\)A�jA�&�A��A�A}ƨA|��Az��AxI�Aw�Au�;AtffArȴAohsAlbAj��Ah�Ad�!Ac��Ab  A_�PA]hsAZ^5AV�HAVZAU�-ATv�AO�-AM"�AK�AIAH~�AG%AEhsAD1AC7LAB�uAB$�AA�A@�/A@(�A?7LA<^5A8ffA6�9A5K�A4n�A3XA2�HA2��A2ZA1hsA0A.�DA.9XA.5?A.5?A-G�A+hsA*��A)�A)hsA)%A(~�A'�hA%�;A$A�A#oA"I�A!�A�HAA�AĜAA�^AĜA�TA33AĜA��A�A&�A�DA5?A�A�jAQ�A�AE�A  A�HA�;A`BAVA�/A=qA�wA
ĜA
$�A	l�A�!A&�A��AZAE�A �A��AI�A��A��A bN@��y@��`@���@��D@� �@��R@�1@�  @�r�@��
@�7L@���@���@�P@�!@��@�l�@�/@۾w@��@�M�@�@�@�J@��@��@ٲ-@�\)@�\)@�%@ӶF@�n�@�?}@�dZ@��@���@θR@��@��T@��@ͺ^@�X@�Ĝ@�1'@�t�@��@���@ʸR@�M�@ɲ-@�I�@�1@��m@���@Ƨ�@őh@Ĭ@�I�@�t�@�@�n�@�ff@�n�@�ff@�V@�E�@�=q@�J@��@�b@�\)@���@��R@���@���@���@���@�v�@�@�X@�7L@��@��j@���@�K�@�x�@���@��u@���@�l�@�@���@�v�@�^5@�J@���@���@�O�@�&�@���@���@��;@��@�@�O�@�/@�%@��@���@�l�@�C�@��H@���@��9@�(�@�ƨ@��P@�C�@��y@�n�@�/@�z�@�(�@�ƨ@��@�\)@�n�@���@�/@��/@��D@�1@��@�K�@���@�^5@�$�@��@���@��@�z�@� �@��;@��@�S�@��@�
=@�@��y@���@�J@��-@���@�X@�Z@���@���@�|�@�33@�o@�@���@��y@��R@�v�@�n�@�-@���@���@��@�p�@�`B@�/@��@���@��@��u@��u@��D@��u@��D@�r�@���@���@�l�@�\)@�+@��@�
=@��@���@�n�@�-@��^@�?}@�/@�&�@�&�@�/@�/@�/@�/@�%@��@���@��P@�\)@�;d@�"�@�ȴ@��!@���@�n�@�M�@�V@�~�@�~�@�=q@�{@��7@��@��@�r�@��9@��@���@��@�b@�|�@�^5@�-@��@�@��T@���@��#@��#@���@��T@���@�@�@�@�J@�{@�{@��@��#@��@�V@��@�Ĝ@��@��@�r�@�j@�9X@���@��m@��
@���@��w@��P@�
=@���@���@�ff@�$�@���@�hs@�X@�X@��@�r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��/A��;A��HA��/A��#A��HA��HA��HA��TA��TA��mA��mA��mA��mA��mA��yA��mA��HA��#A��#A��#A��
A���AþwAîAÙ�A�$�A���A�~�A�^5A���A��A���A��^A���A��jA��#A��mA�G�A�/A�A���A���A���A���A��DA�x�A�K�A�
=A�"�A�K�A��;A���A��7A�M�A�&�A���A��A�$�A�ĜA�ZA�$�A��`A��hA���A��RA��A�|�A��DA��/A��9A�A��A�VA�A�A���A��A�-A��A��A��RA��TA�1A�`BA�"�A��A�ffA��mA���A�K�A���A�;dA��A��;A�C�A�I�A�A��RA�\)A�jA�&�A��A�A}ƨA|��Az��AxI�Aw�Au�;AtffArȴAohsAlbAj��Ah�Ad�!Ac��Ab  A_�PA]hsAZ^5AV�HAVZAU�-ATv�AO�-AM"�AK�AIAH~�AG%AEhsAD1AC7LAB�uAB$�AA�A@�/A@(�A?7LA<^5A8ffA6�9A5K�A4n�A3XA2�HA2��A2ZA1hsA0A.�DA.9XA.5?A.5?A-G�A+hsA*��A)�A)hsA)%A(~�A'�hA%�;A$A�A#oA"I�A!�A�HAA�AĜAA�^AĜA�TA33AĜA��A�A&�A�DA5?A�A�jAQ�A�AE�A  A�HA�;A`BAVA�/A=qA�wA
ĜA
$�A	l�A�!A&�A��AZAE�A �A��AI�A��A��A bN@��y@��`@���@��D@� �@��R@�1@�  @�r�@��
@�7L@���@���@�P@�!@��@�l�@�/@۾w@��@�M�@�@�@�J@��@��@ٲ-@�\)@�\)@�%@ӶF@�n�@�?}@�dZ@��@���@θR@��@��T@��@ͺ^@�X@�Ĝ@�1'@�t�@��@���@ʸR@�M�@ɲ-@�I�@�1@��m@���@Ƨ�@őh@Ĭ@�I�@�t�@�@�n�@�ff@�n�@�ff@�V@�E�@�=q@�J@��@�b@�\)@���@��R@���@���@���@���@�v�@�@�X@�7L@��@��j@���@�K�@�x�@���@��u@���@�l�@�@���@�v�@�^5@�J@���@���@�O�@�&�@���@���@��;@��@�@�O�@�/@�%@��@���@�l�@�C�@��H@���@��9@�(�@�ƨ@��P@�C�@��y@�n�@�/@�z�@�(�@�ƨ@��@�\)@�n�@���@�/@��/@��D@�1@��@�K�@���@�^5@�$�@��@���@��@�z�@� �@��;@��@�S�@��@�
=@�@��y@���@�J@��-@���@�X@�Z@���@���@�|�@�33@�o@�@���@��y@��R@�v�@�n�@�-@���@���@��@�p�@�`B@�/@��@���@��@��u@��u@��D@��u@��D@�r�@���@���@�l�@�\)@�+@��@�
=@��@���@�n�@�-@��^@�?}@�/@�&�@�&�@�/@�/@�/@�/@�%@��@���@��P@�\)@�;d@�"�@�ȴ@��!@���@�n�@�M�@�V@�~�@�~�@�=q@�{@��7@��@��@�r�@��9@��@���@��@�b@�|�@�^5@�-@��@�@��T@���@��#@��#@���@��T@���@�@�@�@�J@�{@�{@��@��#@��@�V@��@�Ĝ@��@��@�r�@�j@�9X@���@��m@��
@���@��w@��P@�
=@���@���@�ff@�$�@���@�hs@�X@�X@��@�r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�Bz�B{�B{�Bz�B�B�\B��B�3B�}B�}B�jB�^B�LB�dB�dB�XB�RB�LB�FB�?B�9B�9B�3B�-B�'B�B��B��B��B�bB�7B�%B�B�B�B}�B{�Bx�Bv�Bt�Bq�Bn�BhsB_;BO�B<jB,BJB��B�B��B��B�FB��B�oBx�B^5BT�BP�BF�B8RB0!B,B$�B1B
�B
�TB
�)B
�
B
��B
ǮB
ǮB
B
�LB
�B
��B
�DB
}�B
x�B
r�B
_;B
<jB
,B
�B
+B	��B	�B	�`B	�B	��B	�B	��B	��B	�B	{�B	o�B	bNB	T�B	A�B	1'B	-B	'�B	�B	PB	B��B��B�B�B�fB�TB�;B�/B�#B�B�
B��B��B�wB�FB�'B�B��B��B��B��B��B��B�uB�{B��B��B�uB�oB�bB�bB�VB�VB�JB�DB�DB�=B�=B�7B�1B�%B�B�B�B�B�B�B� B� B� B� B~�B~�B}�B~�B}�B|�B|�B{�Bw�Bq�Bn�Bl�Bk�Bk�Bk�BjBjBjBhsBgmBffBe`BffBffBffBe`Be`BbNBcTBaHB_;B`BB`BB_;B`BBaHB_;B_;B`BB`BBaHB]/B[#BZB^5BdZBdZBaHB^5B`BBdZBe`BiyBp�Bs�Bs�Bv�Bw�Bz�B�B�=B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�9B�9B�9B�3B�3B�LB�RB�XB�XB�^B�dB�^B�dB�qB��BBBBÖBÖBĜBĜBŢBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�/B�5B�;B�HB�TB�ZB�fB�fB�mB�sB�yB�yB�B�B�B�B�B�B��B��B��B��B��B	B	%B	+B	1B	hB	uB	�B	�B	�B	�B	�B	�B	"�B	&�B	(�B	,B	,B	.B	49B	:^B	=qB	?}B	A�B	E�B	I�B	I�B	L�B	M�B	N�B	O�B	P�B	S�B	XB	ZB	[#B	\)B	^5B	`BB	`BB	`BB	`BB	aHB	ffB	hsB	iyB	jB	r�B	w�B	x�B	y�B	z�B	{�B	|�B	|�B	|�B	}�B	� B	� B	�B	�B	�B	�1B	�1B	�7B	�=B	�=B	�DB	�PB	�VB	�VB	�VB	�VB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�9B	�FB	�FB	�^B	�dB	�jB	�wB	�jB	�jB	�wB	��B	ĜB	ȴB	ȴB	ȴB	ƨB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�yB	�yB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�Bz�B{�B{�B|�B�B�uB��B�^BBÖB�}B�wB�wB�}B�}B�dB�RB�RB�LB�?B�?B�9B�3B�-B�-B�?B�3B��B��B��B�DB�+B�%B�B�B�B~�B{�Bw�Bv�Bt�Bt�Bn�BcTBR�BA�B49BhB��B��B��BÖB�XB�'B��B� BaHBVBT�BJ�B;dB1'B/B/BbB
��B
�fB
�;B
�B
��B
ȴB
��B
ǮB
�^B
�-B
��B
�\B
~�B
y�B
v�B
l�B
?}B
0!B
 �B

=B
B	��B	�yB	�BB	ɺB	�-B	�B	��B	�%B	� B	u�B	gmB	]/B	J�B	2-B	/B	,B	,B	{B	1B	B��B��B�B�B�fB�HB�;B�/B�)B�B�B�B��B�jB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�\B�\B�VB�PB�\B�VB�PB�DB�DB�JB�+B�+B�+B�B�B�B�B�B�B�B�B�B� B� B�B� B~�B~�B|�Br�Br�Bo�Bm�Bl�Bl�Bl�Bl�Bm�BjBiyBiyBiyBgmBgmBffBe`BffBffBgmBdZBcTBcTBcTBdZBbNBbNBaHBcTBffBe`BbNBaHB_;B]/B`BBffBe`Be`BbNBcTBe`BffBjBp�Bs�Bs�Bv�Bx�B~�B�B�VB��B��B��B��B��B��B��B��B�B�B�B�!B�B�B�'B�3B�9B�9B�?B�9B�?B�LB�RB�XB�dB�jB�jB�dB�jB�wB��BBBBÖBÖBĜBŢBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�5B�;B�BB�NB�ZB�`B�fB�fB�sB�sB�yB�B�B�B�B�B�B��B��B��B��B��B	  B	B	%B	1B	
=B	oB	{B	�B	�B	�B	�B	�B	�B	#�B	&�B	)�B	,B	-B	/B	5?B	;dB	>wB	@�B	B�B	F�B	I�B	J�B	L�B	M�B	N�B	O�B	Q�B	T�B	YB	ZB	[#B	]/B	^5B	`BB	`BB	`BB	`BB	bNB	gmB	hsB	iyB	l�B	s�B	w�B	x�B	y�B	z�B	{�B	|�B	|�B	|�B	}�B	� B	� B	�B	�B	�%B	�1B	�1B	�7B	�=B	�=B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�3B	�9B	�9B	�9B	�FB	�FB	�^B	�dB	�jB	�}B	�qB	�jB	�wB	��B	ÖB	ɺB	ɺB	ɺB	ǮB	ǮB	ƨB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�yB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
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
<49X<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447412012010314474120120103144741  AO  ARGQ                                                                        20111130143348  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143348  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144741  IP                  G�O�G�O�G�O�                