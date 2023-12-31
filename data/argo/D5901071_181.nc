CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:42Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143234  20190522121827  1727_5046_181                   2C  D   APEX                            2143                            040306                          846 @����,_�1   @����/�@4�-�c��n��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� DmfDm�fDnfDn�fDofDo� Do��Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffBЙ�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33CL�C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D13D1��D2�D2��D3�D3��D4�D4�fD5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>�3D?�D?�3D@�D@��DA�DA��DB�DB��DC�DC��DD3DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR�3DS�DS��DT�DT��DU�DU�fDV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di�fDj�Dj��Dk�Dk��Dl�Dl��Dm3Dm�3Dn3Dn�3Do3Do��DpfDp��Dq�Dq��Dr�Dr��Ds�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A��A��A��A�"�A�&�A�&�A�(�A�(�A�(�A�&�A�+A�-A�-A�/A�+A�$�A� �A��A�oA�oA�VA�VA�bA�JA�1A�
=A�A��A��#AӸRA��/A���A�G�A��A�|�A���A��;A�t�Aʙ�AʅA�hsA��;A�~�Aƥ�A��AŶFAĺ^A�dZA�A�^5A�E�A���A�5?A���A�7LA���A��jA��jA�`BA��;A�ȴA�"�A�dZA�Q�A��-A��wA��RA�1'A�+A��A�+A��A��A�-A��A��A��`A��A��TA��jA�hsA�ZA��uA�dZA��A�(�A��HA��FA��hA�dZA�t�A�G�A��!A���A� �A�XA�~�A�hsA�VA��PA���A�7LA�bA�ƨA��A�I�A���A�/A�C�A���A�ȴA�p�A���A�bNA��/A��#A}|�Ay�
Au��Aq��Ao;dAk�Ag&�Ae�AcAa��A_�^A^ĜA\9XAZ  AY%AT�uAQ�#AN�RAMG�AL��AK��AKAHffAF��AF=qAF �AE�AEG�AC�AB-A@Q�A?��A?33A>v�A=�#A<A:5?A8�jA7�hA6��A6��A6�\A6ZA5|�A3�wA1O�A0=qA/�^A/\)A/�A.��A-��A,(�A)G�A'��A&�!A$��A#�mA#7LA" �A�mA�A|�A\)AA�AM�AbAv�A5?A1A�PA�AM�A?}A��A=qA��Ap�An�A��A�^Ax�AS�A33A�uAG�A
��A
 �A	�A	��A	�A��A�Av�A�A7LA�-A~�AA�AK�A z�@�~�@��-@��/@���@�x�@�A�@�o@�M�@��h@���@��;@�@�@��H@�(�@��H@�5?@��@���@�@� �@�x�@�G�@柾@��@��@�R@���@�?}@���@�  @�S�@�O�@���@�;d@ڇ+@��@�7L@��@�G�@�A�@Ӿw@�~�@�x�@�Ĝ@�C�@�-@��T@͉7@���@�A�@���@�"�@�~�@ɲ-@�`B@���@ǶF@�\)@�@Ɵ�@�M�@�@���@š�@�/@�Z@��m@å�@�ȴ@�J@�7L@��@��@���@�t�@�S�@�bN@�ff@���@���@��@��9@�I�@�33@��R@�ff@��/@�Q�@�dZ@��+@��@�J@��7@�?}@��@���@���@�=q@�5?@�v�@�v�@���@�7L@�&�@���@���@���@���@��j@��u@�Q�@��;@�ƨ@��P@�+@���@���@���@���@���@��!@��F@���@�
=@�\)@�(�@���@���@�j@�A�@��R@�Z@�ƨ@��w@�\)@�;d@�33@��@�o@�
=@��R@�5?@��@�x�@���@�J@�X@�{@�&�@�7L@���@�{@�{@���@���@��@�hs@�p�@�/@���@��@���@��`@��@��`@���@�A�@��@��m@���@�dZ@��@��H@��!@�n�@���@�7L@��`@�Ĝ@��9@���@�r�@���@��@�C�@��@�@��y@�ff@�@�G�@�Q�@�bN@�j@�r�@�Z@�b@��@��
@���@�K�@�@��@��@�J@���@��^@���@�G�@��@���@�Ĝ@��9@��@�z�@���@��@���@��P@�\)@�o@��y@���@�ff@�5?@��@�J@���@�x�@�G�@���@�bN@�1'@��@���@��P@�+@�o@�o@�
=@���@�-@�J@���@�x�@�G�@���@���@���@�Q�@��@���@�dZ@�@��@��@�@�
=@��@�ff@��@�`B@��@���@��/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�&�A��A��A��A�"�A�&�A�&�A�(�A�(�A�(�A�&�A�+A�-A�-A�/A�+A�$�A� �A��A�oA�oA�VA�VA�bA�JA�1A�
=A�A��A��#AӸRA��/A���A�G�A��A�|�A���A��;A�t�Aʙ�AʅA�hsA��;A�~�Aƥ�A��AŶFAĺ^A�dZA�A�^5A�E�A���A�5?A���A�7LA���A��jA��jA�`BA��;A�ȴA�"�A�dZA�Q�A��-A��wA��RA�1'A�+A��A�+A��A��A�-A��A��A��`A��A��TA��jA�hsA�ZA��uA�dZA��A�(�A��HA��FA��hA�dZA�t�A�G�A��!A���A� �A�XA�~�A�hsA�VA��PA���A�7LA�bA�ƨA��A�I�A���A�/A�C�A���A�ȴA�p�A���A�bNA��/A��#A}|�Ay�
Au��Aq��Ao;dAk�Ag&�Ae�AcAa��A_�^A^ĜA\9XAZ  AY%AT�uAQ�#AN�RAMG�AL��AK��AKAHffAF��AF=qAF �AE�AEG�AC�AB-A@Q�A?��A?33A>v�A=�#A<A:5?A8�jA7�hA6��A6��A6�\A6ZA5|�A3�wA1O�A0=qA/�^A/\)A/�A.��A-��A,(�A)G�A'��A&�!A$��A#�mA#7LA" �A�mA�A|�A\)AA�AM�AbAv�A5?A1A�PA�AM�A?}A��A=qA��Ap�An�A��A�^Ax�AS�A33A�uAG�A
��A
 �A	�A	��A	�A��A�Av�A�A7LA�-A~�AA�AK�A z�@�~�@��-@��/@���@�x�@�A�@�o@�M�@��h@���@��;@�@�@��H@�(�@��H@�5?@��@���@�@� �@�x�@�G�@柾@��@��@�R@���@�?}@���@�  @�S�@�O�@���@�;d@ڇ+@��@�7L@��@�G�@�A�@Ӿw@�~�@�x�@�Ĝ@�C�@�-@��T@͉7@���@�A�@���@�"�@�~�@ɲ-@�`B@���@ǶF@�\)@�@Ɵ�@�M�@�@���@š�@�/@�Z@��m@å�@�ȴ@�J@�7L@��@��@���@�t�@�S�@�bN@�ff@���@���@��@��9@�I�@�33@��R@�ff@��/@�Q�@�dZ@��+@��@�J@��7@�?}@��@���@���@�=q@�5?@�v�@�v�@���@�7L@�&�@���@���@���@���@��j@��u@�Q�@��;@�ƨ@��P@�+@���@���@���@���@���@��!@��F@���@�
=@�\)@�(�@���@���@�j@�A�@��R@�Z@�ƨ@��w@�\)@�;d@�33@��@�o@�
=@��R@�5?@��@�x�@���@�J@�X@�{@�&�@�7L@���@�{@�{@���@���@��@�hs@�p�@�/@���@��@���@��`@��@��`@���@�A�@��@��m@���@�dZ@��@��H@��!@�n�@���@�7L@��`@�Ĝ@��9@���@�r�@���@��@�C�@��@�@��y@�ff@�@�G�@�Q�@�bN@�j@�r�@�Z@�b@��@��
@���@�K�@�@��@��@�J@���@��^@���@�G�@��@���@�Ĝ@��9@��@�z�@���@��@���@��P@�\)@�o@��y@���@�ff@�5?@��@�J@���@�x�@�G�@���@�bN@�1'@��@���@��P@�+@�o@�o@�
=@���@�-@�J@���@�x�@�G�@���@���@���@�Q�@��@���@�dZ@�@��@��@�@�
=@��@�ff@��@�`B@��@���@��/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B1'BT�Bk�B~�B�B�=B��B��B��B��B��B��B��B��B��B��B�B�B�!B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B�\B�7Bt�BgmBiyBs�BbNB�B�TB�
B�9BȴB�BB��B  B��B��B�B�ZB��B�9B�B�-B��B�Bz�Bm�BffB\)BD�B1'B!�B�BVBB
�fB
��B
ĜB
�RB
�B
��B
�PB
�PB
}�B
{�B
u�B
m�B
bNB
B�B
"�B
	7B	�B	��B	�LB	��B	�B	t�B	jB	`BB	VB	W
B	F�B	9XB	.B	�B	\B	B	B	+B	B��B��B��B��B��B��B�B�yB�TB�5B�/B�)B�B�
B��BĜBƨBĜB��B��B��B�}B�qB�^B�RB�LB�LB�?B�9B�-B�B��B��B��B��B��B��B��B�{B�hB�bB�\B�PB�7B�+B�B�B�B�B� B~�B}�B~�B~�B~�B~�B}�B}�B}�B}�B|�B|�Bz�Bx�Bv�Bw�Bx�Bw�Bw�Bw�By�Bz�B{�B{�B{�B�B�B�%B�1B�1B�1B�PB�\B�\B�VB�PB�DB�DB�JB�{B��B��B��B��B�uB�uB�oB�uB�{B��B��B�B�FB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�3B�FB�RB�dB�wB�wB�}B��BĜBǮBɺB��B��B��B��B�B�#B�)B�5B�;B�HB�HB�NB�ZB�sB�B�B�B�B�B�B�B�B�NB��BɺBB��BBÖBƨBƨBÖBBBŢBǮBɺB��B��B��B��B��B��B�
B�B�B�#B�HB�yB�B�B��B��B��B��B��B	B	B	%B	DB	bB	oB	{B	�B	�B	�B	�B	�B	 �B	/B	:^B	B�B	F�B	K�B	M�B	J�B	F�B	G�B	A�B	;dB	@�B	B�B	C�B	F�B	G�B	H�B	H�B	I�B	J�B	M�B	Q�B	VB	ZB	`BB	v�B	z�B	y�B	|�B	�B	�+B	�1B	�1B	�=B	�JB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�FB	�LB	�RB	�XB	�dB	�jB	�jB	�jB	�qB	�qB	�qB	�wB	��B	B	B	ÖB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�;B	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B8RB`BBr�B�B�B�JB��B��B��B��B��B��B��B��B��B��B�B�9B�9B�B��B��B��B�B�-B�-B�B�B�B��B��B��B��B��B��B��B�{B�bBx�BhsBiyBx�Bn�B!�B�mB�)B�?B��B�HBBB��B��B�B�B�B�^B�-B�LB�3B�1B� Bq�Bk�BgmBK�B7LB&�B�BhBJB
�B
�B
ǮB
�^B
�9B
��B
�bB
�hB
~�B
}�B
x�B
o�B
k�B
L�B
,B
uB	��B	�B	B	��B	�+B	x�B	p�B	ffB	YB	_;B	M�B	=qB	<jB	%�B	�B	1B	%B	
=B	+B	1B	  B��B��B��B��B�B�B�mB�;B�;B�5B�)B�)B��BȴBɺBƨBB��BBBB��B�dB�RB�RB�FB�?B�9B�9B�'B��B��B��B��B��B��B��B��B�hB�bB�hB�\B�VB�7B�%B�B�B�B�B�B�B� B� B� B�B~�B~�B~�B}�B}�B|�B|�Bx�By�By�Bx�By�B{�B|�B|�B}�B~�B�B�%B�+B�+B�7B�DB�DB�VB�bB�hB�hB�\B�PB�JB�PB�{B��B��B��B��B��B��B�uB�{B�{B�{B��B��B�qB��B��B��B��B��B��B��B��B��B�B�!B�B�B�B�B�!B�B�B�'B�-B�9B�LB�^B�qB�wB�}B��BBŢBȴB��B��B��B��B�B�#B�)B�/B�;B�;B�HB�HB�TB�`B�yB�B�B�B�B��B�B��B�B�yB�B��BÖB��BÖBĜBǮBȴBĜBÖBŢBƨBɺB��B��B��B��B��B��B��B�B�B�B�#B�HB�B�B�B��B��B��B��B��B	B	B	+B	DB	bB	uB	{B	�B	�B	�B	�B	�B	�B	,B	8RB	A�B	E�B	K�B	P�B	N�B	F�B	J�B	E�B	<jB	@�B	C�B	C�B	F�B	G�B	H�B	H�B	I�B	K�B	N�B	R�B	VB	YB	ZB	u�B	|�B	y�B	{�B	�B	�+B	�1B	�7B	�=B	�JB	�\B	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�FB	�RB	�RB	�^B	�dB	�jB	�jB	�qB	�wB	�wB	�wB	�wB	��B	B	B	ÖB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�;B	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�C�<#�
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
<49X<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447372012010314473720120103144737  AO  ARGQ                                                                        20111130143234  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143234  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144737  IP                  G�O�G�O�G�O�                