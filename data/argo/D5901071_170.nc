CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:39Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143017  20190522121827  1727_5046_170                   2C  D   APEX                            2143                            040306                          846 @��a�@1   @��bW:�@6&�x���c�-1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Fff@�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B)33B133B8ffB@ffBH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33CL�C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CFL�CHL�CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf�Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�fD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@�fDA�DA��DB�DB��DC�DC�fDD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[3D[�3D\3D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df3Df��DgfDg��Dh�Dh��Di�Di��Dj�Dj��DkfDk��Dl�Dl��Dm�Dm��Dn3Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�E�A�K�A�K�A�M�A�O�A�O�A�Q�A�\)A�\)A�^5A�^5A�`BA�G�A�E�A�G�A�Q�A�Q�A�C�AҍPA�O�A���AЅA�r�A��A�ffAǍPA�VAŴ9A���A���A� �A�&�A�9XA�l�A�\)A�C�A�VA��/A���A��uA�p�A���A�;dA��A�O�A�;dA��A���A�\)A���A�jA��A�ĜA�+A�G�A��^A��PA�S�A��`A�hsA�oA��A�E�A��#A��A�+A�A���A��wA�t�A��A�bNA���A��HA�\)A�dZA��RA�oA�5?A��yA��9A� �A�A�?}A��!A�5?A��yA��PA�VA��/A���A�t�A�&�A�&�A��+A�-A���A��!A�33A���A��!A�A���A��A���A�1'A��A��jA��#A�5?A�XA�S�A��A�ȴA�E�A���A�A�A�|�A�I�A�
=A�\)A��hA��7Ap�A~$�A|��A{�Az1'Av�`AtJArM�Ap�HAo�;Ao%Am�;Aj�yAgVAa�#A`ZA^1A\�A[;dAXZAV1'AU��AU;dAT�AR-AP�AO%AM�AJM�AGVAE�^AE+AD��ACA@��A?A>n�A=�A;�A:��A:��A:r�A9/A7t�A5�A5dZA5A45?A2�!A1��A0=qA/?}A.=qA-"�A*��A)��A'��A&r�A%|�A$=qA"��A"�A"�A"{A"bA!A!;dA �uAƨAS�AjA  A�-AS�A�A&�AS�A��A\)A/AAz�A`BAAM�A�yA�#AVA�A9XA
VA	�PA	t�A	�A��A��A1A�A�TA�/A1'A  A
=A�^A/A ��A Q�@�S�@��@�O�@�/@�z�@��@���@�S�@�v�@��7@�X@���@�I�@���@�ƨ@�K�@���@��T@�9X@�n�@�9@�Z@� �@�;d@�&�@�dZ@��`@�J@�u@��
@�dZ@���@�@��@���@ܣ�@�dZ@ڸR@���@���@�I�@�ȴ@�|�@җ�@�n�@��@щ7@���@�j@�|�@Ώ\@��@�p�@̼j@���@�"�@��@�V@ț�@�z�@� �@�dZ@��@�^5@��@���@ÍP@�K�@��H@���@\@���@�/@��@�dZ@���@�ff@��\@��+@�v�@��@�/@���@�z�@�^5@�@�%@�1'@�bN@�X@�$�@�7L@�33@��@���@�7L@��9@�1'@�7L@�V@��-@��u@�z�@�j@�I�@�1'@�1@���@�dZ@��@�{@��@���@��9@��@���@��u@��@��@�I�@�I�@�I�@��@��P@��y@���@���@��+@�n�@�=q@�-@�J@���@�7L@��`@��@�r�@�9X@��
@�;d@�~�@��T@��-@���@���@���@�ƨ@�ƨ@���@�  @�1@���@��m@�t�@�^5@�5?@���@���@��-@��-@��-@���@�p�@�X@��@�V@��`@��@���@��@�A�@���@�dZ@�;d@�o@��@���@�$�@��@��@��T@��h@�O�@�&�@�&�@��@���@���@���@��@�bN@�A�@��m@�C�@��+@��@�@��@��T@��@��T@��#@��^@���@�7L@��@���@��u@��D@�z�@�r�@�j@�9X@��@��w@�t�@�33@�
=@��y@��H@���@�5?@���@��@�j@�1@���@���@��@�ƨ@�t�@�K�@��@�n�@��#@���@���@��/@��9@��D@�Z@�Z@��@��y@��H@��!@���@��+@�{@�/@��@�  @�ƨ@�"�@�;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�I�A�E�A�K�A�K�A�M�A�O�A�O�A�Q�A�\)A�\)A�^5A�^5A�`BA�G�A�E�A�G�A�Q�A�Q�A�C�AҍPA�O�A���AЅA�r�A��A�ffAǍPA�VAŴ9A���A���A� �A�&�A�9XA�l�A�\)A�C�A�VA��/A���A��uA�p�A���A�;dA��A�O�A�;dA��A���A�\)A���A�jA��A�ĜA�+A�G�A��^A��PA�S�A��`A�hsA�oA��A�E�A��#A��A�+A�A���A��wA�t�A��A�bNA���A��HA�\)A�dZA��RA�oA�5?A��yA��9A� �A�A�?}A��!A�5?A��yA��PA�VA��/A���A�t�A�&�A�&�A��+A�-A���A��!A�33A���A��!A�A���A��A���A�1'A��A��jA��#A�5?A�XA�S�A��A�ȴA�E�A���A�A�A�|�A�I�A�
=A�\)A��hA��7Ap�A~$�A|��A{�Az1'Av�`AtJArM�Ap�HAo�;Ao%Am�;Aj�yAgVAa�#A`ZA^1A\�A[;dAXZAV1'AU��AU;dAT�AR-AP�AO%AM�AJM�AGVAE�^AE+AD��ACA@��A?A>n�A=�A;�A:��A:��A:r�A9/A7t�A5�A5dZA5A45?A2�!A1��A0=qA/?}A.=qA-"�A*��A)��A'��A&r�A%|�A$=qA"��A"�A"�A"{A"bA!A!;dA �uAƨAS�AjA  A�-AS�A�A&�AS�A��A\)A/AAz�A`BAAM�A�yA�#AVA�A9XA
VA	�PA	t�A	�A��A��A1A�A�TA�/A1'A  A
=A�^A/A ��A Q�@�S�@��@�O�@�/@�z�@��@���@�S�@�v�@��7@�X@���@�I�@���@�ƨ@�K�@���@��T@�9X@�n�@�9@�Z@� �@�;d@�&�@�dZ@��`@�J@�u@��
@�dZ@���@�@��@���@ܣ�@�dZ@ڸR@���@���@�I�@�ȴ@�|�@җ�@�n�@��@щ7@���@�j@�|�@Ώ\@��@�p�@̼j@���@�"�@��@�V@ț�@�z�@� �@�dZ@��@�^5@��@���@ÍP@�K�@��H@���@\@���@�/@��@�dZ@���@�ff@��\@��+@�v�@��@�/@���@�z�@�^5@�@�%@�1'@�bN@�X@�$�@�7L@�33@��@���@�7L@��9@�1'@�7L@�V@��-@��u@�z�@�j@�I�@�1'@�1@���@�dZ@��@�{@��@���@��9@��@���@��u@��@��@�I�@�I�@�I�@��@��P@��y@���@���@��+@�n�@�=q@�-@�J@���@�7L@��`@��@�r�@�9X@��
@�;d@�~�@��T@��-@���@���@���@�ƨ@�ƨ@���@�  @�1@���@��m@�t�@�^5@�5?@���@���@��-@��-@��-@���@�p�@�X@��@�V@��`@��@���@��@�A�@���@�dZ@�;d@�o@��@���@�$�@��@��@��T@��h@�O�@�&�@�&�@��@���@���@���@��@�bN@�A�@��m@�C�@��+@��@�@��@��T@��@��T@��#@��^@���@�7L@��@���@��u@��D@�z�@�r�@�j@�9X@��@��w@�t�@�33@�
=@��y@��H@���@�5?@���@��@�j@�1@���@���@��@�ƨ@�t�@�K�@��@�n�@��#@���@���@��/@��9@��D@�Z@�Z@��@��y@��H@��!@���@��+@�{@�/@��@�  @�ƨ@�"�@�;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�7B�B�wB��B�BjBaHBaHB`BB`BBaHB`BB`BBjBv�Bw�By�B~�B�B�+B�1B�1B�DB�1B�%B�VB��B��B��B��B��B��B�B�B�3B�wBBBBĜBŢBĜB��BÖBÖBĜBƨBɺB��B��B��B��B��B��B��BǮB�wB�!B��B��B�BffB_;B[#BM�B;dB49B.B�BB��B��B��B�B�NB��BȴB�wB��Br�B`BBP�BG�BB�B@�B7LB.B$�B�B	7B
��B
�HB
��B
��B
ȴB
�}B
�RB
�B
��B
�{B
}�B
q�B
bNB
T�B
E�B
9XB
)�B
�B
{B	��B	�fB	�B	��B	ŢB	�wB	�9B	��B	y�B	YB	L�B	?}B	7LB	.B	$�B	�B	�B	�B	uB	PB	%B	  B��B�`B�)B�B�B�B�B��BȴBŢBÖB�^B�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�=B�JB�DB�DB�JB�JB�DB�=B�1B�B�B�B�B�B� B}�B{�Bz�Bx�Bz�By�Bx�Bw�Bt�Bt�Br�Bq�Bq�Br�Bq�Bo�Bo�Bp�Bo�Bo�Bo�Bn�Bl�Bl�Bl�Bl�Bl�BjBhsBgmBffBe`BdZBdZBcTBdZBcTBcTBcTBcTBbNBbNBbNBbNBbNBaHBaHBaHB`BB_;B^5B]/B\)B]/B\)BZBZBYBYBXBYBZBZBYBXBW
BW
BW
BXBYBXBYBZB[#B`BBo�Bs�Bt�Bv�B{�B� B�B�DB�hB�oB�{B��B��B��B��B��B��B��B�B�B�'B�3B�XB�}BBÖBƨBȴB��B��B��B�)B�5B�;B�HB�`B�mB�sB�B�B�B�B�fB�ZB�NB�HB�ZB�B��B��B��B�B��B��B��B	  B	JB	�B	�B	$�B	$�B	%�B	%�B	&�B	'�B	(�B	)�B	+B	.B	49B	9XB	;dB	;dB	;dB	;dB	;dB	;dB	;dB	;dB	;dB	=qB	?}B	@�B	@�B	A�B	A�B	C�B	F�B	H�B	I�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	L�B	N�B	O�B	Q�B	T�B	T�B	XB	\)B	`BB	bNB	dZB	e`B	e`B	ffB	q�B	s�B	v�B	x�B	y�B	z�B	z�B	z�B	z�B	z�B	y�B	x�B	w�B	w�B	w�B	v�B	t�B	v�B	x�B	y�B	z�B	{�B	~�B	�B	�B	�1B	�PB	�bB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�'B	�-B	�'B	�!B	�'B	�'B	�3B	�9B	�9B	�9B	�9B	�9B	�9B	�?B	�?B	�?B	�^B	�dB	�qB	�qB	�wB	�}B	��B	��B	B	ĜB	ĜB	ŢB	ƨB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�=B�BB��B�DBq�BgmBcTBdZBffBe`BffBk�Bn�Bv�Bx�Bz�B� B�B�+B�7B�PB�PB�=B�=B�{B��B��B��B��B��B��B�B�B�FB��BÖBÖBĜBƨBǮBŢBŢBƨBŢBƨBɺB��B��B��B��B��B��B��B��B��BB�9B��B��B�=BhsB`BB_;BVB=qB6FB6FB%�B%B��B��B��B��B�B��B��BȴB�9Bv�BdZBR�BH�BB�BB�B9XB0!B'�B�BJB%B
�fB
��B
��B
��B
B
�dB
�9B
��B
��B
�B
v�B
hsB
ZB
I�B
>wB
/B
 �B
�B
B	�B	�/B	��B	ǮB	��B	�^B	��B	�+B	]/B	R�B	B�B	<jB	6FB	,B	�B	�B	�B	�B	uB	JB	+B	B�B�HB�)B�)B�;B�ZB�B��BȴB��B�wB�9B�'B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�PB�JB�DB�JB�PB�PB�PB�DB�PB�1B�%B�B�B�B�B�B� By�B{�Bz�Bz�Bz�Bx�Bx�Bv�Bt�Bs�Bs�Br�Bt�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bn�Bm�Bn�Bm�BiyBhsBffBgmBffBe`BdZBe`BdZBdZBdZBdZBdZBcTBcTBcTBbNBbNBbNBaHBaHBbNBaHB_;B^5B]/B\)B^5B]/B^5B]/B\)B[#B[#BZBZBZBZBXBYBZBYBZB[#B]/BdZBp�Bs�Bt�Bw�B|�B�B�B�JB�oB�uB��B��B��B��B��B��B��B�B�B�!B�-B�?B�dB��BÖBÖBƨBȴB��B��B�
B�/B�;B�BB�HB�`B�mB�yB�B�B�B�B�mB�fB�ZB�BB�NB�B��B��B��B�B��B	  B��B��B	DB	�B	 �B	$�B	$�B	%�B	%�B	&�B	(�B	(�B	)�B	-B	/B	49B	9XB	;dB	;dB	;dB	;dB	;dB	;dB	;dB	;dB	<jB	>wB	@�B	@�B	@�B	A�B	A�B	C�B	F�B	H�B	J�B	K�B	K�B	J�B	J�B	J�B	K�B	K�B	K�B	M�B	N�B	O�B	R�B	VB	VB	XB	\)B	`BB	bNB	dZB	e`B	ffB	ffB	q�B	s�B	v�B	x�B	y�B	z�B	z�B	z�B	z�B	z�B	y�B	x�B	w�B	w�B	w�B	w�B	u�B	v�B	x�B	y�B	z�B	{�B	� B	�B	�B	�1B	�VB	�bB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�-B	�'B	�-B	�-B	�9B	�9B	�9B	�9B	�9B	�?B	�9B	�FB	�FB	�FB	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
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
<T��<D��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447332012010314473320120103144733  AO  ARGQ                                                                        20111130143017  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143017  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144733  IP                  G�O�G�O�G�O�                