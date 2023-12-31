CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:46Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143411  20190522121828  1727_5046_196                   2C  D   APEX                            2143                            040306                          846 @������1   @��/�@6�E�����c�t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA��A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BY33B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C�C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�3D93D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP�fDQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[3D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df�fDg�Dg��Dh�Dh�fDi�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�G�A�E�A�E�A�A�A�?}A�33A�1'A�$�A�JA�%A���A���A���A�O�A�A�A�1'A�&�A��A���A��A��7A�VA���A���A�bNA���A�p�A�bNA�S�A��hA�ZA�bA��;A���A��DA�n�A�dZA�S�A�oA���A��A���A�A��^A���A��A��-A�VA���A�=qA��FA�`BA�A�ffA�K�A���A�hsA���A�JA���A���A��\A��yA�VA�\)A���A�jA�(�A�&�A�t�A�
=A�ĜA���A���A�z�A�dZA��A��TA�ĜA�z�A�=qA�1A��;A��TA���A�~�A�Q�A�ZA�M�A�VA�ƨA�=qA��HA��uA�\)A�A�A�7LA�33A���A��A��mA�A�A�E�A�A��A}A|VA{�FAz�AzM�Ay�PAx�9AwƨAv��Au�^Au&�Ar�yAlAi;dAeAd �Acx�Aa�A`��A_|�A^1A[p�AZ�AYp�AX�AX5?AWK�AV$�AU�PAUXAS�AR��ARbAQhsAP�`API�AO�FAO+AM�AKt�AH1'AFȴAE;dAB��A@bNA@5?A?�wA>jA<�/A9�A9\)A8n�A5+A4��A4VA3�7A3
=A0E�A.�`A.-A-|�A-;dA,Q�A+C�A*��A)K�A(E�A'C�A&^5A%��A$v�A#dZA"9XA!ƨA!�A ��A 9XA`BA�DA�RAffAA�A{A�A33A9XA7LA(�A/A�mA7LAjA�hA��A�AhsA�A��A-A��AdZA��A��A/A
�`A
��A
A�A	hsA��A��A�`A^5A�jA��AoA �RA&�A �RA ^5A A�A J@�ȴ@��+@��\@�-@�l�@�%@��@�@��D@�Q�@�;d@�5?@�hs@���@� �@�
=@�+@�$�@��T@��@�X@�j@�1@�t�@�@��@�p�@���@�1@��m@��
@���@�P@���@�!@�+@�5?@�{@�J@�@��T@޸R@�\)@�x�@ְ!@�@�o@�n�@�V@�ff@�5?@̴9@�;d@Ȭ@ǍP@�5?@��#@���@�hs@���@���@��P@�%@�Z@� �@�dZ@�v�@�@�{@�Ĝ@�  @�  @��@�ƨ@�C�@�J@���@���@��@��@��@�%@��`@��@��w@�\)@�S�@��y@�$�@�M�@��\@�~�@�ff@��^@�A�@��w@��@���@�\)@�o@�n�@��@��@�|�@�\)@�C�@�"�@��@�ȴ@���@�$�@��T@���@���@���@�l�@��@��y@���@�~�@�-@���@�p�@�?}@���@�(�@��@���@�ƨ@���@�K�@���@�J@��@��7@�V@��@�;d@�5?@��h@�X@�O�@�O�@��@�j@���@��@��H@��+@�{@��T@�@��^@���@��h@�x�@�hs@�?}@��@���@��j@�9X@�t�@�~�@���@��@��T@��#@��-@��7@�X@�7L@��@�V@�V@�V@�V@��`@���@��@�z�@�A�@��@���@�@��y@���@���@��\@�~�@�n�@�^5@�5?@��7@�?}@��9@��m@��;@�ƨ@��w@���@�K�@�;d@�;d@�33@�+@�+@�"�@�"�@�
=@���@��@���@���@�=q@��#@��^@�x�@�O�@��`@��u@�z�@��@��u@�r�@�A�@� �@���@��w@���@��@�t�@�dZ@�;d@���@��@���@���@�~�@�E�@��@���@���@��@�?}@���@��j@�r�@�1'@�1@���@�t�@�K�@�o@��@���@��!@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�G�A�G�A�E�A�E�A�A�A�?}A�33A�1'A�$�A�JA�%A���A���A���A�O�A�A�A�1'A�&�A��A���A��A��7A�VA���A���A�bNA���A�p�A�bNA�S�A��hA�ZA�bA��;A���A��DA�n�A�dZA�S�A�oA���A��A���A�A��^A���A��A��-A�VA���A�=qA��FA�`BA�A�ffA�K�A���A�hsA���A�JA���A���A��\A��yA�VA�\)A���A�jA�(�A�&�A�t�A�
=A�ĜA���A���A�z�A�dZA��A��TA�ĜA�z�A�=qA�1A��;A��TA���A�~�A�Q�A�ZA�M�A�VA�ƨA�=qA��HA��uA�\)A�A�A�7LA�33A���A��A��mA�A�A�E�A�A��A}A|VA{�FAz�AzM�Ay�PAx�9AwƨAv��Au�^Au&�Ar�yAlAi;dAeAd �Acx�Aa�A`��A_|�A^1A[p�AZ�AYp�AX�AX5?AWK�AV$�AU�PAUXAS�AR��ARbAQhsAP�`API�AO�FAO+AM�AKt�AH1'AFȴAE;dAB��A@bNA@5?A?�wA>jA<�/A9�A9\)A8n�A5+A4��A4VA3�7A3
=A0E�A.�`A.-A-|�A-;dA,Q�A+C�A*��A)K�A(E�A'C�A&^5A%��A$v�A#dZA"9XA!ƨA!�A ��A 9XA`BA�DA�RAffAA�A{A�A33A9XA7LA(�A/A�mA7LAjA�hA��A�AhsA�A��A-A��AdZA��A��A/A
�`A
��A
A�A	hsA��A��A�`A^5A�jA��AoA �RA&�A �RA ^5A A�A J@�ȴ@��+@��\@�-@�l�@�%@��@�@��D@�Q�@�;d@�5?@�hs@���@� �@�
=@�+@�$�@��T@��@�X@�j@�1@�t�@�@��@�p�@���@�1@��m@��
@���@�P@���@�!@�+@�5?@�{@�J@�@��T@޸R@�\)@�x�@ְ!@�@�o@�n�@�V@�ff@�5?@̴9@�;d@Ȭ@ǍP@�5?@��#@���@�hs@���@���@��P@�%@�Z@� �@�dZ@�v�@�@�{@�Ĝ@�  @�  @��@�ƨ@�C�@�J@���@���@��@��@��@�%@��`@��@��w@�\)@�S�@��y@�$�@�M�@��\@�~�@�ff@��^@�A�@��w@��@���@�\)@�o@�n�@��@��@�|�@�\)@�C�@�"�@��@�ȴ@���@�$�@��T@���@���@���@�l�@��@��y@���@�~�@�-@���@�p�@�?}@���@�(�@��@���@�ƨ@���@�K�@���@�J@��@��7@�V@��@�;d@�5?@��h@�X@�O�@�O�@��@�j@���@��@��H@��+@�{@��T@�@��^@���@��h@�x�@�hs@�?}@��@���@��j@�9X@�t�@�~�@���@��@��T@��#@��-@��7@�X@�7L@��@�V@�V@�V@�V@��`@���@��@�z�@�A�@��@���@�@��y@���@���@��\@�~�@�n�@�^5@�5?@��7@�?}@��9@��m@��;@�ƨ@��w@���@�K�@�;d@�;d@�33@�+@�+@�"�@�"�@�
=@���@��@���@���@�=q@��#@��^@�x�@�O�@��`@��u@�z�@��@��u@�r�@�A�@� �@���@��w@���@��@�t�@�dZ@�;d@���@��@���@���@�~�@�E�@��@���@���@��@�?}@���@��j@�r�@�1'@�1@���@�t�@�K�@�o@��@���@��!@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�!B�!B�'B�3B�9B�?B�?B�dB��BBĜBŢBǮB��B�B�5B�ZB�B�B��BBBDB2-BP�BcTBs�B{�B�B�%B�=B�DB�JB�%Bl�B[#B)�B{B��B��B�qB�FB�?B�3B�9B�'B�B��B��B�{B�JB{�Bu�Br�Bp�Be`BVBL�B?}B9XB5?B49B2-B$�B1B��B��B��B�yB��BÖB�qB�dB�XB�3B�B��B�7B}�Bq�Bo�BcTBI�B.BJB
��B
�B
�B
�fB
�NB
�;B
�/B
�#B
��B
�3B
��B
��B
x�B
bNB
`BB
T�B
K�B
G�B
A�B
=qB
9XB
7LB
0!B
-B
%�B
 �B
bB	�B	�NB	��B	��B	ȴB	��B	�^B	�3B	��B	��B	��B	��B	�{B	�hB	�JB	�+B	�B	�B	z�B	u�B	s�B	p�B	m�B	jB	ffB	aHB	XB	N�B	C�B	;dB	33B	%�B	�B	�B	�B	uB	
=B	  B��B�B�yB�sB�`B�HB�/B��B��B��BȴBƨBB�wB�^B�9B�!B�B��B��B��B��B��B��B��B�uB�hB�\B�JB�PB�JB�DB�=B�+B�B~�Bz�Bx�Bu�Bt�Bs�Bq�Bo�Bm�Bm�Bm�Bl�Bk�Bk�BjBjBiyBiyBhsBhsBgmBffBe`BcTBaHB`BB^5B`BB_;B`BBjBv�By�Bx�Bx�Bx�Bz�B{�B|�B�B�B�B�B�1B�VB�PB�VB�bB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�uB��B��B��B��B��B��B��B��B�9B�LB�LB�RB�jB�jB�jB�^B�RB�LB�9B�-B�-B�-B�?B�FB�^B�qB��BÖB��B��B��B��B��B��B��B�
B�B�)B�)B�)B�/B�HB�ZB�mB�B�B�yB�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	
=B	DB	DB	bB	�B	�B	�B	!�B	#�B	)�B	.B	0!B	0!B	1'B	2-B	7LB	9XB	<jB	<jB	>wB	A�B	G�B	K�B	L�B	N�B	Q�B	XB	^5B	gmB	n�B	p�B	q�B	q�B	r�B	u�B	|�B	�B	�B	�%B	�+B	�1B	�1B	�1B	�1B	�1B	�7B	�7B	�7B	�=B	�=B	�DB	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�!B	�!B	�'B	�'B	�-B	�-B	�-B	�-B	�?B	�FB	�RB	�qB	�qB	�qB	�qB	�qB	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�!B�!B�'B�3B�9B�?B�LB�qB��BBĜBŢBȴB��B�B�;B�fB�B�B��BB1BDB1'BQ�BdZBt�B|�B�B�%B�=B�DB�PB�DBp�Be`B.B�BB�B��B�RB�RB�FB�LB�-B�B�B�B��B�bB~�Bw�Bt�Bs�BjBYBP�BB�B;dB8RB6FB8RB.B
=B��B��B��B�B�BŢB�qB�jB�dB�9B�-B��B�VB�1Bt�Bv�Bn�BP�B9XB�B
��B
�B
�B
�mB
�NB
�;B
�/B
�)B
�/B
�RB
��B
��B
�B
cTB
e`B
XB
M�B
I�B
C�B
?}B
;dB
9XB
2-B
0!B
'�B
&�B
!�B	��B	�B	�B	��B	��B	ĜB	�wB	�RB	�-B	��B	��B	��B	��B	�{B	�bB	�7B	�B	�+B	~�B	w�B	u�B	r�B	o�B	l�B	hsB	hsB	^5B	ZB	G�B	@�B	:^B	+B	�B	�B	�B	�B	hB	B��B��B�B�yB�mB�TB�ZB�B��B��BɺBɺBŢB��B�wB�LB�3B�!B�B�B��B��B��B��B��B��B�{B�oB�oB�VB�PB�JB�VB�PB�%B�B}�Bz�Bx�Bv�Bu�Bs�Br�Bp�Bn�Bn�Bm�Bl�Bl�Bk�Bl�Bl�BjBiyBiyBiyBhsBgmBffBcTBbNBcTBffBdZBaHBiyBw�Bz�Bx�By�Bz�Bz�B{�B}�B�B�1B�1B�=B�JB�\B�\B�bB�hB�oB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B�?B�LB�RB�^B�wB��B��B�dB�RB�RB�FB�3B�-B�9B�FB�FB�^B�qB��BŢB��B��B��B��B��B��B��B�B�B�/B�)B�/B�5B�HB�ZB�mB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	+B	
=B	DB	PB	oB	�B	�B	�B	!�B	#�B	+B	/B	0!B	0!B	1'B	33B	7LB	9XB	<jB	<jB	>wB	B�B	H�B	K�B	M�B	O�B	S�B	YB	`BB	hsB	n�B	p�B	q�B	q�B	s�B	v�B	}�B	�B	�B	�+B	�+B	�1B	�1B	�1B	�1B	�1B	�7B	�7B	�7B	�=B	�=B	�JB	�PB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�!B	�!B	�!B	�'B	�'B	�-B	�-B	�-B	�3B	�?B	�LB	�XB	�qB	�qB	�qB	�qB	�qB	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�HB	�NB	�NB	�NB	�NB	�TB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447432012010314474320120103144743  AO  ARGQ                                                                        20111130143411  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143411  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144743  IP                  G�O�G�O�G�O�                