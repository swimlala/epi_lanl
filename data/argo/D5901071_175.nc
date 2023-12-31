CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:40Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143119  20190522121827  1727_5046_175                   2C  D   APEX                            2143                            040306                          846 @����-�1   @��؂�?�@5|�hr��c�I�^51   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B ��B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds` Dy@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�ffA33A#33AC33Ac33A���A���A���A���A�ffAљ�AᙚA�B ��B��B��B��B!��B(��B0ffB8ffB@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33CL�C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD�CF33CH33CJ33CL33CN33CPL�CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%fD%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3�fD4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY3DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db�3Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Dsl�DyL�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A׶FA׬A׬Aװ!A׬A�/A���A���A��A�ƨA֧�A�XA�"�A�oA���A���A�C�AԸRA�E�A��
A�33A�|�A��A��
A�=qAĬA��A�1'A��A�~�A��`A��DA�^5A�1A��A��PA�5?A���A�K�A��A�z�A�oA���A�v�A�ĜA���A��\A��A���A�$�A���A��A�M�A�I�A���A�r�A�jA�l�A��#A���A�&�A��yA�t�A��A��!A�9XA���A��jA�A�A�5?A��wA�O�A��DA��A�(�A���A���A�G�A��A�~�A�+A��RA��A���A�{A�ȴA���A�bNA�"�A��
A��A��uA�E�A�A��A�oA�JA�1A��/A�&�A���A��wA���A�p�A�dZA�ȴA�E�A��+A�
A}�;A|AzAv��Au�At�Asl�Ar �Am�Ak`BAh{AfĜAf�jAc�mA`��A]�FA[|�AXz�AV��AU��AQƨAPAMdZAJ�jAJ$�AI�;AI�#AI�AH��AF^5AD��AA�7A@�jA@-A?�FA?�A>-A=�#A=hsA:�jA8�A89XA7p�A6�\A4M�A2��A0M�A/dZA.�uA,��A+��A+��A+33A*��A*ffA(�A'?}A&�uA$�A#��A"jA!�A �\A   A��A(�A`BA��A��A"�A(�A�AdZA|�AbA&�A�uAVA��At�A�A`BA	x�A=qAO�A��A��A�9A�A-AI�A�A�mA�#A��A�FAdZA E�@��@���@�J@���@�%@�t�@�^5@��@�=q@�9@�bN@�  @�\)@�@��@�p�@�/@�%@��@�V@��@�Ĝ@���@�=q@�j@�  @�ƨ@�"�@���@ݡ�@ܼj@�S�@�{@�?}@�7L@�/@��`@�;d@�=q@Ցh@���@Դ9@��@�@�5?@д9@�(�@�\)@��@ΰ!@�-@ͺ^@�X@̣�@˥�@�-@�`B@�bN@��@��@ư!@�=q@��@ź^@�hs@��@Ĵ9@�j@�9X@ě�@î@��H@�v�@�=q@��7@��D@��F@��y@��!@���@��@��/@�j@�9X@�\)@�M�@�x�@���@���@�l�@��@�
=@��@�M�@��@��^@���@�b@�l�@�ȴ@�@�p�@��7@�x�@�&�@��u@�"�@���@�@�`B@�z�@�1@��@��y@��+@�J@��T@���@��@�`B@�7L@��`@�A�@��F@�l�@�C�@�o@�=q@���@�O�@�&�@��@�V@��@��`@���@�Ĝ@��@���@��u@�j@�Z@�1'@��@�  @��m@��;@��w@��@�ff@�=q@�-@�@�@��@���@�z�@�Z@�9X@�b@�  @�ƨ@�dZ@�33@�;d@�S�@��H@�v�@�E�@�$�@��@��h@�X@��`@��9@�bN@� �@�33@�^5@��^@�z�@�b@�  @�t�@��@���@���@�~�@��\@�ff@�E�@�-@�M�@�ff@�V@�x�@��9@��j@��@�9X@�Q�@�bN@��9@�`B@�&�@���@�z�@�1@�|�@�;d@�ȴ@�|�@�J@���@���@���@�@��@��y@��y@��H@�ȴ@��!@��+@�M�@�J@���@�%@��D@��
@��@�^5@��#@��-@���@��h@���@�j@�9X@�b@��P@�\)@�K�@���@�=q@��@��T@��^@�hs@��@�%@��@�Z@���@��;@�ƨ@��@���@���@��P@�l�@�S�@�33@��@�@��y@�v�@���@���@���@���@�@���@�p�@�O�@�?}@�%@��@�Q�@�ȴ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A׶FA׬A׬Aװ!A׬A�/A���A���A��A�ƨA֧�A�XA�"�A�oA���A���A�C�AԸRA�E�A��
A�33A�|�A��A��
A�=qAĬA��A�1'A��A�~�A��`A��DA�^5A�1A��A��PA�5?A���A�K�A��A�z�A�oA���A�v�A�ĜA���A��\A��A���A�$�A���A��A�M�A�I�A���A�r�A�jA�l�A��#A���A�&�A��yA�t�A��A��!A�9XA���A��jA�A�A�5?A��wA�O�A��DA��A�(�A���A���A�G�A��A�~�A�+A��RA��A���A�{A�ȴA���A�bNA�"�A��
A��A��uA�E�A�A��A�oA�JA�1A��/A�&�A���A��wA���A�p�A�dZA�ȴA�E�A��+A�
A}�;A|AzAv��Au�At�Asl�Ar �Am�Ak`BAh{AfĜAf�jAc�mA`��A]�FA[|�AXz�AV��AU��AQƨAPAMdZAJ�jAJ$�AI�;AI�#AI�AH��AF^5AD��AA�7A@�jA@-A?�FA?�A>-A=�#A=hsA:�jA8�A89XA7p�A6�\A4M�A2��A0M�A/dZA.�uA,��A+��A+��A+33A*��A*ffA(�A'?}A&�uA$�A#��A"jA!�A �\A   A��A(�A`BA��A��A"�A(�A�AdZA|�AbA&�A�uAVA��At�A�A`BA	x�A=qAO�A��A��A�9A�A-AI�A�A�mA�#A��A�FAdZA E�@��@���@�J@���@�%@�t�@�^5@��@�=q@�9@�bN@�  @�\)@�@��@�p�@�/@�%@��@�V@��@�Ĝ@���@�=q@�j@�  @�ƨ@�"�@���@ݡ�@ܼj@�S�@�{@�?}@�7L@�/@��`@�;d@�=q@Ցh@���@Դ9@��@�@�5?@д9@�(�@�\)@��@ΰ!@�-@ͺ^@�X@̣�@˥�@�-@�`B@�bN@��@��@ư!@�=q@��@ź^@�hs@��@Ĵ9@�j@�9X@ě�@î@��H@�v�@�=q@��7@��D@��F@��y@��!@���@��@��/@�j@�9X@�\)@�M�@�x�@���@���@�l�@��@�
=@��@�M�@��@��^@���@�b@�l�@�ȴ@�@�p�@��7@�x�@�&�@��u@�"�@���@�@�`B@�z�@�1@��@��y@��+@�J@��T@���@��@�`B@�7L@��`@�A�@��F@�l�@�C�@�o@�=q@���@�O�@�&�@��@�V@��@��`@���@�Ĝ@��@���@��u@�j@�Z@�1'@��@�  @��m@��;@��w@��@�ff@�=q@�-@�@�@��@���@�z�@�Z@�9X@�b@�  @�ƨ@�dZ@�33@�;d@�S�@��H@�v�@�E�@�$�@��@��h@�X@��`@��9@�bN@� �@�33@�^5@��^@�z�@�b@�  @�t�@��@���@���@�~�@��\@�ff@�E�@�-@�M�@�ff@�V@�x�@��9@��j@��@�9X@�Q�@�bN@��9@�`B@�&�@���@�z�@�1@�|�@�;d@�ȴ@�|�@�J@���@���@���@�@��@��y@��y@��H@�ȴ@��!@��+@�M�@�J@���@�%@��D@��
@��@�^5@��#@��-@���@��h@���@�j@�9X@�b@��P@�\)@�K�@���@�=q@��@��T@��^@�hs@��@�%@��@�Z@���@��;@�ƨ@��@���@���@��P@�l�@�S�@�33@��@�@��y@�v�@���@���@���@���@�@���@�p�@�O�@�?}@�%@��@�Q�@�ȴ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB^5B^5B]/B]/B]/Bv�B~�B~�B�B�1B�=B�hB�{B��B��B��B��B�RB�LB��B��B�`B�FB�B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�XBĜB��B�B��B�fB�ZB�mB�B�B�ZB�B�B��BȴB��B�9B�B��B��B��B��B��B�hB�B|�Bv�Bo�BjBffBaHB[#BVBP�BL�B@�B7LB7LB(�B�BB��B�B��B�9B� Bn�BVBI�BJ�BC�B/B�BbB	7BB
��B
�ZB
�
B
ŢB
�9B
��B
�bB
�PB
�7B
�B
�B
�B
z�B
p�B
gmB
W
B
F�B
8RB
'�B
�B
	7B
B	��B	�B	�fB	��B	�dB	�dB	ŢB	�'B	�\B	r�B	aHB	R�B	R�B	`BB	W
B	I�B	9XB	$�B	$�B	$�B	$�B	!�B	�B	
=B	B��B�B�B�B�yB�`B�TB�5B��B��B��BȴBÖB�dB�9B�9B�B�B��B��B��B��B��B��B��B��B��B��B�oB�hB�\B�PB�JB�=B�1B�+B�B�B�B~�B|�By�Bv�Bt�Bt�Bq�Bn�Bo�Bo�Bl�BhsBdZBcTBbNBbNBaHBaHB_;B\)B^5B^5B^5B^5B^5B]/B\)B^5B^5B^5B\)BZBYBXBT�BS�BR�BS�BS�BS�BT�BS�BXBZBZBXBYBYBZBZBZB\)BbNBcTBcTB`BBffBm�Bp�Bs�Bw�B{�B{�B{�Bz�B� B� B~�B�B�B�B�1B�=B�JB�PB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�3B�LB�^B�}B��BBÖBĜBƨB��B��B�B�B�/B�;B�)B�NB�yB�B�B�B��B��B��B��B��B��B��B	B	B		7B	JB	hB	uB	�B	�B	 �B	"�B	#�B	%�B	-B	0!B	33B	5?B	;dB	=qB	D�B	E�B	G�B	J�B	K�B	L�B	M�B	N�B	O�B	Q�B	W
B	ZB	[#B	[#B	[#B	^5B	bNB	e`B	gmB	gmB	gmB	hsB	\)B	hsB	hsB	iyB	iyB	iyB	jB	iyB	jB	jB	k�B	k�B	k�B	l�B	q�B	s�B	t�B	u�B	v�B	u�B	u�B	v�B	w�B	v�B	w�B	w�B	x�B	y�B	y�B	|�B	}�B	~�B	�B	�+B	�+B	�1B	�1B	�=B	�\B	�bB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�LB	�LB	�FB	�9B	�-B	�'B	�!B	�!B	�LB	�9B	�FB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�;B	�ZB	�`B	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B^5B^5B]/B]/B_;Bw�B~�B~�B�B�1B�DB�oB�{B��B��B��B��B�jB�dB��B��B�B�^B�B��B��B��B��B��B��B��B��B��B��B�B�?B�^B�}BɺB��B��B��B�B�mB�B�B�B�B�B�B��B��BŢB�^B�B��B��B�B��B��B��B�%B~�By�Bq�Bk�BgmBcTB_;BXBR�BO�BC�B9XB9XB-B�BB��B�B�B��B�%Bw�B]/BI�BM�BJ�B7LB#�BoBDB%B  B
�B
�5B
��B
B
��B
�hB
�\B
�=B
�%B
�B
�B
~�B
w�B
r�B
^5B
L�B
>wB
0!B
�B
DB
1B	��B	��B	�B	�B	�wB	�dB	��B	�XB	��B	x�B	hsB	W
B	W
B	jB	\)B	Q�B	@�B	&�B	%�B	$�B	%�B	%�B	�B	\B		7B��B��B�B�B�B�fB�ZB�`B�B��B��B��BɺB�}B�dB�LB�'B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�\B�JB�=B�7B�+B�%B�B�B�B~�Bz�Bw�Bv�Bu�Bu�Bs�Br�Bp�Bn�Bn�BffBcTBcTBbNBbNBaHBcTB_;B^5B^5B^5B_;B^5B`BB`BB_;B_;B`BB\)B[#BZBXBW
BT�BT�BT�BT�BVBVBYBZBZBZB[#BZB\)B\)B]/B_;BcTBdZBe`BffBjBo�Bs�Bu�By�B{�B{�B|�B}�B�B�B� B�B�B�%B�=B�PB�PB�\B�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�LB�^B��BBÖBÖBŢBǮB��B��B�B�B�5B�HB�/B�NB�B�B�B�B��B��B��B��B��B��B��B	B	%B	
=B	PB	oB	�B	�B	�B	 �B	"�B	$�B	&�B	.B	1'B	49B	6FB	<jB	>wB	D�B	F�B	H�B	J�B	K�B	L�B	M�B	N�B	O�B	Q�B	XB	ZB	[#B	[#B	\)B	_;B	cTB	e`B	gmB	gmB	gmB	hsB	\)B	hsB	hsB	iyB	iyB	iyB	jB	iyB	jB	jB	k�B	k�B	k�B	n�B	r�B	s�B	t�B	u�B	v�B	v�B	v�B	v�B	w�B	v�B	w�B	w�B	x�B	z�B	y�B	|�B	}�B	� B	�B	�+B	�+B	�1B	�7B	�=B	�bB	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�LB	�RB	�LB	�?B	�3B	�'B	�'B	�B	�XB	�9B	�?B	�dB	�}B	��B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ŢB	ƨB	ǮB	ŢB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�BB	�ZB	�`B	�`B	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447352012010314473520120103144735  AO  ARGQ                                                                        20111130143119  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143119  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144735  IP                  G�O�G�O�G�O�                