CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:48Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143443  20190522121828  1727_5046_203                   2C  D   APEX                            2143                            040306                          846 @��ۿ�1   @����
@7d�/���c�ě��T1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp�fDqfDq� Dq��Dr� Ds  Ds� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Fff@�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@ffBH��BPffBX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CHL�CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D�3D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"�3D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7fD7�fD8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM�fDN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��DofDo��Dp�Dp�3Dq3Dq��DrfDr��Ds�Ds��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��/A��TA��`A��`A��`A��`A��`A��mA��`A��mA��mA��`A��`A��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�%A�1A�1A�1A�1A�JA�bA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��9A�;dA��
A���A�+A�{A��HA�^5A��A���A��A�dZA�A�A��A��PA��A�\)A�ffA��`A��A�C�A�
=A���A�n�A�(�A�VA��yA�1A���A���A�K�A���A�7LA��
A��jA�O�A���A�bA��A���A�I�A���A��RA�+A���A���A�oA��DA�-A/A}�
Az��Aw�^Au�PAtr�Ar�/Aq��Ap�+Ao33Al�AkdZAj��AiƨAi33Ah�HAh��AhVAf$�Ae�wAe`BAd�Ac��AbffA_+A\jA[��AZ�!AY�wAX�AW�hAV�uAT�9AS�hAQ��AOALr�AIhsAGXAF$�AE�7ADAC��AC�7ABJA@��A>�+A=p�A:�HA9hsA8�yA85?A7+A5��A4{A1�
A0�RA0��A0�A0n�A0-A.�/A.9XA.A-��A-|�A,�A+O�A*ȴA)�
A(�\A'ƨA&�`A&  A%hsA$v�A#|�A"$�A �9A ^5A�
At�A%AM�A��A;dAM�Ap�A5?A��A�AK�A�A�DAO�A�A1A7LA��AbA�PA+A��At�A
A�!A�A��AVA�yAJA�FAK�A ��A Z@��@�n�@���@���@�C�@��y@���@��@��y@�x�@��@�\)@�n�@�{@�-@�/@��/@�b@��@�|�@��@�hs@���@웦@�(�@띲@�@�~�@��@�Z@�=q@�@���@�C�@܃@�(�@�+@�33@��
@�V@ٺ^@��m@�{@��#@�@�/@��@�=q@��@ѩ�@щ7@�b@͉7@˾w@��H@��@�\)@��T@�1@�5?@��@��j@�r�@��@�S�@�ff@�@��`@�l�@�/@���@���@�n�@�p�@���@���@���@��@�l�@��@��@��H@�@��^@��#@�{@�E�@��@��@��@��@��9@�j@�  @��F@�dZ@�;d@�@���@�=q@�E�@�E�@�=q@�$�@���@�p�@�`B@�G�@��@���@���@��D@�j@�Q�@�1'@�(�@�(�@� �@�b@���@��@���@��+@�^5@�M�@�E�@��#@��@�j@�Q�@�(�@�t�@�K�@�;d@�
=@��@�ȴ@��\@��@���@��@�I�@�ƨ@��
@��P@��@��@��+@�-@�@���@��7@��@��@�z�@�I�@�I�@�A�@�9X@�1@�ƨ@��F@�dZ@�o@��!@�{@�@���@�x�@��`@�j@�j@�j@�Z@�9X@�b@��
@��
@��F@��P@��@�o@��R@�ff@�V@�V@��@��T@��-@�p�@�O�@�V@���@�1'@��F@�K�@�"�@��@��+@��^@�`B@�O�@�?}@�/@���@���@�bN@�1'@��@���@�K�@���@��H@���@�E�@�$�@�{@��@��@��h@�x�@�p�@�G�@�/@�&�@��@��@���@���@��@�ƨ@���@���@�dZ@�+@�@��R@�~�@��T@���@��@�X@�G�@�G�@���@���@�j@�1'@��@���@�|�@�S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��/A��TA��`A��`A��`A��`A��`A��mA��`A��mA��mA��`A��`A��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�%A�1A�1A�1A�1A�JA�bA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��9A�;dA��
A���A�+A�{A��HA�^5A��A���A��A�dZA�A�A��A��PA��A�\)A�ffA��`A��A�C�A�
=A���A�n�A�(�A�VA��yA�1A���A���A�K�A���A�7LA��
A��jA�O�A���A�bA��A���A�I�A���A��RA�+A���A���A�oA��DA�-A/A}�
Az��Aw�^Au�PAtr�Ar�/Aq��Ap�+Ao33Al�AkdZAj��AiƨAi33Ah�HAh��AhVAf$�Ae�wAe`BAd�Ac��AbffA_+A\jA[��AZ�!AY�wAX�AW�hAV�uAT�9AS�hAQ��AOALr�AIhsAGXAF$�AE�7ADAC��AC�7ABJA@��A>�+A=p�A:�HA9hsA8�yA85?A7+A5��A4{A1�
A0�RA0��A0�A0n�A0-A.�/A.9XA.A-��A-|�A,�A+O�A*ȴA)�
A(�\A'ƨA&�`A&  A%hsA$v�A#|�A"$�A �9A ^5A�
At�A%AM�A��A;dAM�Ap�A5?A��A�AK�A�A�DAO�A�A1A7LA��AbA�PA+A��At�A
A�!A�A��AVA�yAJA�FAK�A ��A Z@��@�n�@���@���@�C�@��y@���@��@��y@�x�@��@�\)@�n�@�{@�-@�/@��/@�b@��@�|�@��@�hs@���@웦@�(�@띲@�@�~�@��@�Z@�=q@�@���@�C�@܃@�(�@�+@�33@��
@�V@ٺ^@��m@�{@��#@�@�/@��@�=q@��@ѩ�@щ7@�b@͉7@˾w@��H@��@�\)@��T@�1@�5?@��@��j@�r�@��@�S�@�ff@�@��`@�l�@�/@���@���@�n�@�p�@���@���@���@��@�l�@��@��@��H@�@��^@��#@�{@�E�@��@��@��@��@��9@�j@�  @��F@�dZ@�;d@�@���@�=q@�E�@�E�@�=q@�$�@���@�p�@�`B@�G�@��@���@���@��D@�j@�Q�@�1'@�(�@�(�@� �@�b@���@��@���@��+@�^5@�M�@�E�@��#@��@�j@�Q�@�(�@�t�@�K�@�;d@�
=@��@�ȴ@��\@��@���@��@�I�@�ƨ@��
@��P@��@��@��+@�-@�@���@��7@��@��@�z�@�I�@�I�@�A�@�9X@�1@�ƨ@��F@�dZ@�o@��!@�{@�@���@�x�@��`@�j@�j@�j@�Z@�9X@�b@��
@��
@��F@��P@��@�o@��R@�ff@�V@�V@��@��T@��-@�p�@�O�@�V@���@�1'@��F@�K�@�"�@��@��+@��^@�`B@�O�@�?}@�/@���@���@�bN@�1'@��@���@�K�@���@��H@���@�E�@�$�@�{@��@��@��h@�x�@�p�@�G�@�/@�&�@��@��@���@���@��@�ƨ@���@���@�dZ@�+@�@��R@�~�@��T@���@��@�X@�G�@�G�@���@���@�j@�1'@��@���@�|�@�S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBcTBdZBe`Be`Be`BffBffBffBgmBhsBhsBiyBiyBiyBhsBgmB`BBXBO�BI�B+B��B�HB��B�}By�BVB8RB�B�B{BVBB��B�`B�B��B��B��BƨB��B�'B��B�oB�Bu�Bt�Bn�BhsBcTBQ�B?}B7LB(�B�BB
�B
�B
��B
��B
�VB
�B
y�B
q�B
hsB
bNB
]/B
D�B
/B
"�B
�B
uB
JB
B	��B	�B	�B	�B	�`B	�NB	�BB	�5B	�B	��B	��B	��B	ȴB	��B	�LB	��B	��B	��B	�bB	�JB	�+B	�B	|�B	s�B	n�B	dZB	VB	?}B	+B	�B	{B	VB	B	  B	  B��B�B�fB�HB�B��B��B��B��BǮBB�^B�dB�^B�^B�XB�FB�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B�uB�VB�VB�VB�PB�JB�=B�1B�%B�+B�B�B�+B� B~�B~�B}�B{�By�Bx�By�Bx�Bx�Bw�Bw�Bv�Bu�Br�Bp�Bo�Bl�Bl�Bk�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bm�Bm�Bl�Bm�Bn�Bn�Bp�Bq�Bq�Br�Br�Br�Bs�Bs�Br�Bs�Bt�Bu�Bu�Bt�Bt�Bs�Bs�Br�Br�Bo�Bm�BjBbNBcTBjBjBp�Bx�Bz�B|�B~�B~�B~�B~�B�B~�B}�B|�B}�B�%B�1B�%B�B�B�1B�JB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�9B�RB�jB�}B�dB�qB��BĜB��B��B�B�NB�TB�`B�B��B��B��B��B��B	B	B	B	%B		7B		7B		7B		7B	
=B	PB	\B	\B	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	%�B	'�B	'�B	'�B	+B	1'B	49B	49B	5?B	:^B	;dB	;dB	<jB	=qB	>wB	>wB	B�B	D�B	E�B	H�B	K�B	K�B	L�B	O�B	Q�B	W
B	YB	ZB	YB	YB	\)B	`BB	aHB	bNB	bNB	bNB	bNB	cTB	ffB	gmB	iyB	k�B	n�B	r�B	t�B	u�B	u�B	x�B	|�B	|�B	|�B	}�B	}�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�=B	�VB	�\B	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�?B	�LB	�LB	�RB	�^B	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�wB	��B	B	ĜB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBcTBdZBe`Be`Be`BffBffBffBgmBhsBhsBiyBiyBiyBhsBiyBbNBYBP�BN�B6FB��B�TB�
B��B�BbNBC�B �B�B�BhB	7B��B�B�B�B��B��BȴBǮB�RB��B��B�Bw�Bv�Bo�BiyBgmBW
B@�B:^B.B�BJB
�fB
�9B
��B
��B
�oB
�+B
|�B
u�B
iyB
ffB
ffB
L�B
5?B
%�B
 �B
�B
\B
1B
B	��B	�B	�B	�mB	�TB	�BB	�BB	�BB	��B	��B	��B	��B	ƨB	��B	�B	��B	��B	�uB	�\B	�=B	�B	�B	v�B	r�B	hsB	]/B	F�B	0!B	"�B	�B	oB	B	B	B��B��B�yB�yB�5B�B��B��B��B��BɺB�qB�dB�^B�^B�^B�^B�9B�-B�'B�!B�!B�B��B��B��B��B��B��B��B��B��B��B�hB�\B�\B�VB�PB�JB�DB�=B�7B�+B�+B�DB�B�B� B� B� B~�B|�B{�Bz�Bz�By�Bx�Bw�By�Bv�Bt�Bt�Bp�Bp�Bo�Bo�Bn�Bn�Bn�Bn�Bo�Bo�Bn�Bo�Bo�Bo�Bn�Bo�Bp�Bo�Bo�Bq�Br�Br�Br�Bs�Bs�Bs�Bt�Bs�Bt�Bu�Bu�Bu�Bv�Bu�Bu�Bt�Bu�Bt�Br�Bq�Bq�Bm�BffBdZBk�BjBo�Bz�B{�B� B�B~�B~�B� B�B� B}�B}�B}�B�1B�JB�7B�+B�+B�DB�\B�uB��B��B��B��B��B��B��B��B�B�B��B��B�B�B�B�B�B�FB�RB�qBĜB�jB�}BBŢB��B��B�B�TB�ZB�ZB�B��B��B��B��B��B	B	B	B	+B		7B		7B		7B		7B	DB	PB	\B	\B	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	%�B	'�B	'�B	(�B	,B	2-B	49B	49B	5?B	:^B	;dB	;dB	<jB	=qB	>wB	?}B	C�B	E�B	F�B	I�B	K�B	L�B	M�B	O�B	Q�B	W
B	YB	ZB	YB	ZB	]/B	`BB	aHB	bNB	bNB	bNB	bNB	cTB	ffB	gmB	iyB	l�B	o�B	r�B	t�B	u�B	v�B	y�B	|�B	|�B	|�B	}�B	}�B	�B	�B	�B	�B	�%B	�1B	�7B	�1B	�7B	�=B	�DB	�VB	�\B	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�?B	�LB	�LB	�RB	�dB	�qB	�qB	�jB	�jB	�jB	�jB	�qB	�wB	�}B	�}B	��B	B	ĜB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�C�<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447452012010314474520120103144745  AO  ARGQ                                                                        20111130143443  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143443  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144745  IP                  G�O�G�O�G�O�                