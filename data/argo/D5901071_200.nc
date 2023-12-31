CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:47Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143429  20190522121828  1727_5046_200                   2C  D   APEX                            2143                            040306                          846 @�n]?�1   @���@7-�����c� ě��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dss3Dz  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
L�C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(�C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D3D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�fD$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)�fD*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5fD5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds� Dz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A���A��A��A���A���A���A�A���A�A�JA�VA�bA�bA�bA�VA�bA�bA�bA�oA�JA��A���A�JA��A���A���A���A���A��+A�v�A�n�A�bNA�ZA�O�A�?}A�=qA�7LA�1'A�-A�-A�(�A�"�A��A��A�oA�JA�
=A�A��HA���A��A�~�A�M�A��A��A���A�/A�A�?}A�`BA�A�A�$�A��PA��A��mA��A��A���A�v�A���A��HA���A�1A���A���A��9A��A��A�hsA� �A�?}A�v�A�\)A��#A�?}A��A�`BA�
=A���A�A��hA�7LA��DA�ZA�?}A��!A�+A���A�bA�`BA�%A���A�(�A��FA�{A�ffA�+A��hA���A�$�A���At�A}l�A{\)AyAw�^Av�Au�At�+As�ArA�Aq��Aq|�Apv�Ao
=AlVAiG�Ah �AeO�Ac�-AbȴA_`BA^  A[p�AYAX5?AVI�AT5?AR�DAO�wAM��AK��AI��AG��AG33AF��AE�AC��AAS�A@  A>A�A<(�A:VA6A4A3+A2{A.�!A-�mA,�A*�+A(�jA(  A'��A&^5A#p�A"ȴA"�!A"v�A -A�/A�\AJA��A�hA&�A��A-AA��A�A�uAK�A�!A�A�!A�-Ar�A��A�PA�A�uA �A7LA�TA"�A�HAr�A�7A
�9A	%AI�A�A
=Ax�A  A�A^5AbAK�@�C�@���@��@�^5@��@��@���@��;@�M�@��@�z�@�@�@��
@��m@�Z@�dZ@�ȴ@�G�@�+@�J@�O�@�@�@�(�@���@�@�K�@◍@�J@��@��@�|�@���@�M�@�J@݁@��@ܼj@�z�@�j@�b@�
=@��@�j@��H@�J@�@ա�@��@�(�@�v�@Л�@��@Ͼw@�o@�-@��@��
@ʸR@���@ǍP@�ff@�`B@�  @�S�@���@��#@��u@��@�l�@��@��@�b@�o@��T@���@�z�@�A�@���@�o@��R@�5?@��#@��@�`B@�&�@��`@�r�@�b@�dZ@�o@��@�ff@�=q@�@�/@��@��@�Ĝ@���@�bN@��m@�"�@�ȴ@�^5@�E�@�E�@�=q@�{@���@��#@�@�G�@��u@�b@��;@��@�|�@��y@�ȴ@�M�@�5?@��@��h@�`B@�O�@��@��9@�j@�1'@�  @��w@�l�@�;d@��@��y@��\@�n�@�=q@�J@���@��@���@�7L@��@�bN@��@���@���@�l�@��y@���@��+@�^5@�=q@�{@���@�V@��@��D@�I�@�9X@� �@���@�ƨ@��@�\)@��@��@���@���@��@���@��h@�G�@�/@���@�(�@��m@��;@�ƨ@��F@��@�ff@�^5@�ff@�E�@��@�X@��@���@��`@���@�1@��P@�33@��@��R@��@��@���@��`@�%@��`@�Q�@�b@���@���@���@�ff@���@�`B@���@��/@�Ĝ@�1'@��@��y@��y@��+@�J@��@�@�J@��#@��T@��@�J@��T@�O�@��@�/@��@���@���@�Z@���@�G�@��`@�r�@�r�@�Q�@�b@��@�1'@� �@���@���@��@�1'@� �@�@���@�@���@��y@���@��!@���@�=q@���@�O�@�?}@��@���@���@��D@�j@�Z@�1'@��
@�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�A�A���A��A��A���A���A���A�A���A�A�JA�VA�bA�bA�bA�VA�bA�bA�bA�oA�JA��A���A�JA��A���A���A���A���A��+A�v�A�n�A�bNA�ZA�O�A�?}A�=qA�7LA�1'A�-A�-A�(�A�"�A��A��A�oA�JA�
=A�A��HA���A��A�~�A�M�A��A��A���A�/A�A�?}A�`BA�A�A�$�A��PA��A��mA��A��A���A�v�A���A��HA���A�1A���A���A��9A��A��A�hsA� �A�?}A�v�A�\)A��#A�?}A��A�`BA�
=A���A�A��hA�7LA��DA�ZA�?}A��!A�+A���A�bA�`BA�%A���A�(�A��FA�{A�ffA�+A��hA���A�$�A���At�A}l�A{\)AyAw�^Av�Au�At�+As�ArA�Aq��Aq|�Apv�Ao
=AlVAiG�Ah �AeO�Ac�-AbȴA_`BA^  A[p�AYAX5?AVI�AT5?AR�DAO�wAM��AK��AI��AG��AG33AF��AE�AC��AAS�A@  A>A�A<(�A:VA6A4A3+A2{A.�!A-�mA,�A*�+A(�jA(  A'��A&^5A#p�A"ȴA"�!A"v�A -A�/A�\AJA��A�hA&�A��A-AA��A�A�uAK�A�!A�A�!A�-Ar�A��A�PA�A�uA �A7LA�TA"�A�HAr�A�7A
�9A	%AI�A�A
=Ax�A  A�A^5AbAK�@�C�@���@��@�^5@��@��@���@��;@�M�@��@�z�@�@�@��
@��m@�Z@�dZ@�ȴ@�G�@�+@�J@�O�@�@�@�(�@���@�@�K�@◍@�J@��@��@�|�@���@�M�@�J@݁@��@ܼj@�z�@�j@�b@�
=@��@�j@��H@�J@�@ա�@��@�(�@�v�@Л�@��@Ͼw@�o@�-@��@��
@ʸR@���@ǍP@�ff@�`B@�  @�S�@���@��#@��u@��@�l�@��@��@�b@�o@��T@���@�z�@�A�@���@�o@��R@�5?@��#@��@�`B@�&�@��`@�r�@�b@�dZ@�o@��@�ff@�=q@�@�/@��@��@�Ĝ@���@�bN@��m@�"�@�ȴ@�^5@�E�@�E�@�=q@�{@���@��#@�@�G�@��u@�b@��;@��@�|�@��y@�ȴ@�M�@�5?@��@��h@�`B@�O�@��@��9@�j@�1'@�  @��w@�l�@�;d@��@��y@��\@�n�@�=q@�J@���@��@���@�7L@��@�bN@��@���@���@�l�@��y@���@��+@�^5@�=q@�{@���@�V@��@��D@�I�@�9X@� �@���@�ƨ@��@�\)@��@��@���@���@��@���@��h@�G�@�/@���@�(�@��m@��;@�ƨ@��F@��@�ff@�^5@�ff@�E�@��@�X@��@���@��`@���@�1@��P@�33@��@��R@��@��@���@��`@�%@��`@�Q�@�b@���@���@���@�ff@���@�`B@���@��/@�Ĝ@�1'@��@��y@��y@��+@�J@��@�@�J@��#@��T@��@�J@��T@�O�@��@�/@��@���@���@�Z@���@�G�@��`@�r�@�r�@�Q�@�b@��@�1'@� �@���@���@��@�1'@� �@�@���@�@���@��y@���@��!@���@�=q@���@�O�@�?}@��@���@���@��D@�j@�Z@�1'@��
@�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B  B��B  B  B��B  B��B��B��B��B��B  B  B  B  B  B  B  BBPB!�B-B-B-B-B.B0!B1'B1'B1'B49B7LB9XB:^B9XB9XB:^B:^B:^B;dB<jB;dB;dB;dB;dB:^B;dBC�BM�BR�BW
B_;Bn�Bq�B`BBW
BK�B6FB�BJBB��B�fB�^B��B�VB�DB��B�hB�JB�Bs�Bm�BbNBZBVBO�B;dB"�B�B�BPBB�B�`B�5B�BɺB�B�=By�BcTBM�BC�B8RB�B
��B
�`B
�;B
�B
��B
��B
ÖB
�B
��B
�B
u�B
cTB
L�B
<jB
2-B
$�B
�B
DB
B	��B	�B	�B	�ZB	�HB	�/B	��B	ɺB	�^B	��B	��B	�JB	�B	w�B	l�B	bNB	S�B	H�B	A�B	8RB	/B	$�B	�B	PB	B��B��B��B�B�B�sB�BB�#B��B��BƨB�wB�^B�FB�B�B��B��B��B��B��B�uB�VB�VB�VB�PB�7B�+B�1B�%B�B�B�B�B�B�B�B�B� B}�B|�B{�By�By�Bw�By�Bz�Bz�Bz�Bz�Bx�Bx�Bx�Bw�Bv�Bt�Br�Bq�Br�Bq�Bq�Bp�Br�Br�Bo�Bm�Bk�Be`B_;B\)B]/B^5B^5B^5B^5B^5B^5B`BBcTBgmBiyBjBn�Bv�Bw�Bv�Bt�Bu�Bt�Bt�Bu�Bv�Bu�Bv�Bv�By�B{�B}�B~�B}�B�B�B�B�B�B�B�%B�+B�7B�PB�bB�hB�oB�uB�uB�uB�uB�oB�bB�VB�VB�VB�VB�\B�hB�{B��B��B��B�B�B��B��B��B��B��B��B�B�!B�9B�3B�9B�?B�FB�RB�XB�XB�^B�wB�}B��BÖBŢBŢBƨBǮB��B��B��B�B�)B�ZB�mB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	  B	B	B	%B	1B	1B	1B		7B	JB	PB	hB	hB	oB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	$�B	%�B	%�B	&�B	(�B	)�B	-B	/B	/B	/B	0!B	5?B	6FB	:^B	>wB	C�B	E�B	G�B	L�B	Q�B	Q�B	R�B	R�B	S�B	T�B	XB	ZB	\)B	`BB	aHB	aHB	bNB	cTB	e`B	ffB	gmB	gmB	k�B	l�B	q�B	s�B	t�B	w�B	y�B	|�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�DB	�PB	�\B	�bB	�oB	�oB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�XB	�dB	�dB	�qB	�wB	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ƨB	B	B	B	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B  B��B  B  B��B  B��B��B��B��B��B  B  B  B  B  B  B  BB\B#�B-B-B-B-B.B0!B1'B1'B1'B49B7LB9XB:^B9XB9XB:^B:^B:^B;dB<jB;dB;dB;dB;dB;dB<jBD�BM�BS�BYB`BBo�Bt�BcTBZBP�B=qB%�B\BBB�B��B�B�uB�bB��B�oB�VB�+Bu�Bq�Be`B[#BW
BVBD�B&�B�B�BbB	7B�B�mB�;B�5B��B�dB�VB�BiyBP�BF�B@�B'�B+B
�mB
�HB
�#B
��B
��B
ɺB
�9B
��B
�+B
|�B
iyB
Q�B
A�B
7LB
+B
�B
PB
+B	��B	��B	�B	�`B	�TB	�BB	�B	��B	ÖB	��B	��B	�hB	�%B	�B	q�B	jB	[#B	K�B	G�B	>wB	5?B	.B	�B	uB	DB	B��B��B��B�B�B�TB�;B�B��B��BÖB�jB�XB�LB�B��B��B��B��B��B��B��B�bB�VB�VB�bB�DB�7B�1B�7B�1B�%B�B�B�B�B�B�B�B~�B~�B}�B}�B{�B{�B|�B|�B|�B|�B|�B}�B{�Bx�Bx�Bw�Bv�Bw�Bu�Bs�Bt�Bu�Bv�Bu�Bp�Bn�Bm�BiyBaHB_;B_;B_;B_;B_;B_;B`BB`BBaHBdZBgmBiyBjBm�Bx�Bx�By�Bx�Bv�Bu�Bw�Bx�By�By�Bx�Bx�Bz�B|�B~�B� B�B�B�B�B�B�B�B�%B�+B�=B�VB�oB�uB�{B�{B�uB�uB�{B�uB�oB�hB�\B�\B�\B�bB�uB��B��B��B�B�B�B�B��B��B��B��B��B�B�-B�FB�9B�FB�LB�RB�XB�XB�^B�dB�}B��BBĜBŢBŢBǮBȴB��B��B��B�B�)B�ZB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	  B	B	B	+B	1B	1B	1B	
=B	JB	VB	hB	oB	oB	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	$�B	%�B	%�B	'�B	(�B	)�B	-B	/B	/B	/B	1'B	6FB	7LB	:^B	?}B	C�B	E�B	H�B	M�B	Q�B	Q�B	R�B	R�B	T�B	VB	YB	ZB	\)B	`BB	aHB	aHB	bNB	cTB	e`B	ffB	gmB	gmB	l�B	l�B	q�B	s�B	t�B	w�B	y�B	}�B	�B	�B	�B	�B	�B	�1B	�=B	�JB	�DB	�VB	�bB	�hB	�oB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�XB	�dB	�dB	�qB	�wB	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	ÖB	ÖB	ÖB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447442012010314474420120103144744  AO  ARGQ                                                                        20111130143429  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143429  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144744  IP                  G�O�G�O�G�O�                