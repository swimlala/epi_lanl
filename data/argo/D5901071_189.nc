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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143339  20190522121827  1727_5046_189                   2C  D   APEX                            2143                            040306                          846 @�	�����1   @�	�hK�@5�I�^�c�x���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D+��D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5y�D5��D6y�D6��D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�3Dy�fD�)�D�p D���D��fD�fD�` D�� D��fD��D�i�D���D��3D�)�D�VfDڣ3D���D��D�ffD�3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @L��@�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B33B	33B��B��B ��B(��B0��B8��B@��BH��BP��BX��Ba33Bi33Bq33Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B���B�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:�C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZL�C\L�C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�3D$�D$��D%�D%��D&fD&��D'�D'��D(�D(��D)fD)��D*�D*��D+�D+��D,fD,��D-3D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5fD5�fD6fD6�fD7fD7��D8�D8��D9�D9��D:�D:�fD;�D;��D<�D<��D=�D=�fD>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC3DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�fDJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN�fDO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�3D\3D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt  Dy�3D�0 D�vfD�� D���D��D�ffD��fD���D�  D�p D��3D���D�0 D�\�Dک�D��3D�#3D�l�D�D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�  A�A�A�
=A�{A�+A�C�A�VA�^5A�`BA�dZA�ffA�9XA�G�A�G�A�$�A�oA¬A�A�A�A��A��
A�t�A���A�O�A���A���A��yA�hsA��A���A���A�S�A�bA���A�E�A���A�M�A� �A���A�A�A�E�A��A�Q�A��HA���A�1A���A�7LA�  A���A�  A�VA��A�1'A�%A���A�"�A�ƨA��A��A���A� �A�JA��yA��jA���A�r�A��A��PA�(�A���A�5?A�;dA�v�A���A�33A�A��wA�ZA{��Axr�Au�ArbAqAp�AoS�Am��Al^5Ajr�Ah��AhQ�AgƨAg�AfE�Aex�Ac�Aa��A`��A^$�A\�jA["�AZ9XAY��AVI�AS��ARffAPn�AM��AM`BALbAI��AIVAH=qAGhsAC�7A?�#A>��A=�-A<�A<M�A:ĜA8�/A8E�A6��A4�`A4�DA2�A2�A2JA2  A1��A1hsA/|�A.ZA-�TA-XA,^5A+��A+�A*�A(�`A'/A&��A&v�A%��A%K�A%%A$�A$��A$��A$�\A$z�A$=qA#��A"��A"1A VA;dAK�A1'A7LA�jAM�A��AG�A�Ar�AoA{A��A��A�!Ax�A-A?}A�RA9XA�FA+A�A
��A	��A�\A�
A�wA�A��A��A|�AhsAK�A�A��A��AffA7LA5?A&�AQ�A/A �u@��\@��@�n�@��@�V@�Ĝ@�Q�@�;d@�Q�@��@@��T@�  @�V@�-@�/@�Ĝ@畁@�-@��`@�Q�@⟾@��@���@�(�@߅@�33@�
=@ޗ�@�$�@ݡ�@�&�@�z�@ڏ\@��@ו�@֏\@��@��@�X@ӶF@�M�@�J@���@�x�@�?}@��`@ЋD@�Q�@�dZ@θR@�/@�|�@�dZ@�;d@��y@ʟ�@�n�@�-@���@ɉ7@�V@�b@��y@�/@�A�@�b@��
@öF@î@�|�@�;d@�V@�1'@��F@�|�@�;d@��h@���@��R@�=q@���@��@���@���@�`B@���@�K�@�;d@���@��@�1@��;@��R@��@�Q�@��;@���@��\@�ff@���@���@� �@��m@��w@�
=@���@��!@�=q@�`B@�&�@��@��D@�A�@�1@��;@�ƨ@��w@��F@���@���@�l�@�;d@�33@���@��@��\@�M�@���@���@���@�I�@��
@�|�@�l�@�;d@�@�~�@�^5@�M�@�5?@�$�@�@��T@���@���@��h@��@��@�z�@�dZ@�
=@��@���@���@�n�@�^5@�=q@�`B@�&�@��@��j@�z�@�b@���@�+@��H@���@��!@���@��\@�n�@�-@�{@�@��@���@��@���@� �@��
@��@�l�@�33@���@�ff@���@�p�@���@��@�V@��j@�Q�@�1@�9X@��;@�ƨ@��
@�Z@��@�z�@�9X@��;@��w@���@�;d@���@���@��R@��H@��@���@�-@��T@��^@�7L@��/@��9@���@���@��9@���@�1'@��@�C�@�33@�33@�33@�33@��y@��@��R@��!@���@��+@�ff@�V@�$�@��@�{@�@�@��@��#@�?}@���@��@��`@���@��j@���@�r�@�bN@�Z@�Z@�Q�@�1'@��m@���@���@��P@��@���@���@�ff@�{@���@�A�@�@v{@l�@ct�@Z�!@PA�@I7L@F�+@?+@6V@01'@+ƨ@(1'@"~�@��@b@��@�!@�h@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A�  A�A�A�
=A�{A�+A�C�A�VA�^5A�`BA�dZA�ffA�9XA�G�A�G�A�$�A�oA¬A�A�A�A��A��
A�t�A���A�O�A���A���A��yA�hsA��A���A���A�S�A�bA���A�E�A���A�M�A� �A���A�A�A�E�A��A�Q�A��HA���A�1A���A�7LA�  A���A�  A�VA��A�1'A�%A���A�"�A�ƨA��A��A���A� �A�JA��yA��jA���A�r�A��A��PA�(�A���A�5?A�;dA�v�A���A�33A�A��wA�ZA{��Axr�Au�ArbAqAp�AoS�Am��Al^5Ajr�Ah��AhQ�AgƨAg�AfE�Aex�Ac�Aa��A`��A^$�A\�jA["�AZ9XAY��AVI�AS��ARffAPn�AM��AM`BALbAI��AIVAH=qAGhsAC�7A?�#A>��A=�-A<�A<M�A:ĜA8�/A8E�A6��A4�`A4�DA2�A2�A2JA2  A1��A1hsA/|�A.ZA-�TA-XA,^5A+��A+�A*�A(�`A'/A&��A&v�A%��A%K�A%%A$�A$��A$��A$�\A$z�A$=qA#��A"��A"1A VA;dAK�A1'A7LA�jAM�A��AG�A�Ar�AoA{A��A��A�!Ax�A-A?}A�RA9XA�FA+A�A
��A	��A�\A�
A�wA�A��A��A|�AhsAK�A�A��A��AffA7LA5?A&�AQ�A/A �u@��\@��@�n�@��@�V@�Ĝ@�Q�@�;d@�Q�@��@@��T@�  @�V@�-@�/@�Ĝ@畁@�-@��`@�Q�@⟾@��@���@�(�@߅@�33@�
=@ޗ�@�$�@ݡ�@�&�@�z�@ڏ\@��@ו�@֏\@��@��@�X@ӶF@�M�@�J@���@�x�@�?}@��`@ЋD@�Q�@�dZ@θR@�/@�|�@�dZ@�;d@��y@ʟ�@�n�@�-@���@ɉ7@�V@�b@��y@�/@�A�@�b@��
@öF@î@�|�@�;d@�V@�1'@��F@�|�@�;d@��h@���@��R@�=q@���@��@���@���@�`B@���@�K�@�;d@���@��@�1@��;@��R@��@�Q�@��;@���@��\@�ff@���@���@� �@��m@��w@�
=@���@��!@�=q@�`B@�&�@��@��D@�A�@�1@��;@�ƨ@��w@��F@���@���@�l�@�;d@�33@���@��@��\@�M�@���@���@���@�I�@��
@�|�@�l�@�;d@�@�~�@�^5@�M�@�5?@�$�@�@��T@���@���@��h@��@��@�z�@�dZ@�
=@��@���@���@�n�@�^5@�=q@�`B@�&�@��@��j@�z�@�b@���@�+@��H@���@��!@���@��\@�n�@�-@�{@�@��@���@��@���@� �@��
@��@�l�@�33@���@�ff@���@�p�@���@��@�V@��j@�Q�@�1@�9X@��;@�ƨ@��
@�Z@��@�z�@�9X@��;@��w@���@�;d@���@���@��R@��H@��@���@�-@��T@��^@�7L@��/@��9@���@���@��9@���@�1'@��@�C�@�33@�33@�33@�33@��y@��@��R@��!@���@��+@�ff@�V@�$�@��@�{@�@�@��@��#@�?}@���@��@��`@���@��j@���@�r�@�bN@�Z@�Z@�Q�@�1'@��m@���@���@��P@��@���@���@�ff@�{@���@�A�@�@v{@l�@ct�@Z�!@PA�@I7L@F�+@?+@6V@01'@+ƨ@(1'@"~�@��@b@��@�!@�h@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BD�BD�BE�BF�BF�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BH�BR�BS�BT�Be`Bw�B�oB��B�qBÖBĜBŢBǮB��B�NB�B��B��BBB��B��B��B��B�B�B�sB�NB�B�sB�mB�HB�TB�/B��B��B��B�}B�jB�^B�?B�B��B�\B�%B� Bt�Bp�BjBdZB^5BO�B.B%B�B��B�B��B�VB�+By�BgmBW
BL�BJ�BH�BE�BC�B@�B9XB-B�BoBB
�B
�9B
��B
��B
�7B
iyB
N�B
%�B
JB	��B	�NB	�B	��B	��B	��B	�XB	�B	��B	��B	��B	��B	�uB	�PB	�B	u�B	k�B	^5B	Q�B	E�B	>wB	5?B	�B	hB	JB	+B��B��B�B�;B�B��B��B�}B�B�B��B��B��B��B��B��B��B�{B�oB�hB�oB�oB�hB�bB�PB�PB�VB�VB�PB�\B�\B�VB�PB�VB�\B�\B�VB�\B�\B�\B�\B�\B�\B�\B�\B�VB�PB�JB�DB�7B�B�B~�B}�B|�B|�B{�B{�Bz�Bx�Bw�Bw�Bv�Bv�Bs�Br�Bq�Bp�Bo�Bn�Bm�BjBffBgmBffBgmBiyBiyBiyBiyBjBjBjBiyBiyBiyBhsBgmBjBk�Bl�Bm�Bn�Bn�Bn�Bq�Br�Bs�Bs�Br�Bq�Bp�Bp�Br�Bq�Bq�Bt�Bv�Bx�By�By�B|�B�B�B�B�B�=B�VB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�3B�LB�RB�RB�RB�RB�RB�XB�XB�XB�^B�jBB��B��B��B��B��B��B�B�B�B�5B�BB�BB�;B�BB�HB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	%B	%B	%B	%B		7B	JB	VB	bB	uB	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	&�B	&�B	&�B	'�B	'�B	'�B	(�B	+B	+B	-B	-B	/B	1'B	5?B	:^B	;dB	A�B	E�B	H�B	I�B	J�B	J�B	N�B	O�B	P�B	Q�B	R�B	R�B	S�B	S�B	T�B	T�B	T�B	W
B	ZB	aHB	bNB	dZB	e`B	ffB	hsB	hsB	hsB	k�B	m�B	n�B	o�B	p�B	s�B	v�B	y�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�FB	�XB	�jB	�qB	�wB	�}B	�wB	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�)B	�;B	�BB	�BB	�BB	�HB	�ZB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
PB
hB
�B
#�B
)�B
33B
>wB
F�B
O�B
VB
ZB
\)B
aHB
ffB
jB
m�B
o�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BD�BD�BD�BE�BF�BF�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BH�BR�BS�BT�Be`Bv�B�hB��B�qBÖBĜBŢBȴB��B�NB�B��BB%BBB%B��B��B�B�B�B�sB�B�B�B�yB�yB�TB�#B��B��B��B�qB�qB�jB�'B��B�oB�7B�+Bw�Br�Bl�Be`BaHBZB;dB�B�TBȴB�?B��B�hB�PB�Bn�BZBL�BK�BI�BE�BD�BB�B<jB33B�B{BoB
�sB
�LB
�B
��B
��B
q�B
\)B
0!B
{B
B	�`B	�/B	�B	��B	ŢB	�wB	�3B	��B	��B	��B	��B	��B	�bB	�1B	x�B	q�B	bNB	VB	G�B	@�B	>wB	&�B	�B	oB	VB��B��B�B�NB�B�B��BɺB�-B�B��B��B��B��B��B��B��B��B��B�uB�oB�oB�hB�oB�uB�hB�bB�bB�bB�bB�hB�hB�{B�{B�hB�bB�hB�bB�bB�\B�\B�bB�\B�\B�bB�bB�\B�VB�\B�JB�7B�B�B~�B}�B}�B}�B|�B|�B|�Bz�Bx�Bw�By�Bw�Bv�Bt�Br�Bq�Bp�Bo�Bp�Bo�Bk�BjBiyBiyBiyBiyBiyBjBjBjBjBjBjBiyBk�Bm�Bo�Bo�Bq�Bp�Br�Bq�Br�Bs�Bt�Bs�Bs�Bs�Bt�Bt�Bt�Bs�Bt�Bv�Bw�By�Bz�B{�B~�B�B�%B�+B�+B�DB�\B�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�-B�3B�FB�LB�RB�XB�XB�RB�XB�XB�^B�^B�jB�wBŢB��B��B��B��B��B��B�
B�B�#B�5B�BB�BB�HB�NB�NB�yB�B�B�B�B�B��B��B��B��B	  B��B��B��B��B��B	B	+B	%B	%B	+B	1B	
=B	JB	VB	hB	uB	�B	�B	�B	�B	�B	!�B	#�B	#�B	%�B	&�B	&�B	&�B	'�B	'�B	'�B	(�B	+B	+B	-B	.B	0!B	2-B	6FB	:^B	<jB	B�B	F�B	H�B	I�B	K�B	K�B	N�B	O�B	P�B	Q�B	R�B	R�B	S�B	S�B	T�B	T�B	VB	XB	\)B	bNB	bNB	dZB	e`B	ffB	hsB	hsB	hsB	k�B	m�B	n�B	p�B	q�B	t�B	w�B	y�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�FB	�XB	�jB	�qB	�wB	�}B	�wB	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�)B	�;B	�BB	�BB	�BB	�HB	�`B	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
PB
hB
�B
#�B
)�B
33B
>wB
F�B
O�B
VB
ZB
\)B
aHB
ffB
jB
m�B
o�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<��
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
<e`B<e`B<#�
<#�
<#�
<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447402012010314474020120103144740  AO  ARGQ                                                                        20111130143339  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143339  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144740  IP                  G�O�G�O�G�O�                