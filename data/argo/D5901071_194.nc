CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:45Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143402  20190522121828  1727_5046_194                   2C  D   APEX                            2143                            040306                          846 @�_���	1   @�`UU@
@6V�t��c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%y�D%��D&� D'  D'y�D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�B���B�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C2L�C433C633C833C:33C<33C>�C@33CB33CD33CF33CH33CJ33CL33CN33CPL�CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D3D��DfD��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D3D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!�fD"�D"��D#�D#��D$�D$��D%�D%�fD&fD&��D'�D'�fD(fD(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da�3Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds�fDzf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A���A��A���A��A���A��A���A��A��jA��RA��9A��jA��-A���A���A���A���A���A���A�VA��hA���A�  A�$�A���A�l�A�XA�M�A�;dA��A���A�`BA��yA���A�t�A�bNA�{A�&�A��jA��/A�l�A�$�A��A���A�(�A�/A�^5A���A��
A�+A�/A��jA�t�A�VA�$�A�A�A�n�A�G�A�r�A�{A���A�dZA��A�ĜA��A�ƨA���A���A���A���A���A�Q�A�  A��RA��9A��DA��A�&�A���A�-A��A��9A�33A�A�A��9A�?}A�t�A�+A���A��PA��TA�9XA�VA��A�S�A��A�E�A��#AO�A|�HAz�!Ax��Av�!Ar��Ao�Am�TAml�Al�+Aj��Ai33Ah��Ag��Af�HAf�AfE�Ac�wAb��AbI�Aa7LA_�
A^VA\bNAZ��AZZAZ9XAY�AWdZAU
=AS��AS�hAR~�AQ�PAPz�AN{AJ�/AI�AI�AH�AFz�AE��AC�AB�yA?hsA=/A<��A;�A:{A9A6�RA3�FA2�`A1�A/��A.1A,��A+�TA+�A*z�A)?}A'A&��A$�/A#��A#VA"bA!�PA!&�A �jA M�At�A?}A/A+A&�A%A�`A�Az�AbA��A�jA1A�
AK�A�mA�A�A��A�9A-Ap�A?}A�yAbNA�
A�A��A%A1'A�A�AdZAA
�!A
��A
�A
z�A
jA
M�A
5?A
$�A	�A	�7A	VA5?Av�AK�Ar�A�
AO�Az�A M�@��y@��j@�{@��@��@���@�S�@���@�\@�E�@�-@�=q@��@�33@�7@�v�@���@�r�@��@ۥ�@�-@�@��@��@�7L@��@��m@ӝ�@�"�@ҧ�@�E�@��@�@���@��@υ@��#@�C�@��T@�&�@�Ĝ@�l�@�-@��@�z�@�b@å�@�^5@���@�9X@��@�S�@��@�n�@�-@��@���@�X@��`@���@���@���@�{@�/@�V@���@�r�@���@�"�@���@�=q@��@�ȴ@���@�~�@�v�@�^5@�=q@�J@���@��@��9@�r�@��w@�+@�V@��T@��@�hs@�O�@�V@���@��@��@�j@�1'@�|�@�o@���@���@��\@��+@�~�@�v�@��@��@��^@���@��/@���@�ƨ@�+@�@��R@���@�ff@�J@�@�hs@��@���@��@�j@�A�@��
@�o@��!@���@��@��y@��@���@���@�{@��h@��@�p�@�`B@�X@�7L@���@��u@�bN@�A�@�1@��
@��
@��
@���@��F@�t�@�K�@�;d@�o@���@��!@��!@�~�@�-@��@�@��@���@��h@�X@�7L@�j@��F@���@���@��@��P@�|�@�l�@�dZ@�K�@�;d@�
=@��@���@�~�@�V@��@��@��@��T@��#@��#@��#@���@�X@�?}@�/@��@��/@���@�Ĝ@��j@���@���@���@��P@���@�t�@�S�@�K�@�C�@�33@�+@�"�@�o@���@��H@���@��@��j@��@�j@�Q�@��@��@�C�@�@��H@��\@�^5@�5?@�J@���@�p�@�G�@�?}@�/@�&�@���@��9@��9@��D@�Q�@�I�@�A�@�9X@� �@��@��m@���@���@�l�@�K�@�C�@�C�@�;d@�33@�+@�
=@�ȴ@��@��^@��h@�7L@�%@��u@�hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A���A��A���A��A���A��A���A��A��jA��RA��9A��jA��-A���A���A���A���A���A���A�VA��hA���A�  A�$�A���A�l�A�XA�M�A�;dA��A���A�`BA��yA���A�t�A�bNA�{A�&�A��jA��/A�l�A�$�A��A���A�(�A�/A�^5A���A��
A�+A�/A��jA�t�A�VA�$�A�A�A�n�A�G�A�r�A�{A���A�dZA��A�ĜA��A�ƨA���A���A���A���A���A�Q�A�  A��RA��9A��DA��A�&�A���A�-A��A��9A�33A�A�A��9A�?}A�t�A�+A���A��PA��TA�9XA�VA��A�S�A��A�E�A��#AO�A|�HAz�!Ax��Av�!Ar��Ao�Am�TAml�Al�+Aj��Ai33Ah��Ag��Af�HAf�AfE�Ac�wAb��AbI�Aa7LA_�
A^VA\bNAZ��AZZAZ9XAY�AWdZAU
=AS��AS�hAR~�AQ�PAPz�AN{AJ�/AI�AI�AH�AFz�AE��AC�AB�yA?hsA=/A<��A;�A:{A9A6�RA3�FA2�`A1�A/��A.1A,��A+�TA+�A*z�A)?}A'A&��A$�/A#��A#VA"bA!�PA!&�A �jA M�At�A?}A/A+A&�A%A�`A�Az�AbA��A�jA1A�
AK�A�mA�A�A��A�9A-Ap�A?}A�yAbNA�
A�A��A%A1'A�A�AdZAA
�!A
��A
�A
z�A
jA
M�A
5?A
$�A	�A	�7A	VA5?Av�AK�Ar�A�
AO�Az�A M�@��y@��j@�{@��@��@���@�S�@���@�\@�E�@�-@�=q@��@�33@�7@�v�@���@�r�@��@ۥ�@�-@�@��@��@�7L@��@��m@ӝ�@�"�@ҧ�@�E�@��@�@���@��@υ@��#@�C�@��T@�&�@�Ĝ@�l�@�-@��@�z�@�b@å�@�^5@���@�9X@��@�S�@��@�n�@�-@��@���@�X@��`@���@���@���@�{@�/@�V@���@�r�@���@�"�@���@�=q@��@�ȴ@���@�~�@�v�@�^5@�=q@�J@���@��@��9@�r�@��w@�+@�V@��T@��@�hs@�O�@�V@���@��@��@�j@�1'@�|�@�o@���@���@��\@��+@�~�@�v�@��@��@��^@���@��/@���@�ƨ@�+@�@��R@���@�ff@�J@�@�hs@��@���@��@�j@�A�@��
@�o@��!@���@��@��y@��@���@���@�{@��h@��@�p�@�`B@�X@�7L@���@��u@�bN@�A�@�1@��
@��
@��
@���@��F@�t�@�K�@�;d@�o@���@��!@��!@�~�@�-@��@�@��@���@��h@�X@�7L@�j@��F@���@���@��@��P@�|�@�l�@�dZ@�K�@�;d@�
=@��@���@�~�@�V@��@��@��@��T@��#@��#@��#@���@�X@�?}@�/@��@��/@���@�Ĝ@��j@���@���@���@��P@���@�t�@�S�@�K�@�C�@�33@�+@�"�@�o@���@��H@���@��@��j@��@�j@�Q�@��@��@�C�@�@��H@��\@�^5@�5?@�J@���@�p�@�G�@�?}@�/@�&�@���@��9@��9@��D@�Q�@�I�@�A�@�9X@� �@��@��m@���@���@�l�@�K�@�C�@�C�@�;d@�33@�+@�
=@�ȴ@��@��^@��h@�7L@�%@��u@�hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B�B�B�B��B��B��B��B��B�{B�bB��B��B�oB�DB�B�Bx�Bv�Bn�BbNBYBS�B[#B\)BI�B=qB�B��B�B�B�B�B�HB��BǮBƨB�wB�!B��B��B�\B�BiyBYBL�B@�B#�BJB
�B
�NB
�5B
�B
��B
ƨB
�!B
��B
��B
~�B
bNB
VB
N�B
A�B
0!B
 �B
uB
B	�B	��B	ȴB	ŢB	�qB	�3B	�B	��B	��B	��B	��B	�{B	�DB	�+B	�+B	�%B	�B	�7B	�B	{�B	z�B	x�B	t�B	m�B	bNB	\)B	XB	Q�B	L�B	F�B	=qB	1'B	+B	&�B	"�B	�B	�B	JB	B�B�B�B�B�TB�;B��B��BŢB�}B�XB�9B�B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�oB�oB�oB�oB�hB�hB�hB�hB�bB�\B�\B�PB�PB�PB�JB�7B�B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B� B�B� B� B~�B}�B{�Bx�Bs�Br�Br�Br�Bp�Bo�Bn�Bm�Bm�Bn�Bo�Br�Bs�Bt�Bt�Bn�BiyBiyBhsBhsBgmBgmBffBcTBgmBiyBjBjBk�Bl�Bl�Bm�Bl�Bl�Bk�Bn�Bp�Bw�Bz�B|�B|�B�B�B�=B�JB�VB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�}BȴB��B��B��B��B��B�B�B�
B�/B�B�B�B�B�B�B�B��B��B��B��B	B	B		7B	DB	PB	VB	\B	hB	uB	uB	�B	�B	�B	�B	�B	 �B	"�B	"�B	"�B	#�B	#�B	&�B	'�B	(�B	,B	,B	-B	33B	8RB	9XB	;dB	<jB	=qB	?}B	@�B	B�B	E�B	G�B	H�B	H�B	H�B	J�B	Q�B	]/B	bNB	gmB	k�B	k�B	l�B	l�B	p�B	r�B	r�B	s�B	s�B	s�B	s�B	u�B	w�B	x�B	y�B	{�B	|�B	|�B	}�B	}�B	}�B	� B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�1B	�7B	�7B	�=B	�=B	�DB	�DB	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�?B	�LB	�FB	�RB	�XB	�XB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�qB	��B	B	B	B	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�)B	�5B	�BB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�9B�'B�B�B��B��B��B��B��B�uB��B��B��B�PB�%B�1B�B�Bw�BgmB\)BT�B_;BbNBL�BD�B!�BB�B�B�B��B�B��BɺBȴBÖB�FB��B��B�oB�\Bm�B]/BP�BK�B-B�B
�B
�ZB
�;B
�/B
�B
��B
�?B
��B
��B
�JB
gmB
YB
W
B
H�B
6FB
$�B
�B
DB	��B	�
B	ɺB	ǮB	��B	�LB	�B	��B	��B	��B	��B	��B	�PB	�7B	�=B	�=B	�=B	�\B	�+B	|�B	z�B	y�B	|�B	t�B	e`B	^5B	\)B	T�B	P�B	N�B	G�B	49B	.B	+B	(�B	�B	�B	bB	\B��B�B�B�B�ZB�fB�#B��B��BÖB�qB�RB�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�oB�hB�hB�oB�hB�hB�hB�hB�bB�\B�VB�VB�VB�VB�1B�1B�+B�%B�%B�B�B�B�B�B�%B�B�%B�+B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B�B�B|�Bv�Br�Bt�Br�Bp�Bo�Bn�Bn�Bn�Bo�Bv�B{�Bw�Bz�Bv�Bk�BjBiyBk�BhsBiyBhsBhsBiyBiyBk�Bk�Bl�Bm�Bl�Bm�Bl�Bn�Bk�Bq�Bu�By�B{�B}�B~�B�B�%B�DB�PB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�3B��BɺB��B��B��B��B��B�
B�
B�B�HB�B�B�B�B�B�B��B��B��B��B��B	B	%B	
=B	JB	PB	VB	\B	hB	uB	{B	�B	�B	�B	�B	�B	 �B	"�B	"�B	"�B	#�B	$�B	&�B	'�B	(�B	,B	-B	-B	33B	8RB	9XB	;dB	<jB	=qB	?}B	@�B	B�B	E�B	G�B	H�B	H�B	I�B	K�B	R�B	]/B	bNB	gmB	k�B	k�B	l�B	m�B	q�B	r�B	r�B	s�B	s�B	s�B	s�B	v�B	w�B	x�B	y�B	{�B	|�B	|�B	}�B	}�B	}�B	� B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�1B	�7B	�7B	�=B	�=B	�DB	�JB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�?B	�LB	�FB	�RB	�XB	�XB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�}B	��B	B	B	ÖB	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�)B	�5B	�BB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<49X<#�
<e`B<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447422012010314474220120103144742  AO  ARGQ                                                                        20111130143402  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143402  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144742  IP                  G�O�G�O�G�O�                