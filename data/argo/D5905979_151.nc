CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:30Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170930  20220204114426  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�����1   @�ޕ��l@6�$�/�b��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�fD��qD�[�D���D��\D�#3D�UqD��\D��3D��D�^D���D���D� �D�U�Dژ�D��
D�fD�FfD�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BGBO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C7�C9�C;�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C�޸C��C��C�޸C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Do]D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEo]DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^|)D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Dy�)D��RD�V�D���D��=D�D�PRD��=D��D��D�X�D�~fD�߮D��D�P�Dړ�D���D�GD�AGD��D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�~�AŅAŋDAŃA�|�Aŏ\AōPAŏ\AōPAōPAōPAŁA��A��Aé�A���A�A�A�JA���A�;dA���A�ƨA���A��;A���A�oA���A���A�z�A�dZA�dZA�p�A��A�`BA�
=A��PA�ffA�C�A��RA�9XA�I�A�(�A�z�A�
=A���A��/A�p�A�&�A���A��+A�|�A�dZA�9XA���A��TA���A�A�A���A���A��A�{A��A�oA�hsA� �A�JA�A���A�(�A���A��FA�
=A��\A�=qA���A�1'A�?}A�hsA�bNA��DA�hsA�dZA�jA�1'A�t�A��!A��FA�(�A�&�A�E�A���A�|�A���A�bNA��HA�(�A��7A�A��A�9XA���A���A��TA�G�A��hA��A�(�A�^5A��DA��A�l�A��jA���A��A�ƨA��-A�  A��A�ffA���A�;dA�`BA|�+AwAs��Ar(�Ap�HAnJAj��Ah�+Ae�;Ad  A`�yA_+A\�jA[%AZM�AY�;AXbNAV9XAS��AQ�PAM��AK�AK�AGx�AD~�A@�!A?S�A>�+A>A;�;A;
=A:E�A8Q�A7K�A5�A1�TA/�;A-l�A+��A*~�A(ȴA'�A'x�A&A$ĜA#p�A"��A"ZA�mA�\An�AM�A��AƨAhsA�A�AO�AVAĜAt�A�TA�hA%AbNA�A�jA�`AZA�+A�A�HA�A�AO�A
I�A	ƨA	t�A	33A��A�TA�HAn�A1'A�TAl�AĜA-AƨA&�AȴAM�AXA 1'@���@�Z@���@��@�S�@��h@�;d@�@�b@�~�@��@�@���@�%@��@���@�\)@���@݉7@� �@��#@�9X@�@֟�@�n�@��T@ԣ�@��y@��/@���@ͺ^@�V@�j@�@�-@Ȭ@�9X@�t�@���@�Ĝ@�|�@���@�@�V@���@�O�@��@�Q�@�S�@���@��+@���@��\@��7@�G�@�1@��@���@��m@�dZ@��R@��@��@��@�r�@�1'@�  @�+@�ȴ@���@�V@��@���@�V@� �@���@�ƨ@��m@�  @��w@��@�`B@�X@�?}@�V@�/@� �@��P@���@���@�n�@�V@�=q@�5?@��@��#@���@��7@�`B@���@���@�Q�@���@��m@���@�33@���@���@��7@��@�?}@��/@�Z@��@��w@�l�@�+@��!@���@�7L@��@��9@�z�@�1@�+@���@��H@���@�v�@�v�@�$�@���@�x�@�X@�7L@�V@���@��@�I�@�b@��@�ƨ@��P@�K�@��@��@�ȴ@��\@�ff@�E�@�-@�$�@�{@��@��T@���@���@�/@�&�@��@���@�Ĝ@��@��D@�Q�@� �@��P@�"�@��R@��+@�n�@�=q@��@���@�p�@��`@���@��j@���@�Ĝ@��D@�Q�@�(�@�(�@��@�b@�ƨ@�33@��@�n�@��@���@��h@�hs@��7@���@��^@���@�`B@�%@�Ĝ@��D@�j@�(�@��@��F@�dZ@�33@���@�E�@��@��@�p�@��@���@���@�A�@�b@��m@��P@�S�@�;d@��@�
=@���@�V@�@�@�p�@�/@��/@���@�I�@��@��m@��w@��P@�dZ@��@��@�ȴ@���@��+@�n�@�ff@�E�@�@��-@�X@��@���@��@�j@�  @��
@��F@��P@�l�@�K�@��@��H@���@�ȴ@���@��R@�v�@�n�@�v�@�v�@�J@��#@���@!-@w��@o�F@i	l@`��@X�@R�@Kv`@E��@@�@9[W@4�Y@.u%@(6@"a|@u@�C@�M@�@�2@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�t�A�~�AŅAŋDAŃA�|�Aŏ\AōPAŏ\AōPAōPAōPAŁA��A��Aé�A���A�A�A�JA���A�;dA���A�ƨA���A��;A���A�oA���A���A�z�A�dZA�dZA�p�A��A�`BA�
=A��PA�ffA�C�A��RA�9XA�I�A�(�A�z�A�
=A���A��/A�p�A�&�A���A��+A�|�A�dZA�9XA���A��TA���A�A�A���A���A��A�{A��A�oA�hsA� �A�JA�A���A�(�A���A��FA�
=A��\A�=qA���A�1'A�?}A�hsA�bNA��DA�hsA�dZA�jA�1'A�t�A��!A��FA�(�A�&�A�E�A���A�|�A���A�bNA��HA�(�A��7A�A��A�9XA���A���A��TA�G�A��hA��A�(�A�^5A��DA��A�l�A��jA���A��A�ƨA��-A�  A��A�ffA���A�;dA�`BA|�+AwAs��Ar(�Ap�HAnJAj��Ah�+Ae�;Ad  A`�yA_+A\�jA[%AZM�AY�;AXbNAV9XAS��AQ�PAM��AK�AK�AGx�AD~�A@�!A?S�A>�+A>A;�;A;
=A:E�A8Q�A7K�A5�A1�TA/�;A-l�A+��A*~�A(ȴA'�A'x�A&A$ĜA#p�A"��A"ZA�mA�\An�AM�A��AƨAhsA�A�AO�AVAĜAt�A�TA�hA%AbNA�A�jA�`AZA�+A�A�HA�A�AO�A
I�A	ƨA	t�A	33A��A�TA�HAn�A1'A�TAl�AĜA-AƨA&�AȴAM�AXA 1'@���@�Z@���@��@�S�@��h@�;d@�@�b@�~�@��@�@���@�%@��@���@�\)@���@݉7@� �@��#@�9X@�@֟�@�n�@��T@ԣ�@��y@��/@���@ͺ^@�V@�j@�@�-@Ȭ@�9X@�t�@���@�Ĝ@�|�@���@�@�V@���@�O�@��@�Q�@�S�@���@��+@���@��\@��7@�G�@�1@��@���@��m@�dZ@��R@��@��@��@�r�@�1'@�  @�+@�ȴ@���@�V@��@���@�V@� �@���@�ƨ@��m@�  @��w@��@�`B@�X@�?}@�V@�/@� �@��P@���@���@�n�@�V@�=q@�5?@��@��#@���@��7@�`B@���@���@�Q�@���@��m@���@�33@���@���@��7@��@�?}@��/@�Z@��@��w@�l�@�+@��!@���@�7L@��@��9@�z�@�1@�+@���@��H@���@�v�@�v�@�$�@���@�x�@�X@�7L@�V@���@��@�I�@�b@��@�ƨ@��P@�K�@��@��@�ȴ@��\@�ff@�E�@�-@�$�@�{@��@��T@���@���@�/@�&�@��@���@�Ĝ@��@��D@�Q�@� �@��P@�"�@��R@��+@�n�@�=q@��@���@�p�@��`@���@��j@���@�Ĝ@��D@�Q�@�(�@�(�@��@�b@�ƨ@�33@��@�n�@��@���@��h@�hs@��7@���@��^@���@�`B@�%@�Ĝ@��D@�j@�(�@��@��F@�dZ@�33@���@�E�@��@��@�p�@��@���@���@�A�@�b@��m@��P@�S�@�;d@��@�
=@���@�V@�@�@�p�@�/@��/@���@�I�@��@��m@��w@��P@�dZ@��@��@�ȴ@���@��+@�n�@�ff@�E�@�@��-@�X@��@���@��@�j@�  @��
@��F@��P@�l�@�K�@��@��H@���@�ȴ@���@��R@�v�@�n�@�v�@�v�@�J@��#G�O�@!-@w��@o�F@i	l@`��@X�@R�@Kv`@E��@@�@9[W@4�Y@.u%@(6@"a|@u@�C@�M@�@�2@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�`B	�ZB	�`B	�`B	�ZB	�ZB	�yB	�B
e`B
x�B
H�B
P�B
Q�B
H�B
D�B
G�B
J�B
8RB
:^B
_;B
p�B
�=B
��B
��B
�FB
ÖB%B\B	7B�BB�BP�BdZBq�Bk�B`BBjB�B�uB�{B��B��B��B��B�B�B�9B�jB�qB��BÖBȴB��B�B�B��BoB�B�B!�B5?BH�BS�B`BBn�Bo�Bm�BbNBM�BB�B8RB0!B%�B!�B�BDBB��B�B�B��BB1B  B��B��B�mB��BȴB�jB�}B�qB��B�DB�%Bt�B]/BS�BF�B8RB/B#�BuBPB%B
��B
�ZB
��B
�wB
�B
�DB
{�B
n�B
`BB
T�B
@�B
�B	�B	��B	��B	�?B	��B	�DB	x�B	cTB	W
B	C�B	6FB	'�B	�B	�B	hB	
=B��B�B�B�NB�B��BǮB�^B��B��B��B��B��B�hB�bB�oB�7B�Bt�BcTB`BBffBcTB]/BW
BT�BP�BM�BJ�BH�BG�BL�BL�BK�BK�BL�BK�BK�BN�BN�BN�BN�BN�BN�BL�BM�BL�BJ�BF�BL�BS�BcTB]/BR�BD�B=qB;dB6FB33B2-B2-B1'B1'B1'B/B-B,B+B,B(�B(�B'�B(�B'�B'�B&�B'�B'�B&�B%�B#�B#�B#�B#�B$�B$�B$�B#�B"�B#�B'�B+B+B)�B,B(�B-B+B-B0!B2-B5?B7LB8RB8RB:^B:^B:^B<jB?}BC�BE�BC�BD�BG�BK�BK�BO�BN�BO�BO�BT�BW
BW
BYBYBYB]/B^5B_;B`BBaHBdZBgmBjBjBjBm�Br�Bs�Bu�Bv�Bz�B|�B�B�B�B�%B�1B�7B�=B�JB�\B�bB�oB�uB��B��B��B��B��B��B��B�'B�FB�qB��BÖBŢBŢBƨBǮBȴB��B��B��B��B��B��B�B�B�
B�B�5B�HB�ZB�sB�B�B�B��B��B��B��B	B	+B	
=B	PB	\B	bB	oB	{B	�B	�B	�B	�B	�B	"�B	'�B	)�B	+B	,B	-B	0!B	5?B	7LB	9XB	;dB	=qB	@�B	B�B	E�B	G�B	I�B	L�B	N�B	P�B	Q�B	Q�B	R�B	T�B	T�B	VB	YB	^5B	`BB	`BB	cTB	e`B	ffB	gmB	iyB	jB	l�B	o�B	q�B	q�B	r�B	t�B	v�B	v�B	z�B	}�B	~�B	�B	�B	�B	�%B	�+B	�1B	�1B	�=B	�PB	�bB	�oB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�-B	�3B	�9B	�LB	�RB	�RB	�XB	�^B	�dB	�qB	�}B	��B	B	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B

XB
�B
�B
(�B
0�B
:�B
B�B
E�B
O(B
S�B
X�B
]�B
bB
g�B
l=B
qB
u�B
xRB
{�B
.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�"B	�"B	�"B	�"B	�"B	�(B	�"B	�(B	�"B	�(B	�(B	�"B	�"B	�AB	�}B
VB
i�B
9rB
A�B
B�B
9rB
5[B
8lB
;B
)B
+B
O�B
a^B
z�B
�HB
�B
��B
�FB
��B B
��B(B31BA�BT�BbGB\#BP�B[Br�B�B�B�4B�QB�pB��B��B��B��B� B�B�B�,B�IBBʱB�)B�~B�BB?BWB%�B9<BDBP�B_B`"B^BR�B>\B3B(�B �BrB[B	$B��B�B�wB�MB�)B�SB�B��B�B�rB�[B�B��B�RB�
B�B�B�2B{�Bv�BefBM�BD�B7XB)B�B�B,B
�B
��B
�B
�B
ŽB
�9B
��B
|B
l�B
_eB
QB
E�B
1VB
�B	�vB	��B	�gB	�%B	��B	|0B	i�B	TEB	G�B	4�B	'>B	�B	�B	}B	eB�;B��B�B܈B�SB�B�B��B�iB��B��B��B��B��B�zB�tB��BzJBt&Be�BTmBQ\BW�BTnBNJBH&BFBBB>�B;�B9�B8�B=�B=�B<�B<�B=�B<�B<�B?�B?�B?�B?�B?�B?�B=�B>�B=�B;�B7�B=�BEBTsBNOBDB5�B.�B,�B'lB$ZB#TB#TB"NB"NB"NB CB6B0B+B1BBBBBBBBBBBBBBBB	B	B
BB�BBB/B/B)B5B$B<B0B<B!OB#[B&mB(yB)B)�B+�B+�B+�B-�B0�B4�B6�B4�B5�B8�B<�B<�BAB@BABABF*BH6BH6BJCBJCBJCBN[BOaBPgBQnBRtBU�BX�B[�B[�B[�B^�Bc�Bd�Bf�Bg�BlBnBs5BuBBvHBwNByZBz_B{eB}rB��B��B��B��B��B��B��B��B��B��B�B�LB�jB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�%B�%B�+B�>B�UB�hB�zBْBݪB߶B��B��B��B��B�B�#B�GB�YB�kB	 wB	}B	�B	�B	�B	�B		�B	�B	�B	�B		B	B	B	 B	&B	!9B	&VB	(cB	*oB	,{B	.�B	1�B	3�B	6�B	8�B	:�B	=�B	?�B	A�B	C B	C B	DB	FB	FB	GB	J+B	OHB	QUB	QUB	TgB	VrB	WxB	XB	Z�B	[�B	]�B	`�B	b�B	b�B	c�B	e�B	g�B	g�B	k�B	oB	p
B	sB	t"B	v.B	w4B	x:B	y@B	y@B	{LB	~^B	�pB	�}B	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�&B	�>B	�8B	�>B	�DB	�WB	�\B	�\B	�bB	�hB	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�B	�$B	�$B	�*B	�*B	�0B	�6B	�BB	�IB	�OB	�UB	�UB	�[B	�aB	�fB	�lB	�lB	�sB	�yB	�B	܋B	ݑB	ݑB	ޗB	ߞB	�B	�B	�B	�G�O�B	�B	�[B
�B
�B
�B
!�B
+�B
3�B
6�B
@'B
D�B
I�B
N�B
SB
X�B
]:B
b
B
f�B
iOB
l�B
p*111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.16 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0002), vertically averaged dS =-0.015(+/-0.006) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144262022020411442620220204114426  AO  ARCAADJP                                                                    20200619170930    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170930  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170930  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114426  IP                  G�O�G�O�G�O�                