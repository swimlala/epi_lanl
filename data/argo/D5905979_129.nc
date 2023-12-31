CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-02-19T18:23:32Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210219182332  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��\S؆1   @��\��n@749XbN�b�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bԙ�Bי�B���B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�\D�c�D��)D��qD�$)D�W
D��=D�ɚD� D�\{D���D�θD�( D�W\Dڛ�D�
D�
D�X D�RD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@��H@��HA�
A;�
A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��GB��GB��B��B��B��B��B��B��BîBǮBˮBϮB�G�B�G�B�z�B߮B�B�B�B�B�B��B��GB��C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�pC�
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
C7�
C9�
C;�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��RC��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D�)Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4�)D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM�]DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DY|)DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�DtDy��D�
=D�^�D��
D��RD�
D�Q�D��D��{D�
�D�W\D��fD�əD�"�D�R=DږfD��D��D�R�D�3D�θ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�dZA�bNA�ffA�ffA�hsA�jA�l�A�jA�jA�jA�l�A�n�A�n�A�p�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�~�A�~�AAAAAAAAAAAAAAAA�x�A�n�A�M�A�A�^5A��A�bA�Q�A��A���A�33A��PA�+A��PA�{A�XA�oA��9A��A�%A�t�A�ZA�M�A��A�9XA���A�/A��A��RA���A�Q�A�x�A�p�A��A��9A�A���A��A��A��A��PA�(�A�bNA�jA��;A�VA�K�A�~�A��mA��!A��A�jA��A�dZA�C�A��A�z�A��/A�G�A���A��#A�VA��PA��A��mA���A��A��;A��\A�S�A�A�ffA��A��`A��!A�A�^5A~9XA|r�Ay��Av�As��Ar�yAr�uApZAm�wAkdZAjn�AhZAf^5Ac�FA`�jA^$�A[�;AZffAX�+AV��AT�+AR�AQx�AO"�AM�wALM�AJ�AI��AH��AG;dAE�-AEO�AC�TAA��A@��A?"�A=��A;��A;l�A:JA8VA7hsA6�A4v�A333A2Q�A0��A/p�A.z�A,��A*1A);dA(��A(JA&�+A$z�A#�hA"E�A �/A -A\)A�hAp�A�A�\A^5A��A�FA��A�`AbA�PA�`AQ�A\)A��A�
AhsA��Ax�A�9A�wAQ�A��A�AA�A��A
�A	7LAbA?}A�!A��A�7AK�A/AoAM�A/AbNA�A �uAXA ��@�K�@��-@��@��^@�C�@�E�@���@�@�@�F@�!@홚@���@�9X@��@�9@�{@��@�b@��@ߥ�@ߍP@�;d@���@ܛ�@�1@�5?@ى7@�x�@��@�b@��@ղ-@��`@�\)@��@Ͼw@�33@�{@�%@�  @��y@ɡ�@���@��@�\)@���@Ȭ@�o@�M�@���@�X@���@��-@�A�@���@��@�^5@��@��@�O�@�Q�@�;d@��y@�E�@�M�@�ȴ@��@�^5@���@�1'@���@���@���@��+@���@���@�-@���@�M�@�-@���@�1@��m@�9X@��F@�r�@�j@�C�@��@���@���@�/@�x�@�x�@�O�@���@�b@���@�t�@��!@�@��h@�/@�/@��@�z�@��@��w@��@�(�@���@�\)@�dZ@�dZ@��R@�^5@���@�1@�t�@�-@�hs@�O�@�G�@��@�&�@�  @��F@��F@��@��@��;@�1@���@�dZ@���@��\@���@���@���@���@���@�V@���@�?}@�7L@�G�@�?}@�V@���@�Z@���@��@���@��@�  @�  @��;@��j@�%@�/@�%@��D@���@�+@��+@��@�@��-@�{@�@���@�7L@��@���@��u@�bN@�9X@�ƨ@���@���@��@��P@�S�@�|�@�t�@���@��@�;d@��R@��\@�M�@���@��@�{@�{@�V@��\@�v�@�n�@�$�@�@���@���@���@�`B@��u@��@�t�@���@�J@��^@��@�X@�/@�V@���@��/@��9@�j@�bN@�1'@��;@�dZ@�+@��H@���@���@���@�v�@�ff@��\@�v�@�$�@���@��@���@�x�@�X@���@�r�@�(�@���@��F@���@��P@�t�@�C�@��@���@�V@�5?@�=q@�5?@�{@��@��T@���@�hs@�O�@��@�͟@{�;@q7L@iX@c/�@YL�@P�@Hg8@D�I@=T�@4M@-�H@)�@#ƨ@�@��@?@�}@>�@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�dZA�bNA�ffA�ffA�hsA�jA�l�A�jA�jA�jA�l�A�n�A�n�A�p�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�z�A�z�A�z�A�|�A�|�A�~�A�~�AAAAAAAAAAAAAAAA�x�A�n�A�M�A�A�^5A��A�bA�Q�A��A���A�33A��PA�+A��PA�{A�XA�oA��9A��A�%A�t�A�ZA�M�A��A�9XA���A�/A��A��RA���A�Q�A�x�A�p�A��A��9A�A���A��A��A��A��PA�(�A�bNA�jA��;A�VA�K�A�~�A��mA��!A��A�jA��A�dZA�C�A��A�z�A��/A�G�A���A��#A�VA��PA��A��mA���A��A��;A��\A�S�A�A�ffA��A��`A��!A�A�^5A~9XA|r�Ay��Av�As��Ar�yAr�uApZAm�wAkdZAjn�AhZAf^5Ac�FA`�jA^$�A[�;AZffAX�+AV��AT�+AR�AQx�AO"�AM�wALM�AJ�AI��AH��AG;dAE�-AEO�AC�TAA��A@��A?"�A=��A;��A;l�A:JA8VA7hsA6�A4v�A333A2Q�A0��A/p�A.z�A,��A*1A);dA(��A(JA&�+A$z�A#�hA"E�A �/A -A\)A�hAp�A�A�\A^5A��A�FA��A�`AbA�PA�`AQ�A\)A��A�
AhsA��Ax�A�9A�wAQ�A��A�AA�A��A
�A	7LAbA?}A�!A��A�7AK�A/AoAM�A/AbNA�A �uAXA ��@�K�@��-@��@��^@�C�@�E�@���@�@�@�F@�!@홚@���@�9X@��@�9@�{@��@�b@��@ߥ�@ߍP@�;d@���@ܛ�@�1@�5?@ى7@�x�@��@�b@��@ղ-@��`@�\)@��@Ͼw@�33@�{@�%@�  @��y@ɡ�@���@��@�\)@���@Ȭ@�o@�M�@���@�X@���@��-@�A�@���@��@�^5@��@��@�O�@�Q�@�;d@��y@�E�@�M�@�ȴ@��@�^5@���@�1'@���@���@���@��+@���@���@�-@���@�M�@�-@���@�1@��m@�9X@��F@�r�@�j@�C�@��@���@���@�/@�x�@�x�@�O�@���@�b@���@�t�@��!@�@��h@�/@�/@��@�z�@��@��w@��@�(�@���@�\)@�dZ@�dZ@��R@�^5@���@�1@�t�@�-@�hs@�O�@�G�@��@�&�@�  @��F@��F@��@��@��;@�1@���@�dZ@���@��\@���@���@���@���@���@�V@���@�?}@�7L@�G�@�?}@�V@���@�Z@���@��@���@��@�  @�  @��;@��j@�%@�/@�%@��D@���@�+@��+@��@�@��-@�{@�@���@�7L@��@���@��u@�bN@�9X@�ƨ@���@���@��@��P@�S�@�|�@�t�@���@��@�;d@��R@��\@�M�@���@��@�{@�{@�V@��\@�v�@�n�@�$�@�@���@���@���@�`B@��u@��@�t�@���@�J@��^@��@�X@�/@�V@���@��/@��9@�j@�bN@�1'@��;@�dZ@�+@��H@���@���@���@�v�@�ff@��\@�v�@�$�@���@��@���@�x�@�X@���@�r�@�(�@���@��F@���@��P@�t�@�C�@��@���@�V@�5?@�=q@�5?@�{@��@��T@���@�hs@�O�@��@�͟@{�;@q7L@iX@c/�@YL�@P�@Hg8@D�I@=T�@4M@-�H@)�@#ƨ@�@��@?@�}@>�@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�
B
�B
�/B
�B�Be`B��B��B�B��B�B\B�B+B+B0!B8RBG�BO�BS�BYBYB_;B`BB^5B]/B]/BZBW
BVBR�BO�BL�BG�BD�BB�BA�B49B-B �BbB	7B��B�ZB��B��BB�XB�'B�B�B�!B�3B��B��B�dB��BB�?B��B��B�DB|�BjBJ�B.BuB
�B
�BB
ȴB
�9B
��B
�oB
�DB
�%B
�B
s�B
jB
W
B
D�B
1'B
�B
B	��B	�B	�NB	��B	�FB	�B	��B	�bB	� B	l�B	\)B	L�B	@�B	33B	&�B	�B	PB	%B��B�B�B�NB�)B�
B��BɺBǮB��B�qB�LB�'B�B��B��B��B��B�{B�\B�7B�B� B{�Bv�Bu�Bs�Bl�Bk�Bl�Bo�Bk�BcTB_;B^5B\)B[#B\)B\)BVBVBW
BXBXBW
BXBW
BS�BR�BO�BP�BQ�BW
B]/BgmBgmBbNBffBhsBe`B`BBbNBcTBcTB`BBZBS�BN�BL�BP�BZBXBXBW
BXBS�BP�BL�BK�B\)B_;B[#BZB[#BZBZBYBXBS�BM�BI�BK�BT�BZBYBW
BW
BS�BP�BQ�BN�BXBYBYBXBT�BS�BO�BP�BQ�BQ�BP�BO�BP�BP�BR�BO�BM�BM�BN�BN�BQ�BQ�BQ�BZBZBYBcTBl�Bs�Bt�Bs�Bs�Bw�Bw�By�B~�B�B�B�B�%B�DB�\B�hB��B��B��B��B��B��B�B�B�B��B��B��B��B��B�B�B�RB�^B�XB�LB�^BÖBŢB��B�B�
BȴBɺB�B�;B�`B�yB�B�B�yB�B��B��B��B��B��B��B��B��B��B��B	B		7B	1B	+B	
=B	VB	bB	bB	\B	PB	JB	JB	DB	JB	PB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	+B	/B	2-B	5?B	7LB	;dB	@�B	A�B	C�B	F�B	G�B	K�B	N�B	O�B	Q�B	Q�B	Q�B	P�B	O�B	R�B	T�B	\)B	bNB	gmB	iyB	jB	hsB	gmB	e`B	e`B	jB	m�B	q�B	s�B	w�B	{�B	|�B	}�B	�B	�B	�B	�%B	�1B	�7B	�=B	�JB	�PB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�RB	�XB	�XB	�dB	�qB	�qB	�wB	�}B	�}B	�}B	�}B	�}B	��B	B	ÖB	B	ĜB	ÖB	ĜB	ŢB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	��B
 4B
�B
�B
 vB
)_B
5tB
0oB
9	B
C�B
J�B
L0B
Q�B
U�B
[�B
`'B
d�B
nB
tB
wB
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�%B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�8B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�2B
�8B
�8B
�8B
�8B
�8B
�8B
�DB
�JB
�JB
�]B
�nB
��B�BX�B��B��B�HB�B��B�B�B%B&B#DB+uB:�BC BGBL8BL8BR[BScBQVBPPBPPBM?BJ,BI&BFBCB?�B:�B7�B5�B4�B'`B 6B�B�B�cB�	B׋B�1B��B��B��B�^B�?B�EB�XB�jB�.B�.B��B��B��B�vB�5B��B~�Bp,B]�B>B!ZB�B
�B
ӑB
�B
��B
�B
��B
~�B
y~B
ueB
gB
]�B
JhB
7�B
$�B

�B	�nB	�,B	�B	պB	�;B	��B	��B	�DB	��B	swB	`B	O�B	@JB	4B	&�B	kB	#B	 �B��B�hB�2B�B��BϴBʖBǄB�GB�<B�B� B��B��B��B�^B�KB�KB�.B�B��B|�Bv�Bs�BoBjbBi\BgPB`&B_ B`&Bc9B_!BV�BR�BQ�BO�BN�BO�BO�BI�BI�BJ�BK�BK�BJ�BK�BJ�BG�BF�BC�BD�BE�BJ�BP�B[B[BU�BZB\BYBS�BU�BV�BV�BS�BM�BG�BB}B@rBD�BM�BK�BK�BJ�BK�BG�BD�B@sB?mBO�BR�BN�BM�BN�BM�BM�BL�BK�BG�BAzB=bB?oBH�BM�BL�BJ�BJ�BG�BD�BE�BB�BK�BL�BL�BK�BH�BG�BC�BD�BE�BE�BD�BC�BD�BD�BF�BC�BA~BA~BB�BB�BE�BE�BE�BM�BM�BL�BV�B`3Bg^BhdBg^Bg^BkwBkwBm�Br�Bt�Bv�Bx�By�B~�B�B�B�3B�KB�QB�pB�|B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�9B�DBĆB̸BʫB�WB�]BɥB��B� B�B�$B�B�B�OB�ZB�aB�gB�mB�sB�yB��B�B�B�B��B��B��B��B��B	�B	�B	�B	�B	 �B��B��B��B��B	 �B	B		B	
#B	
#B	
#B	
#B	)B	<B	NB	TB	ZB	lB	xB	�B	"�B	%�B	(�B	*�B	.�B	4B	5"B	7.B	:@B	;FB	?_B	BpB	CvB	E�B	E�B	E�B	D}B	CwB	F�B	H�B	O�B	U�B	[B	]B	^B	\	B	[B	X�B	X�B	^B	a&B	e?B	gKB	kcB	o{B	p�B	q�B	t�B	u�B	v�B	y�B	{�B	|�B	}�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�7B	�OB	�bB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�B	�*B	�$B	�*B	�0B	�<B	�<B	�<B	�<B	�BB	�HB	�OB	�UB	�[B	�aB	�mB	�rB	�rB	�B	ǅB	ɑB	ʗB	ʗB	˝B	̤B	̤B	�yB	�B	��B
		B
�B
�B
(�B
#�B
,�B
76B
>B
?�B
E=B
IVB
OB
S�B
X|B
a�B
g�B
j�B
ne111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.16 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20210219182332    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210219182332  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210219182332  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                