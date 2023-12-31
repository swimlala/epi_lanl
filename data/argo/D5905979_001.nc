CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:35Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141335  20220204114410  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؁,RL�B1   @؁,���i@6�Z�1�c� ě��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@���A   A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  BffB ffB(ffB0ffB7��B?��BH  BP��BX  B_��Bh  Bp  Bx  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�RD�3D�k3D��RD��{D��D�X D��D���D�fD�W
D���D��qD�  D�l)Dښ�D��qD�%qD�W
D�
D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��HAp�A=p�A]p�A}p�A��RA��A��A��RAθRA޸RA�RA��RB\)B\)BBB'B/B6��B>��BG\)BP(�BW\)B^��Bg\)Bo\)Bw\)B\)B��B��B��GB��GB�z�B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C{�C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Do]D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%�)D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt�)Dy�D�D�fD��3D��\D��D�R�D���D�ФD�GD�Q�D���D��RD��D�g
DڕpD��RD� RD�Q�D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A��;A��A��#A���A��#A��#A��/A��mA��A��mA��A���A���A���A���A���A���A�Q�A�p�A���A�
=A�M�Aǰ!AđhA� �A��A7A���A���A��\A�A��
A��`A�5?A��7A��TA�?}A�1'A���A�~�A��\A��RA��A�A��;A��A���A���A�v�A�$�A�9XA��uA�Q�A��A�1'A�~�A��-A�&�A�x�A���A���A�E�A�"�A�ĜA��+A��A��A�t�A�hsA��A���A�A�^5A�hsA��A�|�A�K�A���A�?}A�
=A��yA�ĜA���A��`A�ƨA�+A�n�A���A���A�t�A�n�A���A��A���A�ƨA�x�A��yA�oA��A���A��A�?}A�C�A�  A�t�A�C�A��A�`BA~$�A}hsA|jAzVAwVAu33At5?Asx�ArffAq�An��AmC�Ak�7Aj�Ai�#Ag;dAd��Acl�Ab  A_��A]��AZ^5AX~�AX�AV�`AT��ATA�ASƨAS�AP�yAOƨAN��AMƨAMVAK�
AJVAHA�AGx�AFbAC��ACK�AB��AB��AB��AA��AA�A@1A?|�A>^5A=x�A=C�A<�+A;��A:��A9;dA7\)A5��A4ĜA3��A2��A1�7A09XA.��A-t�A-7LA,��A,��A,z�A,1A+��A*��A)��A(��A'�A&�!A%33A$�A$�A#p�A"�!A!�wA!/A �\AC�A�A��AA�7AXA�A�A��AȴAXA�A�uAn�AM�A�A��A�-A/A^5A��A�\Ap�A��AVA�mA�hA
�A��A��A"�A�A$�A�AS�A ȴA ^5@�\)@��\@�X@�9X@��@�o@�5?@�bN@���@�bN@��y@���@�Ĝ@���@�!@�h@�  @�@�x�@�  @�K�@�v�@���@�@�j@���@ߝ�@�-@ܛ�@��@ۍP@�n�@ش9@�33@�M�@�/@� �@���@�ff@���@�z�@�33@���@�~�@�G�@̛�@�C�@ɑh@�%@Ȭ@�\)@�%@�b@�n�@��@��@�^5@���@�I�@���@�
=@��@��`@���@��+@��-@�?}@�Q�@��@���@��^@���@�9X@���@�t�@��y@��R@�-@�V@�r�@�(�@��@�;d@��+@��^@�?}@��@�r�@���@��
@��@�\)@�33@��@���@�M�@�J@�@�7L@��@���@�\)@�dZ@�@���@���@��\@��\@�E�@���@���@���@���@�x�@��u@�1@��w@�o@��@�M�@�-@��#@�O�@�V@���@�Q�@�l�@���@�7L@���@�9X@��@��w@��@���@��@�=q@���@�x�@���@�1'@�b@���@�+@�~�@�E�@�@���@���@���@���@��^@���@�x�@�`B@�%@�Ĝ@�bN@�Q�@�I�@� �@��
@�;d@���@��@�@��^@�V@��j@�Ĝ@��@��@�A�@�b@��m@�ƨ@��@��P@�l�@�C�@���@��+@�5?@��@��^@��@�hs@�X@�V@�V@�%@���@��`@�%@�G�@�`B@�x�@�G�@�/@�&�@�/@�&�@��@��9@�9X@�Z@�Q�@� �@��@�b@�\)@�@��H@��\@�v�@�v�@�$�@��@�7L@���@� �@� �@�(�@� �@�1@��F@���@���@��;@�(�@�j@�z�@�Q�@�(�@��;@�t�@�;d@�33@�o@��y@���@��R@��!@���@��+@�n�@�V@�M�@�5?@�J@���@�X@�&�@��@�V@���@���@C@us�@n�M@hی@a[W@Z�+@P�E@H(�@C�g@=��@6@�@0ѷ@)�3@"d�@��@�@�[@��@�9@
ߤ@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A��;A��A��#A���A��#A��#A��/A��mA��A��mA��A���A���A���A���A���A���A�Q�A�p�A���A�
=A�M�Aǰ!AđhA� �A��A7A���A���A��\A�A��
A��`A�5?A��7A��TA�?}A�1'A���A�~�A��\A��RA��A�A��;A��A���A���A�v�A�$�A�9XA��uA�Q�A��A�1'A�~�A��-A�&�A�x�A���A���A�E�A�"�A�ĜA��+A��A��A�t�A�hsA��A���A�A�^5A�hsA��A�|�A�K�A���A�?}A�
=A��yA�ĜA���A��`A�ƨA�+A�n�A���A���A�t�A�n�A���A��A���A�ƨA�x�A��yA�oA��A���A��A�?}A�C�A�  A�t�A�C�A��A�`BA~$�A}hsA|jAzVAwVAu33At5?Asx�ArffAq�An��AmC�Ak�7Aj�Ai�#Ag;dAd��Acl�Ab  A_��A]��AZ^5AX~�AX�AV�`AT��ATA�ASƨAS�AP�yAOƨAN��AMƨAMVAK�
AJVAHA�AGx�AFbAC��ACK�AB��AB��AB��AA��AA�A@1A?|�A>^5A=x�A=C�A<�+A;��A:��A9;dA7\)A5��A4ĜA3��A2��A1�7A09XA.��A-t�A-7LA,��A,��A,z�A,1A+��A*��A)��A(��A'�A&�!A%33A$�A$�A#p�A"�!A!�wA!/A �\AC�A�A��AA�7AXA�A�A��AȴAXA�A�uAn�AM�A�A��A�-A/A^5A��A�\Ap�A��AVA�mA�hA
�A��A��A"�A�A$�A�AS�A ȴA ^5@�\)@��\@�X@�9X@��@�o@�5?@�bN@���@�bN@��y@���@�Ĝ@���@�!@�h@�  @�@�x�@�  @�K�@�v�@���@�@�j@���@ߝ�@�-@ܛ�@��@ۍP@�n�@ش9@�33@�M�@�/@� �@���@�ff@���@�z�@�33@���@�~�@�G�@̛�@�C�@ɑh@�%@Ȭ@�\)@�%@�b@�n�@��@��@�^5@���@�I�@���@�
=@��@��`@���@��+@��-@�?}@�Q�@��@���@��^@���@�9X@���@�t�@��y@��R@�-@�V@�r�@�(�@��@�;d@��+@��^@�?}@��@�r�@���@��
@��@�\)@�33@��@���@�M�@�J@�@�7L@��@���@�\)@�dZ@�@���@���@��\@��\@�E�@���@���@���@���@�x�@��u@�1@��w@�o@��@�M�@�-@��#@�O�@�V@���@�Q�@�l�@���@�7L@���@�9X@��@��w@��@���@��@�=q@���@�x�@���@�1'@�b@���@�+@�~�@�E�@�@���@���@���@���@��^@���@�x�@�`B@�%@�Ĝ@�bN@�Q�@�I�@� �@��
@�;d@���@��@�@��^@�V@��j@�Ĝ@��@��@�A�@�b@��m@�ƨ@��@��P@�l�@�C�@���@��+@�5?@��@��^@��@�hs@�X@�V@�V@�%@���@��`@�%@�G�@�`B@�x�@�G�@�/@�&�@�/@�&�@��@��9@�9X@�Z@�Q�@� �@��@�b@�\)@�@��H@��\@�v�@�v�@�$�@��@�7L@���@� �@� �@�(�@� �@�1@��F@���@���@��;@�(�@�j@�z�@�Q�@�(�@��;@�t�@�;d@�33@�o@��y@���@��R@��!@���@��+@�n�@�V@�M�@�5?@�J@���@�X@�&�@��@�V@���G�O�@C@us�@n�M@hی@a[W@Z�+@P�E@H(�@C�g@=��@6@�@0ѷ@)�3@"d�@��@�@�[@��@�9@
ߤ@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBT�BS�BS�BS�BW
B\)BVBR�BT�BS�BVBXBYBXBYB[#B\)B]/B^5B]/BcTBffBffB�B�B�-B�jB�sB�B�yB�fB�`B�BB�B��B��B{B5?BA�BD�BB�B@�B8RB@�BF�BP�Bs�B�%B�7B�PB�{B�{B�hB�oB�bB�=B�7B�7B~�Bs�B\)BM�B9XB+B(�B8RB?}BH�BK�BK�BL�BL�BJ�BK�B?}B6FB)�B�B�B{BB��B�B�mB�
B��B�!B��B��B��B�PB�+B�B�Bz�Bs�Bq�Bn�B`BB[#BB�B(�BoBPB  B
�mB
�dB
��B
��B
��B
�uB
�JB
w�B
p�B
jB
]/B
L�B
>wB
6FB
2-B
)�B
%�B
�B
DB	��B	��B	�B	�BB	��B	ÖB	�XB	�-B	��B	��B	�7B	�1B	�B	w�B	q�B	m�B	k�B	aHB	\)B	W
B	Q�B	M�B	H�B	B�B	7LB	2-B	-B	!�B	�B	�B	�B	�B	{B	hB	JB		7B	+B	  B��B��B��B��B�B�sB�NB�5B�B��BɺBƨB��B�jB��B��B��B�}B�}B�qB�dB�XB�9B�3B�B�B��B��B��B��B��B��B��B�oB�JB�=B�B�B�B� B|�Bw�Bs�Br�Bn�BjBiyBiyBiyBgmBgmBffBe`BbNB`BB\)BZBYBXBVBS�BR�BO�BM�BL�BH�BE�BB�B@�B@�B?}B?}B?}B>wB=qB<jB;dB:^B9XB6FB5?B49B1'B49B0!B/B0!B/B/B0!B/B0!B/B/B1'B1'B0!B2-B1'B1'B2-B33B6FB7LB7LB;dB>wB@�B@�BA�BC�BD�BD�BD�BF�BF�BI�BL�BL�BL�BM�BO�BP�BS�BR�BXB\)B^5BbNBcTBcTBe`BgmBjBm�Bo�Bo�Bq�Bq�Br�Bt�Bw�Bx�Bz�Bz�B|�B|�B}�B�B�B�B�B�+B�7B�JB�PB�\B�bB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�3B�9B�9B�^B�wB�}BBɺB��B��B��B��B�B�/B�ZB�ZB�mB�B�B�B�B��B��B��B	  B	B	B	1B	DB	PB	{B	�B	�B	�B	�B	 �B	!�B	"�B	%�B	)�B	,B	/B	2-B	5?B	6FB	7LB	8RB	9XB	:^B	:^B	<jB	>wB	A�B	B�B	D�B	H�B	K�B	Q�B	T�B	YB	YB	ZB	^5B	_;B	aHB	cTB	ffB	gmB	gmB	hsB	l�B	n�B	r�B	t�B	u�B	|�B	~�B	�B	�%B	�7B	�=B	�=B	�=B	�DB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�^B	�dB	�dB	�jB	�wB	ÖB	ŢB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�ZB	�fB	�fB	�fB	�fB	�yB	�B	��B
�B
fB
�B
�B
;B
*�B
4�B
9�B
>�B
D�B
J�B
R�B
Z�B
`�B
e,B
hsB
m�B
p�B
t�B
wf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BK�BJ�BJ�BJ�BM�BR�BL�BI�BK�BJ�BL�BN�BO�BN�BO�BQ�BR�BS�BT�BS�BZB]$B]$By�B��B��B�$B�+B�7B�1B�B�B��B�>B�B��B2B+�B8=B;PB9CB78B/B78B=]BG�BjhB|�B�B� B�+B�+B�B� B�B��B�B�Bu�BjjBR�BD�B0B!�B�B/B66B?lBBBBBC�BC�BAyBBB66B- B �BbBnB8B��B�|B�EB�.B��B�GB��B��B��B�[B�B}�By�By�Bq�Bj�BhuBecBWBQ�B9^B�B	BB#B
��B
�CB
�>B
��B
�}B
�jB
�RB
�(B
n�B
g�B
a`B
TB
C�B
5\B
-,B
)B
 �B
�B
jB
-B	��B	�B	�B	�/B	üB	��B	�HB	�B	��B	��B	�,B	&B	w�B	n�B	h�B	d�B	b}B	XAB	S"B	NB	H�B	D�B	?�B	9�B	.IB	)*B	$B	�B	�B	�B	�B	�B	{B	hB	KB	 8B�,B�B��B��B��B�B�B�wB�SB�:B�B��B��B��B��B�sB��B��B��B��B��B�zB�mB�aB�CB�=B�B�B��B��B��B��B��B��B��B�|B�XB�KB|.Bz!BxBwBs�Bn�Bj�Bi�Be�Ba�B`�B`�B`�B^B^B]xB\sBYaBWUBS=BQ1BP+BO$BMBKBJBF�BD�BC�B?�B<�B9�B7�B7�B6�B6�B6�B5�B4�B3�B2|B1wB0qB-_B,XB+RB(AB+SB';B&5B';B&6B&6B'<B&6B'<B&6B&6B(BB(BB'<B)HB(BB(BB)HB*NB-aB.gB.gB2B5�B7�B7�B8�B:�B;�B;�B;�B=�B=�B@�BC�BC�BC�BD�BF�BH BKBJBO+BSDBUPBYhBZnBZnB\zB^�Ba�Bd�Bf�Bf�Bh�Bh�Bi�Bk�Bn�Bo�Bq�Bq�BtBtBuBy%Bz+B{2B|8B~DB�PB�cB�iB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�+B�>B�DB�JB�PB�PB�tB��B��B��B��B��B��B�B�B�,B�CB�nB�nBށB�B�B�B�B��B��B�B�B�B�B�CB	VB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!B	#B	&+B	)=B	,NB	-UB	.[B	/aB	0gB	1mB	1mB	3yB	5�B	8�B	9�B	;�B	?�B	B�B	H�B	LB	P$B	P$B	Q*B	UBB	VHB	XUB	ZaB	]rB	^yB	^yB	_B	c�B	e�B	i�B	k�B	l�B	s�B	vB	yB	}/B	�AB	�GB	�GB	�GB	�NB	�`B	�lB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�AB	�eB	�kB	�kB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�"B	�.B	�:B	�@B	�@B	�GB	�GB	�GB	�MB	�MB	�MB	�MB	�MB	�SB	�_B	�jB	�kB	�kB	�kB	�}G�O�B	��B	��B	�iB
�B
�B
=B
!�B
+�B
0�B
5�B
;�B
A�B
I�B
Q�B
W�B
\,B
_rB
d�B
g�B
k�B
ne111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.16 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144102022020411441020220204114410  AO  ARCAADJP                                                                    20200618141335    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141335  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20200618141335  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114410  IP                  G�O�G�O�G�O�                