CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-16T19:08:29Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  o(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ژ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230616190829  20230616190829  5904711 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  6217                            2B  A   NAVIS_A                         0487                            011514                          863 @��ݲ�*�1   @��ހ���@)P�`A�7�dd1&�y1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�ff@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33Bϙ�B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�@�Q�AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB��B�Q�BӅBׅB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)COCQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
D �
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D	w
D	�
D
w
D
�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D w
D �
D!w
D!�
D"w
D"�
D#w
D#�
D$w
D$�
D%}pD%�
D&w
D&�
D'w
D'�
D(w
D(�
D)w
D)�
D*w
D*�
D+w
D+�
D,w
D,�
D-w
D-�
D.w
D.�
D/w
D/�
D0w
D0�
D1w
D1�
D2w
D2�
D3w
D3�
D4w
D4�
D5w
D5�
D6w
D6�
D7w
D7�
D8w
D8�
D9w
D9�
D:w
D:�
D;w
D;�
D<w
D<�
D=w
D=�
D>w
D>�
D?w
D?�
D@w
D@�
DAw
DA�
DBw
DB�
DCw
DC�
DDw
DD�
DEw
DE�
DFw
DF�
DGw
DG�
DHw
DH�
DIw
DI�
DJw
DJ�
DKw
DK�
DLw
DL�
DMw
DM�
DNw
DN�
DOw
DO�
DPw
DP�
DQw
DQ�
DRw
DR�
DSw
DS�
DTw
DT�
DUw
DU�
DVw
DV�
DWw
DW�
DXw
DX�
DYw
DY�
DZw
DZ�
D[w
D[�
D\w
D\�
D]w
D]�
D^w
D^�
D_w
D_�
D`w
D`�
Daw
Da�
Dbw
Db�
Dcw
Dc�
Ddw
Dd�
Dew
De�
Dfw
Df�
Dgw
Dg�
Dhw
Dh�
Diw
Di�
Djw
Dj�
Dkw
Dk�
Dlw
Dl�
Dmw
Dm�
Dnw
Dn�
Dow
Do�
Dpw
Dp�
Dqw
Dq�
Drw
Dr�
Dsw
Ds�
Dtw
Dt�
Duw
Du�
Dvw
Dv�
Dww
Dw�
Dxw
Dx�
Dy}pDy�pDzw
Dz�
D{w
D{�
D|w
D|�
D}w
D}�
D~w
D~�
Dw
D�
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�ƨA���A���A���A���A���A���A���A��A��#A��/A��;A��;A��HA��HA��HA��HA��;A��;A��HA��`A��mA��mA��mA��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��A��HA���A���A���Aִ9Aֲ-A֬A֍PA�jA��
AŲ-A��#A�jA��
A��HA��A�K�A�\)A��#A���A��HA���A��A�&�A�XA�$�A���A��A��A�;dA���A�E�A�A�oA�A�A��A���A���AXA{33AuO�Atv�AtE�As|�AoXAj{AhAb�A_XAY��AWAU�-AOƨAL^5AJ9XAE�wAC�TAB5?A?�;A?A=�
A<�A;
=A:�9A9�^A6��A5�PA4�`A4�A3%A2�+A2$�A1�#A1+A0�9A01A.�DA-x�A,��A,�A,^5A+�wA+O�A*�A)��A)dZA)C�A)�A(�/A(��A( �A'dZA&A�A%%A$��A#oA�7Ap�A33A�
A%A �A��A|�Al�A��AQ�A�A�;AAbNA^5A�\A-A�A�AC�A7LA�A�RAbNAJAƨA~�A��A$�A$�A{A�^AC�A�9AI�A(�A��A�7Ap�A7LA�A�\AE�A��AhsA��A�A��AZAC�A
�!A
v�A
Q�A
E�A	�A	A	+A�A�RAz�A��AO�A&�A�yA��AbNA�Ax�A?}A7LAVA�!A=qA��A+A�A�uA(�A�7A�A �A ��A  �@�
=@��7@�V@��@�r�@��@��+@�J@��-@���@�;d@�ff@�@�X@���@��@�w@�\)@�v�@���@��`@�"�@�$�@�G�@�%@���@�j@��;@ꗍ@�?}@�(�@�S�@��@��@���@�~�@��@�7@���@�r�@�(�@��m@���@�F@�
=@�@�&�@�&�@��@�z�@�1'@�ƨ@߾w@�dZ@ާ�@ݺ^@�?}@�bN@���@�dZ@���@�^5@�J@�x�@�V@؛�@�1'@��@�|�@�"�@և+@�{@Ցh@�7L@���@ԣ�@� �@�S�@�
=@ҟ�@�5?@��#@с@�hs@�G�@��@��/@�j@�|�@�\)@Ο�@�@ͩ�@�O�@���@̛�@�I�@��@�t�@ʗ�@ɺ^@��@���@�bN@� �@ǝ�@�K�@��@ƸR@Ɨ�@�=q@ź^@�p�@�?}@��@ě�@��@�ff@�^5@�E�@���@�G�@�&�@���@�z�@��@��m@���@�t�@�C�@�ȴ@�E�@��@�hs@��@�Ĝ@��u@�1'@���@���@��@��\@�ff@�=q@�@���@���@��
@�|�@�dZ@�K�@�33@��@�
=@��H@��@��7@�&�@��D@�I�@�b@�|�@�"�@��y@���@��@���@�p�@�7L@��`@�Z@�b@�l�@�@�ȴ@�n�@�J@��-@�&�@���@���@�r�@�1'@��@���@�\)@�@�-@��h@�G�@��/@�Z@�(�@���@�K�@��H@��R@�v�@�^5@�{@���@�x�@��@��u@�z�@�r�@�I�@�1'@�  @��;@��w@��@�@���@�n�@�^5@�E�@��@��#@��^@��-@��-@���@�7L@���@���@�j@�A�@��@���@�l�@�33@�
=@���@���@���@�n�@�ff@�$�@�@�`B@���@��@�z�@�r�@�A�@��m@���@�|�@�S�@�@�~�@�M�@���@���@�hs@�?}@�G�@�7L@��@�1@��w@�K�@��H@���@��R@�5?@���@�O�@��@��@�I�@� �@�ƨ@�dZ@��@��y@��!@�n�@�E�@��@��@��#@��-@��h@�x�@�`B@�?}@�/@�V@��@��w@�K�@��@���@���@�V@��@��@���@�7L@���@���@���@���@�j@�A�@�9X@�(�@�  @��@�33@��@���@���@��+@�J@��^@�x�@�G�@��@�Z@�  @���@�\)@���@���@�ff@�^5@�V@�@�?}@��9@��@�z�@�r�@�Z@�1'@�1@��
@��@�\)@��@��R@�v�@�-@��T@�@�`B@�G�@��/@��D@�Z@� �@�;@|�@\)@~ȴ@}@}�@|Z@|1@{�
@{�F@{�@z��@z-@y��@y%@xbN@x1'@xb@w��@w�@v�R@vff@u�T@u@uO�@uV@t�/@t�j@t�D@tI�@s�m@sdZ@r�@r�!@rM�@q��@q7L@p��@p�u@pb@o�w@o��@o\)@n�y@m�@m?}@l�/@l9X@l(�@l(�@k��@k�@k"�@j��@j�!@j�!@j��@j�\@jn�@jn�@j=q@jJ@i�7@iG�@i%@h��@hbN@h �@g|�@g�@f�@fȴ@f��@fE�@e�T@e@e�-@e��@e�h@e�@d��@dZ@c�m@c��@b�@b�H@b��@b��@b�!@bn�@bM�@bJ@a�^@aX@aG�@`��@`b@_�;@_�@_K�@_�@^�@^�R@^��@^@]O�@\�@\9X@\1@[�F@[C�@Z��@Z�\@Z�@Y��@Yx�@XĜ@X �@W�@W|�@W;d@V��@V@U@U/@T�/@T�@Tj@T�@S�@S33@R�H@Rn�@Q�7@Q�@P�`@PĜ@Pr�@P1'@O�@O�P@N�y@N��@NE�@N{@M�-@MO�@L�j@LI�@K�F@Kt�@K@J�\@J^5@J�@I�^@H�9@G�;@G�P@GK�@G+@G�@G
=@Fȴ@F��@Fv�@F$�@E�T@E�h@EV@Dz�@D(�@Ct�@Co@B�@B��@B��@Bn�@A�@AX@A&�@A&�@@��@@1'@?�@?|�@?\)@?;d@?;d@>��@>v�@>{@=�-@=�@<�j@<Z@<9X@<1@;�m@;��@;33@:�@:��@:n�@:=q@:�@9�#@9��@97L@8Ĝ@8r�@81'@8b@7�;@7�w@7�P@7l�@7\)@7;d@7;d@6��@6�+@65?@6$�@6@5�-@5p�@5�@4��@4��@49X@4�@3��@3�
@3dZ@2�!@2^5@2-@1��@1X@0�`@0�`@0Ĝ@0A�@0b@/�@/�;@/�;@/��@/|�@/;d@/+@.�y@.��@.v�@.ff@.E�@.5?@.$�@.@-�-@-`B@-?}@,�@,�@,z�@,j@,I�@,(�@,�@,1@+��@+C�@+@*��@*n�@*M�@*M�@*=q@*=q@*�@)�@)7L@(�`@(�9@(r�@(A�@(b@'��@'\)@';d@'
=@&ȴ@&��@&v�@&5?@%�@%�T@%@%�h@%�@$�/@$��@$z�@$9X@$9X@$(�@$1@#�m@#��@#33@#@"��@"��@"n�@"�@!��@!�#@!��@!�^@!��@!��@!��@!x�@!7L@!&�@!�@ �`@ �u@  �@   @�@��@�P@|�@\)@K�@;d@�y@�+@$�@�T@��@p�@O�@V@�@��@j@I�@9X@ƨ@C�@o@@�!@-@��@�#@�^@��@��@%@Ĝ@r�@A�@��@��@|�@\)@;d@�y@$�@�@@`B@?}@?}@V@V@��@z�@9X@�@�m@�
@�
@�
@��@�@S�@C�@33@"�@@�H@��@��@��@��@M�@-@�@�^@�7@hs@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA�ƨA���A���A���A���A���A���A���A��A��#A��/A��;A��;A��HA��HA��HA��HA��;A��;A��HA��`A��mA��mA��mA��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��A��HA���A���A���Aִ9Aֲ-A֬A֍PA�jA��
AŲ-A��#A�jA��
A��HA��A�K�A�\)A��#A���A��HA���A��A�&�A�XA�$�A���A��A��A�;dA���A�E�A�A�oA�A�A��A���A���AXA{33AuO�Atv�AtE�As|�AoXAj{AhAb�A_XAY��AWAU�-AOƨAL^5AJ9XAE�wAC�TAB5?A?�;A?A=�
A<�A;
=A:�9A9�^A6��A5�PA4�`A4�A3%A2�+A2$�A1�#A1+A0�9A01A.�DA-x�A,��A,�A,^5A+�wA+O�A*�A)��A)dZA)C�A)�A(�/A(��A( �A'dZA&A�A%%A$��A#oA�7Ap�A33A�
A%A �A��A|�Al�A��AQ�A�A�;AAbNA^5A�\A-A�A�AC�A7LA�A�RAbNAJAƨA~�A��A$�A$�A{A�^AC�A�9AI�A(�A��A�7Ap�A7LA�A�\AE�A��AhsA��A�A��AZAC�A
�!A
v�A
Q�A
E�A	�A	A	+A�A�RAz�A��AO�A&�A�yA��AbNA�Ax�A?}A7LAVA�!A=qA��A+A�A�uA(�A�7A�A �A ��A  �@�
=@��7@�V@��@�r�@��@��+@�J@��-@���@�;d@�ff@�@�X@���@��@�w@�\)@�v�@���@��`@�"�@�$�@�G�@�%@���@�j@��;@ꗍ@�?}@�(�@�S�@��@��@���@�~�@��@�7@���@�r�@�(�@��m@���@�F@�
=@�@�&�@�&�@��@�z�@�1'@�ƨ@߾w@�dZ@ާ�@ݺ^@�?}@�bN@���@�dZ@���@�^5@�J@�x�@�V@؛�@�1'@��@�|�@�"�@և+@�{@Ցh@�7L@���@ԣ�@� �@�S�@�
=@ҟ�@�5?@��#@с@�hs@�G�@��@��/@�j@�|�@�\)@Ο�@�@ͩ�@�O�@���@̛�@�I�@��@�t�@ʗ�@ɺ^@��@���@�bN@� �@ǝ�@�K�@��@ƸR@Ɨ�@�=q@ź^@�p�@�?}@��@ě�@��@�ff@�^5@�E�@���@�G�@�&�@���@�z�@��@��m@���@�t�@�C�@�ȴ@�E�@��@�hs@��@�Ĝ@��u@�1'@���@���@��@��\@�ff@�=q@�@���@���@��
@�|�@�dZ@�K�@�33@��@�
=@��H@��@��7@�&�@��D@�I�@�b@�|�@�"�@��y@���@��@���@�p�@�7L@��`@�Z@�b@�l�@�@�ȴ@�n�@�J@��-@�&�@���@���@�r�@�1'@��@���@�\)@�@�-@��h@�G�@��/@�Z@�(�@���@�K�@��H@��R@�v�@�^5@�{@���@�x�@��@��u@�z�@�r�@�I�@�1'@�  @��;@��w@��@�@���@�n�@�^5@�E�@��@��#@��^@��-@��-@���@�7L@���@���@�j@�A�@��@���@�l�@�33@�
=@���@���@���@�n�@�ff@�$�@�@�`B@���@��@�z�@�r�@�A�@��m@���@�|�@�S�@�@�~�@�M�@���@���@�hs@�?}@�G�@�7L@��@�1@��w@�K�@��H@���@��R@�5?@���@�O�@��@��@�I�@� �@�ƨ@�dZ@��@��y@��!@�n�@�E�@��@��@��#@��-@��h@�x�@�`B@�?}@�/@�V@��@��w@�K�@��@���@���@�V@��@��@���@�7L@���@���@���@���@�j@�A�@�9X@�(�@�  @��@�33@��@���@���@��+@�J@��^@�x�@�G�@��@�Z@�  @���@�\)@���@���@�ff@�^5@�V@�@�?}@��9@��@�z�@�r�@�Z@�1'@�1@��
@��@�\)@��@��R@�v�@�-@��T@�@�`B@�G�@��/@��D@�Z@� �@�;@|�@\)@~ȴ@}@}�@|Z@|1@{�
@{�F@{�@z��@z-@y��@y%@xbN@x1'@xb@w��@w�@v�R@vff@u�T@u@uO�@uV@t�/@t�j@t�D@tI�@s�m@sdZ@r�@r�!@rM�@q��@q7L@p��@p�u@pb@o�w@o��@o\)@n�y@m�@m?}@l�/@l9X@l(�@l(�@k��@k�@k"�@j��@j�!@j�!@j��@j�\@jn�@jn�@j=q@jJ@i�7@iG�@i%@h��@hbN@h �@g|�@g�@f�@fȴ@f��@fE�@e�T@e@e�-@e��@e�h@e�@d��@dZ@c�m@c��@b�@b�H@b��@b��@b�!@bn�@bM�@bJ@a�^@aX@aG�@`��@`b@_�;@_�@_K�@_�@^�@^�R@^��@^@]O�@\�@\9X@\1@[�F@[C�@Z��@Z�\@Z�@Y��@Yx�@XĜ@X �@W�@W|�@W;d@V��@V@U@U/@T�/@T�@Tj@T�@S�@S33@R�H@Rn�@Q�7@Q�@P�`@PĜ@Pr�@P1'@O�@O�P@N�y@N��@NE�@N{@M�-@MO�@L�j@LI�@K�F@Kt�@K@J�\@J^5@J�@I�^@H�9@G�;@G�P@GK�@G+@G�@G
=@Fȴ@F��@Fv�@F$�@E�T@E�h@EV@Dz�@D(�@Ct�@Co@B�@B��@B��@Bn�@A�@AX@A&�@A&�@@��@@1'@?�@?|�@?\)@?;d@?;d@>��@>v�@>{@=�-@=�@<�j@<Z@<9X@<1@;�m@;��@;33@:�@:��@:n�@:=q@:�@9�#@9��@97L@8Ĝ@8r�@81'@8b@7�;@7�w@7�P@7l�@7\)@7;d@7;d@6��@6�+@65?@6$�@6@5�-@5p�@5�@4��@4��@49X@4�@3��@3�
@3dZ@2�!@2^5@2-@1��@1X@0�`@0�`@0Ĝ@0A�@0b@/�@/�;@/�;@/��@/|�@/;d@/+@.�y@.��@.v�@.ff@.E�@.5?@.$�@.@-�-@-`B@-?}@,�@,�@,z�@,j@,I�@,(�@,�@,1@+��@+C�@+@*��@*n�@*M�@*M�@*=q@*=q@*�@)�@)7L@(�`@(�9@(r�@(A�@(b@'��@'\)@';d@'
=@&ȴ@&��@&v�@&5?@%�@%�T@%@%�h@%�@$�/@$��@$z�@$9X@$9X@$(�@$1@#�m@#��@#33@#@"��@"��@"n�@"�@!��@!�#@!��@!�^@!��@!��@!��@!x�@!7L@!&�@!�@ �`@ �u@  �@   @�@��@�P@|�@\)@K�@;d@�y@�+@$�@�T@��@p�@O�@V@�@��@j@I�@9X@ƨ@C�@o@@�!@-@��@�#@�^@��@��@%@Ĝ@r�@A�@��@��@|�@\)@;d@�y@$�@�@@`B@?}@?}@V@V@��@z�@9X@�@�m@�
@�
@�
@��@�@S�@C�@33@"�@@�H@��@��@��@��@M�@-@�@�^@�7@hs@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��yA��mA��`A��HA��/A��A���A���A���A�ƨA���A�^A�FA�-A�A��A��A��A띲A뛦A뗍A�uA�hA�PA�DA�7A�+A�A�A�A�A�A�A�A�A�A�A�z�A�r�A�n�A�^5A�C�A��A�\)A��`A�-A�A�A��FA���A��/AA�jA�|�A�p�A��PA�|�A�XA�A�K�A�A�A�VA�^5A�A�9A�ĜA�RA��
A�33A�;dA�v�A�!A�+A�+A��A�E�A���A�ĜA�A�jA�PA��mA��/A�-A�O�A��wA���A���A���A�1'A�"�A��A��uA��uA���A�t�B�fA��A�  A��A��A�K�A�hsA�t�A��+A���A��^A�A�ĜA��#A��`A��A�(�A�S�A�n�A�|�A�x�A���A���A��!A���A�%A�JA�VA�{A��A�"�A�/A�I�A�^5A�$�A�-A��A���A�ƨA��TA��A�%A�{A��A��A�9XA�G�A�dZA�hsA�p�A�jA�jA�hsA�x�A�~�A��7A��\A��PA��DA��uA���A��uA��+A���A�A��
A��
A���A��A��TA��A�  A�A�oA��A��A��A�&�A�-A�33A�;dA�A�A�O�A�M�A�E�A�A�A�ZA�hsA�n�A�z�A�v�A���A���A���A���A���A���A���A��^A��9A��-A��A���A�t�BE�B��B�1BB�sB�B>wB:^B�B��BbB�B�B�B�+B��B�qB�BuB�/B�fB�yB�;B��B�-B��B��B�B��B�bBx�BS�B[#B�PB�'B��B5?BE�B=qB!�B�^B  BȴBZB �BA���A�n�A���A��FA���A�ȴA�ȴA���A���A��/A��HA��`A��mA��HA��HA��`A���A�  A���A�1A�1A�
=A�bA�
=A�1A�JA��A��A�$�A�+A�/A�5?A�7LA�7LA�?}A�=qA�?}A�=qA�+A���A��A���A���A�A�%A�
=A�1A�bA��A�$�A�&�A�&�A�+A�1'A�/A�-A�1'A�1'A�5?A�E�A�E�A�Q�A�`BA�hsA�n�A�p�A�t�B�wB�B
�B
2-B�Be`B33B��B�dA���A���A���A�p�A�v�A�t�A�~�A��7A��^A��wA�A��;A��mA��`A��TA��yA��A��A��A���A���A�  A�A�A�A�%A�JA�bA��A��A� �A� �A�&�A�+A�+A�1'A�;dA�;dA�;dA�;dA�7LA�A�A�VA�\)A�^5A�^5A�^5A�^5A�ZA�XA�ffA�jA�n�A�v�A�x�A�x�A��A��A��A��+A��PA��\A��uA��uA��hA���A���A���A���A���A���A��!A��9A��^A��wA���A���A�ĜA�ƨA�ƨA�ĜA�ȴA���A��/A��;A��TA��mA��`A��yA��A���A���A�A�%A�
=A�bA�oA��A�"�A�"�A�"�A�$�A�$�A�&�A�(�A�(�A�(�A�1'A�9XA�;dA�;dA�=qA�?}A�C�A�E�A�E�A�C�A�A�A�E�A�K�A�M�A�O�A�Q�A�XA�\)A�^5A�`BA�bNA�ffA�ffA�ffA�hsA�ffA�hsA�l�A�p�A�v�A�|�A�~�A�z�A�|�A��A��A��A��A��+A��\A��hA���A���A���A���A���A���A���A���A���A��!A��RA��FA��-A��RA���A�ĜA�ȴA���A���A���A��
A��/A��HA��TA��mA��A��A��A��A��A���A���A���A���A���A���A��A��A�A�1A�VA�bA�oA��A��A��A��A�$�A�(�A�+A�+A�-A�/A�1'A�1'A�/A�/A�7LA�;dA�?}A�?}A�A�A�=qA�E�A�I�A�K�A�M�A�K�A�XA�ZA�`BA�bNA�hsA�l�A�p�A�n�A�jA�p�A�x�A��A��+A��+A��+A��+A��7A��DA��DA��DA��\A��hA���A���A���A���A���A���A���A��A��!A��-A��9A��RA��RA��FA��RA���A�ƨA���A���A���A���A�ĜA�A���A�ĜA���A��
A��#A��/A��HA��mA��A��A��A���A���A���A���A���A���A���B   B B B B B +B 1B 	7B 
=B JB PB PB JB PB hB uB �B �B �B �B �B �B �B �B �B �B �B �B  �B �B �B  �B "�B #�B #�B %�B %�B &�B )�B +B -B -B -B .B 0!B 1'B 1'B 1'B 1'B 0!B 1'B 49B 6FB 7LB ;dB ;dB ;dB ;dB ;dB <jB <jB =qB >wB @�B ?}B ?}B D�B D�B E�B G�B G�B H�B H�B G�B H�B K�B M�B O�B O�B P�B Q�B S�B S�B T�B VB W
B YB \)B \)B ]/B ]/B `BB bNB bNB e`B ffB ffB gmB gmB iyB jB k�B k�B o�B q�B r�B r�B s�B t�B t�B u�B x�B y�B z�B z�B {�B |�B }�B � B �B �B �B �B �B �B �%B �=B �VB �\B �hB �hB �hB �hB �oB �oB �oB �uB �uB �{B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �B �B �B �B �B �B �!B �B �3B �?B �?B �FB �FB �LB �RB �XB �^B �dB �jB �jB �qB �qB �wB ��B B ÖB ÖB ÖB ÖB ĜB ĜB ÖB ��B �wB �^B �XB �XB �?B �!B �!B �!B �-B �9B �LB �RB �RB �XB �RB �^B �wB ��B ��B ÖB ƨB ��B ��B ��B ��B ��B ��B ��B �B ��B �
B �B �B �B �#B �)B �/B �NB �TB �ZB �`B �mB �yB �B �B �B �B �B �B �B �B �B �B �B �B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��BBBBBBBB%B%B%B%B1B	7BDBDBJBJBJBJBJBPB\BbBhBhBoBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BbB+BBBBBBBB+B1BB1BDB
=B
=BDBPBPBVBVBVBPBhBoBhBoB{B�B�B�B{B�B�B�B�B�B�B�B�B�B�B �B"�B"�B#�B#�B#�B$�B%�B'�B'�B'�B%�B&�B'�B(�B(�B)�B(�B)�B+B,B,B/B0!B2-B2-4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  A�RA�RA�RA�RA�RA�FA�:A�.A�!A�	A��A��A럿A뙚A땂A�iA�DA�A�A�|�A�x�A�t�A�p�A�l�A�hsA�fgA�bNA�^6A�\)A�XA�VA�S�A�Q�A�O�A�O�A�M�A�K�A�K�A�M�A�M�A�K�A�K�A�K�A�E�A�=qA�9YA�(�A�WA��aA�&�A�!A���A�JA�A홚A���A�jA�+A�G�A�;eA�XA�G�A�"�A�hsA��A�JA��A�(�A�hsA�~�A�]A�A��A���A�%A�A�A�z�A���A���A��HA�cA�A�]A�hsA�5@A�XA�.A��A���A��A�8A���A�l�A�ƨA���A��A�K�A�^6A�^6A�p�A�?}B��A��mA���A��kA��kA��A�34A�?}A�Q�A�t�A��A��PA��]A���A��!A��RA��A��A�9YA�G�A�C�A�bNA�r�A�z�A�ƨA���A��A��A��<A��aA��A���A�{A�(�A��A���A�K�A��DA��iA��A��RA���A��<A��mA��yA�A�oA�/A�34A�;eA�5@A�5@A�34A�C�A�I�A�S�A�ZA�XA�VA�^6A�bNA�^6A�Q�A�bNA��PA���A���A���A���A��A��_A���A���A��0A��TA��mA��mA��A���A���A�%A�JA��A��A�cA�JA�$�A�34A�9YA�E�A�A�A�fgA�jA�d[A�jA�l�A�l�A�r�A��A�~�A�|�A�x�A�fgA�?}B+B�!Bm�B��B��BB#�B�B�}B�;B��BBBBl�B�IB��BB��BB��B��BěB�EB��B�B�VB�bB�Bu�B^5B9XB@�Br�B��B�-B�B+B"�B+B��B�`B�B?}B ŢA�n�A�9YA�hsA��A���A��uA��uA���A���A���A��	A��!A��.A��	A��	A��!A���A���A�ȵA���A���A���A��#A���A���A��A��HA��TA��A���A���A�  A�A�A�
>A�2A�
>A�2A���A�ȵA��_A�A�ƨA���A���A���A���A��#A��mA��A��A��A���A���A���A���A���A���A�  A�cA�cA��A�+A�34A�9YA�;eA�?}B��BB	��B
�B�bBJ�B�B�XB��A��DA�ĜA�d[A�;eA�A�A�?}A�I�A�S�A��A��8A��PA���A��.A��!A��A��:A��kA��kA��_A�A�ȵA���A���A���A���A���A��A��#A��TA��aA��A��A��A���A���A���A�%A�%A�%A�%A�A�JA� �A�&�A�(�A�(�A�(�A�(�A�$�A�"�A�1'A�5@A�9YA�A�A�C�A�C�A�K�A�O�A�O�A�Q�A�XA�ZA�^6A�^6A�\)A�d[A�d[A�n�A�r�A�p�A�t�A�z�A�~�A��A��8A��DA��DA��]A��iA��iA��]A��uA���A���A���A��A��.A��!A��:A��kA�ƨA�ȵA���A���A���A��#A��0A��TA��A��A��A��A��A��A��A��A��A���A�A�%A�%A�2A�
>A�WA�cA�cA�WA�JA�cA��A��A��A��A�"�A�&�A�(�A�+A�-A�1'A�1'A�1'A�34A�1'A�34A�7LA�;eA�A�A�G�A�I�A�E�A�G�A�K�A�O�A�O�A�O�A�Q�A�ZA�\)A�`BA�fgA�hsA�jA�fgA�bNA�bNA�r�A�t�A�z�A��A��A�|�A��A��DA��]A��uA���A���A���A���A���A��	A��A��.A��FA��RA��_A��kA��wA���A�A�A�A�A���A��kA��kA���A���A��A��#A��0A��HA��TA��TA��mA��A��A���A���A���A���A���A���A���A���A�A�%A�
>A�
>A�JA�2A�cA�{A��A��A��A�"�A�$�A�+A�-A�34A�7LA�;eA�9YA�5@A�;eA�C�A�M�A�Q�A�Q�A�Q�A�Q�A�S�A�VA�VA�VA�ZA�\)A�bNA�d[A�hsA�jA�jA�p�A�p�A�v�A�z�A�|�A�~�A��A��A��A��A��DA��iA���A���A���A���A��]A��PA��DA��]A���A���A���A���A��	A��.A��FA��RA��wA���A�ĜA�ƨA�ȵA�ȵA�ȵA�ȵA���A���A���A���A���A��A��#A��/A��;A��UA��aA��aA��UA��aA��A��A���A���A���A���A���B   B B B B B B B %B B B %B 1B 	7B 	7B CB CB IB \B bB nB nB nB tB �B �B �B �B �B �B �B �B �B �B  �B  �B  �B  �B  �B !�B !�B "�B #�B %�B $�B $�B )�B )�B +B -B -B .B .B -B .B 1'B 33B 5?B 5?B 6EB 7LB 9XB 9XB :^B ;dB <jB >wB A�B A�B B�B B�B E�B G�B G�B J�B K�B K�B L�B L�B N�B O�B P�B P�B T�B W
B XB XB YB ZB ZB [#B ^5B _;B `AB `AB aGB bNB cTB e`B glB hrB iyB jB jB jB k�B o�B s�B t�B v�B v�B v�B v�B w�B w�B w�B x�B x�B y�B z�B }�B }�B �B �B �B �B �B �B �%B �7B �=B �7B �7B �IB �VB �\B �bB �bB �bB �bB �nB �tB ��B �{B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �B �'B �!B �-B �?B �LB �RB �XB �dB �^B �jB �wB �}B �}B ��B ��B B ǮB ȴB ɺB ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B �B �B �B �B �)B �)B �)B �)B �)B �#B �)B �;B �AB �GB �NB �NB �TB �ZB �fB �fB �lB �rB �rB �yB �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B  B  B ��B ��B ��B ��B ��B ��B ��B �B �fB �lB �rB �yB �yB �yB �yB �B �B �B �B �B �B �B �B �B �B �B �B �B �B ��B ��B ��B ��B ��B ��B ��B ��B ��B ��B  B  BBBBBBBB%B1B1B	7B	7B	7B
=BCBPBPBPBCBIBPBVBVB\BVB\BbBhBhB{B�B�B�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0260000                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 02 04 2020 160 -0.0260000 0.0000 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20230616190829              20230616190829  AO  ARCAADJP                                                                    20230616190829    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230616190829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230616190829  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230616190829  QCF$                G�O�G�O�G�O�8800            