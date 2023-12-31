CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-16T19:08:23Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܐ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20230616190823  20230616190823  5904711 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  6217                            2B  A   NAVIS_A                         0487                            011514                          863 @ٻ^����1   @ٻ_l��@*w���+�dI�^51   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��B��B��RB��B��RB��RB�Q�B��RB��RB��B��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C��C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D%w
D%�
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
Dp}pDp�
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
Dyw
Dy�
Dzw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЙ�AЛ�AЛ�AЛ�AЛ�AЙ�AС�AХ�AУ�AУ�AХ�AХ�AХ�AН�AБhAЅA�t�A�dZA�G�A��A���A��TA��HA��TA��HA�ƨAϣ�A�K�A�G�A�C�A�A�A�A�A�A�A�C�A�+A�jA�JA���A�bA��yA���A�Q�A���A�x�A�C�A�^5A���A���A�-A�G�A��FA�-A��DA��RA�+A�1'A���A�7LA�dZA���A�O�A}�AyƨAt^5Ar9XAn�Ak�Ai�
Ah �Ac�A]%AY��AX�AW7LAT~�ARjAQG�AO��AM�PAG��ADM�AA��A?��A>��A<z�A;t�A9�wA7K�A61A5O�A3VA1l�A0�DA0I�A0��A1�#A/�wA-x�A-?}A-%A,��A,�uA,n�A+p�A*��A*��A*Q�A)�A(�yA'�
A't�A'+A&^5A&  A%hsA$1A#�hA"{A!|�A!�A �/A ��A n�A -A �A JA�A��A?}A��A5?AXA�DA^5A�A�;AA
=A�A �A��A\)A&�A�9Ar�AA�A�hA+A�RAbNA$�A��AdZA?}AoA��A�A�HA��AM�A �A�A��Al�A33A�yA��A�Av�A=qA��AhsAA�RA��A�\An�AQ�A{AƨA7LA��A��Ar�AE�A�A��AK�A%A��AffAE�A9XA�A��A;dA
��A
��A
jA	��A	dZA�/A�AJA�mA��AhsA+A�A��AffA=qA�A��A��A��A`BA��A�\A-A�FAt�A��A��AbNA(�AJA�#A��A&�A �A �/A ��A Z@�ƨ@�^5@�$�@�J@���@�7L@��9@�+@�v�@�ff@�M�@�{@��^@�G�@�K�@�z�@�  @��
@�F@�@�@�dZ@�S�@�C�@�+@���@�@�j@��y@��@�=q@�9@�ƨ@�@�n�@���@�@��@�@�A�@��H@�ff@��@��@�^@�j@��@�;d@��@�$�@��@��T@�Q�@�dZ@��@ޏ\@�-@ݩ�@�V@��/@ܛ�@��@�33@ڇ+@�J@ى7@�O�@���@�ƨ@��H@և+@�M�@�@��@��@���@�z�@��@��@Ұ!@�5?@Ѳ-@���@У�@�bN@�b@ϝ�@�33@��y@��@���@�p�@�  @�K�@ʇ+@�`B@�z�@�9X@��m@�K�@�@���@�1'@��;@Ý�@�\)@�@���@���@���@��7@�hs@��@��P@�\)@��!@�{@��h@�/@���@��@��w@��@�\)@��H@��\@�ff@�=q@���@���@�?}@���@�j@��@���@�|�@��@��+@���@��7@�7L@�%@�Ĝ@�bN@�1'@�  @�ƨ@���@�dZ@�"�@��@���@��y@���@���@�ff@�$�@�&�@��@��@�1@�\)@�@��R@�^5@�-@��@�J@��@���@�G�@�7L@���@�j@�I�@�(�@��
@��@�$�@���@��h@�V@�Ĝ@��D@��@�ƨ@���@�\)@�M�@��@��#@��@�hs@�&�@���@�z�@�  @�;d@���@�=q@��T@�hs@��@�Ĝ@���@�Z@��
@���@�K�@���@�~�@�ff@���@��h@�x�@�`B@��@�z�@�(�@�  @�ƨ@�C�@�ff@�x�@��u@� �@���@�"�@��@���@���@��\@���@���@��+@�{@���@�x�@�X@�%@��@��u@�r�@�Z@�A�@���@��F@�l�@�33@��@�@��@�v�@�M�@�E�@�-@��@��-@�p�@�/@�V@���@��j@��9@���@�z�@�Z@�A�@�9X@� �@���@�"�@��\@�M�@�-@��#@���@�O�@��`@��@��D@�Z@��@��w@���@�l�@�;d@�@�ȴ@���@�v�@�V@��@��@���@��@�x�@�p�@�X@�/@���@��D@� �@��@�C�@��@�ff@�=q@�$�@�@��#@�@�p�@�&�@���@��`@��/@�Ĝ@��u@�(�@�dZ@�@���@���@�ff@�=q@���@��7@�&�@��@�%@���@���@�Z@� �@��@�@~ff@}�@}�@|Z@|(�@{�@{@z�H@z��@z��@z��@z�!@zn�@y��@y�@x��@x1'@w��@w�@v$�@u@up�@u�@t��@tj@t9X@t�@s��@sƨ@s��@sS�@so@r��@r��@r��@r^5@q��@qhs@p��@p��@p�`@p�9@pr�@pbN@pQ�@o+@m��@m�@m�@mp�@m`B@l��@l(�@kt�@j�@j�\@jn�@jn�@j^5@jM�@j-@jJ@i��@i�7@h��@g|�@f�y@f$�@d�@dI�@d1@c�F@c��@c��@c��@c�@c33@c@b�H@bM�@a�#@a7L@`�`@`�9@`�@`A�@`A�@`1'@_�P@_K�@_�@_
=@^�@]@\�@[ƨ@[o@Z��@Y�@Y�^@Y��@Yhs@X��@X��@X1'@W|�@V��@V5?@V{@V@U�T@U��@U@U��@U��@U��@U�T@U@U�-@UO�@T(�@S�@R�@R��@R=q@Q��@Q��@QG�@P��@PbN@O��@O��@O;d@O
=@N�@Nff@NE�@N@M��@MO�@M/@MV@L��@Lz�@L(�@K��@Kƨ@KdZ@K"�@K@J�H@J��@J-@I��@I�^@I��@Ix�@Ihs@IX@I7L@I7L@I&�@H��@H��@HA�@Hb@G�@G\)@G+@G�@G
=@G
=@G
=@F��@F�y@Fv�@E��@E/@D��@D��@D�@Dz�@DI�@D9X@D1@C��@CC�@C"�@Co@B��@B^5@B=q@A��@A�#@A��@Ahs@@��@@b@?|�@?l�@?l�@?\)@?K�@>��@>��@>$�@<�/@<I�@<�@;��@;33@:��@:�\@:M�@9��@8�9@8r�@8Q�@8Q�@8A�@81'@8 �@8b@8b@7�@7�@7��@7�P@6�R@6��@6ff@5��@4�@4�@4�D@4Z@41@3dZ@3o@2�H@2�H@2��@2��@2M�@2�@1��@1��@1��@1��@1hs@0�9@0r�@0A�@0  @/�w@/�P@/\)@.��@.��@.ff@-�@-�-@-�h@-p�@-�@,�@,�j@,�D@,Z@,�@+ƨ@+dZ@+dZ@+dZ@+"�@*�H@*�\@*^5@*^5@*=q@*�@)�@)��@)7L@(Ĝ@(bN@(b@(  @'�;@'�w@'�@&�R@&ff@&E�@&{@%�h@%O�@%O�@$��@$j@$j@$j@$j@$j@$j@$Z@#��@#�
@#��@#�@#dZ@#C�@#o@"�@"�H@"~�@!�#@!hs@!%@ Ĝ@ �9@ �`@ ��@ b@�;@�@|�@|�@;d@
=@�y@�y@�y@�@��@5?@�@�T@�@?}@V@��@��@��@�/@�@�@��@��@��@�D@Z@(�@1@��@1@�
@C�@"�@�@�H@�H@��@��@�!@-@�7@7L@��@��@��@��@��@��@��@r�@1'@�w@�P@|�@�@�@�@ȴ@�R@��@ff@E�@{@�T@�h@O�@/@�/@�@I�@(�@�
@��@�@S�@33@�H@M�@�@��@��@��@��@�7@x�@hs@G�@G�@7L@&�@%@Ĝ@�9@�@�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЙ�AЛ�AЛ�AЛ�AЛ�AЙ�AС�AХ�AУ�AУ�AХ�AХ�AХ�AН�AБhAЅA�t�A�dZA�G�A��A���A��TA��HA��TA��HA�ƨAϣ�A�K�A�G�A�C�A�A�A�A�A�A�A�C�A�+A�jA�JA���A�bA��yA���A�Q�A���A�x�A�C�A�^5A���A���A�-A�G�A��FA�-A��DA��RA�+A�1'A���A�7LA�dZA���A�O�A}�AyƨAt^5Ar9XAn�Ak�Ai�
Ah �Ac�A]%AY��AX�AW7LAT~�ARjAQG�AO��AM�PAG��ADM�AA��A?��A>��A<z�A;t�A9�wA7K�A61A5O�A3VA1l�A0�DA0I�A0��A1�#A/�wA-x�A-?}A-%A,��A,�uA,n�A+p�A*��A*��A*Q�A)�A(�yA'�
A't�A'+A&^5A&  A%hsA$1A#�hA"{A!|�A!�A �/A ��A n�A -A �A JA�A��A?}A��A5?AXA�DA^5A�A�;AA
=A�A �A��A\)A&�A�9Ar�AA�A�hA+A�RAbNA$�A��AdZA?}AoA��A�A�HA��AM�A �A�A��Al�A33A�yA��A�Av�A=qA��AhsAA�RA��A�\An�AQ�A{AƨA7LA��A��Ar�AE�A�A��AK�A%A��AffAE�A9XA�A��A;dA
��A
��A
jA	��A	dZA�/A�AJA�mA��AhsA+A�A��AffA=qA�A��A��A��A`BA��A�\A-A�FAt�A��A��AbNA(�AJA�#A��A&�A �A �/A ��A Z@�ƨ@�^5@�$�@�J@���@�7L@��9@�+@�v�@�ff@�M�@�{@��^@�G�@�K�@�z�@�  @��
@�F@�@�@�dZ@�S�@�C�@�+@���@�@�j@��y@��@�=q@�9@�ƨ@�@�n�@���@�@��@�@�A�@��H@�ff@��@��@�^@�j@��@�;d@��@�$�@��@��T@�Q�@�dZ@��@ޏ\@�-@ݩ�@�V@��/@ܛ�@��@�33@ڇ+@�J@ى7@�O�@���@�ƨ@��H@և+@�M�@�@��@��@���@�z�@��@��@Ұ!@�5?@Ѳ-@���@У�@�bN@�b@ϝ�@�33@��y@��@���@�p�@�  @�K�@ʇ+@�`B@�z�@�9X@��m@�K�@�@���@�1'@��;@Ý�@�\)@�@���@���@���@��7@�hs@��@��P@�\)@��!@�{@��h@�/@���@��@��w@��@�\)@��H@��\@�ff@�=q@���@���@�?}@���@�j@��@���@�|�@��@��+@���@��7@�7L@�%@�Ĝ@�bN@�1'@�  @�ƨ@���@�dZ@�"�@��@���@��y@���@���@�ff@�$�@�&�@��@��@�1@�\)@�@��R@�^5@�-@��@�J@��@���@�G�@�7L@���@�j@�I�@�(�@��
@��@�$�@���@��h@�V@�Ĝ@��D@��@�ƨ@���@�\)@�M�@��@��#@��@�hs@�&�@���@�z�@�  @�;d@���@�=q@��T@�hs@��@�Ĝ@���@�Z@��
@���@�K�@���@�~�@�ff@���@��h@�x�@�`B@��@�z�@�(�@�  @�ƨ@�C�@�ff@�x�@��u@� �@���@�"�@��@���@���@��\@���@���@��+@�{@���@�x�@�X@�%@��@��u@�r�@�Z@�A�@���@��F@�l�@�33@��@�@��@�v�@�M�@�E�@�-@��@��-@�p�@�/@�V@���@��j@��9@���@�z�@�Z@�A�@�9X@� �@���@�"�@��\@�M�@�-@��#@���@�O�@��`@��@��D@�Z@��@��w@���@�l�@�;d@�@�ȴ@���@�v�@�V@��@��@���@��@�x�@�p�@�X@�/@���@��D@� �@��@�C�@��@�ff@�=q@�$�@�@��#@�@�p�@�&�@���@��`@��/@�Ĝ@��u@�(�@�dZ@�@���@���@�ff@�=q@���@��7@�&�@��@�%@���@���@�Z@� �@��@�@~ff@}�@}�@|Z@|(�@{�@{@z�H@z��@z��@z��@z�!@zn�@y��@y�@x��@x1'@w��@w�@v$�@u@up�@u�@t��@tj@t9X@t�@s��@sƨ@s��@sS�@so@r��@r��@r��@r^5@q��@qhs@p��@p��@p�`@p�9@pr�@pbN@pQ�@o+@m��@m�@m�@mp�@m`B@l��@l(�@kt�@j�@j�\@jn�@jn�@j^5@jM�@j-@jJ@i��@i�7@h��@g|�@f�y@f$�@d�@dI�@d1@c�F@c��@c��@c��@c�@c33@c@b�H@bM�@a�#@a7L@`�`@`�9@`�@`A�@`A�@`1'@_�P@_K�@_�@_
=@^�@]@\�@[ƨ@[o@Z��@Y�@Y�^@Y��@Yhs@X��@X��@X1'@W|�@V��@V5?@V{@V@U�T@U��@U@U��@U��@U��@U�T@U@U�-@UO�@T(�@S�@R�@R��@R=q@Q��@Q��@QG�@P��@PbN@O��@O��@O;d@O
=@N�@Nff@NE�@N@M��@MO�@M/@MV@L��@Lz�@L(�@K��@Kƨ@KdZ@K"�@K@J�H@J��@J-@I��@I�^@I��@Ix�@Ihs@IX@I7L@I7L@I&�@H��@H��@HA�@Hb@G�@G\)@G+@G�@G
=@G
=@G
=@F��@F�y@Fv�@E��@E/@D��@D��@D�@Dz�@DI�@D9X@D1@C��@CC�@C"�@Co@B��@B^5@B=q@A��@A�#@A��@Ahs@@��@@b@?|�@?l�@?l�@?\)@?K�@>��@>��@>$�@<�/@<I�@<�@;��@;33@:��@:�\@:M�@9��@8�9@8r�@8Q�@8Q�@8A�@81'@8 �@8b@8b@7�@7�@7��@7�P@6�R@6��@6ff@5��@4�@4�@4�D@4Z@41@3dZ@3o@2�H@2�H@2��@2��@2M�@2�@1��@1��@1��@1��@1hs@0�9@0r�@0A�@0  @/�w@/�P@/\)@.��@.��@.ff@-�@-�-@-�h@-p�@-�@,�@,�j@,�D@,Z@,�@+ƨ@+dZ@+dZ@+dZ@+"�@*�H@*�\@*^5@*^5@*=q@*�@)�@)��@)7L@(Ĝ@(bN@(b@(  @'�;@'�w@'�@&�R@&ff@&E�@&{@%�h@%O�@%O�@$��@$j@$j@$j@$j@$j@$j@$Z@#��@#�
@#��@#�@#dZ@#C�@#o@"�@"�H@"~�@!�#@!hs@!%@ Ĝ@ �9@ �`@ ��@ b@�;@�@|�@|�@;d@
=@�y@�y@�y@�@��@5?@�@�T@�@?}@V@��@��@��@�/@�@�@��@��@��@�D@Z@(�@1@��@1@�
@C�@"�@�@�H@�H@��@��@�!@-@�7@7L@��@��@��@��@��@��@��@r�@1'@�w@�P@|�@�@�@�@ȴ@�R@��@ff@E�@{@�T@�h@O�@/@�/@�@I�@(�@�
@��@�@S�@33@�H@M�@�@��@��@��@��@�7@x�@hs@G�@G�@7L@&�@%@Ĝ@�9@�@�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�VA�S�A�Q�A�M�A�K�A�I�A�G�A�E�A�C�A�?}A�9XA�5?A�33A�5?A�7LA�;dA�=qA�E�A�S�A�`BA�dZA�bNA�^5A�ZA�ffA�jAكAفA�~�A�|�A�z�A�x�A�t�A�n�AٍPA٧�A���A���A���A��AݼjA�ȴA�$�Aީ�A��9A�7LA�hsA�ƨA���A�ȴAߺ^Aߛ�Aߏ\Aߗ�A�S�A���A�"�A�33A��A���A�^5A�n�A�!A�9A��
A��`A��TA���A��A���A�?}A៾A�S�A�ffA�ffA�O�A���A�XA�FA��TA��A�+A�%B��B�uB~�BA���A�  Bw�B�-B�B��B�\B�A�(�B{Bx�BC�A��A��#A�XA�r�A�A�DA�hA�uA䛦A��A�hA�x�A䝲A�  A��A��A�x�A���A�S�A�A�oA��HA��A��A��A�$�A�1A��A��A���A��A�dZA�A杲A�+A��A�hsA�v�A�A�A�\A�uA啁A囦A噚A嗍A�DA�"�A��A��B�!B
=BaHB|�B~�B~�BI�B��Bp�BH�A��#A���A���A�RA���A�ȴA�ȴA��A���A�A�A�JA�oA��A�"�A�$�A�$�A�&�A�&�A�+A�/A�9XA�A�A�E�A�G�A�I�A�O�A�S�A�XA�\)A�`BA�hsA�jA�hsA�l�A�n�A�t�A�v�A�z�A�v�A�p�A�jA�^5A�z�A��A�l�A柾A敁A�A���A���A�9A�wA�A�A�ĜA�ƨA�ƨA���A���A��A��HA��TA��A��A���A���A���A���A�  A�1A�JA�JA�
=A�JA�oA��A��A�VA���A���A�A�A�-A��A��A�hsA��B��BM�B��B�\Bk�BB33B ��A��hA��PA��A��!B+B�B<jA��-A���A��A�l�AꝲA�^5A���A�(�A���A��A�hsB1A��B aHB/B B ;dB ��B ��A��#A�-A�bB ��B�BB� B��B�!B�B�-B�^B�}B��B�^B�B��B�BT�BbB�?B��B�FB�^B�dB�B�BS�B\BH�BJ�A�r�B�1BJBA�B��A���A�t�A�(�A��!A�A�5?A�`BA�?}A�t�A�|�A�z�A�7A�PA�hA蝲A��A��A��A��A�FA�A���A���A���A���A��A��HA��`A��`A��`A��HA��A���A���A���A�A�%A�1A�VA�{A��A��A��A��A�{A�%A���A�A�A�S�A�VA���A�\)A�?}A�{A�5?A�A�A�M�A�Q�A�VA�XA�ZA�`BA�bNA�dZA�hsA�jA�l�A�p�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�A�A�7A�DA�uA陚A雦A韾A��A��A��A��A��A�A�A�!A�9A�9A�9A�9A�jA�ĜA�ȴA�ȴA���A���A���A���A�ȴA�jA��A�A�bNA�A�ĜA��\A�{A�p�A��-A�ĜA��mA�  A���A��;A���A�9XA���A��A�-A��
A�;dA�v�A�jA��A�oA�
=A��A� �A� �A�$�A�5?A�;dA�5?A�/A�5?A�;dA�A�A�E�A�A�A�5?A�K�A�l�A�ffA�p�A�r�A�r�A�p�A�p�A�t�A�z�A�|�A�|�A�A�+A�7A�7A�DA�DA�\A�hAꕁAꙚAꛦAꛦAꛦA��A��A��A��A��A�A�A�-A�9A�RA�RA�RA�^A�jA�wA�wA�wA�jA�A�ƨA���A���A���A���A��
A��#A��HA��TA��`A��`A��mA��A��A��A��A���A���A���A���A���A�A�A�%A�
=A�
=A�
=A�
=A�JA�JA�bA�{A��A��A� �A�(�A�+A�-A�-A�1'A�1'A�5?A�9XA�;dA�=qA�=qA�;dA�;dA�?}A�I�A�M�A�O�A�Q�A�S�A�VA�XA�\)A�bNA�dZA�dZA�ffA�hsA�jA�l�A�n�A�p�A�t�A�z�A�|�A�A�A�+A�PA�PA�PA�\A�\A�\A�\A�hA땁A뗍A뙚A뛦A럾A��A��A��A�A�A�!A�-A�-A�9A�FA�FA�RA�^A�jA�jA�jA�jA�A�ĜA�ȴA�ȴA�ȴA�ȴA���A�ȴA�ƨA���A��
A��
A��
A��
A���A���A��A��/A��;A��HA��;A��#A���A���A�ĜA�ƨA���A���A��TA��TA��`A��A���A�`BA���A�A���A�"�A��HA��HA��DA�O�A��FA���A�x�A�  A�XA��A�`BA��FA���A�33A�jA�p�A���A��A�M�A�A�z�A�A��\A��A�z�A��A��uA���A�"�A��+A��A�r�A��TA��\A��A�E�A�`BA��TA��mA�^5A�RA�7LA�=qA�A�-A��FA�t�A�K�A�&�A��A���A��A���A�ĜA��TA���A�^A�9A�A��A�A�9A�^A�A���A�ƨA��A�jA���A��A��A�FA�A�-A웦A�uA웦A��A웦A웦A엍A엍A쟾A��A��A왚A�\A�\A앁A�uA�uA�hA�hA왚A��A��A��A�-A�A��A�A�jA�^A�wA���A���A�ƨA�A��A��A��yA��A��;A���A���A��#A��TA��yA��A�%A���A���A�1A�oA�oA�VA���A���A�A��A��TA���A��
A�O�A�wA�A��A�!A��A�A��A��A��A�hA��DA��PA�jA�jA�^5A�1'A�1'A�K�A�7LA�C�A�r�A�~�A�ZA�?}A�7LA�$�A���A�JA���A�/A�+A��A�bA�A�{A� �A��A��A�9XA��A���A���A��TA���A���A�-A�ƨA�ƨA�A�A�A�33A�33A�9XA�$�A�1A�bA�bA�oA�33A�9XA�=qA�I�A�XA�bNA�ZA�VA�1'A�+A�-A���A���A�FA�ĜA�jAA�\A�A�v�A�t�A�ZA�=qA�$�A�
=A��/A�ƨA��A�+A�|�A�A�jA�dZA�n�A�^5A�I�A�I�A�(�A� �A��A�  A�A�VA�A��A��A��A��HA��A��TA��/A��`A��;A��`A���A�ȴA���A���A��RA��^A��FA��jA�ȴA���A�ƨA��!A��A���A��!A��9A��RA�A���A���A�A���A���A��A��TA��HA��/A��
A��/A��
A��A���A�A��wA��wA��FA�A���A���A��HA��TA��HA��A��A���A�A�VA�$�A�oA���A���A���A���A��A�?}A�7LA�t�B}�B��BK�B��B�B�B'�B,B,B&�B(�B�B�B,BE�B`BB}�B��B�RBȴB�#B�sB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB+BJB�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  A�"�A� �A��A��A��A��A�{A�oA�cA�WA�
>A�A�  A���A�  A�A�%A�2A�cA��A�+A�/A�-A�(�A�$�A�1'A�5@A�M�A�K�A�I�A�G�A�E�A�C�A�?}A�9YA�XA�r�Aٕ�Aڗ�Aڟ�A��yA݇+AݓuA��A�t�A�~�A�A�34AߑiAߙ�AߓuA߅A�fgA�ZA�bNA��A❳A��A���A��aA�ƨA�(�A�9YA�z�A�~�A��A�!A�A��DA�r�A�A�
>A�jA��A�1'A�1'A��A♚A�"�A�A�A�_A���A���B�5Bx�BdZB��A�A���B]/B��B�{B�%Bt�BhrA��B��B^5B(�A��mA��A�"�A�=qA�O�A�VA�\)A�^6A�fgA�l�A�\)A�C�A�hsA���A�t�A�l�A�C�A�ĜA��A�K�A��0A�	A�FA�wA�kA��A���A�wA�kA�A��mA�/A�K�A�hsA�Q�A�l�A�34A�A�A�K�A�O�A�ZA�^6A�`BA�fgA�d[A�bNA�VA��A��kA�K�B��B�BF�BbNBdZBdZB/B��BVB.A���A�r�A뙚A�A囦A�uA�uA�FA�ĜA���A���A��A��0A��mA��A��A��A��A��A���A���A�A�JA�cA�oA�{A��A��A�"�A�&�A�+A�34A�5@A�34A�7LA�9YA�?}A�A�A�E�A�A�A�;eA�5@A�(�A�E�A�p�A�7LA�jA�`BA�K�A晚A柿A�~�A�8A�PA�PA�]A�iA�iA旎A杳A��A�	A�A�FA�kA���A�A�ĜA�ȵA���A���A��A��A���A��A��0A��yA��aA��A�ĜA���A�JA���A��aA��aA�34A��TB�AB33Bz�Bt�BP�B�B�B ��A�\)A�XA�kA�z�BbBiyB!�A�|�A�A��mA�7LA�hsA�(�A���A��A�fgA�x�A�34B �A�O�B F�B{B ��B  �B �)B �VA���A�|�A��#B �#BŢBe`B�IB��B�{B��B��B��B��B��B�{B�BffB:^B��B��B�PB��B��B��B�{BiyB9XB��B.B0!A�=qBm�B�B&�B�NA�ĜA�?}A��A�z�A�PA�  A�+A�
>A�?}A�G�A�E�A�S�A�XA�\)A�hsA�r�A�r�A�r�A�t�A�A�PA蕂A藎A虚A蛦A��A�	A�!A�!A�!A�	A�_A���A���A�ƨA���A���A���A��A��<A��HA��HA��HA��TA��<A���A�ƨA���A�v�A��A��A���A�&�A�
>A��<A�  A�JA��A��A� �A�"�A�$�A�+A�-A�/A�34A�5@A�7LA�;eA�?}A�?}A�?}A�A�A�?}A�A�A�A�A�M�A�O�A�S�A�VA�^6A�d[A�fgA�jA�n�A�n�A�p�A�p�A�r�A�x�A�v�A�z�A�~�A�~�A�~�A�~�A�+A�]A�uA�uA闎A闎A闎A闎A�uA�+A��aA���A�-A���A��]A�ZA��<A�;eA�|�A��]A��.A���A�A���A�p�A�A�r�A��yA���A���A�%A�A�A�+A�l�A��0A���A��yA��A��A��A�  A�%A�  A���A�  A�%A�JA�cA�JA�  A��A�7LA�1'A�;eA�=qA�=qA�;eA�;eA�?}A�E�A�G�A�G�A�K�A�Q�A�S�A�S�A�VA�VA�ZA�\)A�`BA�d[A�fgA�fgA�fgA�l�A�n�A�p�A�p�A�r�A�v�A�x�A�|�A�~�A�A�A�A�A�+A�8A�8A�8A�+A�PA�iAꙚAꝳAꝳAꟿA��A��A�	A�A�!A�!A�.A�RA�_A�kA�wA���A�ĜA�ƨA�ȵA�ȵA���A���A���A���A���A���A���A��A��A��#A��<A��TA��yA��A��A���A���A���A���A���A�  A�A�%A�2A�2A�%A�%A�
>A�{A��A��A��A��A� �A�"�A�&�A�-A�/A�/A�1'A�34A�5@A�7LA�9YA�;eA�?}A�E�A�G�A�K�A�M�A�Q�A�XA�XA�XA�ZA�ZA�ZA�ZA�\)A�`BA�bNA�d[A�fgA�jA�p�A�r�A�t�A�v�A�x�A�z�A�|�A�|�A�~�A�A�A�A�A�+A�+A�+A�+A�PA�]A�uA�uA�uA�uA땂A�uA�iA땂A��A��A��A��A럿A럿A��A��A��A�	A��A��A럿A땂A�]A�iA띳A띳A�A�A�!A��AA�+A�ȵA���A���A��A��	A��	A�VA��A��A�ƨA�C�A���A�"�A���A�+A��A�ƨA���A�5@A�;eA�d[A�O�A��A���A�E�A���A�ZA��kA�E�A��_A�^6A���A��A�Q�A��_A�=qA��A�ZA��kA�cA�+A�A�.A�(�A��A�A�2A�v�A���A�A�?}A��A��A��HA�A�FA쟿A�]A�A�DA�A�~�A�x�A�p�A�v�A�~�A�A�PA엎A�iA�r�A�+A웦A��A�FA�A�v�A�|�A�fgA�^6A�fgA�l�A�fgA�fgA�bNA�bNA�jA�p�A�p�A�d[A�ZA�ZA�`BA�^6A�^6A�\)A�\)A�d[A�p�A�p�A�r�A�|�A�v�A�r�A�v�A�+A�A�8A�DA�DA�iA�PA�RA�kA�:A��A��A�ȵA�ĜA��A�A�:A�kA���A���A�A���A��0A��0A��A�ȵA�ȵA���A�RA�A�l�A��A��A��8A�x�A�t�A�z�A��A�x�A�M�A�n�A�l�A�\)A�VA�XA�5@A�5@A�(�A���A���A��A�A�WA�=qA�I�A�$�A�
>A�A��A�ȵA��A�ĜA���A���A��HA��#A���A��<A��A��aA��mA�A��TA�A�ĜA�A�ƨA�A�|�A�iA�iA�x�A�hsA�fgA���A���A�A��A���A��#A��#A��0A���A�A�2A�{A�"�A�-A�$�A� �A���A���A���A�ĜAA�A�]A�+A�fgA�ZA�M�A�A�A�?}A�$�A�2A��A���A��A�iA�l�A�Q�A�G�A�K�A�5@A�/A�9YA�(�A�{A�{A��A��A��TA���A���A��A���A��_A��RA��wA��	A���A��A���A��!A���A��!A훦A�uA핂A�DA�A�A�A�+A�uA�DA�iA�z�A�v�A�t�A�z�A�~�A�A�PA핂A헎A�PA헎A흳A���A��A��	A���A���A���A���A���A흳A�PA�8A�8A�A�PA흳A퟿A��	A��A��	A��RA��kA�ƨA���A��A��A��0A�ƨA�ƨA�ȵA�ƨA��HA�
>A�A�?}BcTB�-B1'B� B�}B��BPBhBhBIBVBBBhB+BE�BcTB�B��B�B��B��B�B�B�B�B�#B�/B�;B�AB�;B�;B�GB�5B�/B�#B�#B�)B�5B�NB�fB�B�B��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0260000                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 02 04 2020 160 -0.0260000 0.0000 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20230616190823              20230616190823  AO  ARCAADJP                                                                    20230616190823    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230616190823    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230616190823  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230616190823  QCF$                G�O�G�O�G�O�8800            