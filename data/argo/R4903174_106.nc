CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-09-03T09:00:59Z creation      
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
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20210903090059  20210903090059  4903174 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               jA   AO  7230                            2B  A   NAVIS_A                         0967                            170425                          863 @ِ�>2�x1   @ِ��-��@7Z�1�c�-V1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         jA   A   F   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @}p�@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HB�
Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB��B�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8RD�xRD���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�8RD�{�D컅D���D�;�D�xRD���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�~�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�\)A�\)A�\)A�\)A�^5A�bNA�`BA�`BA�`BA�bNA�dZA�bNA�\)A�7LA��A�A��A�t�A�I�A��AЋDA�33A���A���A�x�Aʰ!A���A��A�VA�v�A�A�=qA��TA��
A��
A�JA��/A�bNA��HA�G�A��DA��A��A��wA��A��A�|�A�bA��/A�ȴA�|�A�-A�p�A��yA���A�r�A��A�Q�A�dZA��A��A��A��A�v�A�VA��#A�S�A�+A�E�A��;A�z�A��A�A�A�oA�A�A��#A��A��A��mA���A�A��\A���A�VA��#A�$�A�-A�dZA��FA�oA�A��FA��hA�33A��hA�JA�A�`BA���A��FA��yA�S�A�ffA�Q�A��A�7LA��9A��`A�$�A���A��HA�JA�&�A�VA�|�A��jA��A��A�z�A���A��A�dZA~$�Ax~�Au��As�As�;Arn�Ao�AoAo�hAodZAm�Al�DAk�
Aip�Ah��Ag�PAe�Ab�A`�9A^�yA]XA\�9A[��AX�jAV�!ATAPbNANn�AM�FAM33AKO�AI�hAG�wAEAC�-AC33AA�FA@E�A>I�A<ZA<(�A<(�A;�A:JA9�A7�A533A4z�A45?A3XA2��A1C�A0ffA/�;A/t�A.��A-��A-�A,E�A+33A*��A)��A(�DA'�A$��A#VA!p�A�AE�A"�A��AoAr�A|�A��A�9A�TAoA��A~�Av�A^5A`BA�;A��A�\AE�A|�AdZA�A�^AQ�A
=A
{A	�PA��A�A��A�PAK�A�AbNA�/A�A�AoAv�A�AA �@�K�@�&�@�A�@�;d@�-@��D@�~�@�7L@���@�l�@�@�7@�bN@��@��#@�1'@�!@��@���@�9X@�w@�"�@��T@��`@�ƨ@�"�@�O�@��
@ߕ�@���@���@ܴ9@�C�@��@�G�@׾w@��#@ԋD@ӥ�@�~�@���@ѩ�@�V@�Z@�o@�&�@˝�@���@���@��@ǅ@�|�@�|�@�\)@�33@��@�v�@�J@��@��@���@��`@���@��\@��7@��u@��@���@�33@�V@�@�`B@�%@�(�@��@�@�^5@���@��T@��-@��9@��F@�+@��+@�{@��-@�G�@�j@��w@�\)@�o@�V@���@���@��h@�x�@�7L@���@�9X@���@��w@�dZ@��H@��\@�~�@�^5@��@��h@�/@���@�1@��@�@�V@���@�O�@��@�bN@�(�@���@��w@��P@�dZ@�S�@���@�M�@�J@�x�@��u@�bN@��@��P@�l�@��H@�~�@�$�@��@��@��@�j@� �@�b@��@�l�@��@���@��@���@���@�Ĝ@���@�Z@�  @��
@���@�|�@�;d@��@�ȴ@�E�@��#@�V@�v�@�ff@�ff@�J@��h@��9@�Z@�x�@��#@���@�@��h@�hs@�/@���@��9@��@���@�Q�@�9X@�1@���@�o@���@��y@���@�^5@���@��h@�X@�?}@�V@��j@� �@��w@�|�@�S�@��@��!@���@�^5@�-@�{@�{@�{@�5?@�$�@��@�{@��@���@�&�@��/@��u@��@��D@�z�@�bN@�(�@��@�ƨ@���@��P@���@�|�@�l�@�K�@�
=@���@�v�@�V@�$�@�J@�@���@���@�O�@�/@�V@��@���@���@��@�z�@���@���@��@�l�@�33@��@���@��@���@�ff@�$�@��-@�`B@�G�@��@��/@��9@�r�@�1'@��
@�|�@�K�@�o@��!@���@�n�@�J@���@���@���@�x�@�?}@��@�%@��`@�Ĝ@��D@�A�@�1@��@�P@l�@~�@~$�@~@}��@}��@}`B@}V@|�/@|�@|z�@|Z@{�F@{@zM�@y�#@y7L@x��@xĜ@x��@x�u@xr�@x �@xb@w��@w|�@w�@vv�@v{@u��@u��@u�@u?}@t�@tI�@t�@s��@s"�@r�@r�@r�@r�!@rM�@q�@q��@qhs@q&�@p��@p�u@pQ�@o�@o�w@o�P@ol�@oK�@o+@o
=@n�y@nȴ@nv�@n{@m�T@m�T@m��@m?}@l��@l��@l��@lI�@k�m@k�@k33@ko@j�H@j��@jn�@j�@i�@h��@h�9@hA�@h  @g�w@g��@gl�@g
=@fV@f@e��@e�h@eV@d9X@c�
@c33@b��@b�@a��@a�#@a�#@a��@a��@a�7@a%@`Q�@_�@_;d@^$�@]��@]��@]p�@\�D@\(�@[�F@Z�@Z��@Zn�@ZJ@YG�@X��@Xb@WK�@V�y@Vff@V@U��@U�@UO�@UV@T�D@T9X@T1@Sƨ@St�@SS�@So@RM�@R-@Q��@Q��@Q��@P�`@PĜ@P�u@Pb@O��@O��@O|�@O+@N�y@Nff@N{@M@M�h@Mp�@MO�@MO�@M/@L��@LI�@L9X@L�@L1@K�
@K��@K�@KdZ@KC�@Ko@J�H@J��@J�!@J��@J=q@J�@J�@I��@I�@I��@I��@Ix�@I&�@HĜ@H�u@HbN@H1'@G�@G�w@G��@G��@Gl�@G+@F�R@Fv�@F{@E��@E@E��@E?}@D�@D��@Dz�@D(�@C�F@CdZ@B��@BM�@B=q@A��@A��@A%@@bN@@b@?�;@?l�@?
=@>�@>v�@>$�@=`B@=V@<��@<�@<z�@<(�@;��@;ƨ@;dZ@;o@:�H@:��@:n�@9�@9�#@9x�@9X@9G�@9X@9&�@8��@8b@7�@7�;@7�;@7�w@7l�@7+@6�y@6��@6V@6{@5@5��@5�@4�j@4�@4�D@4j@4�@3�m@3�
@3��@3S�@3o@2��@2��@2��@2��@2��@2~�@2~�@2=q@1��@1�@1��@1x�@17L@1&�@0Ĝ@0r�@0b@/��@/�@/��@/�@.��@.@-@-�@-�@-�@-O�@,�@,�D@,Z@,I�@+ƨ@+�@+o@*��@*��@*=q@)��@)�#@)��@)��@)��@)��@)X@)G�@)�@(�`@(bN@(A�@(  @'�w@'��@'�P@'l�@'+@'
=@&�R@&V@&$�@%�@%�T@%�@%��@%�@%p�@%O�@$�/@$��@$��@$��@$�D@$j@$�@#�
@#��@#��@#t�@#dZ@#C�@"�@"n�@"J@!��@!�#@!�^@!��@!x�@!hs@!&�@!�@ ��@ �9@ Q�@ b@   @�P@|�@\)@;d@
=@ȴ@E�@��@�@p�@O�@�@��@�@��@�@9X@�@1@1@��@�F@��@t�@S�@33@"�@"�@"�@o@�@��@n�@-@J@��@�@�^@x�@hs@X@G�@��@��@�u@  @�;@�w@�@l�@;d@��@v�@5?@�T@@@�-@�@`B@`B@?}@��@�j@�j@z�@Z@9X@1@�F@S�@S�@C�@��@~�@^5@=q@-@�@��@�#@�#@x�@X@X@G�@&�@%@Ĝ@r�@1'@ �@�w@�P@\)@
=@�y@ȴ@v�@ff@ff@V@5?@{@��@�-@�h@O�@��@�@�/@��@�j@�D@I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�dZA�\)A�\)A�\)A�\)A�^5A�bNA�`BA�`BA�`BA�bNA�dZA�bNA�\)A�7LA��A�A��A�t�A�I�A��AЋDA�33A���A���A�x�Aʰ!A���A��A�VA�v�A�A�=qA��TA��
A��
A�JA��/A�bNA��HA�G�A��DA��A��A��wA��A��A�|�A�bA��/A�ȴA�|�A�-A�p�A��yA���A�r�A��A�Q�A�dZA��A��A��A��A�v�A�VA��#A�S�A�+A�E�A��;A�z�A��A�A�A�oA�A�A��#A��A��A��mA���A�A��\A���A�VA��#A�$�A�-A�dZA��FA�oA�A��FA��hA�33A��hA�JA�A�`BA���A��FA��yA�S�A�ffA�Q�A��A�7LA��9A��`A�$�A���A��HA�JA�&�A�VA�|�A��jA��A��A�z�A���A��A�dZA~$�Ax~�Au��As�As�;Arn�Ao�AoAo�hAodZAm�Al�DAk�
Aip�Ah��Ag�PAe�Ab�A`�9A^�yA]XA\�9A[��AX�jAV�!ATAPbNANn�AM�FAM33AKO�AI�hAG�wAEAC�-AC33AA�FA@E�A>I�A<ZA<(�A<(�A;�A:JA9�A7�A533A4z�A45?A3XA2��A1C�A0ffA/�;A/t�A.��A-��A-�A,E�A+33A*��A)��A(�DA'�A$��A#VA!p�A�AE�A"�A��AoAr�A|�A��A�9A�TAoA��A~�Av�A^5A`BA�;A��A�\AE�A|�AdZA�A�^AQ�A
=A
{A	�PA��A�A��A�PAK�A�AbNA�/A�A�AoAv�A�AA �@�K�@�&�@�A�@�;d@�-@��D@�~�@�7L@���@�l�@�@�7@�bN@��@��#@�1'@�!@��@���@�9X@�w@�"�@��T@��`@�ƨ@�"�@�O�@��
@ߕ�@���@���@ܴ9@�C�@��@�G�@׾w@��#@ԋD@ӥ�@�~�@���@ѩ�@�V@�Z@�o@�&�@˝�@���@���@��@ǅ@�|�@�|�@�\)@�33@��@�v�@�J@��@��@���@��`@���@��\@��7@��u@��@���@�33@�V@�@�`B@�%@�(�@��@�@�^5@���@��T@��-@��9@��F@�+@��+@�{@��-@�G�@�j@��w@�\)@�o@�V@���@���@��h@�x�@�7L@���@�9X@���@��w@�dZ@��H@��\@�~�@�^5@��@��h@�/@���@�1@��@�@�V@���@�O�@��@�bN@�(�@���@��w@��P@�dZ@�S�@���@�M�@�J@�x�@��u@�bN@��@��P@�l�@��H@�~�@�$�@��@��@��@�j@� �@�b@��@�l�@��@���@��@���@���@�Ĝ@���@�Z@�  @��
@���@�|�@�;d@��@�ȴ@�E�@��#@�V@�v�@�ff@�ff@�J@��h@��9@�Z@�x�@��#@���@�@��h@�hs@�/@���@��9@��@���@�Q�@�9X@�1@���@�o@���@��y@���@�^5@���@��h@�X@�?}@�V@��j@� �@��w@�|�@�S�@��@��!@���@�^5@�-@�{@�{@�{@�5?@�$�@��@�{@��@���@�&�@��/@��u@��@��D@�z�@�bN@�(�@��@�ƨ@���@��P@���@�|�@�l�@�K�@�
=@���@�v�@�V@�$�@�J@�@���@���@�O�@�/@�V@��@���@���@��@�z�@���@���@��@�l�@�33@��@���@��@���@�ff@�$�@��-@�`B@�G�@��@��/@��9@�r�@�1'@��
@�|�@�K�@�o@��!@���@�n�@�J@���@���@���@�x�@�?}@��@�%@��`@�Ĝ@��D@�A�@�1@��@�P@l�@~�@~$�@~@}��@}��@}`B@}V@|�/@|�@|z�@|Z@{�F@{@zM�@y�#@y7L@x��@xĜ@x��@x�u@xr�@x �@xb@w��@w|�@w�@vv�@v{@u��@u��@u�@u?}@t�@tI�@t�@s��@s"�@r�@r�@r�@r�!@rM�@q�@q��@qhs@q&�@p��@p�u@pQ�@o�@o�w@o�P@ol�@oK�@o+@o
=@n�y@nȴ@nv�@n{@m�T@m�T@m��@m?}@l��@l��@l��@lI�@k�m@k�@k33@ko@j�H@j��@jn�@j�@i�@h��@h�9@hA�@h  @g�w@g��@gl�@g
=@fV@f@e��@e�h@eV@d9X@c�
@c33@b��@b�@a��@a�#@a�#@a��@a��@a�7@a%@`Q�@_�@_;d@^$�@]��@]��@]p�@\�D@\(�@[�F@Z�@Z��@Zn�@ZJ@YG�@X��@Xb@WK�@V�y@Vff@V@U��@U�@UO�@UV@T�D@T9X@T1@Sƨ@St�@SS�@So@RM�@R-@Q��@Q��@Q��@P�`@PĜ@P�u@Pb@O��@O��@O|�@O+@N�y@Nff@N{@M@M�h@Mp�@MO�@MO�@M/@L��@LI�@L9X@L�@L1@K�
@K��@K�@KdZ@KC�@Ko@J�H@J��@J�!@J��@J=q@J�@J�@I��@I�@I��@I��@Ix�@I&�@HĜ@H�u@HbN@H1'@G�@G�w@G��@G��@Gl�@G+@F�R@Fv�@F{@E��@E@E��@E?}@D�@D��@Dz�@D(�@C�F@CdZ@B��@BM�@B=q@A��@A��@A%@@bN@@b@?�;@?l�@?
=@>�@>v�@>$�@=`B@=V@<��@<�@<z�@<(�@;��@;ƨ@;dZ@;o@:�H@:��@:n�@9�@9�#@9x�@9X@9G�@9X@9&�@8��@8b@7�@7�;@7�;@7�w@7l�@7+@6�y@6��@6V@6{@5@5��@5�@4�j@4�@4�D@4j@4�@3�m@3�
@3��@3S�@3o@2��@2��@2��@2��@2��@2~�@2~�@2=q@1��@1�@1��@1x�@17L@1&�@0Ĝ@0r�@0b@/��@/�@/��@/�@.��@.@-@-�@-�@-�@-O�@,�@,�D@,Z@,I�@+ƨ@+�@+o@*��@*��@*=q@)��@)�#@)��@)��@)��@)��@)X@)G�@)�@(�`@(bN@(A�@(  @'�w@'��@'�P@'l�@'+@'
=@&�R@&V@&$�@%�@%�T@%�@%��@%�@%p�@%O�@$�/@$��@$��@$��@$�D@$j@$�@#�
@#��@#��@#t�@#dZ@#C�@"�@"n�@"J@!��@!�#@!�^@!��@!x�@!hs@!&�@!�@ ��@ �9@ Q�@ b@   @�P@|�@\)@;d@
=@ȴ@E�@��@�@p�@O�@�@��@�@��@�@9X@�@1@1@��@�F@��@t�@S�@33@"�@"�@"�@o@�@��@n�@-@J@��@�@�^@x�@hs@X@G�@��@��@�u@  @�;@�w@�@l�@;d@��@v�@5?@�T@@@�-@�@`B@`B@?}@��@�j@�j@z�@Z@9X@1@�F@S�@S�@C�@��@~�@^5@=q@-@�@��@�#@�#@x�@X@X@G�@&�@%@Ĝ@r�@1'@ �@�w@�P@\)@
=@�y@ȴ@v�@ff@ff@V@5?@{@��@�-@�h@O�@��@�@�/@��@�j@�D@I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A� �A�$�A�(�A�/A�33A�=qA�E�A�I�A�K�A�K�A�M�A�O�A�Q�A�\)A�bNA�jA�v�AőhAŕ�AœuAōPAŁA�v�AŁAŧ�AŰ!A��A�A��A�G�A�S�A�^5A�ffA�^5A�t�AƅAƃAƃA�z�AƗ�A���A��#A��A���A�  A�1A��A� �A�"�A�"�A�(�A�+A�;dA�G�A�Q�A�M�A�M�A�ZA�l�A�r�A�r�A�p�A�jA�t�A�t�A�n�A�jA�n�Aǉ7Aǉ7AǍPAǓuAǑhAǙ�Aǝ�AǗ�AǗ�Aǡ�Aǥ�Aǡ�Aǥ�Aǡ�Aǧ�Aǩ�Aǡ�Aǟ�Aǡ�Aǣ�Aǣ�Aǧ�Aǧ�Aǥ�Aǣ�Aǟ�Aǝ�AǕ�AǗ�AǑhAǏ\AǋDAǅA�|�A�v�A�n�A�^5A�VA�K�A�;dA�7LA�/A�$�A��A�1A���A��A��TA��HA��
A�ȴAƾwAƮAƏ\AƉ7A�r�A�l�A�jA�n�A�v�A�v�A�t�A�p�A�ffA�jA�dZA�\)A�ZA�Q�A�G�A�G�A�E�A�A�A�A�A�C�A�;dA�/A�-A� �A��A��A��A��A�bA�JA�%A�  A�A�  A���A���A��A���A�  A���A���A��A��A��A��A��A��A��A���A��A���A���A���A���A�  A�A�  A�  A�%A�  A�  A�A�oA�VA��mA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��A��A���A���A���A��A��yA��A��A��A��A���A���A���A���A�A�  A�A�  A���A���A�A�1A�1A�%A�1A�%A�JA�
=A�VA�{A�{A�oA�oA�{A��A� �A��A�"�A��A�"�A�$�A� �A�&�A�-A�1'A�5?A�7LA�7LA�7LA�7LA�;dA�=qA�?}A�=qA�E�A�K�A�I�A�K�A�O�A�Q�A�ZA�`BA�`BA�ffA�p�A�t�A�x�A�|�AƁAƁAƁAƁAƁAƍPAƓuAƕ�Aƕ�AƟ�Aƥ�Aƥ�AƧ�AƩ�AƩ�AƩ�AƬAƩ�AƩ�Aƴ9Aƺ^AƾwA�ĜA���A���A��#A��HA��HA��TA��A��A��A��A���A���A�  A�1A�JA�
=A�
=A�oA��A��A�"�A�&�A�+A�-A�5?A�;dA�A�A�C�A�I�A�Q�A�S�A�S�A�S�A�VA�XA�^5A�^5A�`BA�hsA�r�A�z�A�|�A�~�AǃAǇ+AǋDAǑhAǙ�Aǟ�Aǥ�AǮAǺ^AǸRAǩ�Aǧ�Aǧ�Aǩ�AǬAǬAǬAǩ�Aǰ!AǴ9AǴ9AǼjA�ȴA�ȴA���A���A���A��A��/A��;A��HA��`A��yA��A��A��A���A���A���A���A�  A�A�
=A�bA�oA�{A��A��A�$�A�$�A�+A�1'A�9XA�C�A�M�A�VA�\)A�^5A�dZA�dZA�ffA�n�A�v�A�|�A�n�A�bNA�dZA�ffA�ffA�jA�jA�jA�l�A�jA�jA�n�A�p�A�v�A�|�A�z�A�z�A�z�A�|�A�~�AȁAȃAȃAȃAȅAȉ7AȍPAȏ\AȑhAȗ�Aȗ�Aș�Aȡ�Aȩ�AȮAȲ-Aȴ9AȶFAȸRAȺ^AȺ^AȾwA�ĜA�ƨAȲ-AȮAȮAȮAȮAȮAȰ!AȲ-AȲ-Aȴ9Aȴ9AȶFAȸRAȺ^AȼjAȼjAȼjAȼjAȼjAȾwAȾwAȾwAȾwA���A�ƨA���A���A��#A��;A��HA��TA��`A��A��A��A���A���A��A��#A��#A��/A��#A��/A��HA��`A��TA��`A��mA��mA��yA��yA��A��A��A��A��A��A��A���A���A���A���A���A�%A�
=A�JA�bA�oA�{A��A��A��A� �A��A�
=A�%A�A�A�A�A�%A�%A�%A�%A�%A�1A�
=A�JA�VA�bA�oA�oA�oA�bA�bA�oA�oA�oA�oA�{A��A��A�"�A�(�A�-A�/A�1'A�5?A�7LA�;dA�?}A�A�A�+A� �A��A� �A� �A� �A� �A�"�A�"�A�"�A�$�A�$�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�33A�9XA�=qA�A�A�C�A�I�A�K�A�O�A�Q�A�Q�A�E�A�5?A�33A�7LA�5?A�5?A�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�9XA�;dA�=qA�=qA�?}A�A�A�A�A�C�A�K�A�Q�A�VA�ZA�\)A�^5A�dZA�ffA�jA�XA�M�A�K�A�K�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�Q�A�S�A�S�A�VA�XA�XA�XA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�^5A�bNA�l�A�t�A�x�A�|�AɁAɃA�~�A�ffA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�hsA�jA�jA�jA�jA�jA�jA�jA�jA�jA�l�A�t�A�z�AɁAɅAɋDAɍPAɑhAɋDA�r�A�p�A�n�A�n�A�n�A�p�A�p�A�p�A�p�A�r�A�p�A�r�A�p�A�p�A�p�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�AɁAɇ+AɍPAɓuAɗ�Aə�A�~�A�|�A�z�A�z�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�AɃAɃAɃAɁAɅAɃAɃAɅAɏ\Aə�Aɡ�Aɥ�Aɝ�AɋDAɉ7Aɉ7AɋDAɉ7Aɉ7Aɉ7Aɉ7AɋDAɋDAɋDAɉ7AɋDAɋDAɋDAɋDAɋDAɋDAɍPAɍPAɍPAɍPAɏ\Aɏ\Aɏ\Aɏ\Aɏ\Aɏ\Aɏ\Aɏ\AɑhAɑhAɑhAɓuAɑhAɑhAɑhAɑhAɑhAɑhAɓuAɓuAɓuAɓuAɕ�AɓuAɓuAɕ�Aɕ�Aɕ�Aɕ�Aɕ�Aɗ�Aɗ�Aə�Aə�Aə�Aə�Aə�Aə�Aə�Aɛ�Aɛ�Aə�Aɛ�Aɛ�Aɝ�Aɝ�Aɝ�Aɝ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɝ�Aɟ�Aɟ�Aɟ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɟ�Aɝ�Aɝ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɡ�Aɡ�Aɡ�Aɣ�Aɣ�Aɥ�Aɥ�Aɥ�Aɥ�Aɥ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɩ�Aɧ�Aɩ�Aɩ�Aɩ�AɬAɩ�AɬAɬAɬAɬAɬAɬAɮAɮAɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɲ-Aɰ!Aɲ-Aɰ!Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɴ9Aɴ9Aɴ9Aɲ-Aɲ-Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9AɶFAɶFAɶFAɶFAɶFAɶFAɶFAɸRAɸRAɸRAɺ^AɸRAɸRAɺ^Aɺ^AɸRAɺ^Aɺ^Aɺ^AɸRAɺ^Aɺ^Aɺ^Aɺ^Aɺ^AɼjAɼjAɺ^AɼjAɼjAɼjAɾwAɾwAɾwAɾwAɾwAɼjAɾwAɾwAɾwAɾwAɼjAɾwAɾwAɾwAɾwAɾwAɾwAɾwAɾwA���A���A���A�A�A���A���A�A���A���A�A�A�A�ĜA�ĜA�ĜA�ĜA�A�ĜA�Ĝ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  A��A� �A�$�A�(�A�/A�33A�=qA�E�A�I�A�K�A�K�A�M�A�O�A�Q�A�\)A�bNA�jA�v�AőhAŕ�AœuAōPAŁA�v�AŁAŧ�AŰ!A��A�A��A�G�A�S�A�^5A�ffA�^5A�t�AƅAƃAƃA�z�AƗ�A���A��#A��A���A�  A�1A��A� �A�"�A�"�A�(�A�+A�;dA�G�A�Q�A�M�A�M�A�ZA�l�A�r�A�r�A�p�A�jA�t�A�t�A�n�A�jA�n�Aǉ7Aǉ7AǍPAǓuAǑhAǙ�Aǝ�AǗ�AǗ�Aǡ�Aǥ�Aǡ�Aǥ�Aǡ�Aǧ�Aǩ�Aǡ�Aǟ�Aǡ�Aǣ�Aǣ�Aǧ�Aǧ�Aǥ�Aǣ�Aǟ�Aǝ�AǕ�AǗ�AǑhAǏ\AǋDAǅA�|�A�v�A�n�A�^5A�VA�K�A�;dA�7LA�/A�$�A��A�1A���A��A��TA��HA��
A�ȴAƾwAƮAƏ\AƉ7A�r�A�l�A�jA�n�A�v�A�v�A�t�A�p�A�ffA�jA�dZA�\)A�ZA�Q�A�G�A�G�A�E�A�A�A�A�A�C�A�;dA�/A�-A� �A��A��A��A��A�bA�JA�%A�  A�A�  A���A���A��A���A�  A���A���A��A��A��A��A��A��A��A���A��A���A���A���A���A�  A�A�  A�  A�%A�  A�  A�A�oA�VA��mA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��A��A���A���A���A��A��yA��A��A��A��A���A���A���A���A�A�  A�A�  A���A���A�A�1A�1A�%A�1A�%A�JA�
=A�VA�{A�{A�oA�oA�{A��A� �A��A�"�A��A�"�A�$�A� �A�&�A�-A�1'A�5?A�7LA�7LA�7LA�7LA�;dA�=qA�?}A�=qA�E�A�K�A�I�A�K�A�O�A�Q�A�ZA�`BA�`BA�ffA�p�A�t�A�x�A�|�AƁAƁAƁAƁAƁAƍPAƓuAƕ�Aƕ�AƟ�Aƥ�Aƥ�AƧ�AƩ�AƩ�AƩ�AƬAƩ�AƩ�Aƴ9Aƺ^AƾwA�ĜA���A���A��#A��HA��HA��TA��A��A��A��A���A���A�  A�1A�JA�
=A�
=A�oA��A��A�"�A�&�A�+A�-A�5?A�;dA�A�A�C�A�I�A�Q�A�S�A�S�A�S�A�VA�XA�^5A�^5A�`BA�hsA�r�A�z�A�|�A�~�AǃAǇ+AǋDAǑhAǙ�Aǟ�Aǥ�AǮAǺ^AǸRAǩ�Aǧ�Aǧ�Aǩ�AǬAǬAǬAǩ�Aǰ!AǴ9AǴ9AǼjA�ȴA�ȴA���A���A���A��A��/A��;A��HA��`A��yA��A��A��A���A���A���A���A�  A�A�
=A�bA�oA�{A��A��A�$�A�$�A�+A�1'A�9XA�C�A�M�A�VA�\)A�^5A�dZA�dZA�ffA�n�A�v�A�|�A�n�A�bNA�dZA�ffA�ffA�jA�jA�jA�l�A�jA�jA�n�A�p�A�v�A�|�A�z�A�z�A�z�A�|�A�~�AȁAȃAȃAȃAȅAȉ7AȍPAȏ\AȑhAȗ�Aȗ�Aș�Aȡ�Aȩ�AȮAȲ-Aȴ9AȶFAȸRAȺ^AȺ^AȾwA�ĜA�ƨAȲ-AȮAȮAȮAȮAȮAȰ!AȲ-AȲ-Aȴ9Aȴ9AȶFAȸRAȺ^AȼjAȼjAȼjAȼjAȼjAȾwAȾwAȾwAȾwA���A�ƨA���A���A��#A��;A��HA��TA��`A��A��A��A���A���A��A��#A��#A��/A��#A��/A��HA��`A��TA��`A��mA��mA��yA��yA��A��A��A��A��A��A��A���A���A���A���A���A�%A�
=A�JA�bA�oA�{A��A��A��A� �A��A�
=A�%A�A�A�A�A�%A�%A�%A�%A�%A�1A�
=A�JA�VA�bA�oA�oA�oA�bA�bA�oA�oA�oA�oA�{A��A��A�"�A�(�A�-A�/A�1'A�5?A�7LA�;dA�?}A�A�A�+A� �A��A� �A� �A� �A� �A�"�A�"�A�"�A�$�A�$�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�33A�9XA�=qA�A�A�C�A�I�A�K�A�O�A�Q�A�Q�A�E�A�5?A�33A�7LA�5?A�5?A�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�9XA�;dA�=qA�=qA�?}A�A�A�A�A�C�A�K�A�Q�A�VA�ZA�\)A�^5A�dZA�ffA�jA�XA�M�A�K�A�K�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�Q�A�S�A�S�A�VA�XA�XA�XA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�^5A�bNA�l�A�t�A�x�A�|�AɁAɃA�~�A�ffA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�hsA�jA�jA�jA�jA�jA�jA�jA�jA�jA�l�A�t�A�z�AɁAɅAɋDAɍPAɑhAɋDA�r�A�p�A�n�A�n�A�n�A�p�A�p�A�p�A�p�A�r�A�p�A�r�A�p�A�p�A�p�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�AɁAɇ+AɍPAɓuAɗ�Aə�A�~�A�|�A�z�A�z�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�AɃAɃAɃAɁAɅAɃAɃAɅAɏ\Aə�Aɡ�Aɥ�Aɝ�AɋDAɉ7Aɉ7AɋDAɉ7Aɉ7Aɉ7Aɉ7AɋDAɋDAɋDAɉ7AɋDAɋDAɋDAɋDAɋDAɋDAɍPAɍPAɍPAɍPAɏ\Aɏ\Aɏ\Aɏ\Aɏ\Aɏ\Aɏ\Aɏ\AɑhAɑhAɑhAɓuAɑhAɑhAɑhAɑhAɑhAɑhAɓuAɓuAɓuAɓuAɕ�AɓuAɓuAɕ�Aɕ�Aɕ�Aɕ�Aɕ�Aɗ�Aɗ�Aə�Aə�Aə�Aə�Aə�Aə�Aə�Aɛ�Aɛ�Aə�Aɛ�Aɛ�Aɝ�Aɝ�Aɝ�Aɝ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɝ�Aɟ�Aɟ�Aɟ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɝ�Aɟ�Aɝ�Aɝ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɟ�Aɡ�Aɡ�Aɡ�Aɣ�Aɣ�Aɥ�Aɥ�Aɥ�Aɥ�Aɥ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɧ�Aɩ�Aɧ�Aɩ�Aɩ�Aɩ�AɬAɩ�AɬAɬAɬAɬAɬAɬAɮAɮAɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɰ!Aɲ-Aɰ!Aɲ-Aɰ!Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɲ-Aɴ9Aɴ9Aɴ9Aɲ-Aɲ-Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9AɶFAɶFAɶFAɶFAɶFAɶFAɶFAɸRAɸRAɸRAɺ^AɸRAɸRAɺ^Aɺ^AɸRAɺ^Aɺ^Aɺ^AɸRAɺ^Aɺ^Aɺ^Aɺ^Aɺ^AɼjAɼjAɺ^AɼjAɼjAɼjAɾwAɾwAɾwAɾwAɾwAɼjAɾwAɾwAɾwAɾwAɼjAɾwAɾwAɾwAɾwAɾwAɾwAɾwAɾwA���A���A���A�A�A���A���A�A���A���A�A�A�A�ĜA�ĜA�ĜA�ĜA�A�ĜA�Ĝ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210903090059                              AO  ARCAADJP                                                                    20210903090059    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210903090059  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210903090059  QCF$                G�O�G�O�G�O�8000            