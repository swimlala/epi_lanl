CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-10-16T09:00:40Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211016090040  20211016090040  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               yA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٛSua��1   @ٛT�d�@;�bM���c��\(��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         yA   B   F   @�33@�  A   A   A>ffA`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�<�DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ D�|�D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�P D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @}p�@��@��AA<(�A]A}A��HA�{A��HA��HA��HA��HA��HA��HBp�Bp�Bp�B�
B'p�B/p�B7p�B?p�BGp�BOp�BW
>B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��D w
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
}pD
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
D(�D)w
D)�
D*w
D*�
D+}pD+�
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
DPp�DP�
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
Dgp�Dg�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�~�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�8RD�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�xRDԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�xRDڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�xRD滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�K�D�[�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�A�|�ÁA�~�A�~�A̓A�|�ÁA�~�ÁA�~�A�|�A�x�A�z�A�v�A�r�A�r�A�x�A�~�A�~�A�v�A�v�A�M�A�=qA�+A���AǓuA�I�A��mA��A�S�A�VA��A�33A��A���A�A�A�%A���A�
=A�7LA��A��A��A�VA���A�G�A��A���A� �A��7A��A��A��A�{A���A�;dA�~�A�M�A�+A�C�A���A��A���A�K�A��A���A� �A�;dA�A�A�+A�l�A��A�{A�O�A��+A���A�bA���A�A��A���A�ƨA�33A���A���A�`BA���A�r�A�~�A�1'A���A�A�ZA��A��7A�A}��A|jAy�TAw��Au�At��AtjAs"�Ap�\Am�AlffAk��Aj  Af��AchsA`�A`Q�A_|�A]�A\�uAZ�AXjAU�PAT��AR��AR�AP�/AO
=AL�ALz�AK�AK|�AK
=AJ^5AI�AH�AH �AG&�AF1AD��ADZAD1'AC��AB�DA@��A@A?x�A>�uA>1'A=��A<A;�A:�+A:JA9�hA8JA6�RA6^5A5��A5hsA4��A4  A3�FA3S�A3�A2�/A2Q�A1\)A0��A/��A/VA-A-�A+�;A+dZA*�A*JA(�HA(A'VA&=qA%�hA$�HA$z�A${A"��A"1A!dZA!&�A ��A��A�hAl�A��Av�At�A��AVAoAr�AbA�PAC�A��A�A��A��A�At�A~�A�
A�-A(�Al�A��AQ�A9XA�FA�Ap�AȴA-A�A`BA
��A
��A
JA	7LA��A�FAoA�!A�A\)A�A��A�wA�A�/A�+AQ�A{A�-AO�A �@�S�@�G�@�r�@�"�@���@�Q�@�+@��!@�Q�@�&�@�
=@���@���@웦@ꗍ@���@�p�@�j@��@�\)@�`B@ߝ�@�E�@��T@�@ݑh@���@ڸR@١�@�b@�n�@��m@҇+@��@ύP@�M�@�A�@���@��@�&�@ǍP@��@�V@�C�@�{@��#@�G�@�Q�@�l�@�-@��@�bN@��
@�
=@��\@���@��/@�1@�@���@��@��@�Z@�  @���@�o@�E�@���@��7@�G�@�&�@��@�  @��@�M�@�$�@���@���@���@�G�@�Z@���@��H@�@��^@��h@�X@���@� �@���@�dZ@�+@�@���@�E�@���@�x�@�`B@�V@���@�ƨ@��P@�l�@�33@��\@�E�@���@���@��7@��`@�z�@�b@�ƨ@�C�@���@��!@���@�~�@�-@�@�V@���@�1'@�b@��@��@�M�@�@���@���@�`B@��@�9X@��
@��@�^5@�@��-@��@���@���@�r�@�  @���@��y@��\@�ff@�E�@���@�/@���@�1'@��
@�\)@�
=@��\@��@��7@��`@�I�@�  @��F@���@�|�@�S�@�+@�@��!@�5?@���@��@�hs@�`B@�X@�G�@�&�@���@�Ĝ@�Z@�b@���@��@��
@��F@���@�\)@�;d@�@�v�@��@���@��@���@��/@���@�b@���@���@�"�@���@���@���@��+@�ff@��@�@�x�@�&�@��`@��@���@�1'@�  @��@~�@~V@}�T@}`B@|��@|��@|��@|z�@|j@|Z@|Z@{�
@{C�@z��@z��@zM�@y��@x��@x �@w��@w�P@w\)@w+@v�y@v�R@v��@v�+@vE�@u�T@u�-@u`B@t�/@tZ@t1@sƨ@st�@s"�@r�@r��@r�!@r^5@q�#@q7L@q%@pbN@p �@o�;@o|�@oK�@n�R@n{@m`B@mV@l�/@l�D@l9X@k�F@kS�@k33@j��@j-@i�@i�#@jJ@jJ@i�#@ix�@iX@ihs@i7L@g�;@g+@f�R@f5?@e�-@e`B@e�@d��@d�/@d�j@d�D@dZ@d9X@c��@c��@cS�@c33@cC�@cC�@co@b�H@bM�@b-@b�@a�#@a��@ax�@aX@a%@`��@`1'@_��@_K�@_�@^�y@^�@^ȴ@^��@^��@^V@^E�@^5?@^@]@]��@]`B@]V@\9X@[��@[t�@[o@Z�H@Z�H@Z��@Z��@Z�!@Z�!@Z�\@Z^5@Y�#@Yhs@X��@XĜ@X�9@X��@X�u@XQ�@X1'@W�;@W��@X  @W�w@WK�@V�+@U��@U`B@U�@T�/@T9X@S�
@Sƨ@Sƨ@S�
@St�@R��@R�H@R=q@Q��@Qx�@Q&�@P�9@PbN@P �@O|�@O�@Nȴ@N��@N�+@N5?@N@M��@M�-@M��@Mp�@L�/@Kt�@J��@J��@J^5@J=q@JJ@I��@I��@I�7@I&�@H  @G��@G|�@GK�@G
=@Fȴ@F��@F�+@FV@F$�@E�@E�@E@E`B@E�@EV@D�j@DZ@D(�@C�
@CdZ@B��@B�!@B�\@B=q@A��@A��@A�7@AG�@@��@@��@@�u@@�9@@�u@@Q�@@ �@?�P@>��@>��@>�+@>V@>{@=��@=@=`B@=O�@<��@<�/@<�j@<�@<z�@<9X@<�@;ƨ@;�@;S�@;33@;@:�\@:^5@9��@9x�@8��@8Q�@8 �@8  @7�;@7�P@7l�@7\)@6��@6�+@6v�@6{@5�-@5`B@4��@4�j@49X@3��@3�@3@2��@2~�@2^5@2-@1�#@1�7@1%@0�9@0�@0A�@0 �@0  @/��@/�P@/\)@/K�@/;d@.�@.�+@.ff@.V@.E�@.{@-�T@-�-@-�h@-/@-V@,��@,��@,��@,j@,�@+ƨ@+��@+�@+33@*�@*�H@*��@*~�@*M�@*J@)��@)�7@)hs@)G�@)&�@(��@(��@(�9@(�@(A�@(  @'�;@'��@'l�@';d@'�@&�R@&�+@&V@&$�@&$�@%�@%�T@%�-@$��@$�j@$�D@$Z@$9X@$(�@#ƨ@#�F@#��@#�@#dZ@#"�@"�H@"�!@"~�@"^5@"-@"�@!�@!�#@!��@!�^@!x�@!�@!%@ ��@ �`@ �9@ bN@ 1'@�@��@|�@\)@\)@K�@K�@K�@+@��@��@v�@v�@ff@5?@{@�T@�-@��@�@`B@?}@�/@�@z�@I�@(�@1@�m@�
@��@t�@S�@"�@o@@�@�@��@��@�\@~�@-@��@��@�^@�7@hs@G�@7L@&�@%@Ĝ@�u@�@bN@b@�@��@l�@;d@�@�@��@ȴ@�+@E�@@�@�T@�-@�@/@��@�j@z�@Z@9X@��@�
@�F@��@��@�@S�@C�@33@"�@@��@~�@M�@�@�@��@x�@X@7L@�@�`@�u@Q�@1'@�@�w@�P@�P@|�@l�@l�@K�@�@��@ȴ@��@�+@�+@E�@{@�-@`B@�@��@�j@�@��@�D@j@I�@�@1@�m@�F@��@��@�@dZ@33@o@
�@
��@
n�@
n�@
=q@
�@	�@	�#@	��@	��@	��@	��@	�7@	x�@	X@	7L@	%@��@Ĝ@�9@�u@bN@A�@1'@�;@�@|�@l�@\)@;d@�y@ȴ@�R@��@v�@ff@E�@E�@@@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�|�ÁA�~�A�~�A̓A�|�ÁA�~�ÁA�~�A�|�A�x�A�z�A�v�A�r�A�r�A�x�A�~�A�~�A�v�A�v�A�M�A�=qA�+A���AǓuA�I�A��mA��A�S�A�VA��A�33A��A���A�A�A�%A���A�
=A�7LA��A��A��A�VA���A�G�A��A���A� �A��7A��A��A��A�{A���A�;dA�~�A�M�A�+A�C�A���A��A���A�K�A��A���A� �A�;dA�A�A�+A�l�A��A�{A�O�A��+A���A�bA���A�A��A���A�ƨA�33A���A���A�`BA���A�r�A�~�A�1'A���A�A�ZA��A��7A�A}��A|jAy�TAw��Au�At��AtjAs"�Ap�\Am�AlffAk��Aj  Af��AchsA`�A`Q�A_|�A]�A\�uAZ�AXjAU�PAT��AR��AR�AP�/AO
=AL�ALz�AK�AK|�AK
=AJ^5AI�AH�AH �AG&�AF1AD��ADZAD1'AC��AB�DA@��A@A?x�A>�uA>1'A=��A<A;�A:�+A:JA9�hA8JA6�RA6^5A5��A5hsA4��A4  A3�FA3S�A3�A2�/A2Q�A1\)A0��A/��A/VA-A-�A+�;A+dZA*�A*JA(�HA(A'VA&=qA%�hA$�HA$z�A${A"��A"1A!dZA!&�A ��A��A�hAl�A��Av�At�A��AVAoAr�AbA�PAC�A��A�A��A��A�At�A~�A�
A�-A(�Al�A��AQ�A9XA�FA�Ap�AȴA-A�A`BA
��A
��A
JA	7LA��A�FAoA�!A�A\)A�A��A�wA�A�/A�+AQ�A{A�-AO�A �@�S�@�G�@�r�@�"�@���@�Q�@�+@��!@�Q�@�&�@�
=@���@���@웦@ꗍ@���@�p�@�j@��@�\)@�`B@ߝ�@�E�@��T@�@ݑh@���@ڸR@١�@�b@�n�@��m@҇+@��@ύP@�M�@�A�@���@��@�&�@ǍP@��@�V@�C�@�{@��#@�G�@�Q�@�l�@�-@��@�bN@��
@�
=@��\@���@��/@�1@�@���@��@��@�Z@�  @���@�o@�E�@���@��7@�G�@�&�@��@�  @��@�M�@�$�@���@���@���@�G�@�Z@���@��H@�@��^@��h@�X@���@� �@���@�dZ@�+@�@���@�E�@���@�x�@�`B@�V@���@�ƨ@��P@�l�@�33@��\@�E�@���@���@��7@��`@�z�@�b@�ƨ@�C�@���@��!@���@�~�@�-@�@�V@���@�1'@�b@��@��@�M�@�@���@���@�`B@��@�9X@��
@��@�^5@�@��-@��@���@���@�r�@�  @���@��y@��\@�ff@�E�@���@�/@���@�1'@��
@�\)@�
=@��\@��@��7@��`@�I�@�  @��F@���@�|�@�S�@�+@�@��!@�5?@���@��@�hs@�`B@�X@�G�@�&�@���@�Ĝ@�Z@�b@���@��@��
@��F@���@�\)@�;d@�@�v�@��@���@��@���@��/@���@�b@���@���@�"�@���@���@���@��+@�ff@��@�@�x�@�&�@��`@��@���@�1'@�  @��@~�@~V@}�T@}`B@|��@|��@|��@|z�@|j@|Z@|Z@{�
@{C�@z��@z��@zM�@y��@x��@x �@w��@w�P@w\)@w+@v�y@v�R@v��@v�+@vE�@u�T@u�-@u`B@t�/@tZ@t1@sƨ@st�@s"�@r�@r��@r�!@r^5@q�#@q7L@q%@pbN@p �@o�;@o|�@oK�@n�R@n{@m`B@mV@l�/@l�D@l9X@k�F@kS�@k33@j��@j-@i�@i�#@jJ@jJ@i�#@ix�@iX@ihs@i7L@g�;@g+@f�R@f5?@e�-@e`B@e�@d��@d�/@d�j@d�D@dZ@d9X@c��@c��@cS�@c33@cC�@cC�@co@b�H@bM�@b-@b�@a�#@a��@ax�@aX@a%@`��@`1'@_��@_K�@_�@^�y@^�@^ȴ@^��@^��@^V@^E�@^5?@^@]@]��@]`B@]V@\9X@[��@[t�@[o@Z�H@Z�H@Z��@Z��@Z�!@Z�!@Z�\@Z^5@Y�#@Yhs@X��@XĜ@X�9@X��@X�u@XQ�@X1'@W�;@W��@X  @W�w@WK�@V�+@U��@U`B@U�@T�/@T9X@S�
@Sƨ@Sƨ@S�
@St�@R��@R�H@R=q@Q��@Qx�@Q&�@P�9@PbN@P �@O|�@O�@Nȴ@N��@N�+@N5?@N@M��@M�-@M��@Mp�@L�/@Kt�@J��@J��@J^5@J=q@JJ@I��@I��@I�7@I&�@H  @G��@G|�@GK�@G
=@Fȴ@F��@F�+@FV@F$�@E�@E�@E@E`B@E�@EV@D�j@DZ@D(�@C�
@CdZ@B��@B�!@B�\@B=q@A��@A��@A�7@AG�@@��@@��@@�u@@�9@@�u@@Q�@@ �@?�P@>��@>��@>�+@>V@>{@=��@=@=`B@=O�@<��@<�/@<�j@<�@<z�@<9X@<�@;ƨ@;�@;S�@;33@;@:�\@:^5@9��@9x�@8��@8Q�@8 �@8  @7�;@7�P@7l�@7\)@6��@6�+@6v�@6{@5�-@5`B@4��@4�j@49X@3��@3�@3@2��@2~�@2^5@2-@1�#@1�7@1%@0�9@0�@0A�@0 �@0  @/��@/�P@/\)@/K�@/;d@.�@.�+@.ff@.V@.E�@.{@-�T@-�-@-�h@-/@-V@,��@,��@,��@,j@,�@+ƨ@+��@+�@+33@*�@*�H@*��@*~�@*M�@*J@)��@)�7@)hs@)G�@)&�@(��@(��@(�9@(�@(A�@(  @'�;@'��@'l�@';d@'�@&�R@&�+@&V@&$�@&$�@%�@%�T@%�-@$��@$�j@$�D@$Z@$9X@$(�@#ƨ@#�F@#��@#�@#dZ@#"�@"�H@"�!@"~�@"^5@"-@"�@!�@!�#@!��@!�^@!x�@!�@!%@ ��@ �`@ �9@ bN@ 1'@�@��@|�@\)@\)@K�@K�@K�@+@��@��@v�@v�@ff@5?@{@�T@�-@��@�@`B@?}@�/@�@z�@I�@(�@1@�m@�
@��@t�@S�@"�@o@@�@�@��@��@�\@~�@-@��@��@�^@�7@hs@G�@7L@&�@%@Ĝ@�u@�@bN@b@�@��@l�@;d@�@�@��@ȴ@�+@E�@@�@�T@�-@�@/@��@�j@z�@Z@9X@��@�
@�F@��@��@�@S�@C�@33@"�@@��@~�@M�@�@�@��@x�@X@7L@�@�`@�u@Q�@1'@�@�w@�P@�P@|�@l�@l�@K�@�@��@ȴ@��@�+@�+@E�@{@�-@`B@�@��@�j@�@��@�D@j@I�@�@1@�m@�F@��@��@�@dZ@33@o@
�@
��@
n�@
n�@
=q@
�@	�@	�#@	��@	��@	��@	��@	�7@	x�@	X@	7L@	%@��@Ĝ@�9@�u@bN@A�@1'@�;@�@|�@l�@\)@;d@�y@ȴ@�R@��@v�@ff@E�@E�@@@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz�Bz�By�Bx�Bx�Bw�Bw�Bu�Bv�Bu�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Br�Bu�B�bBhsBK�B8RB&�B�BPB��B��B�B�B�yB�fB�BB�B�B��B��BǮBŢB��B�jB�RB�?B�-B��B��B��B��B��B�oB� Bw�BffBW
BG�BB�B8RB0!B,B'�B'�B%�B�B��B�mB�B��B�jB�B��B�uB�Bk�B_;BR�B?}B1'B'�B"�B�B�BbB  B�B�yB�NB�B��BB�?B��B��B�hB� Br�Be`B[#BXBP�B@�B/B#�B�BuB  B�B�NB�)B�B��BǮB��B�LB��B��B��B��B��B��B�{B�oB�\B�JB�DB�7B�7B�%B�+B�B~�Bw�Bq�Br�Bs�Bm�BiyBcTBcTBcTBdZBbNB_;BZBYBW
BT�BR�BK�BI�BH�BE�BC�BA�B?}B?}B=qB<jB:^B7LB6FB0!B/B+B(�B&�B"�B�B�B�B�B{BbBPBJB
=B1B1BBBB  BB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�ZB
�TB
�TB
�HB
�BB
�5B
�/B
�)B
�B
�B
�
B
�B
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
ɺB
ǮB
ƨB
ŢB
ÖB
ÖB
��B
��B
�}B
�wB
�jB
�dB
�^B
�^B
�XB
�XB
�LB
�FB
�FB
�9B
�-B
�'B
�!B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�'B
�-B
�-B
�3B
�9B
�FB
�RB
�dB
�jB
�qB
�}B
�}B
��B
B
ÖB
ƨB
ȴB
ɺB
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
�B
�)B
�5B
�;B
�;B
�BB
�BB
�HB
�`B
�mB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��BBBBB+BDBJBJBPBPBVB\BhBuB�B�B�B�B!�B$�B$�B$�B%�B&�B(�B-B.B.B.B0!B7LB9XB;dB<jB=qB>wBA�BD�BF�BL�BN�BQ�BT�BZB[#B\)B]/BaHBcTBhsBjBjBk�Bo�Br�Bt�By�B{�B}�B~�B�B�%B�=B�VB�hB�uB��B��B��B��B��B��B��B��B�B�!B�'B�B�B�B�'B�FB�^B�wB�}B�wB�}B��B��B�RB�jBĜB9XBm�Bo�Bt�Bw�Bx�Bw�Bx�B� B� B�B�=B�PB�JB�DB�JB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B�B�9B�9B�9B�9B�?B�^B��BĜBȴBȴBƨBĜB��B��B��B��B��BǮBB��B�`B�TB�`B�`B�fB�sB�yB�yB�B�B�B�B�B�B�B��B��B��B��B��B��BBBB+B1B+B
=BPBhBJBVBhB�BoBuB�BuBuBhB\BhB{B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�BuBoBuBoBoBPBVBJB
=B	7BDBDB	7B+BBBBBBBBBBBBBBBBB%B%B+B+B+B+B1B��B��B��BJBPBVB\BbBbBhBhBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B#�B#�B$�B$�B%�B&�B'�B'�B'�B(�B)�B)�B+B+B,B-B/B0!B0!B1'B2-B2-B49B49B49B5?B6FB6FB6FB?}BP�B)�B\BhBPBB��B��B�B�B5?BE�BE�BC�BC�BC�BC�BE�BE�BE�BF�BF�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BJ�BK�BK�BL�BL�BM�BN�BN�BO�BO�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BQ�BR�BR�BS�BS�BT�BW
BW
BW
BW
BXBXBYBYBZBZBZB[#B[#B\)B\)B]/B]/B^5B_;B_;B`BB`BB`BB`BBaHBbNBbNBcTBcTBcTBcTBdZBdZBe`Be`Be`Be`BffBffBffBffBgmBgmBgmBgmBhsBhsBhsBhsBiyBiyBiyBjBjBjBjBk�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bp�Bp�Bp�Bq�Bq�Bq�Br�Br�Br�Br�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bu�Bv�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bx�Bx�By�By�By�Bz�Bz�Bz�B{�B{�B{�B{�B|�B|�B|�B|�B|�B|�B|�B|�B}�B}�B}�B}�B}�B~�B~�B~�B~�B� B~�B~�B� B� B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�%B�%B�+B�+B�+B�1B�1B�1B�1B�1B�1B�7B�7B�7B�7B�7B�=B�=B�DB�DB�DB�DB�DB�JB�JB�JB�JB�JB�JB�JB�PB�PB�PB�PB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�bB�bB�bB�hB�hB�oB�oB�oB�oB�oB�oB�oB�uB�uB�uB�uB�uB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   Bz�Bz�By�Bx�Bx�Bw�Bw�Bu�Bv�Bu�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bt�Bt�Bu�Bt�Br�Bu�B�bBhsBK�B8RB&�B�BPB��B��B�B�B�yB�fB�BB�B�B��B��BǮBŢB��B�jB�RB�?B�-B��B��B��B��B��B�oB� Bw�BffBW
BG�BB�B8RB0!B,B'�B'�B%�B�B��B�mB�B��B�jB�B��B�uB�Bk�B_;BR�B?}B1'B'�B"�B�B�BbB  B�B�yB�NB�B��BB�?B��B��B�hB� Br�Be`B[#BXBP�B@�B/B#�B�BuB  B�B�NB�)B�B��BǮB��B�LB��B��B��B��B��B��B�{B�oB�\B�JB�DB�7B�7B�%B�+B�B~�Bw�Bq�Br�Bs�Bm�BiyBcTBcTBcTBdZBbNB_;BZBYBW
BT�BR�BK�BI�BH�BE�BC�BA�B?}B?}B=qB<jB:^B7LB6FB0!B/B+B(�B&�B"�B�B�B�B�B{BbBPBJB
=B1B1BBBB  BB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�ZB
�TB
�TB
�HB
�BB
�5B
�/B
�)B
�B
�B
�
B
�B
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
ɺB
ǮB
ƨB
ŢB
ÖB
ÖB
��B
��B
�}B
�wB
�jB
�dB
�^B
�^B
�XB
�XB
�LB
�FB
�FB
�9B
�-B
�'B
�!B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�'B
�-B
�-B
�3B
�9B
�FB
�RB
�dB
�jB
�qB
�}B
�}B
��B
B
ÖB
ƨB
ȴB
ɺB
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
�B
�)B
�5B
�;B
�;B
�BB
�BB
�HB
�`B
�mB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��BBBBB+BDBJBJBPBPBVB\BhBuB�B�B�B�B!�B$�B$�B$�B%�B&�B(�B-B.B.B.B0!B7LB9XB;dB<jB=qB>wBA�BD�BF�BL�BN�BQ�BT�BZB[#B\)B]/BaHBcTBhsBjBjBk�Bo�Br�Bt�By�B{�B}�B~�B�B�%B�=B�VB�hB�uB��B��B��B��B��B��B��B��B�B�!B�'B�B�B�B�'B�FB�^B�wB�}B�wB�}B��B��B�RB�jBĜB9XBm�Bo�Bt�Bw�Bx�Bw�Bx�B� B� B�B�=B�PB�JB�DB�JB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B�B�9B�9B�9B�9B�?B�^B��BĜBȴBȴBƨBĜB��B��B��B��B��BǮBB��B�`B�TB�`B�`B�fB�sB�yB�yB�B�B�B�B�B�B�B��B��B��B��B��B��BBBB+B1B+B
=BPBhBJBVBhB�BoBuB�BuBuBhB\BhB{B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�BuBoBuBoBoBPBVBJB
=B	7BDBDB	7B+BBBBBBBBBBBBBBBBB%B%B+B+B+B+B1B��B��B��BJBPBVB\BbBbBhBhBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B#�B#�B$�B$�B%�B&�B'�B'�B'�B(�B)�B)�B+B+B,B-B/B0!B0!B1'B2-B2-B49B49B49B5?B6FB6FB6FB?}BP�B)�B\BhBPBB��B��B�B�B5?BE�BE�BC�BC�BC�BC�BE�BE�BE�BF�BF�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BJ�BK�BK�BL�BL�BM�BN�BN�BO�BO�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BQ�BR�BR�BS�BS�BT�BW
BW
BW
BW
BXBXBYBYBZBZBZB[#B[#B\)B\)B]/B]/B^5B_;B_;B`BB`BB`BB`BBaHBbNBbNBcTBcTBcTBcTBdZBdZBe`Be`Be`Be`BffBffBffBffBgmBgmBgmBgmBhsBhsBhsBhsBiyBiyBiyBjBjBjBjBk�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bp�Bp�Bp�Bq�Bq�Bq�Br�Br�Br�Br�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bu�Bv�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bx�Bx�By�By�By�Bz�Bz�Bz�B{�B{�B{�B{�B|�B|�B|�B|�B|�B|�B|�B|�B}�B}�B}�B}�B}�B~�B~�B~�B~�B� B~�B~�B� B� B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�%B�%B�+B�+B�+B�1B�1B�1B�1B�1B�1B�7B�7B�7B�7B�7B�=B�=B�DB�DB�DB�DB�DB�JB�JB�JB�JB�JB�JB�JB�PB�PB�PB�PB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�bB�bB�bB�hB�hB�oB�oB�oB�oB�oB�oB�oB�uB�uB�uB�uB�uB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211016090040                              AO  ARCAADJP                                                                    20211016090040    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211016090040  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211016090040  QCF$                G�O�G�O�G�O�C000            