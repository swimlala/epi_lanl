CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-01-21T10:01:57Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  `   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ox   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ۈ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޸   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20220121100157  20220121100157  4903174 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               xA   AO  7230                            2B  A   NAVIS_A                         0967                            170425                          863 @ٳ��X�f1   @ٳ��8�P@6�~��"��c\ě��T1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         xA   B   F   @�33@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@}p�@��@��AA=A]A}A��HA��HA��A��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D�Dw
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
D@}pD@�
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
DI�DJw
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
Dq}pDq�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aė�Aę�Aė�Aė�Aę�Aę�Aě�Aę�Aě�Ağ�Aě�Aę�Ağ�A�x�A�l�A�jA�bNA�VA�E�A��A��A���A��TAöFAé�AËDA�ffA�E�A� �A��A��A��A�1A���A�A��RA���A��hA��A�jA�9XA�VA�%A���A��A��
A��A�S�A��A�1'A���A�p�A��7A�A��RA��jA��FA��hA�A�S�A���A�A�O�A���A�33A���A�1A���A�9XA��hA�oA�ZA�(�A��A�{A���A�oA�%A��A�M�A�A�ffA��A�%A�VA�x�A�r�A��mA�I�A���A�1A�oA���A��A��A�C�A�ƨA��A��A�dZA���A��A���A��PA��A�;dA��A�Q�A���A��PA��A���A��A��RA���A��A�&�A�l�A�^5A\)A~ZA|��A{��Ax�+Au�7AtA�As|�Aq��An�!Ak33Ah�Ag�hAf��AbjA_G�A]�A[�AY�7AW�AU�#AT��AS33AQ33AO�hAMp�AL��AKG�AIt�AFr�AC�AA�A@$�A>VA;A:$�A7��A6�\A5?}A3hsA1��A1�A0M�A.��A-A,��A,VA+�;A+��A*�A)l�A'��A&ȴA&��A&1A$��A#�A"�+A!��A �9A 1AA$�A�FA�A�\A��A��A$�AhsA�HA��A7LA�/A�TAv�A��A�HAjA�A�mA��A��A�TA�A+A	&�A1'A7LAĜA{A�A �A�AjA�#A ��@�C�@��@��!@�M�@���@��@�n�@�p�@�dZ@��^@�p�@�@�@��@�@���@�@��`@�+@�J@�@�%@�t�@�-@�ƨ@�;d@��y@��@�v�@�@�^5@١�@�n�@���@�dZ@���@�V@�7L@Լj@�-@���@Л�@Ѳ-@��#@Ѓ@��y@��y@Ώ\@́@�b@�~�@�@�/@�bN@ư!@���@�/@ċD@å�@�;d@�ff@��@��^@�?}@��j@�|�@�M�@��@�&�@�Q�@��@�|�@�o@���@�~�@�=q@���@���@�x�@�G�@��@��@�Ĝ@��@��;@��@�5?@���@���@��^@��7@��@�O�@��j@���@��H@�E�@�+@���@���@���@��@�X@���@��m@�
=@�E�@�^5@��w@���@�l�@��@���@���@��@�=q@���@� �@�+@���@��@��@�X@��@��@���@�C�@�o@�n�@��@���@���@���@��7@���@���@���@��/@��@�bN@�A�@�A�@�b@���@���@�"�@���@���@���@�n�@�E�@�5?@�{@���@��@�&�@���@��u@�j@�9X@��
@���@��@�33@��@��@���@��@��@��y@���@���@���@�V@�{@�J@�@���@��h@�hs@��/@���@���@��u@��D@�r�@�I�@�Q�@�j@��@�Q�@�A�@�b@���@���@��\@�~�@�n�@�ff@�M�@��@��@�@��^@��h@�O�@���@��@�I�@�1@��m@�ƨ@��P@�\)@�o@���@���@�n�@�M�@�=q@�@��7@�?}@��/@�A�@��m@��@�dZ@�\)@�;d@��y@�v�@�$�@��T@��T@��#@�@���@��@�`B@�7L@��/@��9@�r�@� �@��@���@��P@�K�@�@��@��y@���@���@�~�@�n�@�ff@�M�@�{@��@���@���@��^@��@�G�@�?}@��@��j@�j@�Q�@�1'@�1@�ƨ@���@�|�@�K�@�
=@��R@��\@�E�@�J@���@���@�X@�&�@�V@�V@���@��`@���@���@�Ĝ@��j@��@���@��D@�I�@�b@��@�P@\)@~��@}��@}�@}V@|�j@|j@{t�@z��@z-@y��@y&�@x�`@x�@xA�@xb@w�;@w�;@w+@v��@vv�@v5?@u�-@up�@u�@t9X@s�@r�H@r�!@r-@q�7@p�`@pQ�@o�@n��@n�y@n�@nff@m�h@m`B@m/@mV@l�@l�j@lz�@lz�@l9X@l�@k�m@kt�@k33@j�!@jn�@j=q@i�@i�7@iX@i&�@hbN@g�@g+@fȴ@f��@fff@f$�@f{@f@e��@e�h@ep�@e/@d�/@d�@dZ@d�@cƨ@cS�@c@b��@b~�@b-@a��@aG�@`��@`��@`�9@`bN@` �@_��@_;d@_+@_�@^�+@^5?@^{@]�T@]��@]`B@]?}@]V@\��@\�@[�
@[��@["�@Z�!@Z-@ZJ@Y�^@YX@Y�@X�`@X��@X �@W��@W|�@W�@V��@VV@V5?@V{@V@U�h@U�@U�@U�@U/@T�@T�/@T��@S�m@S��@SS�@SC�@SC�@R��@R^5@R=q@R-@Q�@Q�^@Q��@Qx�@Q&�@P�9@Pr�@PA�@Pb@O|�@O
=@N@M�-@M�h@M�@M`B@M�@L��@Lj@LI�@K�m@Kt�@K33@K"�@J�!@J�\@JJ@Ihs@IG�@I&�@I%@H��@H�u@H �@G��@GK�@Fȴ@F5?@E��@E�-@E�h@E`B@E�@D�j@DI�@D9X@D1@C�m@C�m@C�m@C�
@CS�@B��@BM�@A�^@A��@Ax�@A7L@A%@@�`@@��@@Ĝ@@�@@r�@@A�@?�;@?��@?;d@>��@>��@>�+@>v�@>ff@>5?@=�T@=�-@=p�@<�@<Z@<(�@;�F@;t�@;dZ@;C�@;@:�!@:~�@:J@97L@9%@8�9@8�@8r�@8A�@7�w@7�@7��@7|�@7K�@7
=@6�@6��@6��@6��@6��@6ff@5�@5�@5V@4�@4j@4(�@3ƨ@3t�@3"�@2�H@2��@2��@2��@2��@2��@2��@2��@2~�@2�@1��@1�@1��@1��@1x�@1G�@1%@0��@0�u@0�@0bN@0 �@/�@/K�@/�@.�y@.�y@.ȴ@.�+@.E�@.{@-�-@-�@-V@,��@,��@,j@,Z@,�@+ƨ@+C�@+@*��@*~�@*n�@*n�@*=q@*J@)�#@)��@)x�@(��@(�@(Q�@'�@'�;@'�w@'K�@'
=@'
=@&�@&ȴ@&E�@&{@%�T@%��@%`B@%V@$�@$��@$�@$z�@$Z@$9X@$1@#�F@#dZ@"�@"��@"�!@"~�@"^5@"M�@"-@"�@!��@!x�@!7L@!&�@!&�@!�@!�@!%@ ��@ ��@ Ĝ@ ��@ �@ 1'@   @�@�@��@l�@\)@+@+@�@��@�@�R@V@�T@@@��@�-@`B@�@I�@�@�
@ƨ@��@C�@"�@"�@o@�H@�\@n�@J@�@�@��@�7@G�@�`@��@��@Q�@A�@1'@b@�@��@�@�@l�@K�@+@�y@��@E�@�T@@�h@V@��@�@�/@(�@�m@�m@�
@�
@�
@ƨ@t�@@��@~�@�@�#@x�@�@�`@��@��@��@Ĝ@�9@�9@�u@�@Q�@1'@�;@��@l�@�@�@�y@�@�R@V@$�@{@�@��@�h@?}@��@�D@j@Z@Z@(�@ƨ@�F@��@S�@o@
�H@
��@
��@
^5@
-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144Aė�Aę�Aė�Aė�Aę�Aę�Aě�Aę�Aě�Ağ�Aě�Aę�Ağ�A�x�A�l�A�jA�bNA�VA�E�A��A��A���A��TAöFAé�AËDA�ffA�E�A� �A��A��A��A�1A���A�A��RA���A��hA��A�jA�9XA�VA�%A���A��A��
A��A�S�A��A�1'A���A�p�A��7A�A��RA��jA��FA��hA�A�S�A���A�A�O�A���A�33A���A�1A���A�9XA��hA�oA�ZA�(�A��A�{A���A�oA�%A��A�M�A�A�ffA��A�%A�VA�x�A�r�A��mA�I�A���A�1A�oA���A��A��A�C�A�ƨA��A��A�dZA���A��A���A��PA��A�;dA��A�Q�A���A��PA��A���A��A��RA���A��A�&�A�l�A�^5A\)A~ZA|��A{��Ax�+Au�7AtA�As|�Aq��An�!Ak33Ah�Ag�hAf��AbjA_G�A]�A[�AY�7AW�AU�#AT��AS33AQ33AO�hAMp�AL��AKG�AIt�AFr�AC�AA�A@$�A>VA;A:$�A7��A6�\A5?}A3hsA1��A1�A0M�A.��A-A,��A,VA+�;A+��A*�A)l�A'��A&ȴA&��A&1A$��A#�A"�+A!��A �9A 1AA$�A�FA�A�\A��A��A$�AhsA�HA��A7LA�/A�TAv�A��A�HAjA�A�mA��A��A�TA�A+A	&�A1'A7LAĜA{A�A �A�AjA�#A ��@�C�@��@��!@�M�@���@��@�n�@�p�@�dZ@��^@�p�@�@�@��@�@���@�@��`@�+@�J@�@�%@�t�@�-@�ƨ@�;d@��y@��@�v�@�@�^5@١�@�n�@���@�dZ@���@�V@�7L@Լj@�-@���@Л�@Ѳ-@��#@Ѓ@��y@��y@Ώ\@́@�b@�~�@�@�/@�bN@ư!@���@�/@ċD@å�@�;d@�ff@��@��^@�?}@��j@�|�@�M�@��@�&�@�Q�@��@�|�@�o@���@�~�@�=q@���@���@�x�@�G�@��@��@�Ĝ@��@��;@��@�5?@���@���@��^@��7@��@�O�@��j@���@��H@�E�@�+@���@���@���@��@�X@���@��m@�
=@�E�@�^5@��w@���@�l�@��@���@���@��@�=q@���@� �@�+@���@��@��@�X@��@��@���@�C�@�o@�n�@��@���@���@���@��7@���@���@���@��/@��@�bN@�A�@�A�@�b@���@���@�"�@���@���@���@�n�@�E�@�5?@�{@���@��@�&�@���@��u@�j@�9X@��
@���@��@�33@��@��@���@��@��@��y@���@���@���@�V@�{@�J@�@���@��h@�hs@��/@���@���@��u@��D@�r�@�I�@�Q�@�j@��@�Q�@�A�@�b@���@���@��\@�~�@�n�@�ff@�M�@��@��@�@��^@��h@�O�@���@��@�I�@�1@��m@�ƨ@��P@�\)@�o@���@���@�n�@�M�@�=q@�@��7@�?}@��/@�A�@��m@��@�dZ@�\)@�;d@��y@�v�@�$�@��T@��T@��#@�@���@��@�`B@�7L@��/@��9@�r�@� �@��@���@��P@�K�@�@��@��y@���@���@�~�@�n�@�ff@�M�@�{@��@���@���@��^@��@�G�@�?}@��@��j@�j@�Q�@�1'@�1@�ƨ@���@�|�@�K�@�
=@��R@��\@�E�@�J@���@���@�X@�&�@�V@�V@���@��`@���@���@�Ĝ@��j@��@���@��D@�I�@�b@��@�P@\)@~��@}��@}�@}V@|�j@|j@{t�@z��@z-@y��@y&�@x�`@x�@xA�@xb@w�;@w�;@w+@v��@vv�@v5?@u�-@up�@u�@t9X@s�@r�H@r�!@r-@q�7@p�`@pQ�@o�@n��@n�y@n�@nff@m�h@m`B@m/@mV@l�@l�j@lz�@lz�@l9X@l�@k�m@kt�@k33@j�!@jn�@j=q@i�@i�7@iX@i&�@hbN@g�@g+@fȴ@f��@fff@f$�@f{@f@e��@e�h@ep�@e/@d�/@d�@dZ@d�@cƨ@cS�@c@b��@b~�@b-@a��@aG�@`��@`��@`�9@`bN@` �@_��@_;d@_+@_�@^�+@^5?@^{@]�T@]��@]`B@]?}@]V@\��@\�@[�
@[��@["�@Z�!@Z-@ZJ@Y�^@YX@Y�@X�`@X��@X �@W��@W|�@W�@V��@VV@V5?@V{@V@U�h@U�@U�@U�@U/@T�@T�/@T��@S�m@S��@SS�@SC�@SC�@R��@R^5@R=q@R-@Q�@Q�^@Q��@Qx�@Q&�@P�9@Pr�@PA�@Pb@O|�@O
=@N@M�-@M�h@M�@M`B@M�@L��@Lj@LI�@K�m@Kt�@K33@K"�@J�!@J�\@JJ@Ihs@IG�@I&�@I%@H��@H�u@H �@G��@GK�@Fȴ@F5?@E��@E�-@E�h@E`B@E�@D�j@DI�@D9X@D1@C�m@C�m@C�m@C�
@CS�@B��@BM�@A�^@A��@Ax�@A7L@A%@@�`@@��@@Ĝ@@�@@r�@@A�@?�;@?��@?;d@>��@>��@>�+@>v�@>ff@>5?@=�T@=�-@=p�@<�@<Z@<(�@;�F@;t�@;dZ@;C�@;@:�!@:~�@:J@97L@9%@8�9@8�@8r�@8A�@7�w@7�@7��@7|�@7K�@7
=@6�@6��@6��@6��@6��@6ff@5�@5�@5V@4�@4j@4(�@3ƨ@3t�@3"�@2�H@2��@2��@2��@2��@2��@2��@2��@2~�@2�@1��@1�@1��@1��@1x�@1G�@1%@0��@0�u@0�@0bN@0 �@/�@/K�@/�@.�y@.�y@.ȴ@.�+@.E�@.{@-�-@-�@-V@,��@,��@,j@,Z@,�@+ƨ@+C�@+@*��@*~�@*n�@*n�@*=q@*J@)�#@)��@)x�@(��@(�@(Q�@'�@'�;@'�w@'K�@'
=@'
=@&�@&ȴ@&E�@&{@%�T@%��@%`B@%V@$�@$��@$�@$z�@$Z@$9X@$1@#�F@#dZ@"�@"��@"�!@"~�@"^5@"M�@"-@"�@!��@!x�@!7L@!&�@!&�@!�@!�@!%@ ��@ ��@ Ĝ@ ��@ �@ 1'@   @�@�@��@l�@\)@+@+@�@��@�@�R@V@�T@@@��@�-@`B@�@I�@�@�
@ƨ@��@C�@"�@"�@o@�H@�\@n�@J@�@�@��@�7@G�@�`@��@��@Q�@A�@1'@b@�@��@�@�@l�@K�@+@�y@��@E�@�T@@�h@V@��@�@�/@(�@�m@�m@�
@�
@�
@ƨ@t�@@��@~�@�@�#@x�@�@�`@��@��@��@Ĝ@�9@�9@�u@�@Q�@1'@�;@��@l�@�@�@�y@�@�R@V@$�@{@�@��@�h@?}@��@�D@j@Z@Z@(�@ƨ@�F@��@S�@o@
�H@
��@
��@
^5@
-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A��-A��^A�A�ȴA���A���A��A��/A��HA��HA��yA��A��A��A��A���A��A��A�-A�1'A�;dA�=qA�C�A�K�A�Q�A�XA�XA�XA�VA�S�A�S�A�C�A�A�A�C�A�K�A�S�A�bNA�v�A��DA��\A��hA��uA���A���A���A���A��-A��^A��^A���A��
A���A�  A�VA�"�A�"�A�M�AAuAuAPA�A¬A�ĜA�ȴA���A���A���A��`A���A�1A�JA�1'A�Q�A�ZA�`BAÃAÕ�AîAîAð!AÝ�Aá�Aã�Aá�A���A�1A�\)A���A�?}AőhA��TAƇ+A�oAǩ�A��A�?}A���A���A��A��A��A͟�A�5?AΩ�A�{A�7LA�"�A���A�
=A��A���A΃A�;dA��A�%A��A͸RA͓uA�hsA�+A�JA��A���Ạ�Ạ�A�n�A�"�A��;A��AЕ�A��A�  A�oA��A�(�A�7LA�/A�5?A�/A�-A�
=A��Aв-A�XA�A�A�O�A�jA�dZA�n�A�M�A��A��A���Aϩ�Aϝ�A�x�A�M�A�5?A��A��
A�~�A�
=A̓A���A�
=A��AɼjA���A�x�Aŕ�A�VA���A���A�Aġ�AāA�v�A�M�A�JA���Aĉ7A�5?A�S�A�7LA�bA���A�A���A���A��#A��A��#A���A���AĶFA���Aģ�A�Q�A�+A�JA��A��;A×�A�hsA�/A�%A���A��#A���A��/A��#A��;A��A��A��A��A��A���A�A��A��A� �A� �A� �A�/A�9XA�;dA�A�A�?}A�?}A�E�A�K�A�K�A�C�A�E�A�O�A�XA�^5A�\)A�VA�K�A�A�A�=qA�=qA�S�A�`BA�^5A�bNA�bNA�hsA�hsA�t�A�~�A�|�AÃAÇ+AÍPAÛ�AÝ�A×�AÛ�Aå�AìAîAîAð!AøRA�ĜA�ĜA�ĜA�ƨAþwAüjAüjAüjAüjAüjAþwA���A���A���A���A���A��A��;A��HA��TA��`A��yA��yA��yA��yA��yA��A��A���A���A���A���A�A�%A�
=A�JA�bA�bA�oA��A��A�$�A�&�A�-A�/A�5?A�7LA�7LA�7LA�7LA�7LA�1'A�9XA�A�A�A�A�C�A�C�A�E�A�G�A�K�A�M�A�VA�S�A�\)A�ffA�jA�hsA�hsA�hsA�l�A�v�A�|�A�~�Ać+AċDAċDAď\AēuAĕ�Ağ�Aģ�Aħ�Aİ!AĴ9AĸRAļjAľwA���A�A�ĜA�ƨA���A���A���A���A���A���A���A���A��
A��A��#A��;A��;A��HA��`A��`A��`A��A��A��A���A���A���A���A�A�%A�A�1A�
=A�
=A�VA�oA�oA�{A��A��A��A��A� �A� �A�"�A�&�A�+A�-A�/A�/A�/A�33A�=qA�?}A�C�A�C�A�E�A�I�A�K�A�O�A�Q�A�Q�A�Q�A�S�A�VA�ZA�^5A�`BA�bNA�bNA�bNA�bNA�dZA�hsA�hsA�jA�jA�jA�jA�n�A�n�A�p�A�t�A�v�A�x�A�x�A�x�A�x�A�z�A�|�A�|�AŁAŃAŅAŅAŇ+Aŉ7Aŉ7Aŉ7AōPAōPAŏ\AőhAőhAœuAœuAŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�Ař�Ař�Aś�Aś�Aś�Aŝ�Aş�Aš�Aş�Aš�Aš�Aš�Aš�Aţ�Ať�Ať�Ať�Ať�Aŧ�Aũ�Aũ�Aũ�AŬAŮAŰ!AŲ-AŴ9AŴ9AŶFAŸRAŸRAŸRAŸRAź^Aź^AŸRAŸRAŸRAŸRAź^AŸRAź^Aź^AżjAžwA���A���A���A���A�ĜA�ƨA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A��A��A��#A��/A��;A��;A��;A��;A��HA��HA��TA��TA��TA��TA��TA��TA��`A��TA��TA��`A��`A��`A��mA��mA��mA��mA��mA��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�A�A�A�  A�A�A�A�A�A�%A�%A�%A�A�%A�%A�%A�%A�%A�1A�%A�%A�1A�1A�
=A�
=A�1A�
=A�
=A�JA�
=A�JA�JA�
=A�JA�JA�JA�JA�JA�JA�VA�VA�bA�oA�oA�bA�oA�oA�oA�{A�oA�oA�{A��A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A��A��A��A��A� �A� �A�"�A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�-A�+A�-A�-A�-A�+A�-A�+A�+A�-A�/A�-A�/A�/A�-A�/A�1'A�1'A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�/A�/A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�7LA�7LA�7LA�7LA�5?A�5?A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�9XA�7LA�7LA�7LA�9XA�9XA�9XA�9XA�;dA�9XA�;dA�;dA�;dA�=qA�?}A�=qA�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�C�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�G�A�E�A�E�A�G�A�G�A�G�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�Q�A�S�A�S�A�S�A�S�A�Q�A�S�A�VA�VA�VA�VA�VA�S�A�VA�VA�XA�VA�VA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�ZA�ZA�XA�\)A�\)A�ZA�ZA�\)A�^5A�\)A�^5A�^5A�\)A�\)A�\)A�^5A�^5A�^5A�`BA�`BA�`BA�bNA�bNA�bNA�bNA�`BA�bNA�bNA�`BA�bNA�`BA�bNA�bNA�bNA�bNA�dZA�dZA�bNA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�ffA�ffA���4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444A���A���A���A��-A��^A�A�ȴA���A���A��A��/A��HA��HA��yA��A��A��A��A���A��A��A�-A�1'A�;dA�=qA�C�A�K�A�Q�A�XA�XA�XA�VA�S�A�S�A�C�A�A�A�C�A�K�A�S�A�bNA�v�A��DA��\A��hA��uA���A���A���A���A��-A��^A��^A���A��
A���A�  A�VA�"�A�"�A�M�AAuAuAPA�A¬A�ĜA�ȴA���A���A���A��`A���A�1A�JA�1'A�Q�A�ZA�`BAÃAÕ�AîAîAð!AÝ�Aá�Aã�Aá�A���A�1A�\)A���A�?}AőhA��TAƇ+A�oAǩ�A��A�?}A���A���A��A��A��A͟�A�5?AΩ�A�{A�7LA�"�A���A�
=A��A���A΃A�;dA��A�%A��A͸RA͓uA�hsA�+A�JA��A���Ạ�Ạ�A�n�A�"�A��;A��AЕ�A��A�  A�oA��A�(�A�7LA�/A�5?A�/A�-A�
=A��Aв-A�XA�A�A�O�A�jA�dZA�n�A�M�A��A��A���Aϩ�Aϝ�A�x�A�M�A�5?A��A��
A�~�A�
=A̓A���A�
=A��AɼjA���A�x�Aŕ�A�VA���A���A�Aġ�AāA�v�A�M�A�JA���Aĉ7A�5?A�S�A�7LA�bA���A�A���A���A��#A��A��#A���A���AĶFA���Aģ�A�Q�A�+A�JA��A��;A×�A�hsA�/A�%A���A��#A���A��/A��#A��;A��A��A��A��A��A���A�A��A��A� �A� �A� �A�/A�9XA�;dA�A�A�?}A�?}A�E�A�K�A�K�A�C�A�E�A�O�A�XA�^5A�\)A�VA�K�A�A�A�=qA�=qA�S�A�`BA�^5A�bNA�bNA�hsA�hsA�t�A�~�A�|�AÃAÇ+AÍPAÛ�AÝ�A×�AÛ�Aå�AìAîAîAð!AøRA�ĜA�ĜA�ĜA�ƨAþwAüjAüjAüjAüjAüjAþwA���A���A���A���A���A��A��;A��HA��TA��`A��yA��yA��yA��yA��yA��A��A���A���A���A���A�A�%A�
=A�JA�bA�bA�oA��A��A�$�A�&�A�-A�/A�5?A�7LA�7LA�7LA�7LA�7LA�1'A�9XA�A�A�A�A�C�A�C�A�E�A�G�A�K�A�M�A�VA�S�A�\)A�ffA�jA�hsA�hsA�hsA�l�A�v�A�|�A�~�Ać+AċDAċDAď\AēuAĕ�Ağ�Aģ�Aħ�Aİ!AĴ9AĸRAļjAľwA���A�A�ĜA�ƨA���A���A���A���A���A���A���A���A��
A��A��#A��;A��;A��HA��`A��`A��`A��A��A��A���A���A���A���A�A�%A�A�1A�
=A�
=A�VA�oA�oA�{A��A��A��A��A� �A� �A�"�A�&�A�+A�-A�/A�/A�/A�33A�=qA�?}A�C�A�C�A�E�A�I�A�K�A�O�A�Q�A�Q�A�Q�A�S�A�VA�ZA�^5A�`BA�bNA�bNA�bNA�bNA�dZA�hsA�hsA�jA�jA�jA�jA�n�A�n�A�p�A�t�A�v�A�x�A�x�A�x�A�x�A�z�A�|�A�|�AŁAŃAŅAŅAŇ+Aŉ7Aŉ7Aŉ7AōPAōPAŏ\AőhAőhAœuAœuAŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�Ař�Ař�Aś�Aś�Aś�Aŝ�Aş�Aš�Aş�Aš�Aš�Aš�Aš�Aţ�Ať�Ať�Ať�Ať�Aŧ�Aũ�Aũ�Aũ�AŬAŮAŰ!AŲ-AŴ9AŴ9AŶFAŸRAŸRAŸRAŸRAź^Aź^AŸRAŸRAŸRAŸRAź^AŸRAź^Aź^AżjAžwA���A���A���A���A�ĜA�ƨA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A��A��A��#A��/A��;A��;A��;A��;A��HA��HA��TA��TA��TA��TA��TA��TA��`A��TA��TA��`A��`A��`A��mA��mA��mA��mA��mA��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�A�A�A�  A�A�A�A�A�A�%A�%A�%A�A�%A�%A�%A�%A�%A�1A�%A�%A�1A�1A�
=A�
=A�1A�
=A�
=A�JA�
=A�JA�JA�
=A�JA�JA�JA�JA�JA�JA�VA�VA�bA�oA�oA�bA�oA�oA�oA�{A�oA�oA�{A��A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A��A��A��A��A� �A� �A�"�A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�-A�+A�-A�-A�-A�+A�-A�+A�+A�-A�/A�-A�/A�/A�-A�/A�1'A�1'A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�/A�/A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�7LA�7LA�7LA�7LA�5?A�5?A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�9XA�7LA�7LA�7LA�9XA�9XA�9XA�9XA�;dA�9XA�;dA�;dA�;dA�=qA�?}A�=qA�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�C�A�C�A�C�A�A�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�G�A�E�A�E�A�G�A�G�A�G�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�Q�A�S�A�S�A�S�A�S�A�Q�A�S�A�VA�VA�VA�VA�VA�S�A�VA�VA�XA�VA�VA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�XA�ZA�ZA�XA�\)A�\)A�ZA�ZA�\)A�^5A�\)A�^5A�^5A�\)A�\)A�\)A�^5A�^5A�^5A�`BA�`BA�`BA�bNA�bNA�bNA�bNA�`BA�bNA�bNA�`BA�bNA�`BA�bNA�bNA�bNA�bNA�dZA�dZA�bNA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�ffA�ffA���4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220121100157                              AO  ARCAADJP                                                                    20220121100157    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220121100157  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220121100157  QCF$                G�O�G�O�G�O�C000            