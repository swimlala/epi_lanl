CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-10-26T09:00:57Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211026090057  20211026090057  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               zA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٝ�ua�X1   @ٝ��f`@;��t��c������1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         zA   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� DafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�B�
B��RB��RB��RB��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��C��C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D[p�D[�
D\w
D\�
D]w
D]�
D^w
D^�
D_w
D_�
D`w
D`�pDaw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�xRD���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D��RD�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�xRD绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D�޸1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A���A���A���A�A���A�  A���A���A��A��A��A��`A��TA��TA��;A��;A��HA��;A��#A��
A���A���A���A���AɸRAɟ�A���A�ffA�G�A�1A�C�A�Q�A��FA��wA�S�A�G�A�oA�l�A� �A�5?A��A�dZA�JA�1A��A�ĜA�\)A�  A���A�=qA�$�A�t�A��RA��A�ĜA���A�bA�n�A�|�A�  A��\A�bA�A�$�A�~�A�~�A�\)A�1A���A�t�A���A��uA�l�A�{A���A�&�A���A��A�Q�A���A�  A�z�A���A��!A�l�A��7A��/A�%A��A�~�A���A{"�Ay?}Aw\)At��Atz�As�PAq�#Aq;dAo�wAm?}Ai��AfM�Ae
=AdI�AcG�Aa�wA^�\A]�A[�wAY;dAW
=AU�AS�ASXAR��AR��ARv�AR=qAP5?AM7LAK��AKoAJ��AI�wAH�AG`BAF�!AE�mAD�ADbNAD(�AC�PAB�yABA�AAO�A?�;A>Q�A=�wA=�PA=O�A<��A<5?A;l�A9��A9&�A8Q�A6~�A5K�A3�mA2I�A1��A1XA0�uA/�A/K�A.�/A.n�A-��A-O�A,�/A,�A+&�A*��A)�A)�PA)oA($�A'
=A%��A$jA#�A#A#p�A"��A!�A!�-A!�PA!l�A!�A �+A 9XA (�A�AdZA33AĜA-AXA^5A�AI�A�;AXA
=A�9An�A��A��A��A&�A�HA�A �A�;A��A�A?}A�/A�RAJAQ�AƨA`BA�`AVA �A�FA7LA-A�wA�AVA�^A
�A
 �A	�A	�hA9XA�mAx�A��AbNAC�AĜAAt�A�HAz�A ��@���@���@�j@�bN@�\)@�M�@�x�@�Ĝ@��@�9X@�F@�C�@���@��T@��@��/@�  @���@�1@���@��m@�t�@�
=@���@�j@��;@ڇ+@�/@�Ĝ@���@�  @�ȴ@�^5@ղ-@ա�@�hs@�(�@���@�E�@�X@ЋD@��@�V@�7L@��@�
=@��@��y@�M�@�j@��@ź^@�hs@�7L@��@��@�+@��@�/@�j@���@�;d@�J@�?}@��/@�(�@��@���@��#@��7@�/@���@�^5@�@�V@�I�@��;@��P@�
=@�@�ƨ@�
=@��@���@��+@�-@���@���@�bN@�|�@�;d@�o@���@�{@�x�@�V@�Z@�t�@��H@�5?@��#@�x�@���@���@�Z@�ƨ@�S�@���@�n�@�=q@���@�G�@���@�1@�t�@�K�@�"�@�
=@���@���@��7@���@�bN@�  @���@�E�@�5?@��T@���@��@���@�r�@�A�@��@�\)@�"�@�ȴ@�M�@�J@��@��^@�`B@�%@��u@�j@�A�@�ƨ@�+@��@���@�~�@�5?@���@�V@�z�@�l�@��R@�M�@��@�@���@��7@�`B@�?}@��`@�1'@���@��@��@��m@���@���@�~�@�$�@��@���@�7L@��`@��@�I�@�1@�  @��w@���@�|�@�K�@�
=@���@�ȴ@���@�ff@�@�X@�/@���@��D@�9X@�1@���@��@�dZ@��H@���@�o@�
=@��@���@�V@��T@��-@���@�hs@�7L@���@��`@��`@���@�r�@�Z@�I�@�A�@�b@��@�@�P@
=@~�y@~�R@~v�@~$�@}�@}��@}�@}`B@}?}@}V@|��@|��@|j@|1@{C�@z��@z�\@z��@z��@z~�@y��@y�@y��@y&�@xA�@w�@w|�@wK�@v�+@u?}@t�j@tI�@s�m@sƨ@sƨ@sƨ@s�F@s��@s�@st�@so@q�#@q��@qx�@q&�@pĜ@p�@pr�@pQ�@p �@o�w@o+@nff@n@m��@mp�@m�@l��@l��@k�m@ko@j�!@j�!@j�!@j�!@j�\@j^5@i��@ix�@i%@hbN@hb@g��@g�P@g�P@gl�@fȴ@fff@e�@e��@e?}@d�/@dz�@dI�@d�@d1@c�F@cdZ@b�!@b�@b-@a��@a��@a��@a��@aG�@`�`@`Q�@`1'@_�@_�P@_l�@^��@^�+@^E�@]��@]�@]/@\�j@\�D@[�
@[�m@[t�@[o@[@Z��@Y�#@Yx�@Y�7@Y��@Y&�@XA�@X1'@W�;@W��@W�w@W��@Wl�@W+@W�@V��@V�R@V5?@V{@U��@Tj@St�@SC�@S"�@S33@S��@S�@S33@R�@R��@Rn�@Q�#@QX@PbN@P1'@O�@O�w@O\)@O��@O\)@Nȴ@Nff@N{@M��@L��@L�j@L�D@Lj@LI�@L(�@K�F@KC�@K@J�!@Jn�@J-@I��@Ihs@IG�@H��@HQ�@Hb@G�@G�;@G�@G\)@G;d@F��@F�R@F��@Fv�@Fff@FV@F5?@F{@E�@E�@E�T@E@E�-@E�h@E`B@E/@E/@D��@D�j@D�D@Dj@DI�@D1@C��@Co@B=q@A��@A�7@AG�@A%@@�9@@�u@@bN@@A�@?�P@?
=@>�y@>��@>V@>@=�T@=��@=�h@=p�@=/@<��@<z�@<I�@<9X@<9X@<9X@<�@;�m@;33@:��@:-@9��@9��@9&�@8Ĝ@8r�@8A�@7�;@7K�@7
=@6�@6��@6v�@6V@65?@6{@5�T@5�-@5`B@5`B@5O�@4�j@4I�@41@3��@3��@3dZ@3C�@333@3"�@3o@2�@2n�@1�#@1��@1�7@1x�@1X@1&�@1�@0�`@0�9@0bN@0 �@01'@/�@/��@/\)@/+@.�y@.ȴ@.ff@.V@.E�@.@-��@-@-��@-p�@-?}@-/@,�@,�@,I�@,(�@,�@+�m@+��@+�@+"�@*�H@*��@*�\@*~�@*M�@*J@)�@)��@)�7@)7L@)%@(Ĝ@(�@(A�@(b@'��@'�P@';d@'
=@&��@&�R@&v�@&E�@&@%@%�@%O�@%/@$�/@$�j@$�D@$z�@$I�@$1@#�
@#��@#t�@#S�@#o@"�!@"^5@"J@!��@!��@!7L@!�@!%@ �`@ �u@ A�@�;@�w@|�@K�@;d@+@��@��@v�@V@5?@$�@@�@�@@�-@�h@/@��@��@�j@�@�D@I�@�@�
@t�@C�@33@o@�H@�!@�!@�!@��@��@�\@~�@^5@-@J@�^@hs@7L@&�@�@%@Ĝ@bN@Q�@A�@b@  @�;@�@|�@K�@
=@ȴ@�R@��@�+@�+@ff@E�@{@�@��@�@`B@O�@?}@/@�@�/@��@Z@9X@9X@�@1@��@�F@��@S�@o@�@�H@�H@��@~�@-@�#@�7@&�@�`@�9@�u@�@A�@�@�@�P@|�@K�@+@�@
=@�y@��@V@E�@5?@@@�@?}@?}@�@�@V@��@�/@�j@�@z�@�@��@�
@�@dZ@C�@o@
��@
^5@
-@
J@	�@	��@	��@	��@	x�@	G�@	&�@	%@	%@��@�`@��@�9@bN@A�@A�@1'@ �@�;@��@��@�w@�@��@�P@|�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A���A���A���A�A���A�  A���A���A��A��A��A��`A��TA��TA��;A��;A��HA��;A��#A��
A���A���A���A���AɸRAɟ�A���A�ffA�G�A�1A�C�A�Q�A��FA��wA�S�A�G�A�oA�l�A� �A�5?A��A�dZA�JA�1A��A�ĜA�\)A�  A���A�=qA�$�A�t�A��RA��A�ĜA���A�bA�n�A�|�A�  A��\A�bA�A�$�A�~�A�~�A�\)A�1A���A�t�A���A��uA�l�A�{A���A�&�A���A��A�Q�A���A�  A�z�A���A��!A�l�A��7A��/A�%A��A�~�A���A{"�Ay?}Aw\)At��Atz�As�PAq�#Aq;dAo�wAm?}Ai��AfM�Ae
=AdI�AcG�Aa�wA^�\A]�A[�wAY;dAW
=AU�AS�ASXAR��AR��ARv�AR=qAP5?AM7LAK��AKoAJ��AI�wAH�AG`BAF�!AE�mAD�ADbNAD(�AC�PAB�yABA�AAO�A?�;A>Q�A=�wA=�PA=O�A<��A<5?A;l�A9��A9&�A8Q�A6~�A5K�A3�mA2I�A1��A1XA0�uA/�A/K�A.�/A.n�A-��A-O�A,�/A,�A+&�A*��A)�A)�PA)oA($�A'
=A%��A$jA#�A#A#p�A"��A!�A!�-A!�PA!l�A!�A �+A 9XA (�A�AdZA33AĜA-AXA^5A�AI�A�;AXA
=A�9An�A��A��A��A&�A�HA�A �A�;A��A�A?}A�/A�RAJAQ�AƨA`BA�`AVA �A�FA7LA-A�wA�AVA�^A
�A
 �A	�A	�hA9XA�mAx�A��AbNAC�AĜAAt�A�HAz�A ��@���@���@�j@�bN@�\)@�M�@�x�@�Ĝ@��@�9X@�F@�C�@���@��T@��@��/@�  @���@�1@���@��m@�t�@�
=@���@�j@��;@ڇ+@�/@�Ĝ@���@�  @�ȴ@�^5@ղ-@ա�@�hs@�(�@���@�E�@�X@ЋD@��@�V@�7L@��@�
=@��@��y@�M�@�j@��@ź^@�hs@�7L@��@��@�+@��@�/@�j@���@�;d@�J@�?}@��/@�(�@��@���@��#@��7@�/@���@�^5@�@�V@�I�@��;@��P@�
=@�@�ƨ@�
=@��@���@��+@�-@���@���@�bN@�|�@�;d@�o@���@�{@�x�@�V@�Z@�t�@��H@�5?@��#@�x�@���@���@�Z@�ƨ@�S�@���@�n�@�=q@���@�G�@���@�1@�t�@�K�@�"�@�
=@���@���@��7@���@�bN@�  @���@�E�@�5?@��T@���@��@���@�r�@�A�@��@�\)@�"�@�ȴ@�M�@�J@��@��^@�`B@�%@��u@�j@�A�@�ƨ@�+@��@���@�~�@�5?@���@�V@�z�@�l�@��R@�M�@��@�@���@��7@�`B@�?}@��`@�1'@���@��@��@��m@���@���@�~�@�$�@��@���@�7L@��`@��@�I�@�1@�  @��w@���@�|�@�K�@�
=@���@�ȴ@���@�ff@�@�X@�/@���@��D@�9X@�1@���@��@�dZ@��H@���@�o@�
=@��@���@�V@��T@��-@���@�hs@�7L@���@��`@��`@���@�r�@�Z@�I�@�A�@�b@��@�@�P@
=@~�y@~�R@~v�@~$�@}�@}��@}�@}`B@}?}@}V@|��@|��@|j@|1@{C�@z��@z�\@z��@z��@z~�@y��@y�@y��@y&�@xA�@w�@w|�@wK�@v�+@u?}@t�j@tI�@s�m@sƨ@sƨ@sƨ@s�F@s��@s�@st�@so@q�#@q��@qx�@q&�@pĜ@p�@pr�@pQ�@p �@o�w@o+@nff@n@m��@mp�@m�@l��@l��@k�m@ko@j�!@j�!@j�!@j�!@j�\@j^5@i��@ix�@i%@hbN@hb@g��@g�P@g�P@gl�@fȴ@fff@e�@e��@e?}@d�/@dz�@dI�@d�@d1@c�F@cdZ@b�!@b�@b-@a��@a��@a��@a��@aG�@`�`@`Q�@`1'@_�@_�P@_l�@^��@^�+@^E�@]��@]�@]/@\�j@\�D@[�
@[�m@[t�@[o@[@Z��@Y�#@Yx�@Y�7@Y��@Y&�@XA�@X1'@W�;@W��@W�w@W��@Wl�@W+@W�@V��@V�R@V5?@V{@U��@Tj@St�@SC�@S"�@S33@S��@S�@S33@R�@R��@Rn�@Q�#@QX@PbN@P1'@O�@O�w@O\)@O��@O\)@Nȴ@Nff@N{@M��@L��@L�j@L�D@Lj@LI�@L(�@K�F@KC�@K@J�!@Jn�@J-@I��@Ihs@IG�@H��@HQ�@Hb@G�@G�;@G�@G\)@G;d@F��@F�R@F��@Fv�@Fff@FV@F5?@F{@E�@E�@E�T@E@E�-@E�h@E`B@E/@E/@D��@D�j@D�D@Dj@DI�@D1@C��@Co@B=q@A��@A�7@AG�@A%@@�9@@�u@@bN@@A�@?�P@?
=@>�y@>��@>V@>@=�T@=��@=�h@=p�@=/@<��@<z�@<I�@<9X@<9X@<9X@<�@;�m@;33@:��@:-@9��@9��@9&�@8Ĝ@8r�@8A�@7�;@7K�@7
=@6�@6��@6v�@6V@65?@6{@5�T@5�-@5`B@5`B@5O�@4�j@4I�@41@3��@3��@3dZ@3C�@333@3"�@3o@2�@2n�@1�#@1��@1�7@1x�@1X@1&�@1�@0�`@0�9@0bN@0 �@01'@/�@/��@/\)@/+@.�y@.ȴ@.ff@.V@.E�@.@-��@-@-��@-p�@-?}@-/@,�@,�@,I�@,(�@,�@+�m@+��@+�@+"�@*�H@*��@*�\@*~�@*M�@*J@)�@)��@)�7@)7L@)%@(Ĝ@(�@(A�@(b@'��@'�P@';d@'
=@&��@&�R@&v�@&E�@&@%@%�@%O�@%/@$�/@$�j@$�D@$z�@$I�@$1@#�
@#��@#t�@#S�@#o@"�!@"^5@"J@!��@!��@!7L@!�@!%@ �`@ �u@ A�@�;@�w@|�@K�@;d@+@��@��@v�@V@5?@$�@@�@�@@�-@�h@/@��@��@�j@�@�D@I�@�@�
@t�@C�@33@o@�H@�!@�!@�!@��@��@�\@~�@^5@-@J@�^@hs@7L@&�@�@%@Ĝ@bN@Q�@A�@b@  @�;@�@|�@K�@
=@ȴ@�R@��@�+@�+@ff@E�@{@�@��@�@`B@O�@?}@/@�@�/@��@Z@9X@9X@�@1@��@�F@��@S�@o@�@�H@�H@��@~�@-@�#@�7@&�@�`@�9@�u@�@A�@�@�@�P@|�@K�@+@�@
=@�y@��@V@E�@5?@@@�@?}@?}@�@�@V@��@�/@�j@�@z�@�@��@�
@�@dZ@C�@o@
��@
^5@
-@
J@	�@	��@	��@	��@	x�@	G�@	&�@	%@	%@��@�`@��@�9@bN@A�@A�@1'@ �@�;@��@��@�w@�@��@�P@|�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�)B�)B�#B�B�#B�)B�)B�/B�/B�;B�ZB�yB�B�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�`B��Bl�B:^B,B�BuB	7BB��B��B�B�B�fB�TB�BB��B��BǮBĜB��B�jB�9B�B��B��B�bB�JB�Bt�BhsB^5BP�BF�B>wB33B.B#�B�B+B�B�B�B�mB�)B�B�B��BǮB�!B��B�PB�B{�Bo�BbNBW
BD�B2-B�BDB�mB��B��B��B�{B�%B}�Bn�BhsBdZBZBR�BJ�B8RB#�B+B��B��B�B�B�TB�B��B�jB�B��B��B�oB�uB�uB��B��B��B�uB�DB�bB��B�bB�\B�PB�1B�7B�%B�PB�PB�JB�7B�1B�B�B|�Bz�Bx�Bx�Bv�Bs�Bq�Bk�BgmBcTB_;B^5BVBO�BJ�BH�BG�BE�BC�B@�B?}B>wB;dB:^B9XB5?B2-B1'B.B-B+B%�B"�B�B�B�B�B�B{BuBuBoBoBbB\B\B\BPBJBDB	7B+BBB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�mB
�fB
�ZB
�NB
�HB
�BB
�HB
�BB
�/B
�)B
�#B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
ɺB
ƨB
ŢB
ÖB
��B
�}B
�qB
�qB
�jB
�jB
�jB
�dB
�dB
�^B
�XB
�RB
�FB
�FB
�?B
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�?B
�FB
�?B
�FB
�9B
�?B
�FB
�FB
�FB
�FB
�FB
�LB
�FB
�FB
�FB
�FB
�?B
�FB
�LB
�LB
�RB
�RB
�LB
�LB
�XB
�jB
�wB
�qB
�qB
�wB
�}B
��B
ĜB
ĜB
ŢB
ŢB
ƨB
ȴB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�)B
�/B
�5B
�;B
�BB
�NB
�B
�B
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
��B  BBB+BDBPBbBoBuB{B�B�B�B�B�B�B �B#�B%�B)�B-B1'B1'B2-B33B5?B7LB7LB:^B>wB@�BG�BI�BI�BL�BO�BP�BW
BYBYB\)B\)B]/B^5BaHBcTBdZBe`BgmBjBm�Bo�Bo�Br�Bu�Bw�Bx�By�B{�B� B�B�%B�PB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�?B�LB�dB�qB��BBŢBƨBǮB��B��B��B��B��B��B��B�B�#B�5B�BB�NB�ZB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��BBBBB%B	7BDBDBJBhBhBuB{B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B$�B%�B&�B(�B+B/B/B/B2-B7LB8RB9XB:^B=qBB�BE�BF�BH�BH�BH�BH�BI�BI�BI�BJ�BK�BP�BP�BQ�BR�BT�BVBVBVBW
BXB[#B^5B_;B`BBbNBdZBe`BffBjBm�Bo�Bo�Bo�Bp�Bp�Br�Bs�Bt�Bv�Bw�Bx�By�By�By�By�B|�B}�B�B�B�B�B�B�B�B�B�%B�+B�=B�DB�VB�\B�bB�bB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�-B�3B�9B�?B�FB�FB�FB�LB�RB�XB�dB�qB�}B��B��B��BÖBĜBÖBĜBĜBŢBŢBƨBǮBȴBȴBɺBɺB��BɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�
B�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�)B�)B�)B�)B�/B�/B�5B�;B�;B�HB�NB�NB�NB�NB�NB�NB�ZB�`B�`B�fB�mB�sB�sB�sB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  BBBBBBBBBBBBBBBBBBB%B+B+B+B+B+B1B1B1B1B	7B	7B	7B
=B
=B
=BDBDBDBJBJBJBPBPBPBVBVBVB\B\B\BbBbBbBhBhBhBhBoBoBoBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B$�B$�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B&�B&�B'�B'�B'�B(�B(�B(�B(�B(�B(�B(�B)�B)�B)�B)�B)�B+B+B+B+B,B,B,B,B,B,B,B-B-B-B.B.B.B.B.B/B/B0!B0!B1'B1'B1'B2-B2-B2-B33B33B33B33B33B49B49B49B49B49B5?B5?B5?B5?B6FB6FB6FB7LB8RB8RB8RB8RB8RB�PB�qB�wB�wB�wB�}B�}B�}B�}B�}B��B��B��B��B��B��BBBBBBÖBÖBÖBÖBÖBÖBÖBĜBÖBĜBĜBĜBĜBĜBĜBĜBĜBŢBĜBŢ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B�)B�)B�#B�B�#B�)B�)B�/B�/B�;B�ZB�yB�B�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�`B��Bl�B:^B,B�BuB	7BB��B��B�B�B�fB�TB�BB��B��BǮBĜB��B�jB�9B�B��B��B�bB�JB�Bt�BhsB^5BP�BF�B>wB33B.B#�B�B+B�B�B�B�mB�)B�B�B��BǮB�!B��B�PB�B{�Bo�BbNBW
BD�B2-B�BDB�mB��B��B��B�{B�%B}�Bn�BhsBdZBZBR�BJ�B8RB#�B+B��B��B�B�B�TB�B��B�jB�B��B��B�oB�uB�uB��B��B��B�uB�DB�bB��B�bB�\B�PB�1B�7B�%B�PB�PB�JB�7B�1B�B�B|�Bz�Bx�Bx�Bv�Bs�Bq�Bk�BgmBcTB_;B^5BVBO�BJ�BH�BG�BE�BC�B@�B?}B>wB;dB:^B9XB5?B2-B1'B.B-B+B%�B"�B�B�B�B�B�B{BuBuBoBoBbB\B\B\BPBJBDB	7B+BBB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�mB
�fB
�ZB
�NB
�HB
�BB
�HB
�BB
�/B
�)B
�#B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
ɺB
ƨB
ŢB
ÖB
��B
�}B
�qB
�qB
�jB
�jB
�jB
�dB
�dB
�^B
�XB
�RB
�FB
�FB
�?B
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�?B
�FB
�?B
�FB
�9B
�?B
�FB
�FB
�FB
�FB
�FB
�LB
�FB
�FB
�FB
�FB
�?B
�FB
�LB
�LB
�RB
�RB
�LB
�LB
�XB
�jB
�wB
�qB
�qB
�wB
�}B
��B
ĜB
ĜB
ŢB
ŢB
ƨB
ȴB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�)B
�/B
�5B
�;B
�BB
�NB
�B
�B
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
��B  BBB+BDBPBbBoBuB{B�B�B�B�B�B�B �B#�B%�B)�B-B1'B1'B2-B33B5?B7LB7LB:^B>wB@�BG�BI�BI�BL�BO�BP�BW
BYBYB\)B\)B]/B^5BaHBcTBdZBe`BgmBjBm�Bo�Bo�Br�Bu�Bw�Bx�By�B{�B� B�B�%B�PB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�?B�LB�dB�qB��BBŢBƨBǮB��B��B��B��B��B��B��B�B�#B�5B�BB�NB�ZB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��BBBBB%B	7BDBDBJBhBhBuB{B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B$�B%�B&�B(�B+B/B/B/B2-B7LB8RB9XB:^B=qBB�BE�BF�BH�BH�BH�BH�BI�BI�BI�BJ�BK�BP�BP�BQ�BR�BT�BVBVBVBW
BXB[#B^5B_;B`BBbNBdZBe`BffBjBm�Bo�Bo�Bo�Bp�Bp�Br�Bs�Bt�Bv�Bw�Bx�By�By�By�By�B|�B}�B�B�B�B�B�B�B�B�B�%B�+B�=B�DB�VB�\B�bB�bB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�-B�3B�9B�?B�FB�FB�FB�LB�RB�XB�dB�qB�}B��B��B��BÖBĜBÖBĜBĜBŢBŢBƨBǮBȴBȴBɺBɺB��BɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�
B�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�)B�)B�)B�)B�/B�/B�5B�;B�;B�HB�NB�NB�NB�NB�NB�NB�ZB�`B�`B�fB�mB�sB�sB�sB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  BBBBBBBBBBBBBBBBBBB%B+B+B+B+B+B1B1B1B1B	7B	7B	7B
=B
=B
=BDBDBDBJBJBJBPBPBPBVBVBVB\B\B\BbBbBbBhBhBhBhBoBoBoBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B$�B$�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B&�B&�B'�B'�B'�B(�B(�B(�B(�B(�B(�B(�B)�B)�B)�B)�B)�B+B+B+B+B,B,B,B,B,B,B,B-B-B-B.B.B.B.B.B/B/B0!B0!B1'B1'B1'B2-B2-B2-B33B33B33B33B33B49B49B49B49B49B5?B5?B5?B5?B6FB6FB6FB7LB8RB8RB8RB8RB8RB�PB�qB�wB�wB�wB�}B�}B�}B�}B�}B��B��B��B��B��B��BBBBBBÖBÖBÖBÖBÖBÖBÖBĜBÖBĜBĜBĜBĜBĜBĜBĜBĜBŢBĜBŢ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211026090057                              AO  ARCAADJP                                                                    20211026090057    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211026090057  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211026090057  QCF$                G�O�G�O�G�O�8000            