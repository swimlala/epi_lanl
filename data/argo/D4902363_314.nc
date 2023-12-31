CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-24T21:36:42Z creation;2018-12-24T21:36:48Z conversion to V3.1;2019-12-19T07:24:55Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181224213642  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              :A   JA  I2_0576_314                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؚ� �Q 1   @ؚ� @9lj~��#�d>�*0U1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D3D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�Q�@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��BøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs��Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D�qDw
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
DJ�qDKw
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
DS�qDTw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�~�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�D�RD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D�D�(R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�^5A�^5A�`BA�bNA�bNA�bNA�bNA�bNA�XA�I�A�9XA�7LA��A�JA�A��A��#A��
A���A���A��
A��A��A���A���A���A���A���A�ȴA�ĜA�ĜA�A�ĜA�A�A�A��jA��9A��!A���A���A��hA��PA��DA�|�A�$�A���A��PA�C�A��A��!A�ZA��A�A�+A�?}A��;A���A�ĜA�33A�I�A�\)A��^A��yA�I�A�p�A��^A�=qA��HA�bNA���A��;A�Q�A��A�%A��TA���A�|�A���A��mA��!A���A��A��/A�|�A���A�l�A�&�A��uA��+A�t�A�A���A���A�hsA���A�ĜA�A���A��uA�A�A� �A�A��TA�+A�ZA�$�A�A��/A��jA��hA�I�A� �A�K�A���A���A�jA�z�A}��Ay
=Av��AtjAr��Apv�Ao%AmK�Aj~�Ag�Ac�Ab��AaA`�9A^n�A]7LA[��AY�AX�RAW�TAV�jAVM�AU�PAT�AS+AR$�AP�HANZAK?}AI�AHAFI�AEAE
=ADjAB��AA�FA?��A>z�A=l�A<�/A<z�A;&�A:=qA:9XA:-A9��A9;dA8��A7A6VA333A1t�A1XA17LA0�/A/��A/�hA/�A/&�A.�A-�mA-��A,�yA,�RA,z�A,9XA+?}A*�/A*ȴA*��A*�\A*z�A*VA*�A)�A)�FA)XA(��A(�A'7LA&1'A%dZA#�A"�\A"I�A!�A �A ��A A�A �A�
A;dA�A^5A�A�A�HAjAAAO�AVA��A(�A�TA`BA�A�!AVA-A��A33A�A^5A�RA9XA�A�TA��AC�A
��A
�/A
�!A	XA9XA��A��A��AO�A?}A��A�mA/A$�AAS�A�DAhsA �\@��@�/@� �@���@���@�E�@�/@�9X@��@���@�^5@��@��h@�7@��@�&�@��@�^5@��@�`B@�D@旍@�l�@��@�v�@�j@ڏ\@��@��@��`@�?}@�|�@��T@�(�@��@�{@�z�@��;@���@Ǿw@�\)@š�@��m@�dZ@�C�@�+@�"�@�@�@�@��@��y@�ȴ@�ȴ@�~�@��@��#@���@��-@���@�?}@��u@��@�ff@��@�dZ@�@�\)@�-@�Z@�"�@��\@�=q@�{@�@�O�@���@�bN@���@�^5@�ff@��^@�&�@���@��9@�z�@�Z@�Q�@�1@�  @��m@�ƨ@�ƨ@��@�K�@��@��@��!@�ff@�@��^@�X@��j@�Q�@��;@���@�S�@��\@���@��7@��@���@���@�;d@��@���@���@�n�@���@�Ĝ@�b@��P@��@��R@��\@��+@�ff@���@�x�@���@��@�7L@�  @�dZ@�"�@���@���@�1@�+@�"�@�o@��H@�E�@��u@��
@�l�@�K�@�+@��!@�/@��@���@���@���@�z�@��
@�l�@�o@�ff@��^@�Ĝ@�A�@�(�@�1@���@�o@���@���@��7@�X@�?}@��/@�bN@��@��w@�K�@��H@��+@�ff@�$�@��@��#@��h@�7L@�&�@�V@���@���@��`@��`@��@���@��@�bN@�Q�@��;@�l�@�\)@���@��@���@�/@�%@���@��@���@���@�z�@�bN@�A�@�b@K�@}/@|I�@|(�@{��@{�
@{�
@{��@{@z��@zM�@y�#@yx�@yX@y&�@x��@x�@xbN@x1'@w��@w�P@w\)@v�R@u�@tz�@t9X@s�
@s33@r�H@r��@rn�@q�#@qx�@qX@q7L@qG�@p��@pbN@ol�@n��@n@m��@m`B@k�m@kdZ@j�@jn�@jM�@j-@i��@iX@hĜ@hr�@h1'@g�;@g\)@f�y@f$�@e�@e�@ep�@eO�@e?}@e/@eV@d��@d��@cƨ@cdZ@c33@b=q@a&�@`�u@_�@_�;@_�@_��@_��@_��@_�P@_�P@_�P@_�P@_\)@_�@^�R@^@]O�@\��@[�
@[33@Z�@Z��@Z^5@Y��@Y��@Y��@Yhs@YG�@Y&�@Y%@X�`@X�9@X �@W�@Vȴ@Vȴ@Vff@U�h@T��@T��@Tz�@T9X@T�@S�m@S��@S��@SS�@S"�@S@R��@R�!@R�\@R-@Q��@Qx�@QG�@Q7L@Q&�@Q�@P��@P�`@P�`@P��@PĜ@P�9@PbN@PA�@P1'@P �@P �@Pb@O�@O|�@Nȴ@N��@N��@N��@Nv�@M�@M�-@M`B@L�j@Lj@K�m@K��@K�@Kt�@KC�@K@J�H@J��@J=q@I�#@I�7@IX@I%@H�`@H�9@Hr�@HQ�@H1'@G�@G�P@G�P@G+@F�y@Fȴ@F�+@F{@E�-@E�@D��@D�j@D�@Dj@C��@C�m@C�F@CdZ@Co@B�@B��@Bn�@B-@B�@A�@A�@A�#@A��@A7L@A%@@�9@@��@@�@@bN@@Q�@@ �@?�@?�w@?�P@?+@>��@>ff@=�T@=p�@=?}@=�@<��@<��@<�D@<j@<j@<Z@<�@;o@:��@:��@:n�@:�@9x�@8�`@8��@8��@8��@8Q�@7�@7�P@6�y@6E�@5�h@5O�@5V@4�@4��@4�D@4Z@4j@4I�@4�@3ƨ@3�@3C�@3"�@2�@2��@2^5@1�#@1G�@0�u@0b@/�@/K�@/
=@.�y@.v�@.V@.5?@.$�@.@-�@-�T@-�@-?}@-?}@-/@-�@,��@,�/@,��@,��@,I�@,(�@+�m@+"�@*�!@*�!@*n�@*M�@*-@)��@)��@)��@)��@)�7@)x�@)X@)7L@(�u@(1'@(b@'��@'�P@'K�@'
=@&�@&��@&v�@%@%O�@%�@$�@$I�@#�
@#dZ@"�H@"~�@"n�@!�@!7L@ �9@ �@ r�@ Q�@ 1'@��@��@;d@;d@;d@�@��@E�@��@�h@�@�@`B@`B@/@�@��@j@�m@��@dZ@"�@��@��@�\@=q@J@�@�@��@��@��@�7@X@G�@7L@&�@&�@�@%@��@��@�`@�`@�`@�`@�`@��@Ĝ@Ĝ@��@A�@1'@1'@  @�;@�;@��@�w@�P@�P@�P@|�@|�@\)@\)@\)@;d@�y@��@��@��@��@��@��@��@�+@ff@E�@$�@@@��@�@�@�/@��@��@�j@�@z�@9X@�@�@�@�@�@1@�
@��@C�@@�@�@��@��@��@��@��@��@��@��@��@M�@=q@�@�@�7@X@7L@�@�@%@��@�u@bN@Q�@Q�@Q�@ �@�w@�P@|�@l�@\)@K�@;d@�@
=@��@E�@E�@5?@$�@@��@��@�h@p�@O�@�@�j@�j@�@��@��@�D@�D@z�@j@Z@9X@(�@�@��@�@C�@o@
��@
�!@
�!@
��@
�!@
n�@
J@	�7@	G�@	&�@	%@��@��@��@�9@r�@��@�@��@�y@�@ȴ@ȴ@�R@�R@��@��@�+@E�@@�T@�-@�@/@�@j@�m@�F@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�^5A�^5A�`BA�bNA�bNA�bNA�bNA�bNA�XA�I�A�9XA�7LA��A�JA�A��A��#A��
A���A���A��
A��A��A���A���A���A���A���A�ȴA�ĜA�ĜA�A�ĜA�A�A�A��jA��9A��!A���A���A��hA��PA��DA�|�A�$�A���A��PA�C�A��A��!A�ZA��A�A�+A�?}A��;A���A�ĜA�33A�I�A�\)A��^A��yA�I�A�p�A��^A�=qA��HA�bNA���A��;A�Q�A��A�%A��TA���A�|�A���A��mA��!A���A��A��/A�|�A���A�l�A�&�A��uA��+A�t�A�A���A���A�hsA���A�ĜA�A���A��uA�A�A� �A�A��TA�+A�ZA�$�A�A��/A��jA��hA�I�A� �A�K�A���A���A�jA�z�A}��Ay
=Av��AtjAr��Apv�Ao%AmK�Aj~�Ag�Ac�Ab��AaA`�9A^n�A]7LA[��AY�AX�RAW�TAV�jAVM�AU�PAT�AS+AR$�AP�HANZAK?}AI�AHAFI�AEAE
=ADjAB��AA�FA?��A>z�A=l�A<�/A<z�A;&�A:=qA:9XA:-A9��A9;dA8��A7A6VA333A1t�A1XA17LA0�/A/��A/�hA/�A/&�A.�A-�mA-��A,�yA,�RA,z�A,9XA+?}A*�/A*ȴA*��A*�\A*z�A*VA*�A)�A)�FA)XA(��A(�A'7LA&1'A%dZA#�A"�\A"I�A!�A �A ��A A�A �A�
A;dA�A^5A�A�A�HAjAAAO�AVA��A(�A�TA`BA�A�!AVA-A��A33A�A^5A�RA9XA�A�TA��AC�A
��A
�/A
�!A	XA9XA��A��A��AO�A?}A��A�mA/A$�AAS�A�DAhsA �\@��@�/@� �@���@���@�E�@�/@�9X@��@���@�^5@��@��h@�7@��@�&�@��@�^5@��@�`B@�D@旍@�l�@��@�v�@�j@ڏ\@��@��@��`@�?}@�|�@��T@�(�@��@�{@�z�@��;@���@Ǿw@�\)@š�@��m@�dZ@�C�@�+@�"�@�@�@�@��@��y@�ȴ@�ȴ@�~�@��@��#@���@��-@���@�?}@��u@��@�ff@��@�dZ@�@�\)@�-@�Z@�"�@��\@�=q@�{@�@�O�@���@�bN@���@�^5@�ff@��^@�&�@���@��9@�z�@�Z@�Q�@�1@�  @��m@�ƨ@�ƨ@��@�K�@��@��@��!@�ff@�@��^@�X@��j@�Q�@��;@���@�S�@��\@���@��7@��@���@���@�;d@��@���@���@�n�@���@�Ĝ@�b@��P@��@��R@��\@��+@�ff@���@�x�@���@��@�7L@�  @�dZ@�"�@���@���@�1@�+@�"�@�o@��H@�E�@��u@��
@�l�@�K�@�+@��!@�/@��@���@���@���@�z�@��
@�l�@�o@�ff@��^@�Ĝ@�A�@�(�@�1@���@�o@���@���@��7@�X@�?}@��/@�bN@��@��w@�K�@��H@��+@�ff@�$�@��@��#@��h@�7L@�&�@�V@���@���@��`@��`@��@���@��@�bN@�Q�@��;@�l�@�\)@���@��@���@�/@�%@���@��@���@���@�z�@�bN@�A�@�b@K�@}/@|I�@|(�@{��@{�
@{�
@{��@{@z��@zM�@y�#@yx�@yX@y&�@x��@x�@xbN@x1'@w��@w�P@w\)@v�R@u�@tz�@t9X@s�
@s33@r�H@r��@rn�@q�#@qx�@qX@q7L@qG�@p��@pbN@ol�@n��@n@m��@m`B@k�m@kdZ@j�@jn�@jM�@j-@i��@iX@hĜ@hr�@h1'@g�;@g\)@f�y@f$�@e�@e�@ep�@eO�@e?}@e/@eV@d��@d��@cƨ@cdZ@c33@b=q@a&�@`�u@_�@_�;@_�@_��@_��@_��@_�P@_�P@_�P@_�P@_\)@_�@^�R@^@]O�@\��@[�
@[33@Z�@Z��@Z^5@Y��@Y��@Y��@Yhs@YG�@Y&�@Y%@X�`@X�9@X �@W�@Vȴ@Vȴ@Vff@U�h@T��@T��@Tz�@T9X@T�@S�m@S��@S��@SS�@S"�@S@R��@R�!@R�\@R-@Q��@Qx�@QG�@Q7L@Q&�@Q�@P��@P�`@P�`@P��@PĜ@P�9@PbN@PA�@P1'@P �@P �@Pb@O�@O|�@Nȴ@N��@N��@N��@Nv�@M�@M�-@M`B@L�j@Lj@K�m@K��@K�@Kt�@KC�@K@J�H@J��@J=q@I�#@I�7@IX@I%@H�`@H�9@Hr�@HQ�@H1'@G�@G�P@G�P@G+@F�y@Fȴ@F�+@F{@E�-@E�@D��@D�j@D�@Dj@C��@C�m@C�F@CdZ@Co@B�@B��@Bn�@B-@B�@A�@A�@A�#@A��@A7L@A%@@�9@@��@@�@@bN@@Q�@@ �@?�@?�w@?�P@?+@>��@>ff@=�T@=p�@=?}@=�@<��@<��@<�D@<j@<j@<Z@<�@;o@:��@:��@:n�@:�@9x�@8�`@8��@8��@8��@8Q�@7�@7�P@6�y@6E�@5�h@5O�@5V@4�@4��@4�D@4Z@4j@4I�@4�@3ƨ@3�@3C�@3"�@2�@2��@2^5@1�#@1G�@0�u@0b@/�@/K�@/
=@.�y@.v�@.V@.5?@.$�@.@-�@-�T@-�@-?}@-?}@-/@-�@,��@,�/@,��@,��@,I�@,(�@+�m@+"�@*�!@*�!@*n�@*M�@*-@)��@)��@)��@)��@)�7@)x�@)X@)7L@(�u@(1'@(b@'��@'�P@'K�@'
=@&�@&��@&v�@%@%O�@%�@$�@$I�@#�
@#dZ@"�H@"~�@"n�@!�@!7L@ �9@ �@ r�@ Q�@ 1'@��@��@;d@;d@;d@�@��@E�@��@�h@�@�@`B@`B@/@�@��@j@�m@��@dZ@"�@��@��@�\@=q@J@�@�@��@��@��@�7@X@G�@7L@&�@&�@�@%@��@��@�`@�`@�`@�`@�`@��@Ĝ@Ĝ@��@A�@1'@1'@  @�;@�;@��@�w@�P@�P@�P@|�@|�@\)@\)@\)@;d@�y@��@��@��@��@��@��@��@�+@ff@E�@$�@@@��@�@�@�/@��@��@�j@�@z�@9X@�@�@�@�@�@1@�
@��@C�@@�@�@��@��@��@��@��@��@��@��@��@M�@=q@�@�@�7@X@7L@�@�@%@��@�u@bN@Q�@Q�@Q�@ �@�w@�P@|�@l�@\)@K�@;d@�@
=@��@E�@E�@5?@$�@@��@��@�h@p�@O�@�@�j@�j@�@��@��@�D@�D@z�@j@Z@9X@(�@�@��@�@C�@o@
��@
�!@
�!@
��@
�!@
n�@
J@	�7@	G�@	&�@	%@��@��@��@�9@r�@��@�@��@�y@�@ȴ@ȴ@�R@�R@��@��@�+@E�@@�T@�-@�@/@�@j@�m@�F@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B}�B~�B~�B~�B~�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B� B~�B}�B}�B}�B{�Bx�Bq�Bm�BdZB5?BB6FBL�BB�B)�B[#BbNBI�BQ�B>wB33BF�B>wBC�B:^B7LB49B/B6FB2-B2-B%�B�B�B�B�B�B��B�^B��BÖB��B�^B��B�!B��B�+Bq�BdZBn�Bz�Bq�BXB<jB%�B�B,B0!B/B �B  B
�NB
�HB
��B
�B
�/B
��B
�;B
�)B
�B
��B
ǮB
�?B
�VB
�B
o�B
I�B
1'B
&�B
B	�B	�TB	�)B	�NB	B	ɺB	�9B	�\B	�B	aHB	�uB	�%B	x�B	dZB	l�B	aHB	_;B	\)B	dZB	XB	`BB	T�B	F�B	;dB	49B	&�B	B�B	  B��B�B��B��B�B��B��B��BȴBƨB��B��B�^B�jB��B��B��B�?B�B��B�bBiyB�VB�!B�B��B��B��B�B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�oB�DB�+B� Bt�Bu�B]/B{�B�Bz�Bv�B~�B|�B}�Bz�Bq�Bu�Bo�Bo�Bl�BiyBhsBhsBffB_;BL�BJ�B9XB.BN�BT�BP�BM�BL�B?}B2-B�B��B�B:^BB�B@�B=qB9XB9XB8RB1'B�B�B6FB8RB6FB0!B0!B%�B�B�B�B �B�B{BDBhB�BbB!�B.B(�B�B&�B$�B(�B.B.B'�B�BBoB�B'�B&�B.B0!B%�B�B+B�BJB�B!�B �B#�B\BbB&�B1'B0!B;dB@�B>wBJ�BP�BM�BG�B<jB>wBS�B\)B^5B_;B_;BcTBffBe`BffBffBgmBe`BdZBgmBiyBiyBgmBe`BaHB^5BYBYBdZBcTBYBq�Bn�By�B�B�%B�+B�B�B� B� By�B�7B�\B�1B�=B�bB��B�{B��B��B��B��B��B��B��B��B�uB��B��B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�^B�wB�wB�jB�^B�LB�FB�^B�qB�wB��BĜBŢBB�qBÖB��BĜBĜB�LB�^B�wB�jB�3B�-BĜB��B��B��BɺBÖB��B�#B�5B�/B�B�B�B�B�B�B�B�B�B��B��B��B��B	
=B	oB	hB	\B	PB	{B	�B	�B	#�B	$�B	"�B	%�B	(�B	0!B	/B	33B	8RB	>wB	>wB	B�B	D�B	C�B	F�B	M�B	M�B	N�B	P�B	P�B	P�B	P�B	M�B	Q�B	Q�B	Q�B	P�B	T�B	\)B	YB	aHB	k�B	p�B	w�B	z�B	z�B	{�B	|�B	~�B	� B	� B	}�B	|�B	y�B	�1B	�VB	�\B	�bB	�bB	�\B	�VB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�B	�'B	�-B	�-B	�'B	�B	�!B	�'B	�FB	�dB	�dB	�jB	�RB	�qB	��B	ÖB	ƨB	ŢB	ĜB	ĜB	ŢB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	�B	�#B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�B	�B	�B	�B	�/B	�BB	�;B	�TB	�mB	�mB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�yB	�mB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
B
B
B
B
B
1B

=B

=B
	7B

=B

=B

=B
	7B

=B
JB
PB
PB
\B
bB
bB
hB
hB
hB
bB
uB
oB
uB
{B
uB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
"�B
"�B
$�B
#�B
$�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
&�B
$�B
!�B
&�B
'�B
&�B
%�B
$�B
&�B
)�B
,B
,B
)�B
)�B
-B
)�B
(�B
)�B
-B
.B
0!B
0!B
1'B
1'B
2-B
2-B
0!B
0!B
1'B
2-B
33B
2-B
2-B
0!B
0!B
0!B
0!B
33B
49B
5?B
6FB
6FB
5?B
7LB
8RB
8RB
8RB
8RB
8RB
7LB
8RB
:^B
:^B
9XB
9XB
9XB
9XB
9XB
8RB
:^B
8RB
7LB
:^B
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
>wB
>wB
=qB
;dB
>wB
@�B
@�B
@�B
A�B
B�B
C�B
C�B
B�B
A�B
B�B
E�B
D�B
E�B
D�B
E�B
F�B
G�B
I�B
G�B
G�B
J�B
L�B
N�B
M�B
M�B
L�B
M�B
M�B
O�B
O�B
M�B
N�B
N�B
N�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
P�B
P�B
N�B
P�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
T�B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
XB
ZB
ZB
ZB
ZB
[#B
[#B
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
YB
ZB
]/B
]/B
]/B
]/B
]/B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
_;B
^5B
_;B
^5B
`BB
aHB
bNB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
`BB
_;B
bNB
aHB
`BB
_;B
aHB
bNB
bNB
cTB
bNB
bNB
bNB
cTB
dZB
dZB
cTB
cTB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
dZB
cTB
dZB
gmB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
ffB
ffB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
ffB
iyB
iyB
jB
k�B
l�B
k�B
jB
iyB
iyB
iyB
k�B
m�B
m�B
m�B
m�B
m�B
l�B
k�B
jB
l�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
t�B
u�B
u�B
u�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�9B�9B�9B�9B�9B�B�3B�-B� B� B� B~BB.BBHB.B�B�B�B�B�B�B�AB�'B�B�'B�B�B�B�B�'B�B�'B�B� B� B�B� BB~(B~B~(B|By>Br�Bn�BffB:�B
=B:*BPBF�B/�B\�Bd�BMBTBA�B6zBH�B@�BEB<6B8�B6+B0�B7LB33B3MB'RB�B�B�B�B��B�B��BѷB�mBΊB�B��B��B��B��Bt�BgBo�B{BrGBY�B>�B)BkB,�B0�B/OB!�B�B
�FB
�B
��B
�[B
��B
̘B
ߊB
ܒB
ٚB
�oB
�fB
�`B
�NB
�EB
q�B
NB
4�B
)�B
	RB	ߤB	�B	�VB	�tB	ňB	ˬB	��B	�@B	�+B	ezB	�,B	��B	z�B	gB	n/B	c�B	aHB	]�B	e`B	Y�B	`�B	VB	H1B	=<B	5�B	)B	zB�OB	�B�B�B��B��B��B�MBյB�B�=B��B͟B˒B�6B��B��B�B�;B�`B�;B�IB��Bm�B�B�!B��B��B�5B�DB�qB��B�B�RB�fB��B�8B�tB�bB��B�BB�B�LB�B�,B�:B�;B�B�1B�@B�dB�B�UBv+Bw2B`BB|�B��B|Bw�BcB}qB~wB{Br�Bv`Bp�BpoBm]BjeBiDBi*Bf�B`'BNpBL0B;�B0�BOBBU�BQ�BN�BMPB@�B3�BB�PByB:�BB�BAB=�B9�B9�B8�B1�B!�B!B6�B8�B6�B0�B0�B&�B�B�B�B!|B�B�BB�B�BB"�B.IB)�B�B'�B%�B)�B.�B.}B(�B�B�B�B �B(�B'�B.�B0�B&�BB	B�B�BB# B"B$�B�B�B(>B2GB1�B<6BAUB?�BKDBQBN"BHKB=�B?�BTFB\]B^jB_pB_pBc�Bf�BezBf�Bf�Bg�Be�Bd�Bg�Bi�Bi�Bg�Be�Ba�B_!BZ7BZ�BeFBd�BZ�Br�Bo�Bz�B��B�tB�zB��B�uB��B��B{0B�lB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�HB�ZB�8B�LB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�(B��B�B��BĶB�RB��B��B��B�TB��B�9B�,B�@B�HB�rB��B�{BیBބB�~BٴB�$B�B��B��B��B� B�/B�B�+B�ZB��B��B	
�B	�B	�B	�B	�B	�B	�B	B	#�B	%,B	#TB	&fB	)yB	0oB	/�B	3�B	8�B	>�B	>�B	B�B	D�B	C�B	GB	NB	NB	N�B	P�B	QB	QB	QB	N"B	R B	R B	R B	QhB	U�B	\]B	Y�B	a�B	k�B	qB	w�B	z�B	{B	|B	}<B	.B	�4B	�4B	~]B	}VB	z�B	��B	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�VB	�=B	�5B	�cB	�CB	�OB	�UB	�UB	�cB	�[B	�aB	�aB	�[B	�iB	�oB	��B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	żB	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	�B	�B	��B	�	B	�B	�B	�DB	�PB	�MB	�SB	�#B	�QB	�CB	�)B	�CB	�)B	�CB	�)B	�CB	�QB	�QB	�kB	چB	ݘB	��B	ߤB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�B	�B	�"B	�B	�B	�"B	�"B	�B	�B	�"B	�B	�B	��B	�(B	�(B	�B	�<B
B
-B
-B
;B
 4B
'B
[B
oB
mB
SB
KB

XB

XB
	lB

rB

XB

XB
	lB

rB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
"�B
"�B
#B
#B
#B
$B
$B
$B
# B
# B
$�B
$B
%,B
'B
'B
'B
(
B
'�B
($B
(
B
'B
%,B
"B
'B
($B
'B
&2B
%,B
'8B
*0B
,"B
,"B
*KB
*KB
-)B
*KB
)_B
*KB
-CB
./B
0UB
0UB
1AB
1AB
2GB
2aB
0UB
0UB
1[B
2GB
3hB
2aB
2aB
0UB
0oB
0UB
0�B
3�B
4nB
5ZB
6`B
6zB
5tB
7�B
8�B
8lB
8lB
8lB
8�B
7�B
8lB
:xB
:^B
9�B
9�B
9�B
9�B
9rB
8�B
:xB
8�B
7�B
:�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
>�B
>�B
=�B
;�B
>�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
B�B
A�B
B�B
E�B
D�B
E�B
D�B
E�B
F�B
G�B
I�B
G�B
HB
KB
L�B
N�B
M�B
NB
MB
NB
NB
O�B
O�B
N"B
OB
N�B
O(B
QB
RB
RB
R B
RB
QB
QB
QB
QB
OB
Q B
R B
R B
RB
S�B
T,B
T,B
U2B
W$B
W$B
W?B
W?B
W$B
W?B
W?B
W$B
XB
X+B
XB
X+B
X+B
X+B
YB
Z7B
Z7B
ZB
Z7B
Z7B
Z7B
ZB
Z7B
YKB
XEB
Z7B
Z7B
Z7B
Z7B
[#B
[#B
ZQB
Z7B
[#B
[=B
[=B
[#B
[=B
[#B
[#B
Z7B
YKB
ZQB
]IB
]IB
]IB
]/B
]/B
\)B
\CB
\]B
\]B
\]B
\CB
\]B
\CB
\]B
\]B
]dB
_;B
_VB
_VB
^OB
^jB
^jB
_VB
`\B
`\B
`BB
`BB
_VB
^jB
_pB
^�B
`vB
aHB
bhB
a|B
bhB
bhB
bNB
bhB
bNB
bNB
aHB
`\B
_pB
bNB
abB
`\B
_�B
a|B
b�B
b�B
cnB
b�B
b�B
b�B
cnB
dtB
dtB
c�B
c�B
b�B
d�B
ezB
e`B
ezB
e`B
ezB
d�B
d�B
c�B
d�B
g�B
gmB
ffB
f�B
f�B
f�B
g�B
g�B
f�B
f�B
g�B
iyB
iyB
i�B
iyB
iyB
i�B
i�B
i�B
iyB
i�B
i�B
i�B
h�B
f�B
i�B
i�B
j�B
k�B
l�B
k�B
j�B
i�B
i�B
i�B
k�B
m�B
m�B
m�B
m�B
m�B
l�B
k�B
j�B
l�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
t�B
u�B
u�B
u�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812290035142018122900351420181229003514201812290200212018122902002120181229020021201812300020082018123000200820181230002008  JA  ARFMdecpA19c                                                                20181225063628  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181224213642  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181224213646  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181224213646  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181224213647  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181224213647  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181224213647  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181224213647  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181224213648  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181224213648                      G�O�G�O�G�O�                JA  ARUP                                                                        20181224215616                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181225153509  CV  JULD            G�O�G�O�F�՚                JM  ARCAJMQC2.0                                                                 20181228153514  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181228153514  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181228170021  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181229152008  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                