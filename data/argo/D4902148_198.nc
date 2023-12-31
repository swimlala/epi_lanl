CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-18T15:37:37Z creation;2020-01-18T15:37:41Z conversion to V3.1;2022-11-21T05:27:42Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        d  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ih   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  MD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۰   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20200118153737  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_198                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��"�� 1   @��#�`�@<(�p:��dzn��O�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
Dx}qDx�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��7A��DA��7A��\A��DA��7A��7A��7A��DA��\A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��A��A�p�A�O�A�1'A��A�7LA�v�A��uA�5?A���A���A��7A�z�A�p�A�+A�ƨA�33A���A��#A�VA���A��TA��+A��A��A�9XA�ffA�A��7A�  A�t�A�r�A��A��A�^5A��#A�/A��A�;dA�%A��A���A�  A�ffA�ĜA���A�S�A���A�r�A�v�A��A�M�A���A��A��A��A��PA�A��A�1A��TA���A��wA��FA���A�ĜA�7LA���A���A�A�K�A�+A�VA~��A}�7AzQ�Ay�Aw�FAvjAu\)At=qAs�
Ar�jAqS�Ap�9ApJAo+An�Al~�Al(�Ak��Aj�Ai��Ah�AhZAg�wAf�!Ae�hAc�FAaƨA`5?A_"�A^�A^z�A]�A\��AZE�AXz�AW��AVĜAV^5AV �AUt�AT��AT��AS�-AR�AQhsAO��AO|�ANv�AM|�AL��AK��AJ�jAI��AHȴAH �AGp�AF�9AF9XAEVAD-AB��AA|�AA?}A@�9A?x�A=�;A;��A:�9A9?}A8E�A7�^A7�hA7`BA7;dA733A6��A6ȴA6�\A6z�A6A�A5�A5t�A1&�A/oA.ȴA.A�A-�^A-�A-dZA-G�A-VA,9XA*��A*��A*-A)��A'�;A&�A%�
A$9XA$ �A#�#A#�A#/A"��A!��A!O�A ��A��A  A�FAl�A�A�AhsA�A��AJA�`A�wA�A`BA?}AVA�A�9A�HA|�A~�A��A7LAn�A�A��A��A�A`BAXA�A��A
��A
bNA	��AĜAn�A�AO�AAȴA�uA5?A��AS�A�DA�A;dA�DA�FAoA -@�ȴ@�@�1@�dZ@�~�@�p�@�%@���@��u@��D@�bN@�  @�\)@��@�n�@���@�V@�ƨ@���@�V@�G�@�Ĝ@���@�"�@��T@�&�@��;@�=q@�V@��@ᙚ@�\)@ݡ�@۝�@��@�~�@���@ٺ^@٩�@�X@�j@ם�@�"�@�E�@�9X@�-@���@�(�@�33@ΰ!@�@�\)@ɉ7@��@�;d@�ȴ@���@�X@ċD@�S�@°!@�M�@�@���@�  @�
=@��y@��@���@�?}@��@�@���@���@��@��H@�{@�G�@��F@��7@�V@��j@�;d@�=q@���@�hs@�%@���@�j@�b@���@���@��T@�I�@�|�@�S�@�
=@��y@��@��/@�bN@�(�@�  @��@��@��@��j@��@�1'@��@���@��@�o@���@�V@�-@��@�x�@���@�j@��@���@�V@��@�@�p�@�/@��@�(�@�  @���@���@�v�@��@�%@��@���@�l�@�"�@���@���@���@�ff@�E�@�5?@�{@��T@�X@��j@���@�z�@�A�@�ƨ@��@�o@�$�@��-@�X@��@��/@�A�@��
@���@��P@��@�t�@�;d@��@�
=@��H@���@��!@��\@��@��7@�?}@�&�@��@��u@�bN@�Z@�I�@�9X@�(�@��@��@�o@���@���@�^5@��#@��7@�`B@�X@�%@��@�9X@�1'@���@�K�@��@���@�M�@�@�hs@�Ĝ@�r�@�@�P@\)@~��@~E�@|�j@|z�@|j@|(�@{S�@{@z��@z�\@zM�@zJ@y�#@y&�@x��@xQ�@xA�@x  @w�P@w
=@v��@v5?@u`B@tz�@s��@s��@s"�@r�!@q��@q�@pQ�@p �@o�;@o�P@nȴ@nv�@nV@n{@m�T@m`B@mV@l�/@l��@lj@l9X@k�
@k��@kC�@j��@jM�@jJ@i�@i��@i��@i7L@hbN@hb@g�w@f��@f��@fff@e�T@e?}@d��@d�j@d�j@d�@d�@d�@d�D@dj@d(�@c��@b��@b�!@b��@b~�@bM�@a��@a��@a�@`��@`Ĝ@`�u@` �@_��@_��@_�P@^��@^�R@^ȴ@^�y@^5?@]�T@]��@]`B@]/@\I�@[��@[dZ@["�@Zn�@Y�#@Y��@Y��@Y�7@Y�7@Yx�@Yhs@YG�@Y&�@Y�@X��@X��@X��@XA�@Xb@W�w@W|�@W
=@V�R@V��@V5?@U��@T��@TZ@S��@Sƨ@S�
@Sƨ@S��@SS�@R��@R=q@RJ@Q�^@Q��@Q7L@P�`@Pr�@PA�@O�@O�@O|�@OK�@O;d@O+@N�y@N��@N$�@M�@M@MO�@L�@L��@L�j@Lj@L(�@K�
@Ko@J�H@J��@J^5@I��@Ix�@IX@I&�@HQ�@H  @G��@Gl�@G;d@F�y@F�@FE�@E@E��@E��@E��@E�h@E�@Ep�@E`B@E?}@D�@Dz�@C�m@CC�@C@B��@B��@Bn�@B�@A��@A�^@A��@Ax�@AG�@A�@@�9@@r�@@A�@@ �@?�@?��@?l�@?\)@?�@>v�@>@=@=�-@=p�@<��@<1@;dZ@;dZ@;dZ@;dZ@;S�@;S�@;C�@;C�@;33@;o@:��@:~�@:M�@9�^@9&�@8��@8�9@8��@8b@7�@7|�@7+@6ȴ@6��@6�+@65?@6{@6@6@5��@5@5�-@5�h@5`B@5�@4��@4�j@4�@3��@3��@3��@3t�@3S�@3o@2��@2^5@1�#@1hs@1%@0��@0�9@0�9@0��@0�u@0bN@0Q�@0A�@0  @/
=@.�@.ȴ@.�R@.��@.v�@.E�@.@-�@,�/@,�j@,��@,Z@,(�@,1@+��@+��@+dZ@+C�@*��@*�\@*~�@*n�@*^5@*=q@*�@)�#@)�^@)X@)&�@)�@)�@(��@(�u@(r�@(bN@(Q�@(Q�@(1'@( �@'�@'��@'��@'
=@'
=@&�R@&��@&5?@%/@$�@$�/@$��@$�j@$j@#ƨ@#t�@#33@"�H@"��@"^5@"J@!�@!�^@!�7@ �`@ ��@ A�@ b@�@��@�@�P@|�@K�@�@�+@5?@{@��@p�@?}@V@�@��@�j@�j@�@(�@ƨ@�F@��@dZ@dZ@"�@@@@@�@�H@��@�!@��@n�@�@��@�@��@��@hs@G�@7L@7L@7L@&�@&�@�`@��@A�@  @�@|�@K�@;d@;d@;d@�@ȴ@��@V@{@�T@�-@�h@`B@?}@/@��@�/@�j@�@�@�@�@��@z�@I�@��@�m@C�@��@�!@�\@n�@=q@�@J@hs@G�@7L@%@�9@�u@�u@�u@�u@�u@�@bN@Q�@1'@ �@ �@ �@b@  @l�@�@�R@��@v�@ff@V@E�@5?@$�@�@��@?}@�@�@V@�@�j@�j@�@j@Z@Z@(�@��@�
@ƨ@�@dZ@S�@C�@C�@33@o@
��@
��@
n�@
^5@
M�@
M�@
=q@
-@
�@
J@	��@	�^@	x�@	x�@	hs@	X@	&�@��@Ĝ@�@bN@1'@�@�w@\)@+@�@��@�y@ȴ@�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��7A��DA��7A��\A��DA��7A��7A��7A��DA��\A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��A��A�p�A�O�A�1'A��A�7LA�v�A��uA�5?A���A���A��7A�z�A�p�A�+A�ƨA�33A���A��#A�VA���A��TA��+A��A��A�9XA�ffA�A��7A�  A�t�A�r�A��A��A�^5A��#A�/A��A�;dA�%A��A���A�  A�ffA�ĜA���A�S�A���A�r�A�v�A��A�M�A���A��A��A��A��PA�A��A�1A��TA���A��wA��FA���A�ĜA�7LA���A���A�A�K�A�+A�VA~��A}�7AzQ�Ay�Aw�FAvjAu\)At=qAs�
Ar�jAqS�Ap�9ApJAo+An�Al~�Al(�Ak��Aj�Ai��Ah�AhZAg�wAf�!Ae�hAc�FAaƨA`5?A_"�A^�A^z�A]�A\��AZE�AXz�AW��AVĜAV^5AV �AUt�AT��AT��AS�-AR�AQhsAO��AO|�ANv�AM|�AL��AK��AJ�jAI��AHȴAH �AGp�AF�9AF9XAEVAD-AB��AA|�AA?}A@�9A?x�A=�;A;��A:�9A9?}A8E�A7�^A7�hA7`BA7;dA733A6��A6ȴA6�\A6z�A6A�A5�A5t�A1&�A/oA.ȴA.A�A-�^A-�A-dZA-G�A-VA,9XA*��A*��A*-A)��A'�;A&�A%�
A$9XA$ �A#�#A#�A#/A"��A!��A!O�A ��A��A  A�FAl�A�A�AhsA�A��AJA�`A�wA�A`BA?}AVA�A�9A�HA|�A~�A��A7LAn�A�A��A��A�A`BAXA�A��A
��A
bNA	��AĜAn�A�AO�AAȴA�uA5?A��AS�A�DA�A;dA�DA�FAoA -@�ȴ@�@�1@�dZ@�~�@�p�@�%@���@��u@��D@�bN@�  @�\)@��@�n�@���@�V@�ƨ@���@�V@�G�@�Ĝ@���@�"�@��T@�&�@��;@�=q@�V@��@ᙚ@�\)@ݡ�@۝�@��@�~�@���@ٺ^@٩�@�X@�j@ם�@�"�@�E�@�9X@�-@���@�(�@�33@ΰ!@�@�\)@ɉ7@��@�;d@�ȴ@���@�X@ċD@�S�@°!@�M�@�@���@�  @�
=@��y@��@���@�?}@��@�@���@���@��@��H@�{@�G�@��F@��7@�V@��j@�;d@�=q@���@�hs@�%@���@�j@�b@���@���@��T@�I�@�|�@�S�@�
=@��y@��@��/@�bN@�(�@�  @��@��@��@��j@��@�1'@��@���@��@�o@���@�V@�-@��@�x�@���@�j@��@���@�V@��@�@�p�@�/@��@�(�@�  @���@���@�v�@��@�%@��@���@�l�@�"�@���@���@���@�ff@�E�@�5?@�{@��T@�X@��j@���@�z�@�A�@�ƨ@��@�o@�$�@��-@�X@��@��/@�A�@��
@���@��P@��@�t�@�;d@��@�
=@��H@���@��!@��\@��@��7@�?}@�&�@��@��u@�bN@�Z@�I�@�9X@�(�@��@��@�o@���@���@�^5@��#@��7@�`B@�X@�%@��@�9X@�1'@���@�K�@��@���@�M�@�@�hs@�Ĝ@�r�@�@�P@\)@~��@~E�@|�j@|z�@|j@|(�@{S�@{@z��@z�\@zM�@zJ@y�#@y&�@x��@xQ�@xA�@x  @w�P@w
=@v��@v5?@u`B@tz�@s��@s��@s"�@r�!@q��@q�@pQ�@p �@o�;@o�P@nȴ@nv�@nV@n{@m�T@m`B@mV@l�/@l��@lj@l9X@k�
@k��@kC�@j��@jM�@jJ@i�@i��@i��@i7L@hbN@hb@g�w@f��@f��@fff@e�T@e?}@d��@d�j@d�j@d�@d�@d�@d�D@dj@d(�@c��@b��@b�!@b��@b~�@bM�@a��@a��@a�@`��@`Ĝ@`�u@` �@_��@_��@_�P@^��@^�R@^ȴ@^�y@^5?@]�T@]��@]`B@]/@\I�@[��@[dZ@["�@Zn�@Y�#@Y��@Y��@Y�7@Y�7@Yx�@Yhs@YG�@Y&�@Y�@X��@X��@X��@XA�@Xb@W�w@W|�@W
=@V�R@V��@V5?@U��@T��@TZ@S��@Sƨ@S�
@Sƨ@S��@SS�@R��@R=q@RJ@Q�^@Q��@Q7L@P�`@Pr�@PA�@O�@O�@O|�@OK�@O;d@O+@N�y@N��@N$�@M�@M@MO�@L�@L��@L�j@Lj@L(�@K�
@Ko@J�H@J��@J^5@I��@Ix�@IX@I&�@HQ�@H  @G��@Gl�@G;d@F�y@F�@FE�@E@E��@E��@E��@E�h@E�@Ep�@E`B@E?}@D�@Dz�@C�m@CC�@C@B��@B��@Bn�@B�@A��@A�^@A��@Ax�@AG�@A�@@�9@@r�@@A�@@ �@?�@?��@?l�@?\)@?�@>v�@>@=@=�-@=p�@<��@<1@;dZ@;dZ@;dZ@;dZ@;S�@;S�@;C�@;C�@;33@;o@:��@:~�@:M�@9�^@9&�@8��@8�9@8��@8b@7�@7|�@7+@6ȴ@6��@6�+@65?@6{@6@6@5��@5@5�-@5�h@5`B@5�@4��@4�j@4�@3��@3��@3��@3t�@3S�@3o@2��@2^5@1�#@1hs@1%@0��@0�9@0�9@0��@0�u@0bN@0Q�@0A�@0  @/
=@.�@.ȴ@.�R@.��@.v�@.E�@.@-�@,�/@,�j@,��@,Z@,(�@,1@+��@+��@+dZ@+C�@*��@*�\@*~�@*n�@*^5@*=q@*�@)�#@)�^@)X@)&�@)�@)�@(��@(�u@(r�@(bN@(Q�@(Q�@(1'@( �@'�@'��@'��@'
=@'
=@&�R@&��@&5?@%/@$�@$�/@$��@$�j@$j@#ƨ@#t�@#33@"�H@"��@"^5@"J@!�@!�^@!�7@ �`@ ��@ A�@ b@�@��@�@�P@|�@K�@�@�+@5?@{@��@p�@?}@V@�@��@�j@�j@�@(�@ƨ@�F@��@dZ@dZ@"�@@@@@�@�H@��@�!@��@n�@�@��@�@��@��@hs@G�@7L@7L@7L@&�@&�@�`@��@A�@  @�@|�@K�@;d@;d@;d@�@ȴ@��@V@{@�T@�-@�h@`B@?}@/@��@�/@�j@�@�@�@�@��@z�@I�@��@�m@C�@��@�!@�\@n�@=q@�@J@hs@G�@7L@%@�9@�u@�u@�u@�u@�u@�@bN@Q�@1'@ �@ �@ �@b@  @l�@�@�R@��@v�@ff@V@E�@5?@$�@�@��@?}@�@�@V@�@�j@�j@�@j@Z@Z@(�@��@�
@ƨ@�@dZ@S�@C�@C�@33@o@
��@
��@
n�@
^5@
M�@
M�@
=q@
-@
�@
J@	��@	�^@	x�@	x�@	hs@	X@	&�@��@Ĝ@�@bN@1'@�@�w@\)@+@�@��@�y@ȴ@�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�7B�7B�=B�7B�=B�=B�=B�=B�DB�DB�DB�JB�PB�PB�PB�PB�VB�\B�\B�\B�\B�VB�oB�uB��B��B��B��B��B��B��B��B��B�B�?B�qB�
B�mB�sB�sB�mB�fB�fB�HB��B��B��BȴB�}B��B�\Bs�BdZB^5BQ�BP�BT�BJ�B;dB0!B!�B�B�B{B%B��B��B��B��B��B�B�B��B��B�!B��B�=B|�Bu�Bl�BgmBaHBXBK�B33B�BVB
��B
��B
��B
��B
��B
��B
�B
�HB
�/B
�B
��B
�}B
�B
�bB
�B
y�B
e`B
[#B
Q�B
H�B
@�B
8RB
5?B
.B
$�B
 �B
�B
�B
VB
B
B	��B	��B	�B	�B	�B	�fB	�BB	�B	��B	ǮB	ÖB	�qB	�dB	�RB	�-B	�B	��B	��B	�bB	�PB	�DB	�7B	�%B	�B	�B	z�B	u�B	m�B	ffB	bNB	^5B	XB	S�B	N�B	I�B	D�B	@�B	<jB	9XB	5?B	1'B	+B	$�B	�B	�B	�B	{B	\B	+B��B��B�B�B�B�yB�sB�sB�mB�fB�fB�ZB�ZB�NB�BB�#B��B��B�}B�qB�dB�^B�^B�XB�LB�9B�B�B�B��B��B��B��B��B�{B�uB�oB�bB�VB�JB�=B�+B�B�B� B~�B|�Bz�Bx�Bw�Bu�Bs�Bp�Bn�Bm�Bm�Bl�Bl�Bk�BhsBdZBaHB^5BZBW
BT�BS�BS�BR�BR�BR�BQ�BP�BN�BL�BJ�BI�BH�BG�BF�BE�BD�BD�BC�BB�BA�B@�B?}B>wB=qB<jB;dB9XB8RB8RB8RB8RB7LB7LB6FB6FB6FB6FB6FB6FB5?B5?B49B2-B1'B0!B/B/B.B.B-B,B+B,B+B+B+B)�B)�B)�B)�B+B,B,B,B,B,B,B,B,B,B,B+B,B-B.B.B/B/B.B1'B33B5?B5?B6FB7LB7LB8RB:^B;dB;dB;dB;dB=qB?}B?}B?}B?}BA�BC�BE�BG�BI�BK�BL�BN�BO�BS�B\)B]/B]/BaHBdZBffBgmBhsBiyBjBk�Bl�Bm�Bp�Bv�By�Bz�Bz�Bz�B}�B�B�B�B�B�+B�JB�\B�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�3B�3B�RB�^B�dB��BBĜBƨBǮBǮBȴBȴBɺB��B��B��B��B��B�B�B�B�#B�/B�;B�BB�TB�mB�sB�sB�yB�yB�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	%B	1B	
=B	DB	JB	VB	bB	hB	hB	uB	�B	�B	�B	�B	 �B	"�B	$�B	'�B	+B	/B	2-B	49B	8RB	:^B	;dB	<jB	?}B	E�B	F�B	F�B	F�B	I�B	J�B	K�B	L�B	M�B	N�B	N�B	Q�B	T�B	VB	VB	W
B	YB	\)B	]/B	^5B	bNB	ffB	iyB	jB	k�B	m�B	o�B	r�B	v�B	v�B	w�B	y�B	}�B	� B	�B	�B	�B	�%B	�1B	�7B	�DB	�VB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�9B	�?B	�FB	�LB	�XB	�XB	�^B	�dB	�dB	�dB	�qB	�wB	�}B	�}B	��B	B	B	ÖB	ĜB	ĜB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�TB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
JB
PB
VB
\B
bB
hB
hB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
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
YB
YB
YB
YB
YB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
e`B
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�7B�7B�=B�7B�=B�=B�=B�XB�^B�^B�^B�JB�PB�jB�jB�jB�VB�vB�\B�\B�vB�pB�oB�uB��B��B��B��B��B�B�=B��B��B��B��B�'B�B�B��B��B�B�8B�mB�hBյBѝB� B�#B�AB�B� Bu?Be`B_�BS@BRoBV�BN"B=�B2�B#B�B�B�B�B��B��B�^B��B�2B��B�#BҽB��B��B�]B�JB~]Bw2Bm�Bh�Bc BZ�BO\B6zB�BHB
��B
�<B
�0B
�DB
��B
��B
�B
�4B
�jB
��B
�B
ªB
�GB
��B
�B
}<B
g8B
\�B
SuB
J	B
A�B
9>B
6�B
/�B
%�B
!�B
�B
�B
�B
�B
�B	�B	�*B	�B	�}B	�B	��B	��B	یB	�:B	ɠB	ĶB	�B	�6B	��B	�B	��B	��B	��B	�NB	��B	��B	�	B	��B	��B	�[B	|PB	w�B	o5B	gmB	c�B	_�B	YeB	UMB	PHB	KB	E�B	AoB	=VB	:xB	6FB	2�B	,WB	&�B	 'B	KB	�B	SB	�B		�B��B��B��B�CB��B��B��B��B��B�B��B��B��B�TB�B߾B��B�AB�4B�(B��B��B��B�B��B��B��B��B�"B�eB��B��B�WB��B��B�B�B�NB��B�PB�xB��B��B��B��B�B~]B{�ByrBx�Bv�BuZBq�BoBm�Bm�Bl�BmBl�Bj�BfLBc B`vB\BXBU�BT{BTFBS&BS@BS[BR�BRTBPHBM�BK�BJ�BIlBH�BG_BF%BEBEBDMBCaBBABA�B@�B?cB>�B=�B<�B:�B9�B9>B9rB9	B8B8B6�B6�B6zB6�B6�B6�B5�B5�B5%B4B2�B1'B/�B/�B.�B.�B-�B,�B+�B,�B,"B,=B,"B+6B+�B+�B+QB,=B,�B,�B,�B,WB,WB,qB,�B,�B,�B,�B,�B-wB.B.�B.�B/�B0;B/�B2|B49B5�B5�B6�B7�B8B9$B:�B;�B;�B<B<�B>B?�B?�B@B@�BB�BD�BF�BH�BJ�BL�BM�BO�BQBUMB\�B]�B^OBbBd�Bf�Bg�Bh�Bi�BkBl"BmCBn�Bq�BwLBzB{0B{0B{�B~�B�uB�{B��B��B�B��B��B��B��B��B��B��B��B��B�B��B��B�/B�VB�4B��B��B�XB�XB�>B�DB�DB�_B�B�WB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�7B�=B�B�HB�NB�TB�aBևBּBٚB�qBݘBߊB��B�B�B�B�B�B��B�B�B��B��B��B��B�'B�+B�$B�B�6B�jB	 4B	;B	 B	 B	AB	uB	�B	�B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	B	!-B	#:B	%FB	(XB	+�B	/�B	2�B	4�B	8�B	:�B	;�B	<�B	@B	E�B	F�B	F�B	GB	I�B	J�B	K�B	MB	NB	OB	O(B	R:B	U2B	V9B	VSB	WYB	YeB	\xB	]dB	^�B	b�B	f�B	i�B	j�B	k�B	m�B	o�B	sB	v�B	wB	xB	zDB	~(B	�B	�AB	�GB	�MB	�?B	�fB	�RB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	�B	�&B	�2B	�*B	�B	�B	�"B	�"B	�"B	�=B	�=B	�]B	�iB	�vB	�TB	�TB	�tB	�zB	��B	�rB	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	B	��B	��B	ĶB	��B	��B	�B	�B	�B	�B	�B	�BB	�B	�&B	��B	�,B	�B	�B	�2B	�2B	�9B	�9B	�?B	�?B	�EB	�EB	�1B	�KB	�7B	�qB	�dB	�IB	�jB	ߊB	�B	�B	�B	�B	�sB	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�$B	��B	�B	��B	�B	�B	�B	�<B	�.B	�.B	�.B
 OB
;B
AB
AB
{B
SB
YB
_B
_B
KB
KB
�B
	lB

rB
^B
DB
dB
dB
dB
~B
~B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
!B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#B
#B
#�B
$B
%B
&2B
'B
'B
'B
'8B
($B
)*B
*0B
*0B
+6B
+6B
+B
,"B
,"B
,"B
-)B
-B
-CB
-)B
-)B
.IB
.IB
.cB
/OB
0;B
1AB
1[B
1[B
1AB
2GB
2aB
2|B
3hB
4�B
5ZB
5tB
6FB
6FB
6`B
6zB
6zB
7fB
7�B
7�B
7�B
9�B
9rB
9rB
9�B
9�B
:�B
:�B
:�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
IB
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
MB
M�B
NB
M�B
OB
N�B
N�B
OB
PB
O�B
PB
PB
Q B
QB
QB
RB
R B
SB
SB
S&B
SB
R�B
S&B
S@B
T,B
UB
UB
U2B
UB
UB
VB
VB
VB
VB
VB
VB
V9B
VB
VB
V9B
W?B
W$B
W
B
XEB
XEB
X+B
XEB
Y1B
Y1B
YB
YB
Y1B
XEB
Y1B
YeB
ZQB
ZQB
[WB
[WB
[=B
[=B
[#B
[=B
[=B
\]B
\]B
]IB
]dB
]dB
]dB
^jB
^jB
^OB
^jB
^jB
_VB
_;B
_VB
_VB
_VB
_VB
_pB
_VB
`vB
`\B
`�B
a�B
bNB
b�B
bhB
bhB
bhB
c�B
c�B
d�B
dtB
d�B
d�B
e`B
e`B
e`B
e`B
ezB
ezB
e�B
ezB
ezB
ezB
e`B
f�B
f�B
f�B
e�B
f�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001290034332020012900343320200129003433202211182141432022111821414320221118214143202001300018392020013000183920200130001839  JA  ARFMdecpA19c                                                                20200119003732  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200118153737  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200118153739  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200118153740  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200118153740  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200118153740  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200118153740  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200118153740  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200118153740  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200118153740  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200118153741  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200118153741                      G�O�G�O�G�O�                JA  ARUP                                                                        20200118155354                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200118153240  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200118153217  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20200128153433  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200128153433  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200129151839  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124143  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                