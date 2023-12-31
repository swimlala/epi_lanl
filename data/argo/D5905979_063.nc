CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:09Z creation      
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  yp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20200619170909  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ?A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ج��ȯ�1   @ج�[�p@6�� ě��d��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ?A   B   B   @���@�  A   A   A@  A`  A~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�3D�P D��fD���D�3D�X D���D���D�"�D�`RD���D��RD�=D�PRDڤ{D��D��D�U�D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��AA=A]A|(�A�{A��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��D w
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
Dtp�Dy��D��D�K�D���D��RD��D�S�D��fD��qD�D�[�D��D���D��D�K�Dڠ D�ؤD�)D�QHD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A�C�A�C�A�E�A�G�A�G�A�E�A�A�A�?}A�G�A�I�A�M�A�K�A�K�A�G�A�G�A�E�A�G�A�A�A�(�A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��RA���A�ƨA���A���A���A�ȴA���A�;dA���A��!A�^5A�$�A��`A��9A�C�A�jA�ffA��A�/A��;A��A��\A�z�A�XA�5?A��PA�A�A��;A���A�9XA�x�A�dZA��;A��+A��\A��TA�~�A�+A���A���A�A�|�A���A��DA��+A��A�1'A� �A��A��TA�G�A�A��A���A�n�A��A���A�"�A��A�bA�dZA�C�A�jA��wA�\)A��!A�A�JA��A�ZA� �A�%A���A�bA��A��PAVA}�^A{�
Az�RAy+Aw��AwO�Av~�Ao
=Ah�uAb-A_hsA[�
AV��AQl�AN�AM33AL��AL-AK�mAK��AK|�AKS�AK"�AJ��AJ�jAJ�DAI��AG��AE�hAC�FAA33A=�wA;�hA9
=A7S�A5��A5
=A4�!A4ffA3G�A2�A1;dA/��A/oA.�/A.�DA.{A,�DA+t�A*�+A)�TA(�HA(  A&�9A%�wA%%A$�A#�;A#hsA!�A ~�At�A�#A�uA\)A�/A�9A�A�wAK�A�/A�A�A��AZA�#AȴA�;A��A��A��A|�AE�A�
Ap�A�`A��A$�A
��A
{A��A��A�A��A�A�HA�+A`BA �\@��P@��H@��/@��7@��@��D@�I�@���@�M�@�`B@�V@�j@���@��@��
@ꟾ@��@�h@�Z@���@�bN@��@��T@�Z@�@ۮ@�V@أ�@�~�@�$�@Չ7@��m@�~�@�x�@��/@�C�@��T@̃@���@�^5@�`B@�bN@�
=@���@�7L@�b@�@��@��@�@��@�7L@��u@��F@��@�+@�"�@�t�@���@�b@�  @��m@�ƨ@��m@�(�@�bN@�
=@�%@�j@�Q�@�ȴ@��@�G�@��@�%@��@��`@���@��@�j@��@���@���@���@�bN@�b@��
@���@�\)@�33@���@��@�hs@�?}@�7L@��@�V@���@���@��9@�Q�@���@��y@��!@���@���@�z�@�(�@��R@��+@��h@�(�@��@�v�@�5?@�p�@���@���@��D@�A�@�b@��
@���@��@��R@�5?@��T@���@�hs@��@���@��u@�Q�@� �@��F@�dZ@�C�@��@�E�@���@�/@��j@��D@��D@�z�@�I�@� �@�t�@�o@��H@���@��R@�M�@��7@�X@�O�@�?}@�/@�V@���@���@���@��@�Z@�  @��@�v�@��@���@�x�@�X@�?}@�%@�&�@��`@�bN@�l�@�+@�@�@�
=@��@��R@��R@���@�ff@�-@�@��@��-@�x�@�X@�X@�/@��@���@�G�@�7L@��@�j@�z�@�1'@�|�@�K�@���@��H@��H@��@���@�^5@���@�&�@�(�@�S�@���@�v�@�J@�p�@�G�@�/@�j@���@��F@�t�@�"�@�@��@��y@��@��R@�{@��@�G�@�/@��@��j@���@��D@��@�r�@�Z@�Q�@�I�@�(�@��@���@��m@���@��P@���@��@�dZ@�K�@�C�@�33@��@���@���@��R@���@��+@�V@���@��^@���@��7@��@�x�@�O�@�&�@�Ĝ@~��@y�@k��@b�X@Y�.@Uc�@N�c@Gs@>&�@7��@2��@,�P@(��@"u@-w@�@��@�@�@�a1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�C�A�C�A�C�A�E�A�G�A�G�A�E�A�A�A�?}A�G�A�I�A�M�A�K�A�K�A�G�A�G�A�E�A�G�A�A�A�(�A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��RA���A�ƨA���A���A���A�ȴA���A�;dA���A��!A�^5A�$�A��`A��9A�C�A�jA�ffA��A�/A��;A��A��\A�z�A�XA�5?A��PA�A�A��;A���A�9XA�x�A�dZA��;A��+A��\A��TA�~�A�+A���A���A�A�|�A���A��DA��+A��A�1'A� �A��A��TA�G�A�A��A���A�n�A��A���A�"�A��A�bA�dZA�C�A�jA��wA�\)A��!A�A�JA��A�ZA� �A�%A���A�bA��A��PAVA}�^A{�
Az�RAy+Aw��AwO�Av~�Ao
=Ah�uAb-A_hsA[�
AV��AQl�AN�AM33AL��AL-AK�mAK��AK|�AKS�AK"�AJ��AJ�jAJ�DAI��AG��AE�hAC�FAA33A=�wA;�hA9
=A7S�A5��A5
=A4�!A4ffA3G�A2�A1;dA/��A/oA.�/A.�DA.{A,�DA+t�A*�+A)�TA(�HA(  A&�9A%�wA%%A$�A#�;A#hsA!�A ~�At�A�#A�uA\)A�/A�9A�A�wAK�A�/A�A�A��AZA�#AȴA�;A��A��A��A|�AE�A�
Ap�A�`A��A$�A
��A
{A��A��A�A��A�A�HA�+A`BA �\@��P@��H@��/@��7@��@��D@�I�@���@�M�@�`B@�V@�j@���@��@��
@ꟾ@��@�h@�Z@���@�bN@��@��T@�Z@�@ۮ@�V@أ�@�~�@�$�@Չ7@��m@�~�@�x�@��/@�C�@��T@̃@���@�^5@�`B@�bN@�
=@���@�7L@�b@�@��@��@�@��@�7L@��u@��F@��@�+@�"�@�t�@���@�b@�  @��m@�ƨ@��m@�(�@�bN@�
=@�%@�j@�Q�@�ȴ@��@�G�@��@�%@��@��`@���@��@�j@��@���@���@���@�bN@�b@��
@���@�\)@�33@���@��@�hs@�?}@�7L@��@�V@���@���@��9@�Q�@���@��y@��!@���@���@�z�@�(�@��R@��+@��h@�(�@��@�v�@�5?@�p�@���@���@��D@�A�@�b@��
@���@��@��R@�5?@��T@���@�hs@��@���@��u@�Q�@� �@��F@�dZ@�C�@��@�E�@���@�/@��j@��D@��D@�z�@�I�@� �@�t�@�o@��H@���@��R@�M�@��7@�X@�O�@�?}@�/@�V@���@���@���@��@�Z@�  @��@�v�@��@���@�x�@�X@�?}@�%@�&�@��`@�bN@�l�@�+@�@�@�
=@��@��R@��R@���@�ff@�-@�@��@��-@�x�@�X@�X@�/@��@���@�G�@�7L@��@�j@�z�@�1'@�|�@�K�@���@��H@��H@��@���@�^5@���@�&�@�(�@�S�@���@�v�@�J@�p�@�G�@�/@�j@���@��F@�t�@�"�@�@��@��y@��@��R@�{@��@�G�@�/@��@��j@���@��D@��@�r�@�Z@�Q�@�I�@�(�@��@���@��m@���@��P@���@��@�dZ@�K�@�C�@�33@��@���@���@��R@���@��+@�V@���@��^@���@��7@��@�x�@�O�@�&�G�O�@~��@y�@k��@b�X@Y�.@Uc�@N�c@Gs@>&�@7��@2��@,�P@(��@"u@-w@�@��@�@�@�a1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�LBÖBÖBÖBÖBÖBĜBŢBƨBǮBȴBȴB��B��B��B��B��B��B��B�
B�B�/B�;B�HB�NB�ZB�fB��B%B
=BhBuBoBbB�B&�B.B2-B49B8RB:^B;dB<jB<jB<jB>wB=qB@�B=qB>wBH�BK�BC�B?}B9XB.B(�B"�B�B+B%BB��B�B�yB�TB�mB�yB�mB�;B��B��B��B��B�'B��B�uB�=Bu�BT�B8RB"�B�B{B+B
�B
��B
ŢB
�3B
��B
��B
��B
��B
�oB
r�B
:^B
�B
uB
B	��B	�yB	�#B	��B	ɺB	�oB	_;B	33B	�B	B�B��BȴBĜB��B��B��B��B��B��B��B�}B�wB�jB�^B�9B�B��B��B�bB�1B�Bz�Br�Bl�BjBgmBe`BaHB^5B]/B^5B`BBaHBcTBaHB_;BbNBcTBcTBbNB\)BYBYBYBYBT�BT�BP�BO�BP�BN�BP�BN�BN�BO�BN�BN�BN�BN�BM�BN�BN�BN�BO�BM�BO�BP�BQ�BT�BS�BQ�BR�BR�BS�BP�BL�BJ�BC�B>wB=qBA�B=qB9XB2-B-B-B)�B'�B(�B'�B&�B&�B%�B'�B&�B'�B%�B$�B(�B'�B&�B&�B&�B%�B'�B)�B+B+B,B,B,B/B.B1'B33B2-B33B6FB8RB9XB:^B=qB?}BC�BF�BG�BJ�BL�BN�BQ�BT�BYB^5BaHBaHBaHBdZBdZBffBiyBm�Bu�By�B� B�B�=B�JB�PB�\B�hB�{B��B��B��B�B�B�-B�?B�?B�?B�?B�FB�FB�FB�FB�LB�RB�XBÖBƨBȴB��B��B��B��B��B��B��B�B�B�B�B�B�B�B�#B�)B�NB�fB�fB�B�B�B�B�B�B��B��B	B	%B	+B	DB	VB	\B	bB	oB	uB	�B	�B	�B	�B	 �B	#�B	&�B	)�B	.B	2-B	5?B	8RB	:^B	=qB	@�B	A�B	F�B	I�B	L�B	Q�B	T�B	W
B	XB	YB	YB	ZB	\)B	]/B	]/B	]/B	]/B	]/B	`BB	cTB	dZB	e`B	e`B	hsB	iyB	l�B	p�B	r�B	t�B	u�B	w�B	v�B	u�B	u�B	w�B	w�B	x�B	|�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�DB	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�?B	�FB	�FB	�LB	�^B	�wB	ŢB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�TB	�ZB	�fB	�`B	�ZB	�ZB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
�B
eB
#�B
,�B
2aB
9	B
C�B
I�B
MjB
S�B
W
B
^�B
c�B
h�B
kkB
pUB
tnB
x81111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�/B�;B�ZB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�;B�FB�SB�YB�eB�qB��B�.BFB	qB~B
xBkB�B�B&B*4B,@B0YB2eB3kB4qB4qB4qB6~B5xB8�B5yB6B@�BC�B;�B7�B1aB&B! B�B�B�7B�1B�B��B�B�B�cB�|B�B�|B�KB�	B��B��B��B�:B��B��B�SBm�BMB0nB�B�B�B
�KB
��B
�!B
��B
�YB
�B
�B
��B
��B
��B
j�B
2�B
�B
�B	�8B	��B	�B	�XB	�(B	��B	��B	WyB	+tB	�B�^B��B�AB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�`B�*B�B��B��Bz\Bs2BkBd�Bb�B_�B]�BY�BV�BU�BV�BX�BY�B[�BY�BW�BZ�B[�B[�BZ�BT~BQlBQlBQlBQlBMSBMTBI;BH5BI;BG0BI<BG0BG0BH6BG0BG0BG0BG0BF*BG0BG0BG0BH6BF+BH7BI=BJDBMVBLPBJDBKJBKJBLPBI>BE&BCB;�B6�B5�B9�B5�B1�B*�B%jB%jB"XB MB!SB MBFBFB@B MBFB MB@B:B!SB NBGBGBGBAB NB"ZB#`B#`B$fB$fB$fB'yB&sB)�B+�B*�B+�B.�B0�B1�B2�B5�B7�B;�B?B@BCBE+BG7BJJBM\BQuBV�BY�BY�BY�B\�B\�B^�Ba�Be�BnBr7Bx\B|tB��B��B��B��B��B��B��B�%B�BB�[B�[B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�B�B�%B�1B�1B�=B�UB�gB�nB�nB�nB�nB�nB�tB�zBԀBڥB޽B޽B��B��B��B��B�B�B�B�DB�tB�zB��B	�B	�B	�B	�B	
�B	�B	�B	�B	�B	 B	B	+B	=B	"PB	&gB	*�B	-�B	0�B	2�B	5�B	8�B	9�B	>�B	BB	EB	J>B	MOB	O[B	PaB	QhB	QhB	RnB	TzB	U�B	U�B	U�B	U�B	U�B	X�B	[�B	\�B	]�B	]�B	`�B	a�B	d�B	h�B	k B	mB	nB	pB	oB	nB	nB	pB	pB	q%B	u>B	yUB	|hB	}nB	~tB	zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�0B	�0B	�6B	�=B	�IB	�OB	�[B	�hB	��B	��B	��B	��B	��B	��B	��B	� B	� B	� B	� B	� B	�B	�B	�$B	�CB	�=B	�7B	�7B	�=B	�OB	�UB	�bB	�hB	�hB	�nB	�tB	րB	׆B	؍B	ٓB	۟B	ܤB	ްB	ݪB	ܥB	ܥB	۟B	ܥB	ݫB	ްB	ްB	߷B	߷B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��G�O�B	��B	�!B	��B
�B
B
%B
*�B
1QB
;�B
A�B
E�B
L?B
OQB
V�B
\B
aB
c�B
h�B
l�B
p~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20200619170909    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170909  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170909  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                