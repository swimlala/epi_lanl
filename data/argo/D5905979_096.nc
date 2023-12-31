CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:02Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141402  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               `A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��z�1   @���@6=�E���c�dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    `A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B  B   B*ffB-��B8  B@ffBG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�$�D�\{D��)D�ڏD�HD�Y�D��D��=D�\D�MD���D�њD���D�[�DڝD��qD�
D�S3D��D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��B�
Bp�Bp�Bp�B)�
B-
>B7p�B?�
BG
>BO
>BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB��B��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��D w
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
D�pDw
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
DB�DCw
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
D`}pD`�
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
Dt�=Dy� D� RD�X D���D��D��D�UD���D���D��D�H�D��=D��D��qD�W
Dژ�D���D��D�N�D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�7LA�-A�&�A�;dAܗ�A܍PA܅A܅A�~�A�XA�9XA��A��;A۰!Aە�A�p�A�JA��A�VA���A̟�A��TA�  A��yA�\)A�1A�XA�bNA��A��uA���A�E�A�p�A��wA�;dA��A�ffA�ȴA�ffA�bA�&�A�JA���A�v�A�?}A���A���A�~�A�VA�ĜA���A�ZA���A�A�n�A���A��
A�O�A��+A�C�A�1A��PA�bNA�bA���A��A�`BA�bA��HA�+A�bA��^A���A�G�A���A�K�A�"�A��`A��hA��/A�%A���A�-A�bA��9A���A��A�O�A���A���A�VA�-A�oA�O�A�hsA���A���A�l�A�r�A�K�A~1'Az�AwG�At=qAq�Am�mAl��Ai"�AgO�Ael�AcK�Ab5?AaVA_��A]%A[�mA[�AZ��AY�AXbNAV�RAU�-AU�hAT��ARjAP��AOl�AM�#AM�PAM�AL�HAL=qAKAJ�!AI��AGdZAFVAE`BADz�AC�mABM�AA�A?�A>  A;
=A:A9�hA9K�A8�RA7�^A5�A4��A41'A3�;A3hsA2��A1��A0�yA0�A0{A.n�A-A,1'A+XA*�HA*�A)VA(9XA'l�A'"�A&ȴA&�A%"�A#�TA#�A"A�A ��A�A�yA�AK�Az�AĜA�^A��A$�A�A+AI�A�A�PAE�A�A�9A(�A��A��AhsA+A�DA  A
�A	"�A1'A�Ax�A�A�A�yA�DA-A�An�A1A�PAn�A�7A �A �uA bN@��w@�~�@�t�@�ƨ@�G�@�P@��H@��@�n�@��@�%@�S�@�bN@���@⟾@��@�z�@���@�\)@�o@ް!@�@�|�@ڟ�@�=q@��T@�G�@�1'@�S�@���@�x�@���@�O�@ϕ�@��`@���@��@ɉ7@�j@��m@Ǿw@�\)@Ɨ�@�V@�X@��@�C�@+@��-@���@���@��u@�bN@���@�@�E�@���@�O�@���@��m@�`B@�r�@�A�@���@���@�J@���@��j@�j@�(�@��
@���@��@�\)@��\@��@��@���@��@��+@��@��j@�1'@���@��P@���@��P@�|�@��@���@�x�@�X@�z�@�1'@�9X@���@��\@���@��D@�(�@��w@�o@��+@�p�@���@���@�33@�ȴ@�@��@�O�@��/@���@�Z@��@���@��w@�S�@�@���@���@���@��h@��h@��h@��7@�?}@���@��@�j@�A�@��@��m@��F@�t�@�+@�@��@���@��+@�^5@�{@��h@�O�@�V@�z�@�1'@��m@��F@���@�|�@�S�@�"�@�v�@�n�@�V@��@��@�x�@�j@�1'@�1'@��j@��/@���@���@�bN@��@�V@�J@�p�@�hs@�%@�/@��@���@���@��h@�p�@���@���@��D@�(�@��@�r�@���@��7@�7L@���@�I�@� �@�1'@�Q�@���@�&�@��@���@��9@�z�@�1'@�1@���@��P@�|�@�t�@�dZ@�"�@��y@���@���@�v�@�n�@�ff@�n�@�ff@�^5@�M�@��@��@���@��@�`B@�V@���@�bN@�(�@��@���@���@�t�@�l�@�C�@�+@��@��+@�=q@�J@��@��h@�7L@���@��9@�r�@�9X@�1@��w@���@�C�@�
=@��R@���@�~�@�~�@�v�@�^5@�5?@�@���@��T@���@��@�X@�G�@�&�@�&�@�&�@�&�@��@�@v �@m�@b�H@]0�@T��@N_�@G1�@A��@:H�@3'�@-��@'��@"3�@�$@I�@b@	l@2a@	[W@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�1'A�7LA�-A�&�A�;dAܗ�A܍PA܅A܅A�~�A�XA�9XA��A��;A۰!Aە�A�p�A�JA��A�VA���A̟�A��TA�  A��yA�\)A�1A�XA�bNA��A��uA���A�E�A�p�A��wA�;dA��A�ffA�ȴA�ffA�bA�&�A�JA���A�v�A�?}A���A���A�~�A�VA�ĜA���A�ZA���A�A�n�A���A��
A�O�A��+A�C�A�1A��PA�bNA�bA���A��A�`BA�bA��HA�+A�bA��^A���A�G�A���A�K�A�"�A��`A��hA��/A�%A���A�-A�bA��9A���A��A�O�A���A���A�VA�-A�oA�O�A�hsA���A���A�l�A�r�A�K�A~1'Az�AwG�At=qAq�Am�mAl��Ai"�AgO�Ael�AcK�Ab5?AaVA_��A]%A[�mA[�AZ��AY�AXbNAV�RAU�-AU�hAT��ARjAP��AOl�AM�#AM�PAM�AL�HAL=qAKAJ�!AI��AGdZAFVAE`BADz�AC�mABM�AA�A?�A>  A;
=A:A9�hA9K�A8�RA7�^A5�A4��A41'A3�;A3hsA2��A1��A0�yA0�A0{A.n�A-A,1'A+XA*�HA*�A)VA(9XA'l�A'"�A&ȴA&�A%"�A#�TA#�A"A�A ��A�A�yA�AK�Az�AĜA�^A��A$�A�A+AI�A�A�PAE�A�A�9A(�A��A��AhsA+A�DA  A
�A	"�A1'A�Ax�A�A�A�yA�DA-A�An�A1A�PAn�A�7A �A �uA bN@��w@�~�@�t�@�ƨ@�G�@�P@��H@��@�n�@��@�%@�S�@�bN@���@⟾@��@�z�@���@�\)@�o@ް!@�@�|�@ڟ�@�=q@��T@�G�@�1'@�S�@���@�x�@���@�O�@ϕ�@��`@���@��@ɉ7@�j@��m@Ǿw@�\)@Ɨ�@�V@�X@��@�C�@+@��-@���@���@��u@�bN@���@�@�E�@���@�O�@���@��m@�`B@�r�@�A�@���@���@�J@���@��j@�j@�(�@��
@���@��@�\)@��\@��@��@���@��@��+@��@��j@�1'@���@��P@���@��P@�|�@��@���@�x�@�X@�z�@�1'@�9X@���@��\@���@��D@�(�@��w@�o@��+@�p�@���@���@�33@�ȴ@�@��@�O�@��/@���@�Z@��@���@��w@�S�@�@���@���@���@��h@��h@��h@��7@�?}@���@��@�j@�A�@��@��m@��F@�t�@�+@�@��@���@��+@�^5@�{@��h@�O�@�V@�z�@�1'@��m@��F@���@�|�@�S�@�"�@�v�@�n�@�V@��@��@�x�@�j@�1'@�1'@��j@��/@���@���@�bN@��@�V@�J@�p�@�hs@�%@�/@��@���@���@��h@�p�@���@���@��D@�(�@��@�r�@���@��7@�7L@���@�I�@� �@�1'@�Q�@���@�&�@��@���@��9@�z�@�1'@�1@���@��P@�|�@�t�@�dZ@�"�@��y@���@���@�v�@�n�@�ff@�n�@�ff@�^5@�M�@��@��@���@��@�`B@�V@���@�bN@�(�@��@���@���@�t�@�l�@�C�@�+@��@��+@�=q@�J@��@��h@�7L@���@��9@�r�@�9X@�1@��w@���@�C�@�
=@��R@���@�~�@�~�@�v�@�^5@�5?@�@���@��T@���@��@�X@�G�@�&�@�&�@�&�@�&�G�O�@�@v �@m�@b�H@]0�@T��@N_�@G1�@A��@:H�@3'�@-��@'��@"3�@�$@I�@b@	l@2a@	[W@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB.B0!B.B+B49Bu�B�B�%B�+B�%B�B�+B�DBZB\)BO�B33B�BD�B�
BJB��B�dB�3BÖB��BȴB��B��B��BB��B�/B��B�BŢB�FB��B��B�9B��B��B�%Bv�Bz�B�B�=B�B�+B�JB��B�DB�%B�B�B�7B�PB�=B�JBt�Bn�Bq�B�Bz�BVBN�B%�B'�B�B�B�B�B�BǮB��BǮB�dB�jB�LB�3B��B��B�Bp�Bm�BgmBP�BJ�B/B�B{B
��B
�B
�fB
��B
ÖB
�LB
�-B
��B
�DB
x�B
cTB
I�B
)�B
uB	��B	�TB	�5B	��B	�^B	�B	��B	�bB	�=B	�B	t�B	hsB	aHB	_;B	]/B	T�B	M�B	G�B	D�B	H�B	=qB	1'B	)�B	#�B	!�B	"�B	#�B	#�B	 �B	�B	�B	1B	+B	B��B��B	1B��B��B�B�5B�B�B�B�B�B��BɺBƨBÖB��B�qB�LB�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�PB�=B�+B�B~�B{�Bw�Bt�Br�Bn�BjBgmBe`BbNB_;B[#BZBYBYBW
BT�BR�BS�BP�BN�BM�BM�BK�BI�BE�BB�BB�BD�BD�BD�BD�BD�BB�BC�BB�BA�BA�BC�BA�B@�B?}B?}B>wB>wB>wB>wB>wB>wB>wB=qB=qBA�B?}BB�BB�BA�B?}B=qB@�B?}B?}B?}B>wB>wB@�B=qB<jB<jB<jB<jB;dB;dB:^B;dB?}BB�BF�BF�BH�BI�BL�BL�BL�BL�BK�BK�BL�BM�BP�BR�BT�BS�BW
BYBZBZB]/B^5B_;B`BB`BBaHBk�BjBk�Bl�Bo�Bo�Bp�Bq�Bq�Bq�Bq�Br�Bs�Bv�Bz�B}�B}�B�B�B�B�+B�JB�PB�\B�bB�bB�hB�oB�{B��B��B��B��B��B��B��B��B�B�!B�-B�XBÖBĜB��B��B��B�B�#B�HB�`B�fB�yB�B�B�B�B�B��B��B��B	B	B	B	B	B	B	+B		7B	JB	bB	hB	uB	�B	�B	�B	�B	�B	�B	"�B	$�B	&�B	+B	2-B	49B	8RB	=qB	@�B	I�B	J�B	J�B	K�B	O�B	S�B	W
B	]/B	^5B	bNB	hsB	iyB	gmB	k�B	p�B	v�B	y�B	|�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�VB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�?B	�RB	�RB	�RB	�^B	�dB	�jB	�wB	�}B	�}B	�}B	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�HB	�HB	�NB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
(B
@B
B
#:B
%�B
6B
;�B
D�B
HB
M�B
S�B
ZB
_�B
gB
j�B
oB
s3B
wfB
{B
~�444444444444444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BMB�{B�]B�-B��B��B��B��B��B��B��B��B�*B��B��B��B�DB��B��B�8B��B��B}(Bm�Bq�ByB�@B|"B~.B�MB��B�GB}(BzBx
B�:B�SB�AB�NBk�Be�Bh�ByBq�BMBE�B�B�B�B�B�"B�B�/B��B��B��B�yB�B�aB�HB�B��BxBg�Bd�B^�BHBA�B&;B�B�B
��B
��B
݋B
�B
��B
�uB
�VB
��B
�qB
pB
Z�B
@�B
!0B

�B	�B	ڎB	�pB	�B	��B	�ZB	��B	��B	�B	yNB	l B	_�B	X�B	V�B	TuB	LDB	EB	>�B	;�B	?�B	4�B	(pB	!FB	"B	B	B	"B	"B	B	�B	�B�~B�yB�ZB�<B�0B�B�IB�B��BՇB�iB�iB�iB�iB�WB�,B�B��B��B��B��B��B��B�wB�dB�SB�SB�4B�B�B�)B�B�B��B��B��B��B��B��B��B~�B{tBvUBsCBo+BlBjBe�Ba�B^�B\�BY�BV�BR�BQ|BPwBPwBNjBL^BJRBKYBHFBF:BE4BE4BC)BAB=B9�B9�B;�B;�B;�B;�B;�B9�B:�B9�B8�B8�B:�B8�B7�B6�B6�B5�B5�B5�B5�B5�B5�B5�B4�B4�B8�B6�B9�B9�B8�B6�B4�B7�B6�B6�B6�B5�B5�B7�B4�B3�B3�B3�B3�B2�B2�B1�B2�B6�B9�B>B>B@BA!BD4BD4BD4BD4BC.BC.BD4BE:BHLBJYBLeBK_BNqBP~BQ�BQ�BT�BU�BV�BW�BW�BX�Bb�Ba�Bb�Bc�BgBgBh
BiBiBiBiBjBkBn/BrGBuZBuZBzwBzwB|�B~�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�GB�qB��B��B��B��B��B�)B�;B�SB�qB҄BةB��B��B��B��B��B��B�B�
B�B�(B�@B�eB�qB�qB�qB�qB�xB��B	 �B	�B	�B	�B	
�B	�B	�B		B	B	B	B	.B	:B	FB	"^B	)�B	+�B	/�B	4�B	7�B	AB	BB	BB	C!B	G9B	KRB	NdB	T�B	U�B	Y�B	_�B	`�B	^�B	b�B	g�B	n!B	q2B	tEB	x]B	ziB	ziB	ziB	ziB	ziB	}|B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�KB	�bB	�bB	�bB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�%B	�1B	�>B	�DB	�PB	�VB	�\B	�iB	�iB	�iB	�oB	�oB	�{B	ԁB	ԁB	ՇB	֍B	ؙB	ؙB	ٟB	۫B	ܱB	ݷB	޾B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�G�O�B	��B
wB

�B
mB
�B
MB
-_B
3B
<7B
?dB
D�B
J�B
QOB
WB
^jB
b4B
fgB
jB
n�B
rbB
v444444444444444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200618141402    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141402  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141402  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                