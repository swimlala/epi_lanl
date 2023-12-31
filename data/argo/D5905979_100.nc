CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:17Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170917  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               dA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��8!nP1   @���}6�@79�+�cap��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    dA   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBo��Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D�fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�HD��D�@�D���D���D�$�D�S�D���D��3D�*�D�`�D��{D���D��D�P Dژ�D��D�qD�VD��D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@�Q�@��AA=A]A}A��HA��HA��HA��HA�{A��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_�
Bg�
Bo
>Bwp�Bp�B��RB��RB��RB��RB��RB��B��B��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��B��C�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ��CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���D w
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
D�Dw
D�
Dw
D�
D}pD�
Dw
D�
Dw
D�
Dw
D�
Dw
D�Dw
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
D>}pD>�
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
DKp�DK�
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
Dc�Ddw
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
Dt��Dy�RD�)D�<)D��D��qD� RD�O
D��{D�θD�&D�\)D�� D��\D�
D�K�Dڔ{D��
D��D�Q�D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�5?A�1'A�9XA�A�A�G�A�E�A�K�A�O�A�M�A�VA�\)A�VA�XA�VA�XA�ZA�\)A�9XA��`A���A�33A�A��/A�M�A�ffA�jAӁA���AѬA��A���A���A�dZA���A�?}A�dZA�=qA��#Ać+A�=qA��#A��A��DA��jA�1A�ZA�Q�A��PA��A��wA�=qA��A���A���A��A��mA�`BA���A���A�C�A��^A�XA�K�A�|�A�$�A���A�Q�A��A�%A�XA�bA�
=A�bA�ffA�33A��\A��\A�ZA���A���A��-A���A��^A�1A���A��uA���A���A��9A�XA�ZA��+A�dZA�t�A���A�+A��A��mA���A�Q�A�9XA��
A�|�A�VA��7A�1'A�(�A� �A��;A�ƨA�bNA�JA��A�JAz�Av�AuO�Ar=qAp5?Al~�Ai�Ae�wAc�mAb^5A`VA]XAY��AX9XAV�AV9XAT{APANM�AM|�AM?}AMAL�`AL�/AL��AL�AL�uALAI�AE��ADz�ACx�AB�uAA�AA��AA/A@��A@(�A?��A>�A=�A<�A9x�A7�A5x�A3�A2�`A1�TA/VA-��A-33A+\)A)��A)7LA(jA&�`A%��A%�A$��A#S�A"~�A"$�A!XA ��A ��A ��A �/A 1'A   A�+A�mA�7Ap�A?}A^5A1A�TA�
A��A��AA��A�A�hAA5?A��A;dA�/A��A�A��AA
�RA�yA�RA��A�\A�wA�A%A�-A ��A ^5A �@��@��D@�I�@��R@��/@�@���@�ƨ@�dZ@�-@�ƨ@�S�@���@띲@�\@陚@� �@�!@�J@�7L@���@�^5@�V@�Z@ߕ�@ޏ\@�{@ݲ-@��@�9X@��@ە�@ڸR@�V@��
@�K�@�V@��@�b@�+@��y@�=q@�/@϶F@�V@̴9@ʸR@��T@��@ǥ�@�ȴ@�^5@�=q@�$�@��@�@���@��`@�(�@��m@�l�@�ff@��h@�I�@�S�@���@�M�@�&�@�Q�@��@��@�V@�@�X@���@�1'@�z�@��j@���@�Ĝ@�7L@��T@���@�S�@���@�C�@�o@���@�v�@�%@��w@�~�@�?}@���@�Ĝ@�I�@��;@���@�o@��#@���@�/@��9@�1'@�ƨ@�|�@��T@�?}@���@���@��@�I�@�b@��m@�ƨ@�K�@���@�{@��#@���@���@��h@�`B@�V@���@�1'@��w@�"�@�o@��@�33@��@���@��+@��\@���@�t�@�x�@�7L@�7L@�O�@��@���@�@���@��@�7L@��/@��@��@��!@���@���@���@�J@�J@��#@�x�@��@��/@���@�C�@�ff@�J@���@�@�J@��#@�@��7@���@�Q�@�9X@�b@�1@�  @��
@��F@���@��P@��P@�dZ@���@�v�@�ff@�^5@�@�G�@���@��D@�r�@��@��@���@���@�%@���@�O�@��@��@���@��@�z�@�r�@�bN@�1'@�1'@��m@�^5@�Z@�|�@�\)@���@��@��@�33@��@��@��H@��@��@�v�@�M�@�ff@�^5@�V@�V@�5?@�=q@�-@�J@��T@��^@���@���@�G�@�&�@���@�r�@�bN@�1'@�  @��
@���@��@��R@��\@�v�@�M�@�=q@�$�@��@�J@��#@��h@�X@�/@��@���@��`@�Ĝ@��u@��D@�r�@�bN@�A�@�Q�@�Q�@�I�@�A�@�J#@yq@p�e@d�.@]�-@W��@Qhs@I�@D1'@=�S@8�_@4q@+�m@%=�@!��@��@��@P�@�W@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�5?A�1'A�9XA�A�A�G�A�E�A�K�A�O�A�M�A�VA�\)A�VA�XA�VA�XA�ZA�\)A�9XA��`A���A�33A�A��/A�M�A�ffA�jAӁA���AѬA��A���A���A�dZA���A�?}A�dZA�=qA��#Ać+A�=qA��#A��A��DA��jA�1A�ZA�Q�A��PA��A��wA�=qA��A���A���A��A��mA�`BA���A���A�C�A��^A�XA�K�A�|�A�$�A���A�Q�A��A�%A�XA�bA�
=A�bA�ffA�33A��\A��\A�ZA���A���A��-A���A��^A�1A���A��uA���A���A��9A�XA�ZA��+A�dZA�t�A���A�+A��A��mA���A�Q�A�9XA��
A�|�A�VA��7A�1'A�(�A� �A��;A�ƨA�bNA�JA��A�JAz�Av�AuO�Ar=qAp5?Al~�Ai�Ae�wAc�mAb^5A`VA]XAY��AX9XAV�AV9XAT{APANM�AM|�AM?}AMAL�`AL�/AL��AL�AL�uALAI�AE��ADz�ACx�AB�uAA�AA��AA/A@��A@(�A?��A>�A=�A<�A9x�A7�A5x�A3�A2�`A1�TA/VA-��A-33A+\)A)��A)7LA(jA&�`A%��A%�A$��A#S�A"~�A"$�A!XA ��A ��A ��A �/A 1'A   A�+A�mA�7Ap�A?}A^5A1A�TA�
A��A��AA��A�A�hAA5?A��A;dA�/A��A�A��AA
�RA�yA�RA��A�\A�wA�A%A�-A ��A ^5A �@��@��D@�I�@��R@��/@�@���@�ƨ@�dZ@�-@�ƨ@�S�@���@띲@�\@陚@� �@�!@�J@�7L@���@�^5@�V@�Z@ߕ�@ޏ\@�{@ݲ-@��@�9X@��@ە�@ڸR@�V@��
@�K�@�V@��@�b@�+@��y@�=q@�/@϶F@�V@̴9@ʸR@��T@��@ǥ�@�ȴ@�^5@�=q@�$�@��@�@���@��`@�(�@��m@�l�@�ff@��h@�I�@�S�@���@�M�@�&�@�Q�@��@��@�V@�@�X@���@�1'@�z�@��j@���@�Ĝ@�7L@��T@���@�S�@���@�C�@�o@���@�v�@�%@��w@�~�@�?}@���@�Ĝ@�I�@��;@���@�o@��#@���@�/@��9@�1'@�ƨ@�|�@��T@�?}@���@���@��@�I�@�b@��m@�ƨ@�K�@���@�{@��#@���@���@��h@�`B@�V@���@�1'@��w@�"�@�o@��@�33@��@���@��+@��\@���@�t�@�x�@�7L@�7L@�O�@��@���@�@���@��@�7L@��/@��@��@��!@���@���@���@�J@�J@��#@�x�@��@��/@���@�C�@�ff@�J@���@�@�J@��#@�@��7@���@�Q�@�9X@�b@�1@�  @��
@��F@���@��P@��P@�dZ@���@�v�@�ff@�^5@�@�G�@���@��D@�r�@��@��@���@���@�%@���@�O�@��@��@���@��@�z�@�r�@�bN@�1'@�1'@��m@�^5@�Z@�|�@�\)@���@��@��@�33@��@��@��H@��@��@�v�@�M�@�ff@�^5@�V@�V@�5?@�=q@�-@�J@��T@��^@���@���@�G�@�&�@���@�r�@�bN@�1'@�  @��
@���@��@��R@��\@�v�@�M�@�=q@�$�@��@�J@��#@��h@�X@�/@��@���@��`@�Ĝ@��u@��D@�r�@�bN@�A�@�Q�@�Q�@�I�G�O�@�J#@yq@p�e@d�.@]�-@W��@Qhs@I�@D1'@=�S@8�_@4q@+�m@%=�@!��@��@��@P�@�W@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�`B	��B	�NB	�B	�ZB	��B	�XB	�B	ȴB	ǮB	��B
%�B
)�B
49B
=qB
F�B
W
B
�+B
�JB
�JB
�JB
��B
��B
�mB
��BBbB�BW
B^5B\)BhsBv�B� B�B� B{�B� B�=B�7B��B�-B�BȴB��B�B�B�?B�FBɺBBǮB�dB��B��BB��B��B��B�3B�B��B��B�+B{�B�1B�\B�'B�B1B  B�sB�LB�FB��B�Br�BgmBW
BF�B
�sB
ɺB
�}B
�3B
��B
�PB
v�B
C�B
�B
bB
PB
1B
B
%B	��B	��B	�LB	��B	��B	�B	k�B	R�B	<jB	5?B	,B	"�B	\B�B�B�B�yB�mB�#B��B��B��B��B��B��B��B��BɺBŢB��B��B��B��B�uB�hB�bB�bB�\B�\B�VB�JB�JB�JB�=B�7B�7B�=B�hB�\B�DB�B�B{�By�Bv�Bu�Br�Bp�Bp�Bn�Bn�Bm�Bm�BjBhsBhsBt�B}�B}�B|�B�B�7B�1B�7B�\B��B��B��B��B��B�9B�B�B��B�\B�+B�B�+B�B�B�+B�Bx�Bo�BjB_;BP�BC�B@�BC�BA�B?}B:^B49B5?B7LB7LB49B2-B33B2-B0!B0!B/B.B/B-B,B.B-B,B-B0!B1'B0!B1'B33B5?B7LB7LB9XB;dB<jB<jB=qB?}B@�B@�BA�BC�BC�BC�BF�BG�BH�BG�BG�BF�BF�BE�BC�BC�BC�BF�BK�BN�BN�BN�BN�BO�BO�BO�BO�BR�BR�BR�BS�BW
BYB\)B^5B_;BaHBe`BgmBhsBk�Bm�Bn�Bp�Bz�Bz�B}�B� B�B�1B�bB��B��B��B��B�9B�B��B��B��B��B��B��B��B��B��B�B�B�B�?B�FB�RB�dB�wB��B��B��B��B��B��B��B��B�B�
B�B�B�/B�BB�NB�ZB�ZB�`B�mB�B�B�B�B��B��B��B��B	B	B	B	B	+B	bB	bB	bB	{B	�B	�B	�B	!�B	#�B	$�B	(�B	+B	/B	33B	:^B	;dB	A�B	D�B	F�B	G�B	H�B	H�B	I�B	I�B	I�B	I�B	H�B	L�B	N�B	Q�B	S�B	R�B	S�B	R�B	R�B	W
B	[#B	\)B	\)B	_;B	cTB	ffB	iyB	jB	l�B	o�B	q�B	p�B	p�B	p�B	r�B	s�B	u�B	v�B	w�B	x�B	~�B	�B	�1B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�?B	�^B	�}B	�}B	�}B	B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�)B	�)B	�)B	�/B	�5B	�5B	�BB	�NB	�TB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B
�B
<B
�B
�B
'mB
-�B
49B
?.B
GB
M6B
N�B
T,B
\xB
\CB
`BB
e�B
j�B
q'B
s�B
vz111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�_B	�B	ƑB	��B	�5B	�
B	ąB	�B	��B	�gB	�bB	�tB
�B
 �B
*�B
4B
=TB
M�B
}�B
��B
��B
��B
�~B
�(B
�B
�vB
��BB]BM�BT�BR�B_BmbBv�Bw�Bv�Br�Bv�B��B�B�B��B��B�JB�UB�BΥB��B��B�QB�&B�EB��B�|B�!B�'B�B��B�~B��B��B�TB�*B}�Br�B~�B��B��B�LB��B��B�
B��B��B�bBx�BiQB^BM�B=LB
�B
�iB
�-B
��B
�}B
�B
mB
:PB
EB
 B
B	��B	��B	��B	�B	ǩB	�B	��B	�PB	{�B	bQB	I�B	3;B	,B	"�B	�B	1B�{B�pB�vB�QB�FB��B��B��BƺBƺBŴBŴBĮB¢B��B�~B�`B��B�zB�gB�VB�IB�CB�CB�=B�=B�7B�,B�,B�,B� B�B�B� B�KB�?B�(B|Bx�Br�Bp�Bm�Bl�Bi�Bg�Bg�BeBeBdyBdyBagB_[B_[Bk�Bt�Bt�Bs�B|B�BB�B�BB�B��B�B�B��B�B��B��B��B�CB~B|B~B|B{B~By�Bo�Bf�BajBV'BG�B:�B7rB:�B8xB6lB1NB+)B,/B.<B.<B+*B)B*$B)B'B'B&B%B&B$ B"�B%B$B"�B$B'B(B'B(B*&B,2B.?B.?B0KB2VB3\B3\B4cB6oB7uB7uB8{B:�B:�B:�B=�B>�B?�B>�B>�B=�B=�B<�B:�B:�B:�B=�BB�BE�BE�BE�BE�BF�BF�BF�BF�BI�BI�BI�BJ�BM�BP
BSBU'BV-BX:B\RB^_B_eBbwBd�Be�Bg�Bq�Bq�Bt�Bv�BzB!B�QB��B��B��B��B�&B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�4B�@B�RB�eB�qB�wB´B��B��B��B��B��B��B��B��B�	B�B�.B�:B�FB�FB�LB�YB�jB�B�B�B��B��B��B��B��B��B��B�	B�B	KB	KB	KB	dB	pB	�B	�B	�B	�B	�B	�B	!�B	&B	*B	1DB	2JB	8oB	;�B	=�B	>�B	?�B	?�B	@�B	@�B	@�B	@�B	?�B	C�B	E�B	H�B	J�B	I�B	J�B	I�B	I�B	M�B	RB	SB	SB	VB	Z8B	]JB	`\B	abB	cnB	f�B	h�B	g�B	g�B	g�B	i�B	j�B	l�B	m�B	n�B	o�B	u�B	y�B	B	�1B	�gB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�<B	�[B	�[B	�[B	�mB	��B	��B	��B	��B	êB	İB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�*B	�0B	�*B	�*B	�*B	�*B	�0B	�0B	�6B	�<B	�BB	�BB	�BB	�BB	�OB	�UB	�ZG�O�B	��B	�vB
B
�B
cB
FB
$jB
+B
6B
=�B
DB
E�B
KB
SOB
SB
WB
\�B
apB
g�B
j�B
mP111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200619170917    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170917  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170917  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                