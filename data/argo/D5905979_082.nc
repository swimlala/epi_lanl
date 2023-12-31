CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:13Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170913  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               RA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�Ĝɓ�V1   @�ĝSo�@7���S��c�XbM�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    RA   B   B   @�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�RD�3D�V�D���D��D�
D�XRD�� D�ƸD��D�d{D���D��3D�  D�^Dڔ�D��D�)�D�^fD�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A(�A=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'
>B/p�B7�
B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��B��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca��Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D}pD�
Dw
D�
Dp�D�
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
Dt��Dy�\D��D�R=D��D��=D��D�S�D���D��=D�
D�` D��=D��D��D�Y�DڐRD�ФD�%qD�Y�D�D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�9XA�;dA�5?A�-A�{A��mAϮA�E�A�oA�JA�%A�
=A�bA�  A��AξwA�O�A�dZA˧�A�5?AŋDA��#Aĥ�A�XA��
A�7LA�M�A���A�dZA�ZA��A�I�A�r�A�A�+A��TA�K�A�O�A���A�l�A��A�~�A�+A��9A�C�A��jA�M�A��A�ĜA��DA�dZA�5?A��A���A�XA��A���A�;dA���A���A�33A�1A�ȴA��FA��!A���A�XA���A�M�A���A�t�A� �A��A��!A�E�A�  A���A��hA� �A��;A���A�A�A�(�A��A�A��FA���A��A�M�A��A���A��yA��A��!A��DA��A��A�=qA���A��A�O�A��A��`A���A�XA��\A�XA�|�A�|�A���A�p�A��yA�VA��A��A�9XA�r�A��DA���A���A���A�A�Q�A�+A��A��DA�oA�n�A�Q�A���A~$�A}�A}"�A}�A|�Az�/AyC�Ax�\Aw�TAv�Atv�Aq��An�Ai��Af�!Ad��AaC�A_;dA\�!A[33AY��AX��AU+APĜAN��AN �AL�AL�RAL�AK�wAJJAG�;AF �ACG�A@��A?G�A>��A=/A;�TA:�\A9&�A6�A4r�A333A2�jA2 �A1O�A0�DA/l�A-XA*�jA(��A'��A'A'A'dZA%p�A#�A ĜA"�AJAl�AȴA�wA�yA�+A33A$�AhsA��AA7LA�A��AQ�A��A�wA��A�!AbNA�mA�/AA
ĜA	XAv�AK�A��AA��A�HA��A5?Ax�A ��A M�@���@�j@��@�33@���@��#@��u@���@��@�w@�o@��H@�R@�\@�v�@�^5@�ff@��@�/@�\)@�$�@���@�K�@��@�V@�33@�/@�R@�Ĝ@��y@�/@ۥ�@ؓu@�Ĝ@ӍP@�K�@�$�@�@ёh@Л�@��@�dZ@·+@�`B@�&�@���@�  @�t�@�@ȣ�@�9X@���@�A�@�"�@���@���@�A�@���@�33@�@���@�-@��`@�\)@�ff@��@�@��h@��@��@�1'@�;d@�E�@���@��7@�p�@�O�@�  @�@��!@��@�@�x�@�`B@��/@�1'@�ƨ@�t�@�K�@�+@�"�@���@���@��T@���@���@�?}@� �@�dZ@�33@�+@�@��@��R@�x�@�1'@��@��@��
@�\)@���@���@��7@�ff@�n�@�v�@��\@��R@��y@��@��@�=q@��\@��\@���@�x�@�V@�A�@��@�ȴ@���@�5?@�-@�$�@��-@��#@��h@�V@��@��@��!@��@��@���@��7@��@��7@�x�@�?}@��`@��F@���@��@�/@��9@�j@�A�@��;@�dZ@��@���@�^5@��@���@��#@��^@��-@���@��@�O�@��9@��D@�I�@��
@��@��P@�ƨ@��u@�V@�(�@��@�o@�C�@��@���@�$�@��T@���@�hs@�&�@�/@�%@��/@��@�O�@���@��-@��7@�J@�V@��!@��\@�n�@�5?@�@��7@�p�@��@��@��@�r�@�I�@�1@��m@���@���@�@�5?@�ȴ@�~�@���@�Q�@��@�(�@�1@��@��H@�$�@�?}@��@�1'@��;@��@��P@��@�t�@�K�@�C�@�C�@�C�@�o@���@�v�@�ff@�V@�E�@�{@��^@��@�/@�&�@�&�@��@���@�Ĝ@��j@���@�j@�Z@�1@�|�@�t�@�($@�;@v��@q�N@i�-@_��@Vxl@Pg8@I�t@C�V@=\�@5�@/�q@)s�@"�@�'@��@GE@�H@J�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A�9XA�;dA�5?A�-A�{A��mAϮA�E�A�oA�JA�%A�
=A�bA�  A��AξwA�O�A�dZA˧�A�5?AŋDA��#Aĥ�A�XA��
A�7LA�M�A���A�dZA�ZA��A�I�A�r�A�A�+A��TA�K�A�O�A���A�l�A��A�~�A�+A��9A�C�A��jA�M�A��A�ĜA��DA�dZA�5?A��A���A�XA��A���A�;dA���A���A�33A�1A�ȴA��FA��!A���A�XA���A�M�A���A�t�A� �A��A��!A�E�A�  A���A��hA� �A��;A���A�A�A�(�A��A�A��FA���A��A�M�A��A���A��yA��A��!A��DA��A��A�=qA���A��A�O�A��A��`A���A�XA��\A�XA�|�A�|�A���A�p�A��yA�VA��A��A�9XA�r�A��DA���A���A���A�A�Q�A�+A��A��DA�oA�n�A�Q�A���A~$�A}�A}"�A}�A|�Az�/AyC�Ax�\Aw�TAv�Atv�Aq��An�Ai��Af�!Ad��AaC�A_;dA\�!A[33AY��AX��AU+APĜAN��AN �AL�AL�RAL�AK�wAJJAG�;AF �ACG�A@��A?G�A>��A=/A;�TA:�\A9&�A6�A4r�A333A2�jA2 �A1O�A0�DA/l�A-XA*�jA(��A'��A'A'A'dZA%p�A#�A ĜA"�AJAl�AȴA�wA�yA�+A33A$�AhsA��AA7LA�A��AQ�A��A�wA��A�!AbNA�mA�/AA
ĜA	XAv�AK�A��AA��A�HA��A5?Ax�A ��A M�@���@�j@��@�33@���@��#@��u@���@��@�w@�o@��H@�R@�\@�v�@�^5@�ff@��@�/@�\)@�$�@���@�K�@��@�V@�33@�/@�R@�Ĝ@��y@�/@ۥ�@ؓu@�Ĝ@ӍP@�K�@�$�@�@ёh@Л�@��@�dZ@·+@�`B@�&�@���@�  @�t�@�@ȣ�@�9X@���@�A�@�"�@���@���@�A�@���@�33@�@���@�-@��`@�\)@�ff@��@�@��h@��@��@�1'@�;d@�E�@���@��7@�p�@�O�@�  @�@��!@��@�@�x�@�`B@��/@�1'@�ƨ@�t�@�K�@�+@�"�@���@���@��T@���@���@�?}@� �@�dZ@�33@�+@�@��@��R@�x�@�1'@��@��@��
@�\)@���@���@��7@�ff@�n�@�v�@��\@��R@��y@��@��@�=q@��\@��\@���@�x�@�V@�A�@��@�ȴ@���@�5?@�-@�$�@��-@��#@��h@�V@��@��@��!@��@��@���@��7@��@��7@�x�@�?}@��`@��F@���@��@�/@��9@�j@�A�@��;@�dZ@��@���@�^5@��@���@��#@��^@��-@���@��@�O�@��9@��D@�I�@��
@��@��P@�ƨ@��u@�V@�(�@��@�o@�C�@��@���@�$�@��T@���@�hs@�&�@�/@�%@��/@��@�O�@���@��-@��7@�J@�V@��!@��\@�n�@�5?@�@��7@�p�@��@��@��@�r�@�I�@�1@��m@���@���@�@�5?@�ȴ@�~�@���@�Q�@��@�(�@�1@��@��H@�$�@�?}@��@�1'@��;@��@��P@��@�t�@�K�@�C�@�C�@�C�@�o@���@�v�@�ff@�V@�E�@�{@��^@��@�/@�&�@�&�@��@���@�Ĝ@��j@���@�j@�Z@�1@�|�G�O�@�($@�;@v��@q�N@i�-@_��@Vxl@Pg8@I�t@C�V@=\�@5�@/�q@)s�@"�@�'@��@GE@�H@J�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�1B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�=B
�7B
�7B
�DB
�JB
�PB
�PB
�DB
�=B
��B
�JB
�B
�B
��B
��B
��B
�^B
��B
��B
�9B
�9B
�#B
�5B
�
B
�/BDB=qB]/B�bB��B�3B�dBǮB�`B��BBDB�B!�B)�B.B2-B5?B6FB9XB=qBA�BD�BF�BL�BN�BQ�BXB^5B`BBcTBcTBcTBcTBhsBq�Bu�By�B�B�%B�1B�DB�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{By�BhsBaHB]/BVBN�BA�B!�BbB�TB��B��BɺB�dB�LB�B��B��B�JB�B|�Br�BT�B>wB�B
�TB
ȴB
�FB
�7B
w�B
bNB
?}B
49B
33B
2-B
.B
&�B
�B
hB
JB
B	�B	�HB	��B	�!B	��B	�VB	|�B	l�B	_;B	S�B	K�B	D�B	/B	�B	
=B	%B	B	  B	B	bB	+B��B�B�BB��B�wB�XB�'B�B��B��B��B��B��B��B�{B�oB�PB�=B�+B{�Bx�Bs�Br�Br�Bq�Bm�BffB`BB]/BZBXBW
BT�BQ�BP�BP�BN�BM�BL�BJ�BJ�BJ�BH�BG�BH�BF�BF�BD�BD�BC�BE�BC�BF�BF�BE�BD�B@�B=qB>wB>wB=qB=qB<jB<jB:^B@�B>wB=qB=qB<jB=qB:^B:^B<jB<jB=qB>wB>wB>wB>wB@�BB�BC�BF�BH�BH�BH�BG�BH�BI�BM�BL�BL�BM�BN�BI�BJ�BK�BI�BG�BI�BN�BR�B\)B\)B_;BdZBjBjBjBk�BjBhsBl�BjBiyBhsBiyBk�Bq�Bs�Bv�Bv�Bx�Bx�Bx�B|�B�B�B�B�%B�1B�1B�+B�1B�7B�1B�DB�\B�oB�uB�{B��B��B��B��B��B��B�B�3B�3B�-B�'B�'B�'B�9B�XB�RB�jB�qB�}B��B�wB�}B��BÖBɺB��B��B��B��B��B��B��B�B�ZB�B��B	+B	DB	JB	\B	oB	�B	�B	�B	'�B	/B	6FB	8RB	9XB	9XB	7LB	5?B	5?B	9XB	<jB	>wB	D�B	G�B	N�B	P�B	P�B	Q�B	S�B	W
B	YB	YB	YB	YB	YB	YB	ZB	[#B	\)B	[#B	YB	VB	VB	W
B	ZB	]/B	^5B	`BB	aHB	aHB	aHB	cTB	e`B	iyB	m�B	m�B	n�B	o�B	p�B	r�B	r�B	s�B	s�B	u�B	w�B	y�B	� B	�%B	�B	�B	�B	�+B	�+B	�+B	�=B	�7B	�7B	�DB	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�FB	�FB	�RB	�^B	�jB	�qB	�jB	�jB	�jB	�^B	�qB	�jB	�jB	��B	��B	��B	�}B	�wB	�}B	�wB	�qB	�dB	�^B	�XB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	�wB	�wB	�}B	�}B	B	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�CB	�B
1B
NB
IB
)B
0oB
7fB
>B
DB
KB
N�B
V�B
\xB
a�B
d�B
i�B
m�B
p�B
wf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
�B
�B
�B
�B
�B
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
�B
��B
z�B
z�B
�B
�B
�fB
��B
�NB
�xB
��B
��B
ӣB
ֵB
ϋB
հB�B5�BU�B��B�GB��B��B�"B��B�AB�wB�B�B:B"kB&�B*�B-�B.�B1�B5�B9�B=
B?BE:BGFBJYBP}BV�BX�B[�B[�B[�B[�B`�BjBn/BrFBzwB~�B��B��B��B��B��B��B�B�B�(B�;B�;B�;B�GB�MB�GB�SB�`B�SB�MB�GB�;B�/B�(B�/B�B�B��BrHB`�BY�BU�BNsBGIB9�B>B�B��B�eB�RB�3B��B��B��B�YB�B��B{�BulBk/BMB6�BB
��B
�?B
��B
��B
p_B
Z�B
8B
,�B
+�B
*�B
&�B
B
$B

 B
�B	��B	�PB	��B	�^B	��B	�@B	��B	u�B	e.B	W�B	L�B	DmB	=BB	'�B	DB	�B��B��B��B��B		B��B��B�cB��BƂB�'B�	B��B��B��B��B��B�xB�GB�;B�/B�#B�B��B�Bt�Bq�BlmBkgBkgBjaBfIB_BX�BU�BR�BP�BO�BM�BJ�BI�BI�BG�BF�BE�BC}BC}BC}BApB@jBApB?eB?eB=YB=YB<SB>_B<SB?eB?eB>`B=ZB9AB6/B76B76B60B60B5)B5)B3B9BB76B60B60B5)B60B3B3B5*B5*B61B77B77B77B77B9CB;OB<VB?gBAsBAsBAtB@nBAtBBzBF�BE�BE�BF�BG�BB{BC�BD�BB{B@oBB{BG�BK�BT�BT�BW�B]Bc?Bc?Bc?BdEBc?Ba3BeKBc?Bb9Ba3Bb9BdEBjjBlvBo�Bo�Bq�Bq�Bq�Bu�By�B}�B}�B~�B��B��B�B��B��B��B�B�B�.B�4B�:B�eB�eB�kB�wB��B��B��B��B��B��B��B��B��B��B�B�B�'B�.B�:B�@B�4B�:B�@B�SB�wBĄBƏBʨB̴B̴BͺBͺB��B�B�_B��B��B	�B	B	B	(B	?B	RB	^B	 �B	'�B	.�B	1	B	2B	2B	0B	-�B	-�B	2B	5!B	7-B	=RB	@dB	G�B	I�B	I�B	J�B	L�B	O�B	Q�B	Q�B	Q�B	Q�B	Q�B	Q�B	R�B	S�B	T�B	S�B	Q�B	N�B	N�B	O�B	R�B	U�B	V�B	X�B	Y�B	Y�B	Y�B	\	B	^B	b.B	fFB	fFB	gMB	hRB	iXB	kdB	kdB	ljB	ljB	nwB	p�B	r�B	x�B	~�B	}�B	z�B	{�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�"B	�.B	�@B	�RB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�B	�B	�B	�"B	�B	�B	�3B	�9B	�3B	�.B	�(B	�.B	�(B	�"B	�B	�B	�	B	�B	�	B	�B	�B	�B	�B	�"B	�(B	�(B	�.B	�.B	�@B	�MB	�MB	�MB	�SB	�SB	�YB	�_B	�dB	�jB	�qB	�qB	�qB	�}B	ǉB	ȏB	ȏB	ʜB	ʜB	ˢB	ͮG�O�B	��B	��B	��B
 �B
	�B
�B
!�B
)B
0B
6�B
<�B
C�B
G�B
O3B
U$B
Z\B
]:B
bXB
f�B
i�B
p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170913    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170913  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170913  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                