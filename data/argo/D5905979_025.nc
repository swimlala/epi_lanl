CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:58Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170858  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؋��=�1   @؋�qր@7�|�hs�c֏\(��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB~ffB�  B�  B�33B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1y�D1��D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�Z=D���D�޸D�'\D�`�D���D��D��D�W\D���D��fD�D�`RDڋ�D��RD�=D�\�D�D�Ӆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��AA=A]A}A��HA��HA��A��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bw�
B}�
B��RB��RB��B��RB��B��B��B��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#��C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CICK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg��Ci��Ck�)Cm�)Co�)Cq�)Cs��Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��GC��C��C��GC��GC��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
�pDw
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
D �D!p�D!�
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
D/�pD0w
D0�
D1p�D1�D2w
D2�
D3w
D3�
D4w
D4�
D5}pD5�
D6w
D6�
D7w
D7�
D8p�D8�
D9w
D9�
D:w
D:�
D;w
D;�
D<p�D<�
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
DBp�DB�
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
DL}pDL�
DMw
DM�
DNw
DN�DOw
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
DW�pDX}pDX�
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
Dt��Dy��D�=D�U�D��qD��=D�"�D�\)D��HD��)D�D�R�D��D���D��D�[�Dڇ\D���D��D�XRD�3D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�I�A�O�A�M�A�Q�A�M�A�O�A�VA�Q�A�Q�A�S�A�\)A�`BA�^5A�\)A�^5A�`BA�bNA�bNA�dZA�dZA�^5A�XA�\)A�VA�O�A�K�A�G�A�A�A�$�A�A��/A�^5A�A�~�AȲ-A�\)A�"�A��PA�JA�A�jA�r�A�hsA���A� �A�hsA��RA��
A���A�Q�A�VA��DA�Q�A���A��A�K�A���A��!A��A�M�A�XA��A�K�A���A��A�G�A��yA��A�5?A�t�A���A�^5A�bA��yA�dZA���A�"�A���A�A��#A�A��
A���A�`BA��9A���A��A�M�A���A�oA���A�I�A��!A�VA��#A�`BA�+A�=qA�Q�A� �A��A�M�A�"�A��uA���A�l�A��A���A���A��^A�jA�{AK�A}�A|1Az��Ay?}As�Aq�AlȴAj��Ai?}Ah�RAg�Ad�Ab�A_�hA]�A\ȴA[l�AZn�AYS�AV�yARZAN�9AMƨAL�AI�AF�9AE?}AD�!ADbNACt�AA�^AA�A@�RA@A?��A?G�A>�+A=x�A<�A<5?A:�!A9�A9K�A7A6ffA57LA3�A2�A/�#A/;dA.5?A,r�A+�A)ƨA(�A'�FA&��A&�A&bA%�A$�9A#��A"�HA"I�A!��A v�A =qA�PA�AȴAr�A{A��A��A�PA��A��A%AVA��A��A"�A�AjA�A
=A��A^5A{A��A��AZA
z�A	�AK�A��A �A"�A�`Av�AZA{A�hA��A��A �A Q�A 1'@��P@�@��@�|�@�@�ȴ@���@���@�j@���@�V@���@�w@�J@�z�@�Z@�A�@@�5?@�hs@�j@��@�@��@�$�@�O�@��@�b@���@柾@��T@���@�D@�A�@��;@�dZ@�v�@�5?@�V@���@�@�G�@ܓu@�S�@���@ڸR@ٺ^@��@��@�l�@�ff@�j@ӝ�@��@�n�@��`@�I�@Ο�@�v�@��#@�V@�9X@�K�@�^5@�%@�I�@�dZ@¸R@��@��@���@��u@�Q�@��
@�@���@�p�@�?}@���@��T@���@��-@���@�X@�ƨ@���@�/@���@��m@�dZ@��R@�O�@�Ĝ@�%@� �@�K�@��y@���@��@���@��/@�I�@���@��@�V@�p�@�z�@�(�@��w@�9X@�9X@�1'@�|�@���@�~�@��#@�7L@�z�@���@���@���@��@��F@�S�@��@��H@�~�@�=q@�J@���@��@�%@�Ĝ@��u@��D@�Z@� �@�ƨ@�dZ@�
=@�^5@�V@�5?@�@���@�X@�/@�%@��j@�Q�@���@��w@�S�@�o@�
=@�@�@��R@�v�@�^5@��@�@��-@��7@�`B@��@��@�A�@��@���@��m@�ƨ@�ƨ@�ƨ@���@�\)@�;d@��@�@���@�M�@�$�@�@��@���@�@���@��#@��#@��#@��@���@��T@���@���@��@�@�{@���@�hs@��@�Ĝ@���@��@�r�@�bN@�b@���@�+@�@���@��y@�~�@��@���@���@���@��7@�&�@���@���@�%@�V@�Ĝ@�j@��F@�+@�"�@�
=@���@��+@��@��T@���@�hs@�`B@�G�@�/@�/@�&�@���@�1'@�1'@�b@�  @���@���@��;@���@�t�@�S�@�o@��@�~�@�=q@��@���@���@���@���@��h@�x�@�O�@�&�@�%@��@���@�
=@}��@t�@l$@c�
@]�3@T�D@L�5@E�@A8�@;�@3�P@-��@)�@%�@!+@(�@�@��@�o@
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�K�A�I�A�O�A�M�A�Q�A�M�A�O�A�VA�Q�A�Q�A�S�A�\)A�`BA�^5A�\)A�^5A�`BA�bNA�bNA�dZA�dZA�^5A�XA�\)A�VA�O�A�K�A�G�A�A�A�$�A�A��/A�^5A�A�~�AȲ-A�\)A�"�A��PA�JA�A�jA�r�A�hsA���A� �A�hsA��RA��
A���A�Q�A�VA��DA�Q�A���A��A�K�A���A��!A��A�M�A�XA��A�K�A���A��A�G�A��yA��A�5?A�t�A���A�^5A�bA��yA�dZA���A�"�A���A�A��#A�A��
A���A�`BA��9A���A��A�M�A���A�oA���A�I�A��!A�VA��#A�`BA�+A�=qA�Q�A� �A��A�M�A�"�A��uA���A�l�A��A���A���A��^A�jA�{AK�A}�A|1Az��Ay?}As�Aq�AlȴAj��Ai?}Ah�RAg�Ad�Ab�A_�hA]�A\ȴA[l�AZn�AYS�AV�yARZAN�9AMƨAL�AI�AF�9AE?}AD�!ADbNACt�AA�^AA�A@�RA@A?��A?G�A>�+A=x�A<�A<5?A:�!A9�A9K�A7A6ffA57LA3�A2�A/�#A/;dA.5?A,r�A+�A)ƨA(�A'�FA&��A&�A&bA%�A$�9A#��A"�HA"I�A!��A v�A =qA�PA�AȴAr�A{A��A��A�PA��A��A%AVA��A��A"�A�AjA�A
=A��A^5A{A��A��AZA
z�A	�AK�A��A �A"�A�`Av�AZA{A�hA��A��A �A Q�A 1'@��P@�@��@�|�@�@�ȴ@���@���@�j@���@�V@���@�w@�J@�z�@�Z@�A�@@�5?@�hs@�j@��@�@��@�$�@�O�@��@�b@���@柾@��T@���@�D@�A�@��;@�dZ@�v�@�5?@�V@���@�@�G�@ܓu@�S�@���@ڸR@ٺ^@��@��@�l�@�ff@�j@ӝ�@��@�n�@��`@�I�@Ο�@�v�@��#@�V@�9X@�K�@�^5@�%@�I�@�dZ@¸R@��@��@���@��u@�Q�@��
@�@���@�p�@�?}@���@��T@���@��-@���@�X@�ƨ@���@�/@���@��m@�dZ@��R@�O�@�Ĝ@�%@� �@�K�@��y@���@��@���@��/@�I�@���@��@�V@�p�@�z�@�(�@��w@�9X@�9X@�1'@�|�@���@�~�@��#@�7L@�z�@���@���@���@��@��F@�S�@��@��H@�~�@�=q@�J@���@��@�%@�Ĝ@��u@��D@�Z@� �@�ƨ@�dZ@�
=@�^5@�V@�5?@�@���@�X@�/@�%@��j@�Q�@���@��w@�S�@�o@�
=@�@�@��R@�v�@�^5@��@�@��-@��7@�`B@��@��@�A�@��@���@��m@�ƨ@�ƨ@�ƨ@���@�\)@�;d@��@�@���@�M�@�$�@�@��@���@�@���@��#@��#@��#@��@���@��T@���@���@��@�@�{@���@�hs@��@�Ĝ@���@��@�r�@�bN@�b@���@�+@�@���@��y@�~�@��@���@���@���@��7@�&�@���@���@�%@�V@�Ĝ@�j@��F@�+@�"�@�
=@���@��+@��@��T@���@�hs@�`B@�G�@�/@�/@�&�@���@�1'@�1'@�b@�  @���@���@��;@���@�t�@�S�@�o@��@�~�@�=q@��@���@���@���@���@��h@�x�@�O�@�&�@�%@��G�O�@�
=@}��@t�@l$@c�
@]�3@T�D@L�5@E�@A8�@;�@3�P@-��@)�@%�@!+@(�@�@��@�o@
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�
B�
B�B�
B�B�
B�
B�B�
B�
B�
B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�
B�)B�NBP�B|�B�uB��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�=B�B|�B{�Bq�Bp�BgmB]/BQ�BI�BB�B<jB6FB49B.B#�B�B�BbB	7B��B��B��B��B�NB�
B��B��BɺBÖB�?B�B��B��B�\B�+Bq�B\)BG�B&�B
�B
�)B
�B
��B
��B
��B
�hB
�+B
y�B
jB
Q�B
>wB
&�B
oB
1B	��B	�B	ŢB	�'B	��B	�B	u�B	p�B	e`B	O�B	D�B	8RB	.B	&�B	�B	�B	VB��B�5BɺB�}B��B�?B��B��B��B��B��B��B�oB�hB�VB�VB�PB�DB�=B�+B�+B�7B�1B�B�B{�B{�Bs�Bs�Br�Bp�Bq�Bn�Bo�BjBk�BjBhsBgmBffBe`BdZBcTBaHB`BB_;B^5B\)B\)B\)BYBXBW
BVBT�BQ�BP�BN�BL�BK�BI�BG�BD�BC�BC�BA�B>wB=qB<jB<jB;dB:^B8RB6FB33B33B1'B2-B2-B2-B2-B2-B2-B2-B2-B2-B33B1'B2-B1'B1'B7LB9XB:^B:^B:^B<jB=qB=qB=qB<jB;dB<jB<jB;dB;dB;dB<jB<jB=qB=qB=qB=qB>wB>wB>wB@�B@�B@�BA�BB�BB�BC�BC�BD�BC�BB�BD�BD�BD�BC�BC�BD�BG�BG�BK�BK�BM�BVB[#B]/BaHBdZBiyBl�BjBm�BbNBbNBdZBffBe`BgmBm�Bn�Bs�Br�Bt�Bt�Bs�Bs�Bt�Bw�By�B}�B}�B}�B� B}�B|�B� B�B�B� B{�B� B�+B�DB�=B�7B�1B�7B�PB�\B�bB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B�B�3B�RB�XB�FB�RB�RB�XB�LB�RB�^B��BBĜBȴB��B��B��B��B�B�B�5B�BB�`B�sB�yB�B�B�B��B��B	  B	B	B	B	1B	\B	oB	{B	�B	�B	!�B	"�B	%�B	)�B	-B	/B	33B	7LB	9XB	;dB	=qB	?}B	A�B	D�B	E�B	E�B	F�B	H�B	K�B	L�B	N�B	R�B	T�B	YB	\)B	_;B	`BB	bNB	cTB	e`B	gmB	hsB	k�B	m�B	q�B	v�B	w�B	x�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�1B	�1B	�DB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�?B	�LB	�dB	�jB	�qB	�}B	��B	B	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�)B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�TB	�*B	��B
�B
�B
B
/B
(>B
0�B
7�B
;dB
CB
K�B
Q4B
T,B
YB
\�B
a�B
g�B
kQB
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�BB�BB�HB�BB�HB�BB�BB�HB�BB�BB�BB�BB�NB�HB�HB�HB�NB�NB�NB�NB�NB�HB�HB�HB�HB�HB�NB�<B�BB�`BمBHBtB��B��B��B��B�B�B�$B�$B�B�B�B�B��B��B�B��B��B�B��B��B��B��B��B��B��B��B��B��B�pBzFBt#BsBh�Bg�B^�BTgBI%B@�B9�B3�B-�B+tB%OBB�B�B�B vB�!B�B�B��BِB�MB�(B�B��B��B��B�TB�B��B��B~tBh�BSuB>�B:B
�B
�B
�fB
��B
�B
��B
��B
~�B
q8B
a�B
IMB
5�B
MB
	�B	��B	�BB	�B	�B	��B	��B	y}B	m5B	hB	\�B	GTB	<B	/�B	%�B	bB	1B	B	�B�pBմB�:B��B�
B��B�sB�UB�BB�<B�1B�B��B��B��B��B��B��B��B~�B~�B��B�B{�Bx�BsoBsoBk?Bk?Bj:Bh.Bi4Bf"Bg)Bb
BcBb
B_�B^�B]�B\�B[�BZ�BX�BW�BV�BU�BS�BS�BS�BP�BO�BN�BM�BL�BI{BHtBFhBD\BCWBAJB?>B<,B;'B;'B9B6B5B3�B3�B2�B1�B/�B-�B*�B*�B(�B)�B)�B)�B)�B)�B)�B)�B)�B)�B*�B(�B)�B(�B(�B.�B0�B1�B1�B1�B3�B5B5B5B3�B2�B3�B3�B2�B2�B2�B3�B3�B5B5B5B5B6B6B6B8B8B8B9B:$B:$B;+B;+B<1B;+B:$B<1B<1B<1B;+B;,B<2B?CB?CBC\BC\BEhBM�BR�BT�BX�B[�BaBdBbBe%BY�BY�B[�B]�B\�B_Be&Bf-BkJBjDBlPBlPBkJBkJBlPBocBqoBu�Bu�Bu�Bw�Bu�Bt�Bw�Bx�By�Bw�Bs|Bw�B~�B��B��B��B�B��B��B��B��B��B��B�B�B�-B�EB�^B�^B�dB�jB�dB�pB�|B��B��B��B��B��B��B��B��B��B��B��B�B�!B�.B�EB�^B�jB�}B̏B͕BѭB��B��B��B�B�	B�B�-B�?B�^B�pB��B��B��B��B��B	�B		�B	B	B	9B	WB	]B	oB	!�B	$�B	&�B	*�B	.�B	0�B	2�B	4�B	7B	9B	<&B	=,B	=,B	>2B	@>B	CQB	DWB	FcB	J{B	L�B	P�B	S�B	V�B	W�B	Y�B	Z�B	\�B	^�B	_�B	cB	eB	i1B	nPB	oVB	p\B	qbB	rhB	snB	tuB	v�B	z�B	|�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�CB	�CB	�JB	�\B	�gB	�mB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�%B	�+B	�1B	�6B	�6B	�6B	�6B	�6B	�6B	�CB	�UB	�UB	�gB	�nB	�nB	�nB	�tB	̀B	͆B	ΌB	ΌB	ϒB	џB	ӪB	ӪB	԰B	նB	նB	ּB	ּB	��B	��B	��B	��B	��G�O�B	�B	�;B	�"B
rB
�B
�B
�B
(<B
.�B
2�B
:�B
CDB
H�B
K�B
P�B
TxB
YB
_B
b�B
fB
ja111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20200619170858    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170858  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170858  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                