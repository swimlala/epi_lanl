CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:41Z AOML 3.0 creation; 2016-05-31T21:48:58Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230641  20160531144859  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_012                   2C  D   APEX                            5370                            041511                          846 @�H��� 1   @�H�&�  @7���vȴ�b�I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�	�D�FfD���D�� D���D�9�D��3D��fD�  D�6fD���D���D� D�6fDچfD�� D�fD�@ D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�@��AA=A]A}A��HA��HA��A��HA��HA��HA��HA��HBp�Bp�B
>Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
Dt�pDy��D�D�A�D��RD�˅D��RD�5D�~�D���D���D�1�D��RD��RD��D�1�Dځ�D�˅D��D�;�D�k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��yA��A��TA��A��
A���A���A�ȴA���A��^A��wA��wA��jA��FA��FA�ƨA���A��DA���A��A�?}A��DA�A��
A�ȴA��;A��#A�%A��
A�A�A���A��FA�`BA�A�A��FA�jA�t�A���A�K�A��A�hsA�l�A�v�A�^5A��A��hA�ȴA���A�VA��#A���A���A�/A���A��A�dZA��
A���A�?}A�A�O�A�ƨA�1'A�dZA��A�^5A���A�ZA�-A��+A���A�oA��TA�ffA�JA��yA���A�r�A�I�A�ȴA�%A�z�A�VA�-A��7A��uA�r�A�/A�  A��HA��hA�p�A�+A��A�\)A�bNA��/A��A�G�A���A�`BA��A�z�A��A��9A��/A��+A�bNA��uA�~�A�^5A�jA��A�C�A��PA�FA~��Az�Axr�AvZAu7LAtbAr�\Ar�ApȴAl �Ai�#Ag�
Ae�7Ae�TAe�Ae�;Ae?}Ac�AaA^^5A^  AZ�uAVȴAU�ATĜAR�AQ;dAN �AK�
AH��AGx�AG�PAH��AI/AHȴAG�wAFADr�ADJABĜAAx�A@M�A?�7A?\)A>��A=?}A<=qA<$�A;ƨA;t�A;VA9?}A8bNA7ƨA6�HA5�A4r�A3�A2�DA2$�A1��A1+A0�+A/+A. �A-�A+��A*1'A(Q�A&M�A%��A%C�A$M�A#G�A"r�A!hsA ZA\)A��A�A  A\)A33A%AZA�uAƨA�A��A�AA+AbNA/A=qAVA-A+AbNA33A
A��A�
A�A�/A1A;dA��AbA�+A��A ��A bNA 1@�l�@���@�hs@�o@���@�z�@�t�@���@�@��`@��u@��@��@��@��@@�ff@���@���@�1@�^@�G�@��@�P@���@���@�1'@�t�@�z�@�z�@���@�5?@���@�o@�~�@�x�@�7L@ۅ@ܓu@۾w@���@�(�@�I�@ׅ@��@���@җ�@�7L@ёh@љ�@Ь@�  @�@�ƨ@�C�@ʇ+@�ȴ@ʰ!@�E�@�$�@ɡ�@�I�@ǍP@�V@�X@��@��@�r�@�1'@� �@î@�l�@�@���@��@��u@�1@���@��@���@�/@�b@��y@�v�@��^@��`@��
@�;d@���@�^5@��`@�dZ@��P@�hs@�$�@�=q@�?}@�j@�1@�dZ@��@�?}@�  @��@��9@���@���@�E�@�V@��T@�~�@�E�@���@�r�@��@���@��h@�?}@�r�@��@�|�@�v�@�{@��@��j@���@���@��9@�bN@�(�@�  @��;@���@�l�@�;d@�v�@�-@��@��-@���@���@�@��@�/@�$�@�n�@��`@��@��P@�l�@�"�@�"�@��@���@���@��y@�{@�`B@�%@�z�@�Z@�A�@�1@���@�t�@���@�~�@�M�@���@���@�p�@�G�@�?}@�V@��`@��`@���@���@�1'@��w@��P@�t�@�dZ@�S�@�S�@��@��@�=q@�=q@�=q@�-@��T@�@��^@���@�hs@�O�@�hs@�x�@�&�@��`@��@��u@��@��@�Z@��@��m@��m@���@��@��@�\)@�;d@��y@��y@��R@�M�@�$�@���@��-@��h@��7@�hs@��@���@��`@�Ĝ@��j@��/@���@���@��9@� �@�  @� �@�Z@�Z@�b@��;@��w@���@��@�ȴ@�n�@�^5@�E�@�@���@��h@�hs@�G�@�%@��j@�z�@�bN@�9X@���@��@��j@zn�@s�
@kƨ@cdZ@[C�@R�\@K�m@Fv�@Ax�@<j@7�P@2�H@.�@'��@#C�@��@  @z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��yA��A��TA��A��
A���A���A�ȴA���A��^A��wA��wA��jA��FA��FA�ƨA���A��DA���A��A�?}A��DA�A��
A�ȴA��;A��#A�%A��
A�A�A���A��FA�`BA�A�A��FA�jA�t�A���A�K�A��A�hsA�l�A�v�A�^5A��A��hA�ȴA���A�VA��#A���A���A�/A���A��A�dZA��
A���A�?}A�A�O�A�ƨA�1'A�dZA��A�^5A���A�ZA�-A��+A���A�oA��TA�ffA�JA��yA���A�r�A�I�A�ȴA�%A�z�A�VA�-A��7A��uA�r�A�/A�  A��HA��hA�p�A�+A��A�\)A�bNA��/A��A�G�A���A�`BA��A�z�A��A��9A��/A��+A�bNA��uA�~�A�^5A�jA��A�C�A��PA�FA~��Az�Axr�AvZAu7LAtbAr�\Ar�ApȴAl �Ai�#Ag�
Ae�7Ae�TAe�Ae�;Ae?}Ac�AaA^^5A^  AZ�uAVȴAU�ATĜAR�AQ;dAN �AK�
AH��AGx�AG�PAH��AI/AHȴAG�wAFADr�ADJABĜAAx�A@M�A?�7A?\)A>��A=?}A<=qA<$�A;ƨA;t�A;VA9?}A8bNA7ƨA6�HA5�A4r�A3�A2�DA2$�A1��A1+A0�+A/+A. �A-�A+��A*1'A(Q�A&M�A%��A%C�A$M�A#G�A"r�A!hsA ZA\)A��A�A  A\)A33A%AZA�uAƨA�A��A�AA+AbNA/A=qAVA-A+AbNA33A
A��A�
A�A�/A1A;dA��AbA�+A��A ��A bNA 1@�l�@���@�hs@�o@���@�z�@�t�@���@�@��`@��u@��@��@��@��@@�ff@���@���@�1@�^@�G�@��@�P@���@���@�1'@�t�@�z�@�z�@���@�5?@���@�o@�~�@�x�@�7L@ۅ@ܓu@۾w@���@�(�@�I�@ׅ@��@���@җ�@�7L@ёh@љ�@Ь@�  @�@�ƨ@�C�@ʇ+@�ȴ@ʰ!@�E�@�$�@ɡ�@�I�@ǍP@�V@�X@��@��@�r�@�1'@� �@î@�l�@�@���@��@��u@�1@���@��@���@�/@�b@��y@�v�@��^@��`@��
@�;d@���@�^5@��`@�dZ@��P@�hs@�$�@�=q@�?}@�j@�1@�dZ@��@�?}@�  @��@��9@���@���@�E�@�V@��T@�~�@�E�@���@�r�@��@���@��h@�?}@�r�@��@�|�@�v�@�{@��@��j@���@���@��9@�bN@�(�@�  @��;@���@�l�@�;d@�v�@�-@��@��-@���@���@�@��@�/@�$�@�n�@��`@��@��P@�l�@�"�@�"�@��@���@���@��y@�{@�`B@�%@�z�@�Z@�A�@�1@���@�t�@���@�~�@�M�@���@���@�p�@�G�@�?}@�V@��`@��`@���@���@�1'@��w@��P@�t�@�dZ@�S�@�S�@��@��@�=q@�=q@�=q@�-@��T@�@��^@���@�hs@�O�@�hs@�x�@�&�@��`@��@��u@��@��@�Z@��@��m@��m@���@��@��@�\)@�;d@��y@��y@��R@�M�@�$�@���@��-@��h@��7@�hs@��@���@��`@�Ĝ@��j@��/@���@���@��9@� �@�  @� �@�Z@�Z@�b@��;@��w@���@��@�ȴ@�n�@�^5@�E�@�@���@��h@�hs@�G�@�%@��j@�z�@�bN@�9X@���@��@��j@zn�@s�
@kƨ@cdZ@[C�@R�\@K�m@Fv�@Ax�@<j@7�P@2�H@.�@'��@#C�@��@  @z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBe`Be`Be`Be`Be`Be`Be`Be`Be`BffBffBffBffBffBhsBq�B�hB��BI�Bn�Br�Bt�Bs�B�B�{B��B�HB��B�NB�B�;B�B�/B�#B�NB�B��B�HB�
B��BǮB��BȴB��B��BÖB�qB��B�oB�{B��B�!B��B��B��B�PBz�B�B�uB�RB�FB�B�B��B�oB��B�hB�B}�BdZBD�B0!B#�B'�B'�B#�B!�B�B�B�BDB�B�`B�TB�;B��B��BB��B��B�B�yB��B��B�BYB>wB33B"�BPB��B��B�B��B�BiyBW
B9XB �BDB
�NB
�RB
�B
T�B
>wB
2-B
#�B
�B	��B	�ZB	��B	ŢB	�jB	�RB	��B	ɺB	��B	�B	r�B	[#B	r�B	y�B	|�B	v�B	iyB	VB	C�B	=qB	 �B��B�B��B��B�B�B�BȴBÖBɺB��B	\B	{B	\B	+B	B		7B	B��B��B��B��B��B�B�B�B�B�B�B�`B�BB�#B�B��B��BȴBŢBĜB��B�}B�jB�XB�FB�-B�B��B�{B�JB�+B�%B�B� B|�By�Bw�Bt�Bp�BjBe`BaHBbNBbNB`BB_;B_;B[#BXBVBQ�BP�BO�BK�BJ�BH�BD�BC�BC�BB�B@�B<jB:^B=qB?}B?}B?}B>wB:^B49B2-B49B33B33B49B33B1'B2-B1'B1'B0!B/B.B.B-B,B+B,B-B-B-B0!B/B0!B2-B1'B0!B/B0!B6FB;dB>wBM�BQ�BE�B=qB@�BK�BB�BA�BG�B_;BiyBhsBdZBdZBgmBe`BbNB^5BW
BT�BZB`BB_;B_;BZBVBVBXB^5B_;B_;BaHBaHB_;B]/B`BB_;B_;B_;BbNBe`BiyBjBk�Bk�BjBjBjBl�Bp�Bq�Bp�Bs�Bs�Bu�Bw�By�Bz�B{�B|�B�B�%B�B�+B�DB�JB�hB��B��B��B��B��B�PB�+B�+B�bB�hB�bB�DB�VB�bB�hB��B��B��B��B��B��B��B�B�'B�'B�'B�3B�FB�XB�jBÖBǮB��B��B��B��B��B�B�#B�/B�BB�BB�HB�NB�`B�B�B�B��B	B	B	B	B	B	%B		7B	
=B	JB	bB	{B	�B	�B	uB	oB	{B	�B	�B	�B	"�B	&�B	&�B	'�B	'�B	(�B	)�B	,B	/B	1'B	5?B	6FB	8RB	:^B	<jB	=qB	@�B	C�B	G�B	H�B	K�B	O�B	P�B	T�B	W
B	[#B	]/B	^5B	`BB	dZB	ffB	hsB	l�B	p�B	t�B	y�B	z�B	{�B	~�B	�B	�B	�%B	�%B	�%B	�+B	�DB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�?B	�LB	�RB	�XB	�^B	�^B	�dB	�XB	�^B	�dB	�jB	�}B	��B	B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	�)B	��B
  B
JB
�B
�B
(�B
2-B
7LB
<jB
@�B
G�B
K�B
Q�B
T�B
\)B
aHB
e`B
jB
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BekBeiBekBekBekBeiBekBekBeiBfsBfpBfuBfsBfpBh�Bq�B�wB�BI�Bn�Br�Bt�Bs�B�#B��B�B�_B��B�dB�B�MB�B�CB�6B�^B�'B�B�ZB�!B� B��B��B��B��B��BêB��B�B��B��B��B�4B��B��B��B�bBz�B�B��B�bB�YB�,B�B��B��B��B�xB�'B~BdkBD�B01B#�B( B'�B#�B!�B�B�B�BRB�B�sB�cB�IB��B�	BB��B��B�B�B��B��B�BY#B>�B3BB"�B\B��B��B�B��B�Bi�BWB9eB �BSB
�^B
�fB
�,B
UB
>�B
2DB
#�B
�B	��B	�oB	��B	źB	��B	�kB	��B	��B	��B	�4B	r�B	[>B	r�B	y�B	}	B	v�B	i�B	V!B	C�B	=�B	 �B�B��B��B��B�B�AB�2B��BùB��B��B	~B	�B	|B	KB	<B		ZB	>B�B��B��B�B��B��B�B��B��B��B��B�B�dB�BB�1B�B��B��B��B��B��B��B��B�xB�mB�QB�.B��B��B�pB�QB�JB�AB�(B}BzBw�Bt�Bp�Bj�Be�BaqBbvBbvB`iB_eB_cB[KBX8BV.BRBQBP	BK�BJ�BH�BD�BC�BC�BB�B@�B<�B:�B=�B?�B?�B?�B>�B:�B4cB2WB4dB3]B3\B4dB3]B1QB2WB1QB1RB01B/,B.<B.@B-6B,1B+,B,1B-9B-:B-:B0JB/FB0KB2WB1RB0KB/DB0LB6oB;�B>�BM�BRBE�B=�B@�BK�BB�BA�BG�B_cBi�Bh�Bd~Bd�Bg�Be�BbuB^]BW3BU$BZGB`kB_bB_dBZDBV,BV+BX8B^^B_eB_dBarBarB_dB]XB`lB_aB_bB_aBbvBe�Bi�Bj�Bk�Bk�Bj�Bj�Bj�Bl�Bp�Bq�Bp�Bs�Bs�Bu�Bw�BzB{B|B}B�3B�JB�CB�QB�jB�oB��B��B��B��B��B��B�uB�SB�TB��B��B��B�kB�|B��B��B��B� B��B� B��B��B�B�&B�NB�JB�MB�VB�jB�zB��BøB��B��B��B��B��B�B�:B�DB�RB�dB�fB�kB�rB�B�B�B�B��B	(B	9B	3B	-B	:B	GB		XB	
_B	iB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	'B	'	B	(B	(B	)B	*B	,(B	/;B	1EB	5_B	6fB	8sB	:~B	<�B	=�B	@�B	C�B	G�B	H�B	K�B	O�B	QB	UB	W'B	[@B	]KB	^SB	`_B	dvB	f�B	h�B	l�B	p�B	t�B	y�B	z�B	|B	B	�$B	�5B	�AB	�@B	�AB	�FB	�_B	�rB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�0B	�?B	�?B	�HB	�ZB	�fB	�nB	�qB	�{B	�yB	��B	�rB	�yB	�B	��B	��B	��B	ªB	ªB	ïB	ĸB	��B	��B	��B	��B	��B	�B	�BB	��B
 B
dB
�B
�B
)B
2CB
7eB
<B
@�B
G�B
K�B
RB
UB
\?B
a]B
evB
j�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.14 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448592016053114485920160531144859  AO  ARCAADJP                                                                    20140721230641    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230641  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230641  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144859  IP                  G�O�G�O�G�O�                